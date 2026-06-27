#!/usr/bin/env bash

force_wasmcli_version=0
release=0

for arg in "$@"
do
    case $arg in
        --force-wasmcli-version)
            force_wasmcli_version=1
            ;;
        --release)
            release=1
            ;;
        *)
            echo "Unknown option \"$arg\""
            exit 1
    esac
done

wasmbind_pkg_version=$(grep -ozP '(?<=name\s?=\s?\"wasm-bindgen\"\r?\nversion\s=\s\")([^"]+)' Cargo.lock | tr -d '\0')

if [[ ! -z "$wasmbind_pkg_version" ]]
then
    if [[ $force_wasmcli_version == 1 ]]
    then
        cargo install "wasm-bindgen-cli@$wasmbind_pkg_version"
        cargo_install_result=$?
        if [[ $cargo_install_result != 0 ]]
        then
            echo "Error: \"cargo install wasm-bindgen-cli@$wasmbind_pkg_version\" exited with error code $cargo_install_result"
            exit $cargo_install_result
        fi
    else 
        wasmcli_version=$(wasm-bindgen --version | sed 's/wasm-bindgen //')
        if [[ "$wasmcli_version" != "$wasmbind_pkg_version" ]]
        then
            echo "Expected wasm-bindgen-cli version: \"$wasmbind_pkg_version\", got \"$wasmcli_version\"" >&2
            echo "Pass --force-wasmcli-version to install it with cargo" >&2
            exit 1
        fi
    fi
else
    echo "Could not grep wasm-bindgen version out of Cargo.lock. Version check skipped." >&2
fi

do_release=""
target_dir="debug"
if [[ $release == 1 ]]
then
    do_release="--release"
    target_dir="release"
fi

mkdir -p web/wasm

cargo build --target wasm32-unknown-unknown -F wasm $do_release
wasm-bindgen --target web --out-dir web/wasm target/wasm32-unknown-unknown/$target_dir/cuprous.wasm
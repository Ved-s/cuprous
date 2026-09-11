#!/usr/bin/env bash

set -e

version=$(grep -ozP '(?<=name\s=\s\"wasm-bindgen\"\nversion\s=\s\")([^"]+)' Cargo.lock | tr -d '\0')
if [[ -z "$version" ]]
then
    exit 1
fi

echo "$version"

if [[ ! -z "$GITHUB_OUTPUT" ]]
then
    echo "version=$version" >> $GITHUB_OUTPUT
fi
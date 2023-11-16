# script requires rust cargo with nightly wasm32-unknown-unknown target
# and wasm-bindgen, which can be installed with cargo install wasm-bindgen-cli

cargo build --target wasm32-unknown-unknown -F wasm --release
wasm-bindgen target/wasm32-unknown-unknown/release/cuprous.wasm --out-dir web/wasm --target web
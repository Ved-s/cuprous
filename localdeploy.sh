# script requires rust cargo with nightly wasm32-unknown-unknown target
# and wasm-bindgen, which can be installed with cargo install wasm-bindgen-cli

RUSTFLAGS=--cfg=web_sys_unstable_apis cargo build --target wasm32-unknown-unknown -F wasm --release
wasm-bindgen target/wasm32-unknown-unknown/release/rls.wasm --out-dir web/wasm --target web
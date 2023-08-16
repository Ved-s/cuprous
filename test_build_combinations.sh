cargo check

cargo check -F single_thread

cargo check -F wasm --target wasm32-unknown-unknown

RUSTFLAGS=--cfg=web_sys_unstable_apis cargo check -F wasm --target wasm32-unknown-unknown
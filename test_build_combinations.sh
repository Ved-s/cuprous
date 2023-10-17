cargo check

cargo check -F single_thread

cargo check -F wasm --target wasm32-unknown-unknown

RUSTFLAGS= cargo check -F wasm --target wasm32-unknown-unknown
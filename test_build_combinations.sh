echo Native...
cargo check -q || exit
echo Native OK

echo Native single thread...
cargo check -q -F single_thread || exit
echo Native single thread OK

echo Experimental wasm...
cargo check -q -F wasm --target wasm32-unknown-unknown || exit
echo Experimental wasm OK

echo Wasm...
RUSTFLAGS= cargo check -q -F wasm --target wasm32-unknown-unknown || exit
echo Wasm OK

echo Linux...
cargo check -q --target x86_64-unknown-linux-gnu || exit
echo Linux OK

echo Windows...
cargo check -q --target x86_64-pc-windows-gnu || exit
echo Windows OK
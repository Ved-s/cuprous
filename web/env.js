// Hack for fixing weird `env.now() -> f64` import from wasm
export function now() {
    performance.now()
}
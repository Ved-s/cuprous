use parking_lot::Mutex;
use wasm_bindgen::prelude::*;

pub static INPUT_STATE: Mutex<Option<(String, String)>> = Mutex::new(None);

#[wasm_bindgen(js_namespace = cuprous)]
extern "C" {
    pub fn request_load();
    pub fn save_state(name: String, data: String);
}

#[wasm_bindgen]
pub fn set_state_input(name: String, data: String) {
    *INPUT_STATE.lock() = Some((name, data));
} 

/// (name, data)
pub fn take_state_input() -> Option<(String, String)> {
    INPUT_STATE.lock().take()
}
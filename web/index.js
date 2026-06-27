
import init, { web_main } from "./wasm/cuprous.js"

async function run() {
    await init();
    await web_main(document.getElementById("main-canvas"));
}
run()
import init, { web_main, set_state_input } from "./wasm/cuprous.js"

window.cuprous = {
    set_state_input: set_state_input,
    request_load: function() {
        let input = document.createElement("input");
        input.type = "file";
        input.oninput = async function() {
            let name = input.files[0].name;
            let text = await input.files[0].text();
            window.cuprous.set_state_input(name, text);
        }
        input.click();
    },
    save_state: function(name, text) {
        let blob = new Blob([text], {type: 'text/plain'});
        let url = URL.createObjectURL(blob);
        let a = document.createElement("a");
        a.download = name;
        a.href = url;
        a.click();
    }
}

async function run() {
    await init();
    await web_main("main");
}
run()
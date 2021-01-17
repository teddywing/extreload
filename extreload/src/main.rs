use headless_chrome;
use headless_chrome::protocol::Method;
use serde_json;
use tungstenite::{connect, Message};
use url::Url;

fn main() {
    let (mut socket, _response) = connect(
        Url::parse("ws://127.0.0.1:53954/devtools/browser/0a276302-a6e8-4f7e-9fbf-6ea97b55aa99").unwrap(),
    ).expect("Can't connect");

    let get_targets = headless_chrome::protocol::target::methods::GetTargets {};

    socket.write_message(
        Message::Text(serde_json::to_string(&get_targets.to_method_call(1)).unwrap()),
    ).expect("Failed to write socket message");

    loop {
        let msg = socket.read_message().expect("Error reading message");
        println!("Received: {}", msg);
    }
}

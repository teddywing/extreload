use chrome_native_messaging;
use serde::Serialize;

use std::io;

#[derive(Serialize)]
struct ExtensionIdMessage<'a> {
    ids: &'a[&'a str]
}

fn main() {
    let ids = vec!["extension_id"];

    chrome_native_messaging::send_message(
        io::stdout(),
        &ExtensionIdMessage { ids: &ids }
    ).expect("TODO: handle error");

    chrome_native_messaging::event_loop(|value| -> Result<(), &str> {
        match value {
            _ => Ok(()),
        }
    });
}

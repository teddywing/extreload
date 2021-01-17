var NATIVE_HOST_ID = 'com.teddywing.extreload';
var port = null;

port = chrome.runtime.connectNative(NATIVE_HOST_ID);
port.onMessage.addListener(on_native_message);
port.onDisconnect.addListener(on_disconnected);

function on_native_message(message) {
	console.log(message);

	if (message.ids) {
		message.ids.forEach(function(id) {
			// Disable extension
			chrome.management.setEnabled(id, false, function() {
				console.log('Disabled', id);

				// Enable extension
				chrome.management.setEnabled(id, true, function() {
					console.log('Enabled', id);

					// Reload the current tab
					chrome.tabs.reload();
				});
			});
		});
	}
}

function on_disconnected() {
	console.warn('Native host disconnected');
}

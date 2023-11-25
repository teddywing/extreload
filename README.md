extreload
=========

Reload Chrome extensions from the command line. Facilitates Chrome extension
development.

Communicates with Chrome over the [DevTools Protocol].


## Manifest V3
I tried to add support for Manifest V3 extensions in the branch
[manifest-v3-support], but couldn’t get tab reloading to work properly.
Ultimately, I gave up and decided to use the [chromedp] API rather than lower
level JSON over the WebSocket. Manifest V3 support is available in a rewrite of
this program, [Swextreload].


## Usage
Chrome must be started with the `--remote-debugging-port` flag to enable the
DevTools Protocol, and the `--silent-debugger-extension-api` flag to allow debug
access to extensions. On Mac OS X:

	$ /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
		--silent-debugger-extension-api \
		--remote-debugging-port=0 &

When Chrome is launched, the DevTools Protocol socket URL will be printed to the
console. That WebSocket URL must be passed to `extreload` with the
`--socket-url` argument. For example:

	$ extreload \
		--socket-url ws://127.0.0.1:55755/devtools/browser/208ae571-d691-4c98-ad41-3a15d507b656 \
		--reload-current-tab \
		ooeilikhhbbkljfdhbglpalaplegfcmj


## Install
On Mac OS X, the program can be installed with Homebrew:

	$ brew install teddywing/formulae/extreload

Pre-built binaries for Mac OS X and Linux can be found on the [releases page].


## Build
I’ve only tested the build with SBCL.

	$ git submodule init && git submodule update
	$ make build


## License
Copyright © 2021 Teddy Wing. Licensed under the GNU GPLv3+ (see the included
COPYING file).


[manifest-v3-support]: https://github.com/teddywing/extreload/tree/manifest-v3-support
[chromedp]: https://godocs.io/github.com/chromedp/chromedp
[Swextreload]: https://github.com/teddywing/swextreload
[DevTools Protocol]: https://chromedevtools.github.io/devtools-protocol/
[releases page]: https://github.com/teddywing/extreload/releases

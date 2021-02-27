extreload
=========

Reload Chrome extensions from the command line. Facilitates Chrome extension
development.

Communicates with Chrome over the [DevTools Protocol].


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


## Build
I’ve only tested the build with SBCL.

	$ make build


## License
Copyright © 2021 Teddy Wing. Licensed under the GNU GPLv3+ (see the included
COPYING file).


[DevTools Protocol]: https://chromedevtools.github.io/devtools-protocol/

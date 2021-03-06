# Copyright (c) 2021  Teddy Wing
#
# This file is part of Extreload.
#
# Extreload is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Extreload is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Extreload. If not, see <https://www.gnu.org/licenses/>.


LISP ?= sbcl

VERSION := $(shell fgrep ':version' extreload.asd | awk -F '"' '{ print $$2 }')

MAN_PAGE := doc/extreload.1

DIST := $(abspath dist)
DIST_PRODUCT := $(DIST)/bin/extreload
DIST_MAN_PAGE := $(DIST)/share/man/man1/extreload.1


.PHONY: build
build: extreload

extreload: extreload.asd lib/* src/*.lisp
	$(LISP) --load extreload.asd \
		--eval '(ql:quickload :extreload)' \
		--eval '(asdf:make :extreload)' \
		--eval '(quit)'


.PHONY: doc
doc: $(MAN_PAGE)

$(MAN_PAGE): doc/extreload.1.txt
	a2x --no-xmllint --format manpage $<


.PHONY: dist
dist: $(DIST_PRODUCT) $(DIST_MAN_PAGE)

$(DIST):
	mkdir -p $@

$(DIST)/bin: $(DIST)
	mkdir -p $@

$(DIST)/share/man/man1: $(DIST)
	mkdir -p $@

$(DIST_PRODUCT): $(DIST)/bin extreload
	cp extreload $<

$(DIST_MAN_PAGE): $(DIST)/share/man/man1 $(MAN_PAGE)
	cp $(MAN_PAGE) $<


.PHONY: pkg
pkg: extreload_$(VERSION)_darwin-x86_64.tar.bz2

extreload_$(VERSION)_darwin-x86_64.tar.bz2: dist
	tar cjv -s /dist/extreload_$(VERSION)_darwin-x86_64/ -f $@ dist

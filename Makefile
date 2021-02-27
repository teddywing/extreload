LISP ?= sbcl

.PHONY: build
build:
	$(LISP) --load extreload.asd \
		--eval '(ql:quickload :extreload)' \
		--eval '(asdf:make :extreload)' \
		--eval '(quit)'


.PHONY: doc
doc: doc/extreload.1

doc/extreload.1: doc/extreload.1.txt
	a2x --no-xmllint --format manpage $<

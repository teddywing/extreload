LISP ?= sbcl

build:
	$(LISP) --load extreload.asd \
		--eval '(ql:quickload :extreload)' \
		--eval '(asdf:make :extreload)' \
		--eval '(quit)'

release:
	# ecl --eval '(require "asdf")' \
	# 	--load extreload.asd \
	# 	--eval '(ql:quickload :extreload)' \
	# 	--eval '(asdf:make :extreload)' \
	# 	--eval '(quit)'
	ecl --eval '(require "asdf")' \
		--eval '(require "uiop")' \
		--load extreload.asd \
		--eval '(ql:quickload :extreload)' \
		--eval '(asdf:make-build :extreload \
			:type :program \
			:move-here #P"./" \
			:epilogue-code '"'"'(progn \
				(extreload:main) \
				(si:exit)))' \
		--eval '(quit)'


.PHONY: doc
doc: doc/extreload.1

doc/extreload.1: doc/extreload.1.txt
	a2x --no-xmllint --format manpage $<

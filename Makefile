EMACS   ?= emacs
PACKAGE := hackernews
RM      ?= rm -f

%.elc: %.el
	$(EMACS) --quick --batch --funcall=batch-byte-compile $<

all: lisp

.PHONY: help
help:
	$(info Available targets)
	$(info )
	$(info [all]     Same as 'lisp' (default))
	$(info clean     Clean the build directory)
	$(info emacs-Q   Launch emacs -Q with Hackernews loaded)
	$(info lisp      Byte-compile Elisp sources)
	@:

.PHONY: emacs-Q
emacs-Q:
	$(EMACS) --quick --load=$(PACKAGE).el

lisp: $(PACKAGE).elc

.PHONY: clean
clean:
	$(RM) $(PACKAGE).elc

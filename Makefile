EMACS ?= emacs
RM ?= rm -f

els := hackernews.el
elcs := $(els:.el=.elc)

.PHONY: all
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
	$(EMACS) -Q -l $(els)

.PHONY: lisp
lisp: $(elcs)

.PHONY: clean
clean:
	$(RM) $(elcs)

%.elc: %.el
	$(EMACS) -Q -batch -f batch-byte-compile $<

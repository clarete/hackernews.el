EMACS   ?= emacs
PACKAGE  = hackernews
RM      ?= rm -f

%.elc: %.el
	$(EMACS) --quick --batch --funcall=batch-byte-compile $<

all: lisp

help:
	$(info Available options)
	$(info - clean   : Clean the build directory)
	$(info - emacs-Q : Launch emacs -Q with Hackernews loaded)
	$(info - lisp    : Byte-compile Elisp sources)

emacs-Q:
	$(EMACS) --quick --load=$(PACKAGE).el

lisp: $(PACKAGE).elc

clean:
	$(RM) $(PACKAGE).elc

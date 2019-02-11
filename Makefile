EMACS   ?= emacs
PACKAGE  = hackernews
VERSION ?= git
TARGET   = $(PACKAGE)-$(VERSION)
RM      ?= rm -f

%.elc: %.el
	$(EMACS) --quick --batch --funcall=batch-byte-compile $<

all: lisp

help:
	$(info Available options)
	$(info - package : Create a tar archive)
	$(info - clean   : Clean the build directory)
	$(info - emacs-Q : Launch emacs -Q with Hackernews loaded)
	$(info - lisp    : Byte-compile Elisp sources)

package:
	mkdir -p $(TARGET)
	cp hackernews.el hackernews-pkg.el README.md Screenshot.png COPYING $(TARGET)
	tar cf $(TARGET).tar $(TARGET)
	$(RM) -r $(TARGET)

emacs-Q:
	$(EMACS) --quick --load=$(PACKAGE).el

lisp: $(PACKAGE).elc

clean:
	$(RM) $(PACKAGE).elc $(PACKAGE)-*.tar

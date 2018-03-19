PACKAGE  = hackernews
VERSION ?= git
TARGET   = $(PACKAGE)-$(VERSION)
RM      ?= rm -f

help:
	$(info Available options)
	$(info - package : Build a tar package ready to be uploaded to Marmalade)
	$(info - clean   : Clean the build directory)

all: package

package:
	mkdir -p $(TARGET)
	cp hackernews.el hackernews-pkg.el README.md Screenshot.png COPYING $(TARGET)
	tar cf $(TARGET).tar $(TARGET)
	$(RM) -r $(TARGET)

clean:
	$(RM) $(PACKAGE)-*.tar

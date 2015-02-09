PACKAGE=hackernews
VERSION?=git
TARGET=$(PACKAGE)-$(VERSION)

all:
	$(info Available options)
	$(info  - package: Builds a tar package ready to be uploaded to marmalade)
	$(info  - clean: Clean the build directory)

package:
	mkdir -p $(TARGET)
	cp hackernews.el hackernews-pkg.el README.md Screenshot.png COPYING $(TARGET)
	tar cf $(TARGET).tar $(TARGET)
	rm -rf $(TARGET)

clean:
	rm -rf $(PACKAGE)-*.tar

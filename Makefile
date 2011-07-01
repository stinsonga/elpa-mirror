# Makefile for GNU Emacs Lisp Package Archive.

ARCHIVE_DIR=archive

## Deploy the package archive to archive/
archive: packages
	mkdir -p $(ARCHIVE_DIR)
	admin/package-update.sh $(ARCHIVE_DIR)

## Deploy the package archive to archive/ including the Org daily and
## admin scripts.
full-archive: .PHONY
	mkdir -p $(ARCHIVE_DIR)
	admin/package-update.sh $(ARCHIVE_DIR) 1

.PHONY:

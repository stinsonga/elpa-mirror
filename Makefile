# Makefile for GNU Emacs Lisp Package Archive.

EMACS=emacs

ARCHIVE_TMP=archive-tmp
PACKAGE_SITE_DIR=site

## Set up the source files for direct usage, by pointing
## `package-directory-list' to site/
site: packages
	mkdir -p $(PACKAGE_SITE_DIR)

.PHONY: archive-tmp process-archive archive-full org-fetch

## Deploy the package archive to archive/, with packages in
## archive/packages/:
archive: archive-tmp
	$(MAKE) $(MFLAGS) process-archive

archive-tmp: packages
	mkdir -p $(ARCHIVE_TMP)
	cp -r packages $(ARCHIVE_TMP)

process-archive:
	cd $(ARCHIVE_TMP)/packages; $(EMACS) -batch -l $(CURDIR)/admin/archive-contents.el -f batch-make-archive
	@cd $(ARCHIVE_TMP)/packages; \
	for pt in *; do \
	    if [ -d $$pt ]; then \
		echo "Creating tarball $${pt}.tar" && \
		tar -cf $${pt}.tar $$pt --remove-files; \
	    fi; \
	done
	mkdir -p archive/packages
	mv archive/packages archive/packages-old
	mv $(ARCHIVE_TMP)/packages archive/packages
	chmod -R a+rX archive/packages
	rm -rf archive/packages-old
	rm -rf $(ARCHIVE_TMP)

## Deploy the package archive to archive/ including the Org daily and
## admin scripts:
archive-full: archive-tmp org-fetch
	$(MAKE) $(MFLAGS) process-archive
	mkdir -p archive/admin
	cp admin/* archive/admin/

org-fetch: archive-tmp
	cd archive-tmp/packages; \
	pkgname=`curl -s http://orgmode.org/pkg/daily/|perl -ne 'push @f, $$1 if m/(org-\d{8})\.tar/; END { @f = sort @f; print "$$f[-1]\n"}'`; \
	wget http://orgmode.org/pkg/daily/$${pkgname}.tar -O $${pkgname}.tar; \
	if [ -f $${pkgname}.tar ]; then \
		tar xf $${pkgname}.tar; \
		rm -f $${pkgname}.tar; \
		mv $${pkgname} org; \
	fi

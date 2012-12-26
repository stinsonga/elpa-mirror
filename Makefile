# This is part of ENWC
#
#  Copyright (C) 2012 Ian Dunn.
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

LISPDIR=lisp
DOCDIR=doc/

ALLSRC=$(wildcard $(LISPDIR)/*.el)
ALLELC=$(wildcard $(LISPDIR)/*.elc)

PREFIX=/usr/local
INFODIR=$(PREFIX)/info
SITELISP=$(PREFIX)/share/emacs/site-lisp/enwc

.PHONY: all install lisp clean
.PRECIOUS: %.elc
all: lisp

lisp:
	$(MAKE) -C $(LISPDIR)

install:
	install -m 644 $(ALLELC) $(SITELISP)
	install -m 644 $(ALLSRC) $(SITELISP)

uninstall:
	-rm -f $(SITELISP)/*.elc
	-rm -f $(SITELISP)/*.el

clean:
	$(MAKE) -C $(LISPDIR) clean

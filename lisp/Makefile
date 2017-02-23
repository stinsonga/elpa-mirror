# This is part of ENWC
#
#  Copyright (C) 2012-2017 Ian Dunn.
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

EMACS=emacs --batch
ALLSRC= enwc-backend.el enwc.el enwc-wicd.el enwc-nm.el
ALLELC=$(wildcard *.elc)
SOURCE=$(ALLSRC)
TARGET=$(patsubst %.el,%.elc,$(SOURCE))

all: $(TARGET)

%.elc: %.el
	@$(EMACS) \
	-L "." \
	-f batch-byte-compile $<

enwc-autoloads.el: $(SOURCE)
	@$(EMACS) \
	--eval "(require 'package)" \
	--eval "(setq inhibit-message t)" \
	--eval "(package-generate-autoloads \"enwc\" \"$$(pwd)\")"

clean:
	-rm -f *.elc

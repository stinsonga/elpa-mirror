EMACS ?= emacs
RM ?= rm
ELC = gobject-align.elc gobject-snippet.elc gobject-minor-mode.elc

all: $(ELC)

%.elc: %.el
	$(EMACS) -Q -batch --eval "(setq load-path (cons nil load-path))" \
		-f batch-byte-compile $<

check:
	$(EMACS) -Q -batch --eval "(setq load-path (cons nil load-path))" \
		-l ert -l gobject-tests.el -f ert-run-tests-batch-and-exit

clean:
	$(RM) -rf $(ELC)

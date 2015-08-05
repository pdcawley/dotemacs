### -*- mode: makefile-gmake -*-

DIRS = override lib elisp site-lisp el-get initscripts elisp/external
SPECIAL = cus-dirs.el autoloads.el
ELPA = elpa
INIT_SOURCE = $(wildcard *.el)
LIB_SOURCE = $(wildcard override/*.el) $(wildcard lib/*.el) \
			 $(wildcard elisp/*.el) $(wildcard site-lisp/*.el) \
			 $(wildcard initscripts/*.el) $(wildcard initscripts/external/*.el)
INIT_ORG_FILES = $(wildcard initscripts/*.org) \
				 $(wildcard $(USER)/*.org)
INIT_ORG_FILESO = $(INIT_ORG_FILES:.org=.el)

TARGET = autoloads.elc $(patsubst %.el,%.elc, $(LIB_SOURCE) $(INIT_ORG_FILESO))
EMACS = emacs
EMACS_BATCH = $(EMACS) -Q -batch
MY_LOADPATH = -L . $(patsubst %,-L %,$(DIRS)) -l pdc-utils
BATCH_LOAD = $(EMACS_BATCH) $(MY_LOADPATH)

all: el $(SPECIAL) $(TARGET)
		for dir in $(DIRS); do \
			$(BATCH_LOAD) -f batch-byte-recompile-directory $$dir; \
		done

el: $(INIT_ORG_FILES)
		$(BATCH_LOAD) -l load-path --eval '(mapc (lambda (x) (org-babel-load-file (symbol-name x))) (quote ($(INIT_ORG_FILES))))'

cus-dirs.el: Makefile $(LIB_SOURCE)
		$(EMACS_BATCH) -l cus-dep -f custom-make-dependencies $(DIRS)
		mv cus-load.el cus-dirs.el

autoloads.el: Makefile autoloads.in $(LIB_SOURCE)
		cp -p autoloads.in autoloads.el
		-rm -f autoloads.elc
		$(EMACS_BATCH) -l $(shell pwd)/autoloads -l easy-mmode \
			-f generate-autoloads $(shell pwd)/autoloads.el $(DIRS) \
			$(shell find $(DIRS) $(ELPA) -maxdepth 1 -type d -print)

autoloads.elc: autoloads.el

%.elc: %.el
		$(BATCH_LOAD) -l load-path -f batch-byte-compile $<

%.el: %.org
		$(BATCH_LOAD) -l load-path -f batch-byte-compile

init.elc: init.el
		@rm -f $@
		$(BATCH_LOAD) -l init -f batch-byte-compile $<

clean:
		rm -f autoloads.el* cus-dirs.el *.elc

fullclean: clean
		rm -f $(TARGET)

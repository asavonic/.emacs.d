EMACS := emacs
TOP := $(shell pwd)
LOAD_PATH :=					\
	-L .					\
	-L $(TOP)/use-package			\
	-L $(TOP)/dash				\
	-L $(TOP)/with-editor			\
	-L $(TOP)/ghub				\
	-L $(TOP)/graphql			\
	-L $(TOP)/treepy			\
	-L $(TOP)/magit-popup			\
	-L $(TOP)/magit/lisp

PATH := $(TOP)/cask/bin:$(PATH)

export LOAD_PATH
export PATH

define bytecomp
	$(EMACS) -Q --batch -L $(LOAD_PATH) \
	   --eval '(setq byte-compile-error-on-warn t)' \
	   -f batch-byte-compile $(1)
endef


all:						\
	use-package/use-package.elc		\
	with-editor/with-editor.elc		\
	ghub/ghub.elc				\
	magit-popup/magit-popup.elc		\
	magit/magit.elc			\
	dash/dash.elc


use-package/use-package.elc:
	make -C use-package elc

with-editor/with-editor.elc:
	make -C with-editor lisp

ghub/ghub.elc:
	make -C ghub lisp

magit-popup/magit-popup.elc:
	make -C magit-popup lisp

magit/magit.elc:
	make -C magit lisp
	make -C magit/lisp

dash/dash.elc:
	$(call bytecomp, dash/dash-functional.el)
	$(call bytecomp, dash/dash.el)


clean:
	make -C use-package clean

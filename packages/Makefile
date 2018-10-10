TOP := $(shell pwd)
LOAD_PATH :=					\
	-L .

PATH := $(TOP)/cask/bin:$(PATH)

export LOAD_PATH
export PATH

all: use-package/use-package.elc

use-package/use-package.elc:
	make -C use-package elc

clean:
	make -C use-package clean

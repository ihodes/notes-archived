.PHONY: all
all:
	cabal install --overwrite-policy=always


.PHONY: build
build:
	notes build


.PHONY: watch
watch:
	notes watch

SRCS := $(wildcard *_test.ml)
BUILD = $(addprefix _build/,$(SRCS:.ml=.byte))

.PHONY: all build test clean

all: build test

build: $(BUILD)

_build/%.byte: %.ml
	ocamlbuild -no-links -tag debug $(basename $<).byte

test:
	for t in $(BUILD); do \
		./$$t; \
	done

clean:
	rm -rf _build

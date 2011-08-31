SRCS := hello_scgi.ml
BUILD = $(addprefix _build/,$(SRCS:.ml=.byte))

.PHONY: all build

all: build

build: $(BUILD)

_build/%.byte: %.ml
	ocamlbuild -no-links -tag debug $(basename $<).byte

clean:
	rm -rf _build

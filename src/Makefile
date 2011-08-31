NAME = ocamlcgi
SRCS := $(NAME)_common.ml netstring.ml scgi.ml
CMAS := $(NAME).cma
CMXAS := $(NAME).cmxa
CMIS := $(SRCS:.ml=.cmi)
LIBS = $(CMAS) $(CMXAS)
BUILD = $(addprefix _build/,$(LIBS)) $(addprefix _build/,$(CMIS)) _build/$(NAME).a

.PHONY: all install uninstall clean

all: $(NAME).mllib
	ocamlbuild -tag debug $(LIBS)

install:
	ocamlfind install $(NAME) $(BUILD) META

uninstall:
	ocamlfind remove $(NAME)

clean:
	rm -f $(NAME).mllib
	ocamlbuild -clean

$(NAME).mllib:
	echo $(SRCS) | perl -pe 's/ /\n/g'  | perl -pe 's/^./\U$$&/g' | sed -e "s/\.ml//g" > $@


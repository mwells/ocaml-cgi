NAME = scgi
SRCS := $(shell ls *.ml | grep -v myocamlbuild | xargs)
MLI_SRCS = $(shell ls *.ml | grep -v myocamlbuild | grep -v compatability | xargs)
CMAS := $(NAME).cma
CMXAS := $(NAME).cmxa
LIBS = $(CMAS) $(CMXAS)
MLIS = $(MLI_SRCS:.ml=.mli)
BUILD = $(addprefix _build/,$(LIBS)) $(addprefix _build/,$(MLIS)) _build/$(NAME).cmi _build/$(NAME).a

.PHONY: all install uninstall clean

all: $(NAME).mlpack
	ocamlbuild -tag debug $(LIBS)

install:
	ocamlfind install $(NAME) $(BUILD) META

uninstall:
	ocamlfind remove $(NAME)

clean:
	rm -f $(NAME).mlpack
	ocamlbuild -clean

$(NAME).mlpack:
	echo $(SRCS) | perl -pe 's/ /\n/g'  | perl -pe 's/^./\U$$&/g' | sed -e "s/\.ml//g" | grep -v myocamlbuild | sort > $@

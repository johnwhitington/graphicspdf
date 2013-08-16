MODS = graphicspdf

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli)

RESULT = graphicspdf

PACKS = bigarray camlpdf

LIBINSTALL_FILES = graphicspdf.a graphicspdf.cma graphicspdf.cmxa \
$(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi)

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : byte-code-library native-code-library htdoc

install : libinstall

-include OCamlMakefile


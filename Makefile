MODS = graphicspdf

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli)

RESULT = graphicspdf

PACKS = camlpdf

LIBINSTALL_FILES = graphicspdf.a graphicspdf.cma graphicspdf.cmx graphicspdf.cmxa \
$(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi)

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : byte-code-library native-code-library htdoc

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf

install : libinstall

-include OCamlMakefile


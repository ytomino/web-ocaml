include Makefile.variables

MLI=web.mli web__HTML.mli
MLSRC=$(MLI:.mli=.ml)
MLINIT=
CSRC=

LIBNAME=web
STUBLIBNAME=

OCAMLCFLAGS=-w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
CFLAGS=

LDFLAGS=
LDLIB=
CCLIB=

include Makefile.rules

$(BUILDDIR)/web.cmo $(BUILDDIR)/web.cmx $(BUILDDIR)/web.cmi: \
	private override OCAMLCFLAGS+=-no-alias-deps -w -49
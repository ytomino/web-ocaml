include Makefile.variables

MLI=web.mli web__CGI.mli web__HTML.mli
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

$(BUILDDIR)/web__CGI.cmi: $(BUILDDIR)/web.cmi
$(BUILDDIR)/web__CGI.cmx: $(BUILDDIR)/web.cmx

include Makefile.variables

MLI=web.mli web__CGI.mli web__HTML.mli web__HTML__Input.mli web__Locking.mli
MLSRC=$(MLI:.mli=.ml)
MLINIT=
CSRC=

LIBNAME=web
STUBLIBNAME=

DEBUG=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
CFLAGS=

LDFLAGS=
LDLIB=
CCLIB=

include Makefile.rules

PARENT_MODULES=web web__HTML

$(foreach e,$(PARENT_MODULES), \
  $(BUILDDIR)/$(e).cmi $(BUILDDIR)/$(e).cmo $(BUILDDIR)/$(e).cmx): \
	private override OCAMLCFLAGS+=-no-alias-deps -w -49

$(BUILDDIR)/web__CGI.cmi: $(BUILDDIR)/web.cmi
$(BUILDDIR)/web__CGI.cmx: $(BUILDDIR)/web.cmx
$(BUILDDIR)/web__HTML__Input.cmi: $(BUILDDIR)/web.cmi $(BUILDDIR)/web__HTML.cmi
$(BUILDDIR)/web__HTML__Input.cmx: $(BUILDDIR)/web.cmx $(BUILDDIR)/web__HTML.cmx

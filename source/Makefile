include Makefile.variables

MLI=web.mli web__CGI.mli web__HTML.mli web__HTML__Input.mli web__Locking.mli \
    web__XML.mli web__RSS.mli
MLSRC=$(MLI:.mli=.ml)
MLINIT=
CSRC=

LIBNAME=web
STUBLIBNAME=

DEBUG=
ASSERT=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) $(if $(ASSERT),,-noassert) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
CFLAGS=

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
$(BUILDDIR)/web__RSS.cmo: $(BUILDDIR)/web__XML.cmi
$(BUILDDIR)/web__RSS.cmx: $(BUILDDIR)/web__XML.cmx
$(BUILDDIR)/web__XML.cmo: $(BUILDDIR)/web__HTML.cmi
$(BUILDDIR)/web__XML.cmx: $(BUILDDIR)/web__HTML.cmx

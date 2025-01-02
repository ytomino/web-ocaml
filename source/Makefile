include Makefile.variables

MLI=web.mli web__CGI.mli web__HTML.mli web__HTML__Input.mli web__Locking.mli \
    web__Template.mli web__XML.mli web__RSS.mli
MLSRC=$(MLI:.mli=.ml)
MLINIT=
CSRC=

LIBNAME=web
STUBLIBNAME=

OCAML_VERSION_MAJOR?=4
# OCAML_VERSION_MAJOR is defined in Makefile.config of OCaml 5.x

DEBUG=
ASSERT=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) $(if $(ASSERT),,-noassert) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS) \
              $(and $(filter-out false,$(FUNCTION_SECTIONS)), \
                -function-sections)
OCAML_INCLUDE_FLAGS=$(and $(filter 5,$(OCAML_VERSION_MAJOR)),-I +unix)
CFLAGS=
C_INCLUDE_FLAGS=

LDLIB=
CCLIB=

include Makefile.rules

PARENT_MODULES=web web__HTML

$(foreach e,$(PARENT_MODULES), \
  $(BUILDDIR)/$(e).cmi $(BUILDDIR)/$(e).cmo $(BUILDDIR)/$(e).cmx) \
: private override OCAMLCFLAGS+=-no-alias-deps -w -49

$(BUILDDIR)/web__CGI.cmi: $(BUILDDIR)/web.cmi
$(BUILDDIR)/web__CGI.cmx: $(BUILDDIR)/web.cmx
$(BUILDDIR)/web__HTML__Input.cmi: $(BUILDDIR)/web.cmi $(BUILDDIR)/web__HTML.cmi
$(BUILDDIR)/web__HTML__Input.cmx: $(BUILDDIR)/web.cmx $(BUILDDIR)/web__HTML.cmx
$(BUILDDIR)/web__RSS.cmo: $(BUILDDIR)/web__XML.cmi
$(BUILDDIR)/web__RSS.cmx: $(BUILDDIR)/web__XML.cmx
$(BUILDDIR)/web__XML.cmo: $(BUILDDIR)/web__HTML.cmi
$(BUILDDIR)/web__XML.cmx: $(BUILDDIR)/web__HTML.cmx

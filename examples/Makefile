BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

PROGRAMPREFIX=$(addsuffix -,$(filter-out $(BUILD),$(TARGET)))

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(or $(filter-out $(OCAMLC),$(OCAMLC:c=opt)),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

ifndef OCAML_VERSION_MAJOR
 OCAML_VERSION_MAJOR:=$(basename \
                        $(basename \
                          $(shell $(or $(OCAMLC),$(OCAMLOPT)) -version)))
endif

DEBUG=
ASSERT=1

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAMLCFLAGS_EXE=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS_EXE=$(OCAMLCFLAGS_EXE) $(addprefix -ccopt ,$(LDFLAGS))
OCAML_INCLUDE_FLAGS=$(and $(filter 5,$(OCAML_VERSION_MAJOR)),-I +unix)
LDFLAGS?=

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
WEB_BUILDDIR=$(BUILDDIR)/web$(BUILDSUFFIX)

BINLN=$(and $(filter $(BUILD),$(TARGET)), \
        $(if $(findstring mingw,$(BUILD))$(findstring msys,$(BUILD)),,bin))

EXAMPLES=$(basename $(wildcard *.ml))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check interactive clean $(TESTS)

all \
: $(and $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
  $(and $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) $(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/web.cma
	$(OCAMLC) $(strip \
		-c $(OCAMLCFLAGS) \
		-o $(BUILDDIR)/$*.cmo $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) $<)
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) \
		-o $@ $(OCAML_INCLUDE_FLAGS) unix.cma -I $(BUILDDIR) web.cma \
		$(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/web.cmxa
	$(OCAMLOPT) $(strip \
		-c $(OCAMLOPTFLAGS) \
		-o $(BUILDDIR)/$*.cmx $(OCAML_INCLUDE_FLAGS) -I $(BUILDDIR) $<)
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) \
		-o $@ $(OCAML_INCLUDE_FLAGS) unix.cmxa -I $(BUILDDIR) web.cmxa \
		$(BUILDDIR)/$*.cmx)

$(and $(OCAMLC),$(BUILDDIR)/web.cma) $(and $(OCAMLOPT),$(BUILDDIR)/web.cmxa) \
&: ../source/web*
	$(MAKE) $(strip \
		-C ../source install \
		BUILDDIR=$(abspath $(WEB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)) \
		ASSERT=$(ASSERT))

$(BINLN):
	ln -s $(BUILDDIR) $@

check: all $(TESTS)

$(TESTS): % \
: $(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
  $(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(BUILDDIR)/web.cma
	$(RLWRAP) ocaml $(strip $(OCAML_INCLUDE_FLAGS) unix.cma -I $(BUILDDIR) web.cma)

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) $(strip \
		-C ../source clean uninstall \
		BUILDDIR=$(abspath $(WEB_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)

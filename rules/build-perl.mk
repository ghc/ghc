
# Build a perl script.  Invoke like this:
#
# driver/mangler_PERL_SRC = ghc-asm.lprl
# driver/mangler_dist_PROG = ghc-asm
#
# $(eval $(call build-perl,driver/mangler,dist))

define build-perl
# $1 = dir
# $2 = distdir

ifeq "$$($1_$2_LIBEXEC)" "YES"
$1_$2_INPLACE = $(INPLACE_LIB)/$$($1_$2_PROG)
else
$1_$2_INPLACE = $(INPLACE_BIN)/$$($1_$2_PROG)
endif

$(call all-target,$$($1_$2_INPLACE))

$(call clean-target,$1,$2,$1/$2 $$($1_$2_INPLACE))
.PHONY: clean_$1
clean_$1 : clean_$1_$2

ifneq "$$(BINDIST)" "YES"
$1/$2/$$($1_$2_PROG).prl: $1/$$($1_PERL_SRC) $$(UNLIT)
	$$(MKDIRHIER) $1/$2
	$$(RM) -f $$@
	$$(UNLIT) $$(UNLIT_OPTS) $$< $$@

$1/$2/$$($1_$2_PROG): $1/$2/$$($1_$2_PROG).prl
	$$(RM) -f $$@
	echo '#!$$(PERL)'                                  >> $$@
	echo '$$$$TARGETPLATFORM  = "$$(TARGETPLATFORM)";' >> $$@
	cat $$<                                            >> $$@
	$$(EXECUTABLE_FILE) $$@

$$($1_$2_INPLACE): $1/$2/$$($1_$2_PROG)
	$$(MKDIRHIER) $$(dir $$@)
	$$(RM) -f $$@
	$$(CP) $$< $$@
	$$(EXECUTABLE_FILE) $$@
endif

endef

TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# A set of driver tests

# Things to test:
#
#  - one-shot vs. --make
#  - hierarchical vs. flat
#  - -odir vs. no -odir
#  - -hidir vs. no -hidir
#  - root module vs. found modules

OBJSUFFIX = .o

# -----------------------------------------------------------------------------
# One-shot compilations, non-hierarchical modules

test011:
	@$(RM) A.hi
	@$(RM) A$(OBJSUFFIX)
	$(TEST_HC) -c A.hs
	test -f A.hi
	test -f A$(OBJSUFFIX)

# test -o
test012:
	@$(RM) A.hi
	@$(RM) A.ooo
	$(TEST_HC) -c A.hs -o A.ooo
	test -f A.ooo

# test -ohi
test013:
	@$(RM) A.xhi
	@$(RM) A$(OBJSUFFIX)
	$(TEST_HC) -c A.hs -ohi A.xhi
	test -f A.xhi

# test -odir
test014:
	@$(RM) A.hi
	@$(RM) obj/A$(OBJSUFFIX)
	@$(MKDIRHIER) obj
	$(TEST_HC) -c A.hs -odir obj
	test -f obj/A$(OBJSUFFIX)

# test -hidir
test015:
	@$(RM) hi/A.hi
	@$(RM) A$(OBJSUFFIX)
	@$(MKDIRHIER) hi
	$(TEST_HC) -c A.hs -hidir hi
	test -f hi/A.hi

# -----------------------------------------------------------------------------
# One-shot compilation, hierarchical modules

test021:
	@$(RM) B/C.hi
	@$(RM) B/C$(OBJSUFFIX)
	$(TEST_HC) -c B/C.hs
	test -f B/C.hi
	test -f B/C$(OBJSUFFIX)

# test -o
test022:
	@$(RM) B/C.hi
	@$(RM) B/C.ooo
	$(TEST_HC) -c B/C.hs -o B/C.ooo
	test -f B/C.ooo

# test -ohi
test023:
	@$(RM) B/C.xhi
	@$(RM) B/C$(OBJSUFFIX)
	$(TEST_HC) -c B/C.hs -ohi B/C.xhi
	test -f B/C.xhi

# test -odir
test024:
	@$(MKDIRHIER) obj
	@$(RM) B/C.hi
	@$(RM) obj/B/C$(OBJSUFFIX)
	@-rmdir obj/B
	$(TEST_HC) -c B/C.hs -odir obj
	test -f obj/B/C$(OBJSUFFIX)

# test -hidir
test025:
	@$(MKDIRHIER) hi
	@$(RM) hi/B/C.hi
	@$(RM) B/C$(OBJSUFFIX)
	$(TEST_HC) -c B/C.hs -hidir hi
	test -f hi/B/C.hi

# This is a hierarchical module that lives in a subdirectory.
test026:
	@$(RM) d1/P/Q.hi
	@$(RM) d1/P/Q$(OBJSUFFIX)
	$(TEST_HC) -c d1/P/Q.hs
	test -f d1/P/Q.hi
	test -f d1/P/Q$(OBJSUFFIX)

# -----------------------------------------------------------------------------
# Compilation-manager compilations, flat modules

test031:
	@$(RM) A.hi
	@$(RM) A$(OBJSUFFIX)
	$(TEST_HC) --make A.hs
	test -f A.hi
	test -f A$(OBJSUFFIX)

# test -odir
test032:
	@$(RM) A.hi
	@$(RM) obj/A$(OBJSUFFIX)
	@$(MKDIRHIER) obj
	$(TEST_HC) --make A.hs -odir obj
	test -f obj/A$(OBJSUFFIX)

# test -hidir
test033:
	@$(RM) hi/A.hi
	@$(RM) A$(OBJSUFFIX)
	@$(MKDIRHIER) hi
	$(TEST_HC) --make A.hs -hidir hi
	test -f hi/A.hi

# -----------------------------------------------------------------------------
# Compilation-manager compilations, hierarchical modules

test041:
	@$(RM) B/C.hi
	@$(RM) B/C$(OBJSUFFIX)
	$(TEST_HC) --make B/C.hs
	test -f B/C.hi
	test -f B/C$(OBJSUFFIX)

# test -odir
test042:
	@$(MKDIRHIER) obj
	@$(RM) B/C.hi
	@$(RM) obj/B/C$(OBJSUFFIX)
	$(TEST_HC) --make B/C.hs -odir obj
	test -f obj/B/C$(OBJSUFFIX)

# test -hidir
test043:
	@$(MKDIRHIER) hi
	@$(RM) hi/B/C.hi
	@$(RM) B/C$(OBJSUFFIX)
	$(TEST_HC) --make B/C.hs -hidir hi
	test -f hi/B/C.hi

# -----------------------------------------------------------------------------
# Compilation-manager compilations, hierarchical modules, non-root modules

test051:
	@$(RM) d2/R/S.hi
	@$(RM) d2/R/S$(OBJSUFFIX)
	@$(RM) d1/P/Q.hi
	@$(RM) d1/P/Q$(OBJSUFFIX)
	$(TEST_HC) --make -id1 -id2 R.S
	test -f d2/R/S.hi
	test -f d2/R/S$(OBJSUFFIX)
	test -f d1/P/Q.hi
	test -f d1/P/Q$(OBJSUFFIX)

# test -odir
test052:
	@$(RM) d2/R/S.hi
	@$(RM) obj/R/S$(OBJSUFFIX)
	@$(RM) d1/P/Q.hi
	@$(RM) obj/P/Q$(OBJSUFFIX)
	$(TEST_HC) --make -id1 -id2 -odir obj R.S 
	test -f d2/R/S.hi
	test -f obj/R/S$(OBJSUFFIX)
	test -f d1/P/Q.hi
	test -f obj/P/Q$(OBJSUFFIX)

# test -hidir
test053:
	@$(RM) hi/R/S.hi
	@$(RM) d2/R/S$(OBJSUFFIX)
	@$(RM) hi/P/Q.hi
	@$(RM) d1/P/Q$(OBJSUFFIX)
	$(TEST_HC) --make -id1 -id2 -hidir hi R.S
	test -f hi/R/S.hi
	test -f d2/R/S$(OBJSUFFIX)
	test -f hi/P/Q.hi
	test -f d1/P/Q$(OBJSUFFIX)

# -----------------------------------------------------------------------------
# Other tests

# Test the -keep-hc-files works without --make
test060:
	@$(RM) A.hi A.o A.hc
	$(TEST_HC) -c -keep-hc-files -fvia-C A.hs
	test -f A.hc

# Test the -keep-hc-files works with --make
test061:
	@$(RM) A.hi A.o A.hc
	$(TEST_HC) --make -keep-hc-files -fvia-C A.hs
	test -f A.hc

# Test that -main-is works with --make
test062:
	@$(RM) Hello.hi Hello.o Hello.hc
	$(TEST_HC) --make -main-is Hello.hello Hello -o hello
	./hello

# Test that -i deletes the list of search paths.
# The compilation will fail, but we ignore the failure and just
# compare the output, because make might give us a different exit code.
test063:
	@$(RM) A.hi A.o A.hc D.hi D.o D.hc
	-@$(TEST_HC) --make -i -v0 D.hs

# Test -E
test064:
	@$(RM) A.hi A.hspp
	$(TEST_HC) -E A.hs
	test -f A.hspp

# Test -E/-cpp
test065:
	@$(RM) A.hi A.hspp
	$(TEST_HC) -E -cpp A.hs
	test -f A.hspp

# Test -E/-o
test066:
	@$(RM) A.hi A.tmp
	$(TEST_HC) -E A.hs -o A.tmp
	test -f A.tmp

# Test -E/-cpp/-o
test067:
	@$(RM) A.hi A.tmp
	$(TEST_HC) -cpp -E A.hs -o A.tmp
	test -f A.tmp

# Test -C
test068:
	@$(RM) A.hi A.hc
	$(TEST_HC) -C A.hs
	test -f A.hc

# Test -C/-o
test069:
	@$(RM) A.hi A.tmp
	$(TEST_HC) -C A.hs -o A.tmp
	test -f A.tmp

# Test -S
test070:
	@$(RM) A.hi A.s
	$(TEST_HC) -S A.hs
	test -f A.s

# Test -S/-o
test071:
	@$(RM) A.hi A.tmp
	$(TEST_HC) -S A.hs -o A.tmp
	test -f A.tmp

# No recompilation required, but we requested a .hc file --> should recompile
# Broken briefy in ghc-6.3.
test080:
	@$(RM) A.hi A.o A.hc
	$(TEST_HC) -c A.hs
	test -f A.o
	$(TEST_HC) -C A.hs
	test -f A.hc


# -----------------------------------------------------------------------------
# --make tests

# Test for overlap between home module and package module
test100:
	@$(RM) A.hi A.o A.hc
	$(TEST_HC) --make -ioverlap Overlap

# -----------------------------------------------------------------------------
# dependency-generation tests

DEPFILE= depend

# Test that we can create a dependency file, and that we can
# modify an existing dependency file.
test200:
	@$(RM) $(DEPFILE)
	$(TEST_HC) -M -optdep-f -optdep depend D
	test -f $(DEPFILE)
	$(TEST_HC) -M -optdep-f -optdep depend D B.C A
	test -f $(DEPFILE)
	$(TEST_HC) -M -optdep-f -optdep depend D.hs B/C.hs A.hs
	test -f $(DEPFILE)

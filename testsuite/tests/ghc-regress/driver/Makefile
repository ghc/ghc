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
	$(TEST_HC) -c B/C.hs -odir obj
	test -f obj/B/C$(OBJSUFFIX)

# test -hidir
test025:
	@$(MKDIRHIER) hi
	@$(RM) hi/B/C.hi
	@$(RM) B/C$(OBJSUFFIX)
	$(TEST_HC) -c B/C.hs -hidir hi
	test -f hi/B/C.hi

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

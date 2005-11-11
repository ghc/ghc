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
	@$(RM) A$(OBJSUFFIX)oo
	$(TEST_HC) -c A.hs -o A$(OBJSUFFIX)oo
	test -f A$(OBJSUFFIX)oo

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

# test stub generation
test016:
	@$(RM) F.hi F$(OBJSUFFIX) F_stub.c F_stub.h
	@$(TEST_HC) -c F.hs
	test -f F_stub$(OBJSUFFIX)
	test -f F_stub.c
	test -f F_stub.h

# test -stubdir (filename differs from module name)
test017:
	@$(RM) F.hi F$(OBJSUFFIX) stub/TestStub_stub.c stub/TestStub_stub.h stub/TestStub_stub$(OBJSUFFIX) F_stub$(OBJSUFFIX) 
	@$(TEST_HC) -c F.hs -stubdir stub
	test -f F_stub$(OBJSUFFIX)
	test -f stub/TestStub_stub.c
	test -f stub/TestStub_stub.h

# test -odir with stubs (filename differs from module name)
test018:
	@$(RM) F.hi F$(OBJSUFFIX) F_stub.c F_stub.h obj/TestStub_stub$(OBJSUFFIX)
	@$(MKDIRHIER) obj
	@$(TEST_HC) -c F.hs -odir obj
	test -f obj/TestStub_stub$(OBJSUFFIX)

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
	@$(RM) B/C$(OBJSUFFIX)oo
	$(TEST_HC) -c B/C.hs -o B/C$(OBJSUFFIX)oo
	test -f B/C$(OBJSUFFIX)oo

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

# test -odir with non-Haskell compilation
test024a:
	@$(MKDIRHIER) obj
	@$(RM) B/stub.c
	@$(RM) obj/B/stub$(OBJSUFFIX)
	@-rmdir obj/B
	@echo >B/stub.c
	$(TEST_HC) -c B/stub.c -odir obj
	test -f obj/B/stub$(OBJSUFFIX)

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

# test stub generation
test027:
	@$(RM) B/F.hi B/F$(OBJSUFFIX) B/F_stub.c B/F_stub.h
	@$(TEST_HC) -c B/F.hs
	test -f B/F_stub.c
	test -f B/F_stub.h

# test -stubdir
test028:
	@$(RM) B/F.hi B/F$(OBJSUFFIX) stub/B/F_stub.c stub/B/F_stub.h
	@$(TEST_HC) -c B/F.hs -stubdir stub
	test -f stub/B/F_stub.c
	test -f stub/B/F_stub.h

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

# test stub generation
test034:
	@$(RM) F.hi F$(OBJSUFFIX) F_stub.c F_stub.h
	@$(TEST_HC) --make F.hs
	test -f F_stub.c
	test -f F_stub.h

# test -stubdir (filename differs from module name)
test035:
	@$(RM) F.hi F$(OBJSUFFIX) stub/Stubs_stub.c stub/Stubs_stub.h
	@$(TEST_HC) --make F.hs -stubdir stub
	test -f stub/TestStub_stub.c
	test -f stub/TestStub_stub.h

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

# test stub generation
test044:
	@$(RM) B/F.hi B/F$(OBJSUFFIX) B/F_stub.c B/F_stub.h
	@$(TEST_HC) --make B/F.hs
	test -f B/F_stub.c
	test -f B/F_stub.h

# test -stubdir
test045:
	@$(RM) B/F.hi B/F$(OBJSUFFIX) stub/B/F_stub.c stub/B/F_stub.h
	@$(TEST_HC) --make B/F.hs -stubdir stub
	test -f stub/B/F_stub.c
	test -f stub/B/F_stub.h

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
	@$(RM) A.hi A$(OBJSUFFIX) A.hc
	$(TEST_HC) -c -keep-hc-files -fvia-C A.hs
	test -f A.hc

# Test the -keep-hc-files works with --make
test061:
	@$(RM) A.hi A$(OBJSUFFIX) A.hc
	$(TEST_HC) --make -keep-hc-files -fvia-C A.hs
	test -f A.hc

# Test that -main-is works with --make
test062.1:
	@$(RM) Hello.hs Hello.hi Hello$(OBJSUFFIX) Hello.hc hello
	echo "module Hello where { hello = putStrLn \"ok\" }" >Hello.hs
	$(TEST_HC) --make -main-is Hello.hello Hello -o hello
	./hello
test062.2:
	@$(RM) Hello.hs Hello.hi Hello$(OBJSUFFIX) Hello.hc hello
	echo "module Hello where { main = putStrLn \"ok\" }" >Hello.hs
	$(TEST_HC) --make -main-is Hello Hello -o hello
	./hello
test062.3:
	@$(RM) Hello.hs Hello.hi Hello$(OBJSUFFIX) Hello.hc hello
	echo "module Main (hello) where { hello = putStrLn \"ok\" }" >Hello.hs
	$(TEST_HC) --make -main-is hello Hello.hs -o hello
	./hello

# Test that -i deletes the list of search paths.
# The compilation will fail, but we ignore the failure and just
# compare the output, because make might give us a different exit code.
test063:
	@$(RM) A.hi A$(OBJSUFFIX) A.hc D.hi D$(OBJSUFFIX) D.hc
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
	@$(RM) A.hi A$(OBJSUFFIX) A.hc
	$(TEST_HC) -c A.hs
	test -f A$(OBJSUFFIX)
	$(TEST_HC) -C A.hs
	test -f A.hc



# -----------------------------------------------------------------------------
# --make tests

# Test for overlap between home module and package module
test100:
	@$(RM) A.hi A$(OBJSUFFIX) A.hc
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

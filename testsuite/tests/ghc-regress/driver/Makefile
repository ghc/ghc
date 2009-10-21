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
	$(RM) A.hi
	$(RM) A$(OBJSUFFIX)
	"$(TEST_HC)" -c A.hs
	test -f A.hi
	test -f A$(OBJSUFFIX)

# test -o
test012:
	$(RM) A.hi
	$(RM) A$(OBJSUFFIX)oo
	"$(TEST_HC)" -c A.hs -o A$(OBJSUFFIX)oo
	test -f A$(OBJSUFFIX)oo

# test -ohi
test013:
	$(RM) A.xhi
	$(RM) A$(OBJSUFFIX)
	"$(TEST_HC)" -c A.hs -ohi A.xhi
	test -f A.xhi

# test -odir
test014:
	$(RM) -f A014.hi
	$(RM) -rf obj014
	mkdir obj014
	"$(TEST_HC)" -c A014.hs -odir obj014
	test -f obj014/A014$(OBJSUFFIX)
	test -f A014.hi

# test -hidir
test015:
	$(RM) -f A015$(OBJSUFFIX)
	$(RM) -rf hi015
	mkdir hi015
	"$(TEST_HC)" -c A015.hs -hidir hi015
	test -f A015$(OBJSUFFIX)
	test -f hi015/A015.hi

# test stub generation
test016:
	$(RM) F.hi F$(OBJSUFFIX) F_stub.c F_stub.h
	"$(TEST_HC)" -c F.hs
	test -f F_stub$(OBJSUFFIX)
	test -f F_stub.c
	test -f F_stub.h

# test -stubdir (filename differs from module name)
test017:
	$(RM) F.hi F$(OBJSUFFIX) stub/TestStub_stub.c stub/TestStub_stub.h stub/TestStub_stub$(OBJSUFFIX) F_stub$(OBJSUFFIX) 
	"$(TEST_HC)" -c F.hs -stubdir stub
	test -f F_stub$(OBJSUFFIX)
	test -f stub/TestStub_stub.c
	test -f stub/TestStub_stub.h

# test -odir with stubs (filename differs from module name)
test018:
	$(RM) -f F018.hi F018$(OBJSUFFIX) F018_stub.c F018_stub.h
	$(RM) -rf obj018
	mkdir obj018
	"$(TEST_HC)" -c F018.hs -odir obj018
	test -f obj018/TestStub018_stub$(OBJSUFFIX)

# test for bug #3093
test018a:
	$(RM) -f F018a.hi F018a_stub.c F018a_stub.h *.obj.018
	"$(TEST_HC)" -c F018a.hs -osuf obj.018
	test -f F018a_stub.obj.018

# test -outputdir
test019:
	$(RM) -rf out019
	"$(TEST_HC)" -c F.hs -outputdir out019
	test -f out019/TestStub.hi
	test -f out019/TestStub$(OBJSUFFIX)
	test -f out019/TestStub_stub$(OBJSUFFIX)
	test -f out019/TestStub_stub.c
	test -f out019/TestStub_stub.h

# -----------------------------------------------------------------------------
# One-shot compilation, hierarchical modules

test021:
	$(RM) B/C.hi
	$(RM) B/C$(OBJSUFFIX)
	"$(TEST_HC)" -c B/C.hs
	test -f B/C.hi
	test -f B/C$(OBJSUFFIX)

# test -o
test022:
	$(RM) B/C.hi
	$(RM) B/C$(OBJSUFFIX)oo
	"$(TEST_HC)" -c B/C.hs -o B/C$(OBJSUFFIX)oo
	test -f B/C$(OBJSUFFIX)oo

# test -ohi
test023:
	$(RM) B/C.xhi
	$(RM) B/C$(OBJSUFFIX)
	"$(TEST_HC)" -c B/C.hs -ohi B/C.xhi
	test -f B/C.xhi

# test -odir
test024:
	$(RM) -f B/C024.hi
	$(RM) -rf obj024
	mkdir obj024
	"$(TEST_HC)" -c B/C024.hs -odir obj024
	test -f obj024/B/C024$(OBJSUFFIX)

# test -odir with non-Haskell compilation
test024a:
	$(RM) -f B/stub.c
	$(RM) -rf obj
	mkdir obj
	echo >B/stub.c
	"$(TEST_HC)" -c B/stub.c -odir obj
	test -f obj/B/stub$(OBJSUFFIX)

# test -hidir
test025:
	$(RM) -rf hi
	mkdir hi
	$(RM) B/C$(OBJSUFFIX)
	"$(TEST_HC)" -c B/C.hs -hidir hi
	test -f hi/B/C.hi

# This is a hierarchical module that lives in a subdirectory.
test026:
	$(RM) d1/P/Q.hi
	$(RM) d1/P/Q$(OBJSUFFIX)
	"$(TEST_HC)" -c d1/P/Q.hs
	test -f d1/P/Q.hi
	test -f d1/P/Q$(OBJSUFFIX)

# test stub generation
# -fvia-C, because we want to check that the .hc file can #include the stub.h
test027:
	$(RM) B/F.hi B/F$(OBJSUFFIX) B/F_stub.c B/F_stub.h
	"$(TEST_HC)" -c B/F.hs -fvia-C
	test -f B/F_stub.c
	test -f B/F_stub.h

# test -stubdir
# -fvia-C, because we want to check that the .hc file can #include the stub.h
test028:
	$(RM) B/F.hi B/F$(OBJSUFFIX) stub/B/F_stub.c stub/B/F_stub.h
	"$(TEST_HC)" -c B/F.hs -stubdir stub -fvia-C
	test -f stub/B/F_stub.c
	test -f stub/B/F_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, flat modules

test031:
	$(RM) A.hi
	$(RM) A$(OBJSUFFIX)
	"$(TEST_HC)" -v0 --make A.hs
	test -f A.hi
	test -f A$(OBJSUFFIX)

# test -odir
test032:
	$(RM) A.hi
	$(RM) -rf obj
	mkdir obj
	"$(TEST_HC)" -v0 --make A.hs -odir obj
	test -f obj/A$(OBJSUFFIX)

# test -hidir
test033:
	$(RM) -rf hi
	$(RM) A$(OBJSUFFIX)
	mkdir hi
	"$(TEST_HC)" -v0 --make A.hs -hidir hi
	test -f hi/A.hi

# test stub generation
test034:
	$(RM) F.hi F$(OBJSUFFIX) F_stub.c F_stub.h
	"$(TEST_HC)" -v0 --make F.hs
	test -f F_stub.c
	test -f F_stub.h

# test -stubdir (filename differs from module name)
test035:
	$(RM) F.hi F$(OBJSUFFIX) stub/Stubs_stub.c stub/Stubs_stub.h
	"$(TEST_HC)" -v0 --make F.hs -stubdir stub
	test -f stub/TestStub_stub.c
	test -f stub/TestStub_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, hierarchical modules

test041:
	$(RM) B/C.hi
	$(RM) B/C$(OBJSUFFIX)
	"$(TEST_HC)" -v0 --make B/C.hs
	test -f B/C.hi
	test -f B/C$(OBJSUFFIX)

# test -odir
test042:
	$(RM) -rf obj
	mkdir obj
	$(RM) B/C.hi
	"$(TEST_HC)" -v0 --make B/C.hs -odir obj
	test -f obj/B/C$(OBJSUFFIX)

# test -hidir
test043:
	$(RM) -f B/C$(OBJSUFFIX)
	$(RM) -rf hi
	mkdir hi
	"$(TEST_HC)" -v0 --make B/C.hs -hidir hi
	test -f hi/B/C.hi

# test stub generation
test044:
	$(RM) B/F.hi B/F$(OBJSUFFIX) B/F_stub.c B/F_stub.h
	"$(TEST_HC)" -v0 --make B/F.hs
	test -f B/F_stub.c
	test -f B/F_stub.h

# test -stubdir
test045:
	$(RM) B/F.hi B/F$(OBJSUFFIX) stub/B/F_stub.c stub/B/F_stub.h
	"$(TEST_HC)" -v0 --make B/F.hs -stubdir stub
	test -f stub/B/F_stub.c
	test -f stub/B/F_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, hierarchical modules, non-root modules

test051:
	$(RM) d2/R/S.hi
	$(RM) d2/R/S$(OBJSUFFIX)
	$(RM) d1/P/Q.hi
	$(RM) d1/P/Q$(OBJSUFFIX)
	"$(TEST_HC)" -v0 --make -id1 -id2 R.S
	test -f d2/R/S.hi
	test -f d2/R/S$(OBJSUFFIX)
	test -f d1/P/Q.hi
	test -f d1/P/Q$(OBJSUFFIX)

# test -odir
test052:
	$(RM) d2/R/S.hi
	$(RM) d1/P/Q.hi
	$(RM) -rf obj
	mkdir obj
	"$(TEST_HC)" -v0 --make -id1 -id2 -odir obj R.S 
	test -f d2/R/S.hi
	test -f obj/R/S$(OBJSUFFIX)
	test -f d1/P/Q.hi
	test -f obj/P/Q$(OBJSUFFIX)

# test -hidir
test053:
	$(RM) hi/R/S.hi
	$(RM) d2/R/S$(OBJSUFFIX)
	$(RM) hi/P/Q.hi
	$(RM) d1/P/Q$(OBJSUFFIX)
	"$(TEST_HC)" -v0 --make -id1 -id2 -hidir hi R.S
	test -f hi/R/S.hi
	test -f d2/R/S$(OBJSUFFIX)
	test -f hi/P/Q.hi
	test -f d1/P/Q$(OBJSUFFIX)

# -----------------------------------------------------------------------------
# Other tests

# Test the -keep-hc-files works without --make
test060:
	$(RM) A.hi A$(OBJSUFFIX) A.hc
	"$(TEST_HC)" -c -keep-hc-files -fvia-C A.hs
	test -f A.hc

# Test the -keep-hc-files works with --make
test061:
	$(RM) A.hi A$(OBJSUFFIX) A.hc
	"$(TEST_HC)" -v0 --make -keep-hc-files -fvia-C A.hs
	test -f A.hc

# Test that -keep-s-files works with --make and -fvia-C
test061a.1:
	$(RM) A.hi A$(OBJSUFFIX) A.s
	"$(TEST_HC)" -v0 --make -keep-s-files -fvia-C A.hs
	test -f A.s

# Test that -keep-s-files works without --make
test061a.2:
	$(RM) A.hi A$(OBJSUFFIX) A.s
	"$(TEST_HC)" -c -keep-s-files -fvia-C A.hs
	test -f A.s

# Test that -main-is works with --make
test062.1:
	$(RM) Hello.hs Hello.hi Hello$(OBJSUFFIX) Hello.hc hello
	echo "module Hello where { hello = putStrLn \"ok\" }" >Hello.hs
	"$(TEST_HC)" -v0 --make -main-is Hello.hello Hello -o hello
	./hello
test062.2:
	$(RM) Hello.hs Hello.hi Hello$(OBJSUFFIX) Hello.hc hello
	echo "module Hello where { main = putStrLn \"ok\" }" >Hello.hs
	"$(TEST_HC)" -v0 --make -main-is Hello Hello -o hello
	./hello
test062.3:
	$(RM) Hello.hs Hello.hi Hello$(OBJSUFFIX) Hello.hc hello
	echo "module Main (hello) where { hello = putStrLn \"ok\" }" >Hello.hs
	"$(TEST_HC)" -v0 --make -main-is hello Hello.hs -o hello
	./hello
test062.4:
	$(RM) B/Hello.hs B/Hello.hi B/Hello$(OBJSUFFIX) B/Hello.hc hello
	echo "module B.Hello (hello) where { hello = putStrLn \"ok\" }" >B/Hello.hs
	"$(TEST_HC)" -v0 --make -main-is B.Hello.hello B/Hello.hs -o hello
	./hello
test062.5:
	$(RM) B/Hello.hs B/Hello.hi B/Hello$(OBJSUFFIX) Hello.hc hello
	echo "module B.Hello (main) where { main = putStrLn \"ok\" }" >B/Hello.hs
	"$(TEST_HC)" -v0 --make -main-is B.Hello B/Hello.hs -o hello
	./hello

# Test that -i deletes the list of search paths.
# The compilation will fail, but we ignore the failure and just
# compare the output, because make might give us a different exit code.
test063:
	$(RM) A.hi A$(OBJSUFFIX) A.hc D.hi D$(OBJSUFFIX) D.hc
	-"$(TEST_HC)" --make -i -v0 D.hs

# Test -E
test064:
	$(RM) A.hi A.hspp
	"$(TEST_HC)" -E A.hs
	test -f A.hspp

# Test -E/-cpp
test065:
	$(RM) A.hi A.hspp
	"$(TEST_HC)" -E -cpp A.hs
	test -f A.hspp

# Test -E/-o
test066:
	$(RM) A.hi A.tmp
	"$(TEST_HC)" -E A.hs -o A.tmp
	test -f A.tmp

# Test -E/-cpp/-o
test067:
	$(RM) A.hi A.tmp
	"$(TEST_HC)" -cpp -E A.hs -o A.tmp
	test -f A.tmp

# Test -C
test068:
	$(RM) A.hi A.hc
	"$(TEST_HC)" -C A.hs
	test -f A.hc

# Test -C/-o
test069:
	$(RM) A.hi A.tmp
	"$(TEST_HC)" -C A.hs -o A.tmp
	test -f A.tmp

# Test -S
test070:
	$(RM) A.hi A.s
	"$(TEST_HC)" -S A.hs
	test -f A.s

# Test -S/-o
test071:
	$(RM) A.hi A.tmp
	"$(TEST_HC)" -S A.hs -o A.tmp
	test -f A.tmp

# No recompilation required, but we requested a .hc file --> should recompile
# Broken briefy in ghc-6.3.
test080:
	$(RM) A.hi A$(OBJSUFFIX) A.hc
	"$(TEST_HC)" -c A.hs
	test -f A$(OBJSUFFIX)
	"$(TEST_HC)" -C A.hs
	test -f A.hc


# Tests for #2248, -o and .exe

define test081
$(RM) $(1).hs $(1).hi $(1)$(OBJSUFFIX) $(1).hc $(1)$(exeext)
echo "module Main where { main = putStrLn \"ok\" }" >$(1).hs
"$(TEST_HC)" --make -v0 $(1).hs
test -f $(1)$(exeext)
endef

test081.1:
	$(call test081, Test_0)
test081.2:
	$(call test081, Test.0)

# -----------------------------------------------------------------------------
# --make tests

# Test for overlap between home module and package module
test100:
	$(RM) overlap/List.hi overlap/List.o
	$(RM) overlap/Overlap.hi overlap/Overlap.o
	"$(TEST_HC)" -v0 --make -ioverlap Overlap

# -----------------------------------------------------------------------------
# dependency-generation tests

DEPFILE= depend

# Test that we can create a dependency file, and that we can
# modify an existing dependency file.
test200:
	$(RM) $(DEPFILE)
	"$(TEST_HC)" -M -dep-makefile $(DEPFILE) D
	test -f $(DEPFILE)
	"$(TEST_HC)" -M -dep-makefile $(DEPFILE) D B.C A
	test -f $(DEPFILE)
	"$(TEST_HC)" -M -dep-makefile $(DEPFILE) D.hs B/C.hs A.hs
	test -f $(DEPFILE)

# -----------------------------------------------------------------------------

2566::
	if "$(TEST_HC)" -c foo.bar; then false else true; fi

.PHONY: mode001
# We use
#     sed '2,$$d'
# rather than
#     head -1
# as the latter gives
#     ghc: <stdout>: hFlush: resource vanished (Broken pipe)
# on OS X
mode001:
	"$(TEST_HC)" --make --help        | sed '2,$$d'
	"$(TEST_HC)" --help --make        | sed '2,$$d'
	"$(TEST_HC)" --interactive --help | sed '2,$$d'
	"$(TEST_HC)" --help --interactive | sed '2,$$d'
	"$(TEST_HC)" --version --help     | sed "s/, version.*//"
	"$(TEST_HC)" --help --version        | sed '2,$$d'
	"$(TEST_HC)" --interactive --version | sed "s/, version.*//"
	"$(TEST_HC)" --version --interactive | sed "s/, version.*//"

# Test for building DLLs with ghc -shared, see #2745
shared001:
	$(RM) Shared001.hi Shared001.o HSdll.dll.a HSdll.dll Shared001_stub.*
	"$(TEST_HC)" -shared Shared001.hs

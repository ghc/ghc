TOP=../..
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

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))
TEST_HC_OPTS_NO_RECOMP_NO_RTSOPTS = $(filter-out -rtsopts,$(TEST_HC_OPTS_NO_RECOMP))

# -----------------------------------------------------------------------------
# One-shot compilations, non-hierarchical modules

test011:
	$(RM) A011.hi
	$(RM) A011$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c A011.hs
	test -f A011.hi
	test -f A011$(OBJSUFFIX)

# test -o
test012:
	$(RM) A012.hi
	$(RM) A012$(OBJSUFFIX)oo
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c A012.hs -o A012$(OBJSUFFIX)oo
	test -f A012$(OBJSUFFIX)oo

# test -ohi
test013:
	$(RM) A013.xhi
	$(RM) A013$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c A013.hs -ohi A013.xhi
	test -f A013.xhi

# test -odir
test014:
	$(RM) -f A014.hi
	$(RM) -rf obj014
	mkdir obj014
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c A014.hs -odir obj014
	test -f obj014/A014$(OBJSUFFIX)
	test -f A014.hi

# test -hidir
test015:
	$(RM) -f A015$(OBJSUFFIX)
	$(RM) -rf hi015
	mkdir hi015
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c A015.hs -hidir hi015
	test -f A015$(OBJSUFFIX)
	test -f hi015/A015.hi

# test stub generation
test016:
	$(RM) F016.hi F016$(OBJSUFFIX) F016_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c F016.hs
	test -f F016_stub.h

# test -stubdir (filename differs from module name)
test017:
	$(RM) F017.hi F017$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c F017.hs -stubdir stub017
	test -f stub017/TestStub017_stub.h

# test -odir with stubs (filename differs from module name)
test018:
	$(RM) -f F018.hi F018$(OBJSUFFIX) F018_stub.h
	$(RM) -rf obj018
	mkdir obj018
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c F018.hs -odir obj018

# test for bug #3093
test018a:
	$(RM) -f F018a.hi F018a_stub.h *.obj.018
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c F018a.hs -osuf obj.018
	test -f F018a.obj.018

# test -outputdir
test019:
	$(RM) -rf out019
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c F019.hs -outputdir out019
	test -f out019/TestStub019.hi
	test -f out019/TestStub019_stub.h

# -----------------------------------------------------------------------------
# One-shot compilation, hierarchical modules

test021:
	$(RM) B021/C.hi
	$(RM) B021/C$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c B021/C.hs
	test -f B021/C.hi
	test -f B021/C$(OBJSUFFIX)

# test -o
test022:
	$(RM) B022/C.hi
	$(RM) B022/C$(OBJSUFFIX)oo
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c B022/C.hs -o B022/C$(OBJSUFFIX)oo
	test -f B022/C$(OBJSUFFIX)oo

# test -ohi
test023:
	$(RM) B023/C.xhi
	$(RM) B023/C$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c B023/C.hs -ohi B023/C.xhi
	test -f B023/C.xhi

# test -odir
test024:
	$(RM) -f B024/C.hi
	$(RM) -rf obj024
	mkdir obj024
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c B024/C.hs -odir obj024
	test -f obj024/B024/C$(OBJSUFFIX)

# test -odir with non-Haskell compilation
test024a:
	$(RM) -rf B024a
	$(RM) -rf obj024a
	mkdir B024a
	mkdir obj024a
	echo >B024a/stub.c
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c B024a/stub.c -odir obj024a
	test -f obj024a/B024a/stub$(OBJSUFFIX)

# test -hidir
test025:
	$(RM) -rf hi025
	mkdir hi025
	$(RM) B025/C$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c B025/C.hs -hidir hi025
	test -f hi025/B025/C.hi

# This is a hierarchical module that lives in a subdirectory.
test026:
	$(RM) d026/P/Q.hi
	$(RM) d026/P/Q$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c d026/P/Q.hs
	test -f d026/P/Q.hi
	test -f d026/P/Q$(OBJSUFFIX)

# test stub generation
# -fvia-C, because we want to check that the .hc file can #include the stub.h
test027:
	$(RM) B027/F.hi B027/F$(OBJSUFFIX) B027/F_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c B027/F.hs -fno-warn-deprecated-flags -fvia-C
	test -f B027/F_stub.h

# test -stubdir
# -fvia-C, because we want to check that the .hc file can #include the stub.h
test028:
	$(RM) B028/F.hi B028/F$(OBJSUFFIX)
	$(RM) stub028/B028/F_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c B028/F.hs -stubdir stub028 -fno-warn-deprecated-flags -fvia-C
	test -f stub028/B028/F_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, flat modules

test031:
	$(RM) A031.hi
	$(RM) A031$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make A031.hs
	test -f A031.hi
	test -f A031$(OBJSUFFIX)

# test -odir
test032:
	$(RM) A032.hi
	$(RM) -rf obj032
	mkdir obj032
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make A032.hs -odir obj032
	test -f obj032/A032$(OBJSUFFIX)

# test -hidir
test033:
	$(RM) -rf hi033
	$(RM) A033$(OBJSUFFIX)
	mkdir hi033
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make A033.hs -hidir hi033
	test -f hi033/A033.hi

# test stub generation
test034:
	$(RM) F034.hi F034$(OBJSUFFIX) F034_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make F034.hs
	test -f F034_stub.h

# test -stubdir (filename differs from module name)
test035:
	$(RM) F035.hi F$(OBJSUFFIX) stub035/Stubs_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make F035.hs -stubdir stub035
	test -f stub035/TestStub035_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, hierarchical modules

test041:
	$(RM) B041/C.hi
	$(RM) B041/C$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make B041/C.hs
	test -f B041/C.hi
	test -f B041/C$(OBJSUFFIX)

# test -odir
test042:
	$(RM) -rf obj042
	mkdir obj042
	$(RM) B042/C.hi
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make B042/C.hs -odir obj042
	test -f obj042/B042/C$(OBJSUFFIX)

# test -hidir
test043:
	$(RM) -f B043/C$(OBJSUFFIX)
	$(RM) -rf hi043
	mkdir hi043
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make B043/C.hs -hidir hi043
	test -f hi043/B043/C.hi

# test stub generation
test044:
	$(RM) B044/F.hi B044/F$(OBJSUFFIX) B044/F_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make B044/F.hs
	test -f B044/F_stub.h

# test -stubdir
test045:
	$(RM) B045/F.hi B045/F$(OBJSUFFIX)
	$(RM) stub045/B045/F_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make B045/F.hs -stubdir stub045
	test -f stub045/B045/F_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, hierarchical modules, non-root modules

test051:
	$(RM) d051_2/R/S.hi
	$(RM) d051_2/R/S$(OBJSUFFIX)
	$(RM) d051_1/P/Q.hi
	$(RM) d051_1/P/Q$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -id051_1 -id051_2 R.S
	test -f d051_2/R/S.hi
	test -f d051_2/R/S$(OBJSUFFIX)
	test -f d051_1/P/Q.hi
	test -f d051_1/P/Q$(OBJSUFFIX)

# test -odir
test052:
	$(RM) d052_2/R/S.hi
	$(RM) d052_1/P/Q.hi
	$(RM) -rf obj052
	mkdir obj052
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -id052_1 -id052_2 -odir obj052 R.S
	test -f d052_2/R/S.hi
	test -f obj052/R/S$(OBJSUFFIX)
	test -f d052_1/P/Q.hi
	test -f obj052/P/Q$(OBJSUFFIX)

# test -hidir
test053:
	$(RM) hi053/R/S.hi
	$(RM) d053_2/R/S$(OBJSUFFIX)
	$(RM) hi053/P/Q.hi
	$(RM) d053_1/P/Q$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -id053_1 -id053_2 -hidir hi053 R.S
	test -f hi053/R/S.hi
	test -f d053_2/R/S$(OBJSUFFIX)
	test -f hi053/P/Q.hi
	test -f d053_1/P/Q$(OBJSUFFIX)

# -----------------------------------------------------------------------------
# Other tests

# Test that -keep-s-files works with --make and -fvia-C
test061a:
	$(RM) A061a.hi A061a$(OBJSUFFIX) A061a.s
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -keep-s-files -fno-warn-deprecated-flags -fvia-C A061a.hs
	test -f A061a.s

# Test that -keep-s-files works without --make
test061b:
	$(RM) A061b.hi A061b$(OBJSUFFIX) A061b.s
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c -keep-s-files -fno-warn-deprecated-flags -fvia-C A061b.hs
	test -f A061b.s

# Test that -main-is works with --make
test062a:
	$(RM) Hello062a.hs Hello062a.hi Hello062a$(OBJSUFFIX) Hello062a.hc hello062a
	echo "module Hello062a where { hello062a = putStrLn \"ok\" }" >Hello062a.hs
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -main-is Hello062a.hello062a Hello062a -o hello062a
	./hello062a
test062b:
	$(RM) Hello062b.hs Hello062b.hi Hello062b$(OBJSUFFIX) Hello062b.hc hello062b
	echo "module Hello062b where { main = putStrLn \"ok\" }" >Hello062b.hs
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -main-is Hello062b Hello062b -o hello062b
	./hello062b
test062c:
	$(RM) Hello062c.hs Hello062c.hi Hello062c$(OBJSUFFIX) Hello062c.hc hello062c
	echo "module Main (hello062c) where { hello062c = putStrLn \"ok\" }" >Hello062c.hs
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -main-is hello062c Hello062c.hs -o hello062c
	./hello062c
test062d:
	$(RM) -rf B062d
	$(RM) hello062d
	mkdir B062d
	echo "module B062d.Hello062d (hello062d) where { hello062d = putStrLn \"ok\" }" >B062d/Hello062d.hs
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -main-is B062d.Hello062d.hello062d B062d/Hello062d.hs -o hello062d
	./hello062d
test062e:
	$(RM) -rf B062e
	$(RM) Hello062e.hc hello062e
	mkdir B062e
	echo "module B062e.Hello062e (main) where { main = putStrLn \"ok\" }" >B062e/Hello062e.hs
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -main-is B062e.Hello062e B062e/Hello062e.hs -o hello062e
	./hello062e

# Test that -i deletes the list of search paths.
# The compilation will fail, but we ignore the failure and just
# compare the output, because make might give us a different exit code.
test063:
	$(RM) A063.hi A063$(OBJSUFFIX) A063.hc D063.hi D063$(OBJSUFFIX) D063.hc
	-"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --make -i -v0 D063.hs

# Test -E
test064:
	$(RM) A064.hi A064.hspp
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -E A064.hs
	test -f A064.hspp

# Test -E/-cpp
test065:
	$(RM) A065.hi A065.hspp
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -E -cpp A065.hs
	test -f A065.hspp

# Test -E/-o
test066:
	$(RM) A066.hi A066.tmp
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -E A066.hs -o A066.tmp
	test -f A066.tmp

# Test -E/-cpp/-o
test067:
	$(RM) A067.hi A067.tmp
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -cpp -E A067.hs -o A067.tmp
	test -f A067.tmp

# Test -S
test070:
	$(RM) A070.hi A070.s
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -S A070.hs
	test -f A070.s

# Test -S/-o
test071:
	$(RM) A071.hi A071.tmp
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -S A071.hs -o A071.tmp
	test -f A071.tmp


# Tests for #2248, -o and .exe

define test081
$(RM) $(1).hs $(1).hi $(1)$(OBJSUFFIX) $(1).hc $(1)$(exeext)
echo "module Main where { main = putStrLn \"ok\" }" >$(1).hs
"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --make -v0 $(1).hs
test -f $(1)$(exeext)
endef

test081a:
	$(call test081, Test_081a)
test081b:
	$(call test081, Test.081b)

# -----------------------------------------------------------------------------
# --make tests

# Test for overlap between home module and package module
test100:
	$(RM) overlap/List.hi overlap/List.o
	$(RM) overlap/Overlap.hi overlap/Overlap.o
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -ioverlap Overlap

# -----------------------------------------------------------------------------
# dependency-generation tests

DEPFILE200 = depend200

# Test that we can create a dependency file, and that we can
# modify an existing dependency file.
test200:
	$(RM) $(DEPFILE200)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -M -dep-suffix "" -dep-makefile $(DEPFILE200) D200
	test -f $(DEPFILE200)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -M -dep-suffix "" -dep-makefile $(DEPFILE200) D200 B200.C A200
	test -f $(DEPFILE200)
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -M -dep-suffix "" -dep-makefile $(DEPFILE200) D200.hs B200/C.hs A200.hs
	test -f $(DEPFILE200)

# -----------------------------------------------------------------------------

T2566::
	if "$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c foo2566.bar; then false else true; fi

.PHONY: mode001
# We use
#     sed '2,$$d'
# rather than
#     head -1
# as the latter gives
#     ghc: <stdout>: hFlush: resource vanished (Broken pipe)
# on OS X
mode001:
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --make --help        | sed '2,$$d'
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --help --make        | sed '2,$$d'
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --interactive --help | sed '2,$$d'
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --help --interactive | sed '2,$$d'
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --version --help     | sed "s/, version.*//"
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --help --version        | sed '2,$$d'
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --interactive --version | sed "s/, version.*//"
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --version --interactive | sed "s/, version.*//"

# Test for building DLLs with ghc -shared, see #2745
shared001:
	$(RM) Shared001.hi Shared001.o HSdll.dll.a HSdll.dll Shared001_stub.*
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP_NO_RTSOPTS) -v0 -shared Shared001.hs

# -----------------------------------------------------------------------------

T3674:
	$(RM) T3674*.o T3674*.hi T3674_pre
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --make T3674_pre.hs
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --make T3674.hs

.PHONY: rtsopts001
rtsopts001:
	$(RM) rtsOpts$(exeext) rtsOpts.hi rtsOpts.o
	"$(TEST_HC)" $(filter-out -rtsopts,$(TEST_HC_OPTS_NO_RECOMP)) -v0 --make rtsOpts -rtsopts
	echo "Normal"
	-./rtsOpts +RTS -C0 2>&1
	$(RM) rtsOpts$(exeext) rtsOpts.hi rtsOpts.o
	"$(TEST_HC)" $(filter-out -rtsopts,$(TEST_HC_OPTS_NO_RECOMP)) -v0 --make rtsOpts
	echo "No RTS opts"
	-./rtsOpts +RTS -C0 2>&1

.PHONY: rtsopts002
rtsopts002:
	$(RM) rtsopts002$(exeext) rtsopts002.hi rtsopts002.o
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make rtsopts002 -rtsopts=some
	./rtsopts002
	./rtsopts002 +RTS --info >/dev/null
	if ./rtsopts002 +RTS -C0; then exit 1; else exit 0; fi
	GHCRTS= ./rtsopts002
	GHCRTS=--info ./rtsopts002 >/dev/null
	if GHCRTS=-C0 ./rtsopts002; then exit 1; else exit 0; fi

.PHONY: withRtsOpts
withRtsOpts:
	$(RM) withRtsOpts$(exeext) withRtsOpts.hi withRtsOpts.o withRtsOpts.out
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make withRtsOpts -with-rtsopts=--info
	./withRtsOpts | head -1
	# Also test for non-safe RTS opts, these should be allowed too,
	# since the author has explicitly requested them. (#5114)
	$(RM) withRtsOpts$(exeext) withRtsOpts.hi withRtsOpts.o withRtsOpts.out
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make withRtsOpts -with-rtsopts=-twithRtsOpts.out
	./withRtsOpts | head -1
	test -f withRtsOpts.out

.PHONY: T3364
T3364:
	-"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -H

.PHONY: T706
T706:
	cp T706a.hs T706.hs
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) $(ghciWayFlags) -c T706.hs
	cp T706b.hs T706.hs
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) $(ghciWayFlags) -c T706.hs
	echo "test" | "$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) --interactive -ignore-dot-ghci -v0 T706

.PHONY: T5584
T5584:
	$(RM) -rf T5584_out
	mkdir T5584_out
	"$(TEST_HC)" -odir T5584_out -c T5584/A.hs-boot

.PHONY: T5198
T5198:
	$(RM) -rf T5198dump
	[ ! -d T5198dump ]
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c T5198.hs -dumpdir T5198dump -ddump-to-file -ddump-simpl
	[ -f T5198dump/T5198.dump-simpl ]

.PHONY: T7060a
T7060a:
	$(RM) -rf T7060dump
	[ ! -d T7060dump ]
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -O -c T7060.hs -dumpdir T7060dump -ddump-to-file -ddump-rules
	[ -f T7060dump/T7060.dump-rules ]

.PHONY: T7060b
T7060b:
	$(RM) -rf T7060dump
	[ ! -d T7060dump ]
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -O -c T7060.hs -dumpdir T7060dump -ddump-to-file -ddump-rule-firings
	[ -f T7060dump/T7060.dump-rule-firings ]

.PHONY: T7060c
T7060c:
	$(RM) -rf T7060dump
	[ ! -d T7060dump ]
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -O -c T7060.hs -dumpdir T7060dump -ddump-to-file -ddump-rule-rewrites
	[ -f T7060dump/T7060.dump-rule-rewrites ]

.PHONY: T7060
T7060: T7060a T7060b T7060c

.PHONY: T7130
T7130:
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -fflul-laziness

.PHONY: T7563
T7563:
	-"$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -C T7563.hs

.PHONY: T6037
T6037:
	-LC_ALL=C "$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c T6037.hs

.PHONY: T2507
T2507:
	-LC_ALL=C "$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -c T2507.hs


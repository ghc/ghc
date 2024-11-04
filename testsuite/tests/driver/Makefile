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

TEST_HC_OPTS_NO_RTSOPTS = $(filter-out -rtsopts,$(TEST_HC_OPTS))

# -----------------------------------------------------------------------------
# One-shot compilations, non-hierarchical modules

test011:
	$(RM) A011.hi
	$(RM) A011$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -c A011.hs
	test -f A011.hi
	test -f A011$(OBJSUFFIX)

# test -o
test012:
	$(RM) A012.hi
	$(RM) A012$(OBJSUFFIX)oo
	"$(TEST_HC)" $(TEST_HC_OPTS) -c A012.hs -o A012$(OBJSUFFIX)oo
	test -f A012$(OBJSUFFIX)oo

# test -ohi
test013:
	$(RM) A013.xhi
	$(RM) A013$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -c A013.hs -ohi A013.xhi
	test -f A013.xhi

# test -odir
test014:
	$(RM) -f A014.hi
	$(RM) -rf obj014
	mkdir obj014
	"$(TEST_HC)" $(TEST_HC_OPTS) -c A014.hs -odir obj014
	test -f obj014/A014$(OBJSUFFIX)
	test -f A014.hi

# test -hidir
test015:
	$(RM) -f A015$(OBJSUFFIX)
	$(RM) -rf hi015
	mkdir hi015
	"$(TEST_HC)" $(TEST_HC_OPTS) -c A015.hs -hidir hi015
	test -f A015$(OBJSUFFIX)
	test -f hi015/A015.hi

# test stub generation
test016:
	$(RM) F016.hi F016$(OBJSUFFIX) F016_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -c F016.hs
	test -f F016_stub.h

# test -stubdir (filename differs from module name)
test017:
	$(RM) F017.hi F017$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -c F017.hs -stubdir stub017
	test -f stub017/TestStub017_stub.h

# test -odir with stubs (filename differs from module name)
test018:
	$(RM) -f F018.hi F018$(OBJSUFFIX) F018_stub.h
	$(RM) -rf obj018
	mkdir obj018
	"$(TEST_HC)" $(TEST_HC_OPTS) -c F018.hs -odir obj018

# test for bug #3093
test018a:
	$(RM) -f F018a.hi F018a_stub.h *.obj.018
	"$(TEST_HC)" $(TEST_HC_OPTS) -c F018a.hs -osuf obj.018
	test -f F018a.obj.018

# test -outputdir
test019:
	$(RM) -rf out019
	"$(TEST_HC)" $(TEST_HC_OPTS) -c F019.hs -outputdir out019
	test -f out019/TestStub019.hi
	test -f out019/TestStub019_stub.h

# -----------------------------------------------------------------------------
# One-shot compilation, hierarchical modules

test021:
	$(RM) B021/C.hi
	$(RM) B021/C$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B021/C.hs
	test -f B021/C.hi
	test -f B021/C$(OBJSUFFIX)

# test -o
test022:
	$(RM) B022/C.hi
	$(RM) B022/C$(OBJSUFFIX)oo
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B022/C.hs -o B022/C$(OBJSUFFIX)oo
	test -f B022/C$(OBJSUFFIX)oo

# test -ohi
test023:
	$(RM) B023/C.xhi
	$(RM) B023/C$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B023/C.hs -ohi B023/C.xhi
	test -f B023/C.xhi

# test -odir
test024:
	$(RM) -f B024/C.hi
	$(RM) -rf obj024
	mkdir obj024
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B024/C.hs -odir obj024
	test -f obj024/B024/C$(OBJSUFFIX)

# test -odir with non-Haskell compilation
test024a:
	$(RM) -rf B024a
	$(RM) -rf obj024a
	mkdir B024a
	mkdir obj024a
	echo >B024a/stub.c
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B024a/stub.c -odir obj024a
	test -f obj024a/B024a/stub$(OBJSUFFIX)

# test -hidir
test025:
	$(RM) -rf hi025
	mkdir hi025
	$(RM) B025/C$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B025/C.hs -hidir hi025
	test -f hi025/B025/C.hi

# This is a hierarchical module that lives in a subdirectory.
test026:
	$(RM) d026/P/Q.hi
	$(RM) d026/P/Q$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -c d026/P/Q.hs
	test -f d026/P/Q.hi
	test -f d026/P/Q$(OBJSUFFIX)

# test stub generation
# -fvia-C, because we want to check that the .hc file can #include the stub.h
test027:
	$(RM) B027/F.hi B027/F$(OBJSUFFIX) B027/F_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B027/F.hs -fno-warn-deprecated-flags -fvia-C
	test -f B027/F_stub.h

# test -stubdir
# -fvia-C, because we want to check that the .hc file can #include the stub.h
test028:
	$(RM) B028/F.hi B028/F$(OBJSUFFIX)
	$(RM) stub028/B028/F_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B028/F.hs -stubdir stub028 -fno-warn-deprecated-flags -fvia-C
	test -f stub028/B028/F_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, flat modules

test031:
	$(RM) A031.hi
	$(RM) A031$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make A031.hs
	test -f A031.hi
	test -f A031$(OBJSUFFIX)

# test -odir
test032:
	$(RM) A032.hi
	$(RM) -rf obj032
	mkdir obj032
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make A032.hs -odir obj032
	test -f obj032/A032$(OBJSUFFIX)

# test -hidir
test033:
	$(RM) -rf hi033
	$(RM) A033$(OBJSUFFIX)
	mkdir hi033
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make A033.hs -hidir hi033
	test -f hi033/A033.hi

# test stub generation
test034:
	$(RM) F034.hi F034$(OBJSUFFIX) F034_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make F034.hs
	test -f F034_stub.h

# test -stubdir (filename differs from module name)
test035:
	$(RM) F035.hi F$(OBJSUFFIX) stub035/Stubs_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make F035.hs -stubdir stub035
	test -f stub035/TestStub035_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, hierarchical modules

test041:
	$(RM) B041/C.hi
	$(RM) B041/C$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make B041/C.hs
	test -f B041/C.hi
	test -f B041/C$(OBJSUFFIX)

# test -odir
test042:
	$(RM) -rf obj042
	mkdir obj042
	$(RM) B042/C.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make B042/C.hs -odir obj042
	test -f obj042/B042/C$(OBJSUFFIX)

# test -odir with stubs
test042stub:
	$(RM) -rf obj042stub
	mkdir obj042stub
	$(RM) B042stub/C.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make B042stub/C.hs -odir obj042stub
	test -f obj042stub/B042stub/C$(OBJSUFFIX)

# test -hidir
test043:
	$(RM) -f B043/C$(OBJSUFFIX)
	$(RM) -rf hi043
	mkdir hi043
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make B043/C.hs -hidir hi043
	test -f hi043/B043/C.hi

# test stub generation
test044:
	$(RM) B044/F.hi B044/F$(OBJSUFFIX) B044/F_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make B044/F.hs
	test -f B044/F_stub.h

# test -stubdir
test045:
	$(RM) B045/F.hi B045/F$(OBJSUFFIX)
	$(RM) stub045/B045/F_stub.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make B045/F.hs -stubdir stub045
	test -f stub045/B045/F_stub.h

# -----------------------------------------------------------------------------
# Compilation-manager compilations, hierarchical modules, non-root modules

test051:
	$(RM) d051_2/R/S.hi
	$(RM) d051_2/R/S$(OBJSUFFIX)
	$(RM) d051_1/P/Q.hi
	$(RM) d051_1/P/Q$(OBJSUFFIX)
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -id051_1 -id051_2 R.S
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
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -id052_1 -id052_2 -odir obj052 R.S
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
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -id053_1 -id053_2 -hidir hi053 R.S
	test -f hi053/R/S.hi
	test -f d053_2/R/S$(OBJSUFFIX)
	test -f hi053/P/Q.hi
	test -f d053_1/P/Q$(OBJSUFFIX)

# -----------------------------------------------------------------------------
# Other tests

# Test that -keep-s-files works with --make and -fvia-C
test061a:
	$(RM) A061a.hi A061a$(OBJSUFFIX) A061a.s
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -keep-s-files -fno-warn-deprecated-flags -fvia-C A061a.hs
	test -f A061a.s

# Test that -keep-s-files works without --make
test061b:
	$(RM) A061b.hi A061b$(OBJSUFFIX) A061b.s
	"$(TEST_HC)" $(TEST_HC_OPTS) -c -keep-s-files -fno-warn-deprecated-flags -fvia-C A061b.hs
	test -f A061b.s

# Test that -main-is works with --make
test062a:
	$(RM) Hello062a.hs Hello062a.hi Hello062a$(OBJSUFFIX) Hello062a.hc hello062a
	echo "module Hello062a where { hello062a = putStrLn \"ok\" }" >Hello062a.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -main-is Hello062a.hello062a Hello062a -o hello062a
	./hello062a
test062b:
	$(RM) Hello062b.hs Hello062b.hi Hello062b$(OBJSUFFIX) Hello062b.hc hello062b
	echo "module Hello062b where { main = putStrLn \"ok\" }" >Hello062b.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -main-is Hello062b Hello062b -o hello062b
	./hello062b
test062c:
	$(RM) Hello062c.hs Hello062c.hi Hello062c$(OBJSUFFIX) Hello062c.hc hello062c
	echo "module Main (hello062c) where { hello062c = putStrLn \"ok\" }" >Hello062c.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -main-is hello062c Hello062c.hs -o hello062c
	./hello062c
test062d:
	$(RM) -rf B062d
	$(RM) hello062d
	mkdir B062d
	echo "module B062d.Hello062d (hello062d) where { hello062d = putStrLn \"ok\" }" >B062d/Hello062d.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -main-is B062d.Hello062d.hello062d B062d/Hello062d.hs -o hello062d
	./hello062d
test062e:
	$(RM) -rf B062e
	$(RM) Hello062e.hc hello062e
	mkdir B062e
	echo "module B062e.Hello062e (main) where { main = putStrLn \"ok\" }" >B062e/Hello062e.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -main-is B062e.Hello062e B062e/Hello062e.hs -o hello062e
	./hello062e

# Test that -i deletes the list of search paths.
# The compilation will fail, but we ignore the failure and just
# compare the output, because make might give us a different exit code.
test063:
	$(RM) A063.hi A063$(OBJSUFFIX) A063.hc D063.hi D063$(OBJSUFFIX) D063.hc
	-"$(TEST_HC)" $(TEST_HC_OPTS) --make -i -v0 D063.hs

# Test -E
test064:
	$(RM) A064.hi A064.hspp
	"$(TEST_HC)" $(TEST_HC_OPTS) -E A064.hs
	test -f A064.hspp

# Test -E/-cpp
test065:
	$(RM) A065.hi A065.hspp
	"$(TEST_HC)" $(TEST_HC_OPTS) -E -cpp A065.hs
	test -f A065.hspp

# Test -E/-o
test066:
	$(RM) A066.hi A066.tmp
	"$(TEST_HC)" $(TEST_HC_OPTS) -E A066.hs -o A066.tmp
	test -f A066.tmp

# Test -E/-cpp/-o
test067:
	$(RM) A067.hi A067.tmp
	"$(TEST_HC)" $(TEST_HC_OPTS) -cpp -E A067.hs -o A067.tmp
	test -f A067.tmp

# Test -S
test070:
	$(RM) A070.hi A070.s
	"$(TEST_HC)" $(TEST_HC_OPTS) -S A070.hs
	test -f A070.s

# Test -S/-o
test071:
	$(RM) A071.hi A071.tmp
	"$(TEST_HC)" $(TEST_HC_OPTS) -S A071.hs -o A071.tmp
	test -f A071.tmp


# Tests for #2248, -o and .exe

define test081
$(RM) $(1).hs $(1).hi $(1)$(OBJSUFFIX) $(1).hc $(1)$(exeext)
echo "module Main where { main = putStrLn \"ok\" }" >$(1).hs
"$(TEST_HC)" $(TEST_HC_OPTS) --make -v0 $(1).hs
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
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -ioverlap Overlap

# -----------------------------------------------------------------------------
# dependency-generation tests

DEPFILE200 = depend200

# Test that we can create a dependency file, and that we can
# modify an existing dependency file.
test200:
	$(RM) $(DEPFILE200)
	"$(TEST_HC)" $(TEST_HC_OPTS) -M -dep-suffix "" -dep-makefile $(DEPFILE200) D200
	test -f $(DEPFILE200)
	"$(TEST_HC)" $(TEST_HC_OPTS) -M -dep-suffix "" -dep-makefile $(DEPFILE200) D200 B200.C A200
	test -f $(DEPFILE200)
	"$(TEST_HC)" $(TEST_HC_OPTS) -M -dep-suffix "" -dep-makefile $(DEPFILE200) D200.hs B200/C.hs A200.hs
	test -f $(DEPFILE200)

# -----------------------------------------------------------------------------

T2566::
	if "$(TEST_HC)" $(TEST_HC_OPTS) -c foo2566.bar; then false else true; fi

.PHONY: mode001
# We use
#     sed '2,$$d'
# rather than
#     head -1
# as the latter gives
#     ghc: <stdout>: hFlush: resource vanished (Broken pipe)
# on OS X
mode001:
	"$(TEST_HC)" $(TEST_HC_OPTS) --make --help              | sed '2,$$d'
	"$(TEST_HC)" $(TEST_HC_OPTS) --help --make              | sed '2,$$d'

	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) --help         | sed '2,$$d'
	"$(TEST_HC)" --help $(TEST_HC_OPTS_INTERACTIVE)         | sed '2,$$d'

	"$(TEST_HC)" $(TEST_HC_OPTS) --version --help           | sed "s/, version.*//"
	"$(TEST_HC)" $(TEST_HC_OPTS) --help --version           | sed '2,$$d'

	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) --version      | sed "s/, version.*//"
	"$(TEST_HC)" --version $(TEST_HC_OPTS_INTERACTIVE)      | sed "s/, version.*//"

	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) --show-options | grep -- --show-options | sed '2,$$d'
	"$(TEST_HC)" --show-options $(TEST_HC_OPTS_INTERACTIVE) | grep -- --show-options | sed '2,$$d'

# Test for building DLLs with ghc -shared, see #2745
shared001:
	$(RM) Shared001.hi Shared001.o HSdll.dll.a HSdll.dll Shared001_stub.*
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RTSOPTS) -v0 -shared Shared001.hs

static001:
	$(RM) Static001.hi Static001.o liba.a Static001_stub.*
	"$(TEST_HC)" $(TEST_HC_OPTS_NO_RTSOPTS) -v0 -staticlib Static001.hs

# -----------------------------------------------------------------------------

T3674:
	$(RM) T3674*.o T3674*.hi T3674_pre
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -v0 T3674_pre.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -v0 T3674.hs

.PHONY: rtsopts001
rtsopts001:
	$(RM) rtsOpts$(exeext) rtsOpts.hi rtsOpts.o
	"$(TEST_HC)" $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 --make rtsOpts -rtsopts
	echo "Normal"
	-./rtsOpts +RTS -C0 2>&1
	$(RM) rtsOpts$(exeext) rtsOpts.hi rtsOpts.o
	"$(TEST_HC)" $(filter-out -rtsopts,$(TEST_HC_OPTS)) -v0 --make rtsOpts
	echo "No RTS opts"
	-./rtsOpts +RTS -C0 2>&1

.PHONY: rtsopts002
rtsopts002:
	$(RM) rtsopts002$(exeext) rtsopts002.hi rtsopts002.o
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make rtsopts002 -rtsopts=some
	./rtsopts002
	./rtsopts002 +RTS --info >/dev/null
	if ./rtsopts002 +RTS -C0; then exit 1; else exit 0; fi
	GHCRTS= ./rtsopts002
	GHCRTS=--info ./rtsopts002 >/dev/null
	if GHCRTS=-C0 ./rtsopts002; then exit 1; else exit 0; fi

.PHONY: withRtsOpts
withRtsOpts:
	$(RM) withRtsOpts$(exeext) withRtsOpts.hi withRtsOpts.o withRtsOpts.out
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make withRtsOpts -with-rtsopts=--info
	./withRtsOpts | head -1
	# Also test for non-safe RTS opts, these should be allowed too,
	# since the author has explicitly requested them. (#5114)
	$(RM) withRtsOpts$(exeext) withRtsOpts.hi withRtsOpts.o withRtsOpts.out
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make withRtsOpts -with-rtsopts=-twithRtsOpts.out
	./withRtsOpts | head -1
	test -f withRtsOpts.out

.PHONY: T3364
T3364:
	-"$(TEST_HC)" $(TEST_HC_OPTS) -H

.PHONY: T706
T706:
	cp T706a.hs T706.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghciWayFlags) -c T706.hs
	cp T706b.hs T706.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghciWayFlags) -c T706.hs
	echo "test" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T706

.PHONY: T5584
T5584:
	$(RM) -rf T5584_out
	mkdir T5584_out
	"$(TEST_HC)" -odir T5584_out -c T5584_in/A.hs-boot

.PHONY: T5198
T5198:
	$(RM) -rf T5198dump
	[ ! -d T5198dump ]
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T5198.hs -dumpdir T5198dump -ddump-to-file -ddump-simpl
	[ -f T5198dump/T5198.dump-simpl ]

.PHONY: T7060a
T7060a:
	$(RM) -rf T7060dump
	[ ! -d T7060dump ]
	"$(TEST_HC)" $(TEST_HC_OPTS) -O -c T7060.hs -dumpdir T7060dump -ddump-to-file -ddump-rules
	[ -f T7060dump/T7060.dump-rules ]

.PHONY: T7060b
T7060b:
	$(RM) -rf T7060dump
	[ ! -d T7060dump ]
	"$(TEST_HC)" $(TEST_HC_OPTS) -O -c T7060.hs -dumpdir T7060dump -ddump-to-file -ddump-rule-firings
	[ -f T7060dump/T7060.dump-rule-firings ]

.PHONY: T7060c
T7060c:
	$(RM) -rf T7060dump
	[ ! -d T7060dump ]
	"$(TEST_HC)" $(TEST_HC_OPTS) -O -c T7060.hs -dumpdir T7060dump -ddump-to-file -ddump-rule-rewrites
	[ -f T7060dump/T7060.dump-rule-rewrites ]

.PHONY: T7060
T7060: T7060a T7060b T7060c

.PHONY: T7130
T7130:
	"$(TEST_HC)" $(TEST_HC_OPTS) -fflul-laziness

.PHONY: T7563
T7563:
	-"$(TEST_HC)" $(TEST_HC_OPTS) -C T7563.hs

# Below we set LC_ALL=C to request standard ASCII output in the resulting error
# messages. Unfortunately, versions of MacOS prior to Sonoma and Windows still
# use a Unicode encoding even with LC_ALL=C, so we expect these tests to fail there.

.PHONY: T6037
T6037:
	-LC_ALL=C "$(TEST_HC)" $(TEST_HC_OPTS) -c T6037.hs

.PHONY: T2507
T2507:
	-LC_ALL=C "$(TEST_HC)" $(TEST_HC_OPTS) -c T2507.hs

.PHONY: T8959a
T8959a:
	-LC_ALL=C "$(TEST_HC)" $(TEST_HC_OPTS) -c T8959a.hs -XUnicodeSyntax

.PHONY: T703
T703:
	$(RM) -rf T703
	[ ! -d T703 ]
	"$(TEST_HC)" $(TEST_HC_OPTS) --make T703.hs -v0
	! readelf -W -l T703 2>/dev/null | grep 'GNU_STACK' | grep -q 'RWE'

.PHONY: T2182
T2182:
	$(RM) T2182_A.hi T2182.hi
	! "$(TEST_HC)" $(TEST_HC_OPTS) --make T2182_A.hs T2182.hs -v0
	! "$(TEST_HC)" $(TEST_HC_OPTS) --make T2182.hs T2182_A.hs -v0

.PHONY: write_interface_oneshot
write_interface_oneshot:
	$(RM) -rf write_interface_oneshot/A011.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -hidir write_interface_oneshot_hidir -fno-code -fwrite-interface -c A011.hs
	test -f write_interface_oneshot_hidir/A011.hi

.PHONY: write_interface_make
write_interface_make:
	$(RM) -rf write_interface_make/A011.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -hidir write_interface_make_hidir -fno-code -fwrite-interface --make A011.hs
	test -f write_interface_make_hidir/A011.hi

.PHONY: T9938
T9938:
	$(RM) -rf T9938.o T9938.hi T9938
	"$(TEST_HC)" $(TEST_HC_OPTS) -O2 -c T9938.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -O2 T9938.o -o T9938

.PHONY: T9938B
T9938B:
	$(RM) -rf T9938B.o T9938B.hi T9938B
	"$(TEST_HC)" $(TEST_HC_OPTS) -O2 -c T9938B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -O2 T9938B.o -o T9938B

.PHONY: T10182
T10182:
	$(RM) -rf T10182.o T10182a.o T10182.o-boot T10182.hi T10182a.hi T10182.hi-boot
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T10182.hs-boot
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T10182a.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T10182.hs

.PHONY: T10320
T10320:
	$(RM) -rf T10320 T10320.dump-rule-rewrites T10320.hi T10320.o
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -ddump-to-file -ddump-rule-rewrites -fenable-rewrite-rules T10320.hs
	[ -s T10320.dump-rule-rewrites ]
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -fforce-recomp -ddump-to-file -ddump-rule-rewrites T10320.hs
	[ -f T10320.dump-rule-rewrites ] && [ ! -s T10320.dump-rule-rewrites ]

.PHONY: T10869
T10869:
	$(RM) -rf T10869.hi T10869.o T10869.hspp T10869
	$(RM) -rf T10869A.hi T10869A.o T10869A.hspp
	"$(TEST_HC)" $(TEST_HC_OPTS) -c -keep-hscpp-files T10869A.hs T10869.hs
	test -f T10869.hscpp
	test -f T10869A.hscpp

.PHONY: T12135
T12135:
	$(RM) -rf T12135.o T12135.hi T12135 T12135a T12135b
	mkdir T12135a T12135b
	cp T12135a.h T12135a/T12135.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -IT12135b -IT12135a --make T12135.hs
	./T12135
	cp T12135b.h T12135b/T12135.h
	"$(TEST_HC)" $(TEST_HC_OPTS) -IT12135b -IT12135a --make T12135.hs
	./T12135

.PHONY: T10923
T10923:
	$(RM) -rf T10923.o T10923.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -O0 -c T10923.hs
	# should NOT output "compilation is NOT required"
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -O -c T10923.hs

.PHONY: T13604
T13604:
	$(RM) -rf T13604.o T13604.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -O0 -c T13604.hs
	# SHOULD output "compilation is NOT required"
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -O -c -fignore-optim-changes T13604.hs

.PHONY: T13604a
T13604a:
	$(RM) -rf T13604a.o T13604a.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -O0 -c -fhpc T13604a.hs
	# SHOULD output "compilation is NOT required"
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -O0 -c -fignore-hpc-changes T13604a.hs

.PHONY: T12955
T12955:
	! "$(TEST_HC)" $(TEST_HC_OPTS) --make T12955
	! "$(TEST_HC)" $(TEST_HC_OPTS) --make T12955 -fbuilding-cabal-package

.PHONY: T12971
T12971:
	mkdir -p ä
	TMP=ä "$(TEST_HC)" $(TEST_HC_OPTS) --make T12971

.PHONY: T14452
T14452:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v -c -O2 T14452.hs 2>&1 | grep 'O3' \
	| awk 'NF{print $$NF; exit}' | sed 's/\"//g'
	# awk: extract last word of line, sed: remove quotes

.PHONY: T17786
T17786:
	# N.B. Check for the presence of caret diagnostics.
	"$(TEST_HC)" $(TEST_HC_OPTS) -fdiagnostics-show-caret -v "$(shell cygpath -w -a T17786.hs)" 2>&1 | grep --quiet '3 |'

.PHONY: OneShotTH
OneShotTH:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c OneShotTH.hs
	# The module should *not* be recompiled
	"$(TEST_HC)" $(TEST_HC_OPTS) -c OneShotTH.hs

T17481:
	echo 'main = putStrLn "Hello from A"' > A.hs
	echo 'main = putStrLn "Hello from B"' > B.hs

	ln -f A.hs Main.hs
	echo "Main.hs is now:"
	cat Main.hs
	echo "Compiling and running Main.hs:"
	"$(TEST_HC)" $(TEST_HC_OPTS) Main.hs && ./Main

	ln -f B.hs Main.hs
	echo "Main.hs is now:"
	cat Main.hs
	echo "Compiling and running Main.hs:"
	"$(TEST_HC)" $(TEST_HC_OPTS) Main.hs && ./Main

	echo "Touching Main.hs"
	touch Main.hs
	echo "Compiling and running Main.hs:"
	"$(TEST_HC)" $(TEST_HC_OPTS) Main.hs && ./Main

T14923:
	echo "{-# LANGUAGE UndecidableInstances #-}" > Foo.hs
	echo "module Foo where" >> Foo.hs

	cat Foo.hs

	"$(TEST_HC)" $(TEST_HC_OPTS) -c Foo.hs;

	sed -i -e 's/^{-\#/--&/' Foo.hs # comment out the pragma

	# Check sed actually worked
	cat Foo.hs

	# This should recompile
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Foo.hs;
	# This should not
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Foo.hs;
	# And this should not
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Foo.hs;

T20084:
	# Compile
	"$(TEST_HC)" $(TEST_HC_OPTS) T20084.hs
	# Don't compile
	"$(TEST_HC)" $(TEST_HC_OPTS) T20084.hs

.PHONY: T20316
T20316:
	echo 'main = print "Hello World"' > T20316.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -fno-code T20316.hs -ddump-to-file -ddump-timings;
	echo "*** non-module.dump-timings ***"
	cat non-module.dump-timings | grep alloc | sed 's/alloc.*//'

.PHONY: T16476a
T16476a:
	echo '--make' > resp
	echo '-XCPP' >> resp
	echo '-DTEST' >> resp
	echo 'T16476a.hs' >> resp
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 @resp
	./T16476a

# Tests that response files and -unit @file arguments work as expected.
.PHONY: T16476b
T16476b:
	echo '-XCPP' >> unit-a
	echo '-DTEST' >> unit-a
	echo 'T16476b.hs' >> unit-a

	echo '--make' > resp
	echo '-unit' > resp
	echo '@unit-a' > resp

	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 @resp
	./T16476b

.PHONY: T20569
T20569:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T20569/A.hs -i -iT20569 -hidir=interface
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T20569/B.hs -i -iT20569 -hidir=interface

.PHONY: T21349
T21349:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 Main -working-dir T21349
	[ ! -f T21349/B.o ] || (echo "object file exists" && exit 2)
	[ ! -f T21349/B.hi ] || (echo "interface file exists" && exit 2)

.PHONY: T21869
T21869:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 T21869.hs -S
	[ -f T21869.s ] || (echo "assembly file does not exist" && exit 2)
	[ ! -f T21869.o ] || (echo "object file exists" && exit 2)

.PHONY: T22044
T22044:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -E -cpp -x hs T22044.bazoo -o T22044.hs -DBAZOO=1
	# Test the file exists and is preprocessed
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 T22044.hs

.PHONY: T22669
T22669:
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T22669
	! test -f T22669.o-boot


T23339:
	$(RM) T23339.hi
	$(RM) T23339$(OBJSUFFIX)
	$(RM) -rf "$(PWD)/tmp"
	mkdir -p tmp
	"$(TEST_HC)" -tmpdir "$(PWD)/tmp" $(TEST_HC_OPTS) -v0 T23339.hs
	find . -name "*.c" -exec cat {} \; | grep "init__ip_init"

T23339B:
	$(RM) T23339.hi
	$(RM) T23339$(OBJSUFFIX)
	$(RM) -rf "$(PWD)/tmp"
	mkdir -p tmp
	"$(TEST_HC)" -tmpdir "$(PWD)/tmp" $(TEST_HC_OPTS) -v0 T23339B.hs -finfo-table-map
	# Check that the file is kept and is the right one
	find . -name "*.c" -exec cat {} \; | grep "init__ip_init"

TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# #2386 requires batch-compile not --make
# Very important: without -O
T2386:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 -c T2386_Lib.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 -c T2386.hs

T7445:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -package template-haskell -v0 -c T7445a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -package template-haskell -v0 -c T7445.hs

HC_OPTS = -XTemplateHaskell -package template-haskell

TH_spliceE5_prof::
	$(RM) TH_spliceE5_prof*.o TH_spliceE5_prof*.hi TH_spliceE5_prof*.dyn_o TH_spliceE5_prof*.dyn_hi TH_spliceE5_prof
	'$(TEST_HC)' $(TEST_HC_OPTS) $(HC_OPTS) $(ghcThWayFlags) --make -no-link -v0 TH_spliceE5_prof.hs
	# Using `-osuf .p.o` should work. Note the dot before the `p` (#9760), and
	# the dot between the `p` and the `o` (#5554).
	'$(TEST_HC)' $(TEST_HC_OPTS) $(HC_OPTS) --make -v0 TH_spliceE5_prof.hs -prof -fprof-auto -osuf .p.o -o $@
	./$@

# With -fexternal-interpreter, we don't have to build the non-profiled
# objects first.
TH_spliceE5_prof_ext::
	$(RM) TH_spliceE5_prof_ext*.o TH_spliceE5_prof_ext*.hi TH_spliceE5_prof_ext*.p.o
	'$(TEST_HC)' $(TEST_HC_OPTS) $(HC_OPTS) --make -v0 TH_spliceE5_prof_ext.hs -prof -fprof-auto -fexternal-interpreter -o $@
	./$@

.PHONY: TH_Depends
TH_Depends:
	$(RM) TH_Depends_external.txt
	$(RM) TH_Depends TH_Depends.exe
	$(RM) TH_Depends.o TH_Depends.hi
	$(RM) TH_Depends_External.o TH_Depends_External.hi
	echo "first run" > TH_Depends_external.txt
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -v0 TH_Depends
	./TH_Depends
	sleep 2
	echo "second run" > TH_Depends_external.txt
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -v0 TH_Depends
	./TH_Depends

.PHONY: TH_Depends_Dir
TH_Depends_Dir:
	rm -rf TRIGGER_RECOMP
	rm -rf DONT_TRIGGER_RECOMP
	$(RM) TH_Depends_Dir TH_Depends_Dir.exe
	$(RM) TH_Depends_Dir.o TH_Depends_Dir.hi
	$(RM) TH_Depends_Dir_External.o TH_Depends_Dir_External.hi

	mkdir TRIGGER_RECOMP
	mkdir DONT_TRIGGER_RECOMP

# First build with an empty dependent directory
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -package directory -package template-haskell -v0 TH_Depends_Dir
	./TH_Depends_Dir
	
# Create a file in the dependent directory to trigger recompilation
	sleep 2
	echo "dummy" > TRIGGER_RECOMP/dummy.txt
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -package directory -package template-haskell -v0 TH_Depends_Dir
	./TH_Depends_Dir
	
# Remove the file to check that recompilation is triggered
	sleep 2
	$(RM) TRIGGER_RECOMP/dummy.txt
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -package directory -package template-haskell -v0 TH_Depends_Dir
	./TH_Depends_Dir

# Should not trigger recompilation
	sleep 2
	echo "dummy" > DONT_TRIGGER_RECOMP/dummy.txt
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -package directory -package template-haskell -v0 TH_Depends_Dir
	./TH_Depends_Dir

# Should trigger a recompilation. Note that we should also see the change
# in the non-dependent directory now, since it is still rechecked as long
# as we recompile, it just doesn't *trigger* a recompilation.
	sleep 2
	rm -rf TRIGGER_RECOMP
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -package directory -package template-haskell -v0 TH_Depends_Dir
	./TH_Depends_Dir

T8333:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(ghcThWayFlags) T8333.hs < /dev/null

# This was an easy way to re-use the stdout testing
# to check the contents of a generated file.
T8624:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 -c T8624.hs && cat T8624.th.hs
	 $(RM) T8624.th.hs

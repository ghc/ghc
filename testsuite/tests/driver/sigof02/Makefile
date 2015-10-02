TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

S02_OPTS=$(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof02 -i -itmp_sigof02
sigof02:
	rm -rf tmp_sigof02
	mkdir tmp_sigof02
	'$(GHC_PKG)' field containers key | sed 's/^.*: *//' > tmp_sigof02/containers
	'$(TEST_HC)' $(S02_OPTS) -c Map.hs-boot -sig-of "Map is `cat tmp_sigof02/containers`:Data.Map.Strict"
	'$(TEST_HC)' $(S02_OPTS) --merge-requirements Map
	'$(TEST_HC)' $(S02_OPTS) -c Main.hs
	'$(TEST_HC)' $(S02_OPTS) -package containers tmp_sigof02/Main.o -o tmp_sigof02/StrictMain
	! ./tmp_sigof02/StrictMain
	'$(TEST_HC)' $(S02_OPTS) -c Map.hs-boot -sig-of "Map is `cat tmp_sigof02/containers`:Data.Map.Lazy"
	'$(TEST_HC)' $(S02_OPTS) --merge-requirements Map
	'$(TEST_HC)' $(S02_OPTS) -c Main.hs
	'$(TEST_HC)' $(S02_OPTS) -package containers tmp_sigof02/Main.o -o tmp_sigof02/LazyMain
	./tmp_sigof02/LazyMain

S02T_OPTS=$(TEST_HC_OPTS_NO_RECOMP) -fno-code -fwrite-interface -outputdir tmp_sigof02t -i -itmp_sigof02t
sigof02t:
	rm -rf tmp_sigof02t
	mkdir tmp_sigof02t
	'$(TEST_HC)' $(S02T_OPTS) -c Map.hs-boot
	'$(TEST_HC)' $(S02T_OPTS) --merge-requirements Map
	'$(TEST_HC)' $(S02T_OPTS) -c Main.hs

S02M_OPTS=$(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof02m
sigof02m:
	rm -rf tmp_sigof02m
	mkdir tmp_sigof02m
	'$(GHC_PKG)' field containers key | sed 's/^.*: *//' > tmp_sigof02m/containers
	'$(TEST_HC)' $(S02M_OPTS) --make Main.hs -sig-of "Map is `cat tmp_sigof02m/containers`:Data.Map.Strict" -o tmp_sigof02m/StrictMain
	! ./tmp_sigof02m/StrictMain
	'$(TEST_HC)' $(S02M_OPTS) --make Main.hs -sig-of "Map is `cat tmp_sigof02m/containers`:Data.Map.Lazy" -o tmp_sigof02m/LazyMain
	./tmp_sigof02m/LazyMain

sigof02mt:
	rm -rf tmp_sigof02mt
	mkdir tmp_sigof02mt
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof02mt --make Main.hs -fno-code -fwrite-interface

S02D_OPTS=$(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof02d -i -itmp_sigof02d
sigof02d:
	rm -rf tmp_sigof02d
	mkdir tmp_sigof02d
	'$(GHC_PKG)' field containers key | sed 's/^.*: *//' > tmp_sigof02d/containers
	'$(TEST_HC)' $(S02D_OPTS) -c Map.hs-boot -sig-of "Map is `cat tmp_sigof02d/containers`:Data.Map.Lazy, MapAsSet is `cat tmp_sigof02d/containers`:Data.Map.Lazy"
	'$(TEST_HC)' $(S02D_OPTS) --merge-requirements Map
	'$(TEST_HC)' $(S02D_OPTS) -c MapAsSet.hs-boot -sig-of "Map is `cat tmp_sigof02d/containers`:Data.Map.Lazy, MapAsSet is `cat tmp_sigof02d/containers`:Data.Map.Lazy"
	'$(TEST_HC)' $(S02D_OPTS) --merge-requirements MapAsSet
	'$(TEST_HC)' $(S02D_OPTS) -c Double.hs
	'$(TEST_HC)' $(S02D_OPTS) -package containers tmp_sigof02d/Main.o -o tmp_sigof02d/Double
	./tmp_sigof02d/Double

S02DT_OPTS=$(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof02dt -i -itmp_sigof02dt -fno-code -fwrite-interface
sigof02dt:
	rm -rf tmp_sigof02dt
	mkdir tmp_sigof02dt
	'$(TEST_HC)' $(S02DT_OPTS) -c Map.hs-boot
	'$(TEST_HC)' $(S02DT_OPTS) --merge-requirements Map
	'$(TEST_HC)' $(S02DT_OPTS) -c MapAsSet.hs-boot
	'$(TEST_HC)' $(S02DT_OPTS) --merge-requirements MapAsSet
	! '$(TEST_HC)' $(S02DT_OPTS) -c Double.hs

sigof02dm:
	rm -rf tmp_sigof02dm
	mkdir tmp_sigof02dm
	'$(GHC_PKG)' field containers key | sed 's/^.*: *//' > tmp_sigof02dm/containers
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof02dm --make Double.hs -sig-of "Map is `cat tmp_sigof02dm/containers`:Data.Map.Lazy, MapAsSet is `cat tmp_sigof02dm/containers`:Data.Map.Lazy" -o tmp_sigof02dm/Double
	./tmp_sigof02dm/Double

sigof02dmt:
	rm -rf tmp_sigof02dmt
	mkdir tmp_sigof02dmt
	# doesn't typecheck due to lack of alias
	! '$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -outputdir tmp_sigof02dmt -fno-code -fwrite-interface --make Double.hs -o tmp_sigof02dmt/Double

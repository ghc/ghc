TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: generic-unit-info-space-single_Prep
generic-unit-info-space-single_Prep:


.PHONY: generic-unit-info-space-single
generic-unit-info-space-single:
	"$(TEST_HC)" $(TEST_HC_OPTS) -Wall -v0 generic-unit-info-space.hs -rtsopts \
		-package ghc -package base -package ghc-internal -threaded

	./generic-unit-info-space$(exeext) \
		"`'$(TEST_HC)' --print-libdir | tr -d '\r'`" \
		"`'$(GHC_PKG)' --no-user-package-db list --simple-output | wc -w`" \
		-clear-package-db -global-package-db -no-user-package-db \
		+RTS -hT --no-automatic-heap-samples -RTS

# Generate 50 trivial home units. The number of 'UnitInfo's must not increase
# linearly in the number of home units
HOME_UNITS_01 := 50
UNITS := $(foreach p,$(shell seq 0 $$(( $(HOME_UNITS_01) - 1 ))),-unit @unitp$(p))
.PHONY: generic-unit-info-space-mhu_Prep
generic-unit-info-space-mhu_Prep:
	./genMhu.sh $(HOME_UNITS_01)


.PHONY: generic-unit-info-space-mhu
generic-unit-info-space-mhu:
	"$(TEST_HC)" $(TEST_HC_OPTS) -Wall -v0 generic-unit-info-space.hs -rtsopts \
		-package ghc -package base -package ghc-internal -threaded

	./generic-unit-info-space$(exeext) \
		"`'$(TEST_HC)' --print-libdir | tr -d '\r'`" \
		"`'$(GHC_PKG)' --global list --simple-output | wc -w`" \
		$(UNITS) \
		+RTS -hT --no-automatic-heap-samples -RTS


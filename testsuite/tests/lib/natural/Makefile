TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

CHECK = grep -q -- '$1' naturalConstantFolding.simpl || \
        echo "$2 didn't constant fold"

.PHONY: naturalConstantFolding
naturalConstantFolding:
	'$(TEST_HC)' -Wall -v0 -O --make naturalConstantFolding -fforce-recomp -ddump-simpl > naturalConstantFolding.simpl
# All the 100nnn values should be constant-folded away
	! grep -q '\<100[0-9][0-9][0-9]\>' naturalConstantFolding.simpl || { echo "Unfolded values found"; grep '\<100[0-9][0-9][0-9]\>' naturalConstantFolding.simpl; }
	$(call CHECK,\<200121\>,plusNatural)
	$(call CHECK,\<683678240\>,timesNatural)
	$(call CHECK,\<989\>,minusNatural)
	$(call CHECK,\<0\>,negateNatural)
	$(call CHECK,\<50024\>,gcdNatural)
	$(call CHECK,\<1001100300\>,lcmNatural)
	$(call CHECK,\<532\>,andNatural)
	$(call CHECK,\<239579\>,orNatural)
	$(call CHECK,\<239055\>,xorNatural)
	$(call CHECK,\<813\>,quotRemNatural)
	$(call CHECK,\<60\>,quotRemNatural)
	$(call CHECK,\<219\>,divModNatural)
	$(call CHECK,\<196\>,divModNatural)
	$(call CHECK,\<1601040\>,shiftLNatural)
	$(call CHECK,\<6254\>,shiftRNatural)
	$(call CHECK,\<641\>,quotNatural)
	$(call CHECK,\<68\>,remNatural)
	$(call CHECK,\<642\>,divNatural)
	$(call CHECK,\<90\>,modNatural)
	$(call CHECK,\<200145\>,wordToNatural)
	$(call CHECK,\<200149\>,naturalToWord)
	$(call CHECK,\<200153\>,intToNatural)
	./naturalConstantFolding
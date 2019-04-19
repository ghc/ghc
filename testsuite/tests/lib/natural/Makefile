TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

CHECK = grep -q -- '$1' naturalConstantFolding.simpl || \
        echo "$2 didn't constant fold"

CHECK2 = grep -q -- '$1' naturalConstantFolding.simpl || \
         grep -q -- '$2' naturalConstantFolding.simpl || \
         echo "$3 didn't constant fold"

.PHONY: naturalConstantFolding
naturalConstantFolding:
	'$(TEST_HC)' -Wall -v0 -O --make naturalConstantFolding -fforce-recomp -ddump-simpl > naturalConstantFolding.simpl
# All the 100nnn values should be constant-folded away
	! grep -q '\<100[0-9][0-9][0-9]\>' naturalConstantFolding.simpl || { echo "Unfolded values found"; grep '\<100[0-9][0-9][0-9]\>' naturalConstantFolding.simpl; }
	# Bit arithmetic
	$(call CHECK,\<532\>,andNatural)
	$(call CHECK,\<239055\>,xorNatural)
	$(call CHECK,\<16\>,bitNatural)
	$(call CHECK,\<239579\>,orNatural)
	$(call CHECK,\<1601040\>,shiftLNatural)
	$(call CHECK,\<6254\>,shiftRNatural)
	$(call CHECK,\<12\>,popCountNatural)
	# Arithmetic
	$(call CHECK,\<200121\>,plusNatural)
	$(call CHECK,\<683678240\>,timesNatural)
	$(call CHECK,\<989\>,minusNatural)
	$(call CHECK,\<0\>,negateNatural)
	$(call CHECK,\<1\>,signumNaturalP)
	$(call CHECK,\<0\>,signumNaturalZ)
	# Quotients and remainders
	$(call CHECK2,\<813\>,\<60\>,quotRemNatural)
	$(call CHECK2,\<219\>,\<196\>,divModNatural)
	$(call CHECK,\<641\>,quotNatural)
	$(call CHECK,\<68\>,remNatural)
	$(call CHECK,\<642\>,divNatural)
	$(call CHECK,\<90\>,modNatural)
	$(call CHECK,\<50024\>,gcdNatural)
	$(call CHECK,\<1001100300\>,lcmNatural)
	# Conversions
	$(call CHECK,\<200109\>,naturalFromInteger)
	$(call CHECK,\<200113\>,naturalToInteger)
	$(call CHECK,\<200145\>,wordToNatural)
	$(call CHECK,\<200149\>,naturalToWord)
	$(call CHECK,\<200153\>,intToNatural)
	$(call CHECK,\<200157\>,naturalToInt)
	$(call CHECK,\<200189.0\>,doubleFromNatural)
	$(call CHECK,\<200193.0\>,floatFromNatural)
	$(call CHECK,\<200007\>,int64ToNatural)
	$(call CHECK,\<200015\>,word64ToNatural)
	# Ordering and Equality
	./naturalConstantFolding
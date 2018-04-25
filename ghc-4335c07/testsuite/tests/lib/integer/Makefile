TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

CHECK = grep -q -- '$1' integerConstantFolding.simpl || \
        echo "$2 didn't constant fold"

.PHONY: integerConstantFolding
integerConstantFolding:
	'$(TEST_HC)' -Wall -v0 -O --make integerConstantFolding -fforce-recomp -ddump-simpl > integerConstantFolding.simpl
# All the 100nnn values should be constant-folded away
	! grep -q '\<100[0-9][0-9][0-9]\>' integerConstantFolding.simpl || { echo "Unfolded values found"; grep '\<100[0-9][0-9][0-9]\>' integerConstantFolding.simpl; }
	$(call CHECK,\<200007\>,plusInteger)
	$(call CHECK,\<683234160\>,timesInteger)
	$(call CHECK,-991\>,minusIntegerN)
	$(call CHECK,\<989\>,minusIntegerP)
	$(call CHECK,-200011\>,negateInteger)
	$(call CHECK,\<200019\>,absInteger)
	$(call CHECK,\<50024\>,gcdInteger)
	$(call CHECK,\<1001100300\>,lcmInteger)
	$(call CHECK,\<532\>,andInteger)
	$(call CHECK,\<239575\>,orInteger)
	$(call CHECK,\<239041\>,xorInteger)
	$(call CHECK,-200059\>,complementInteger)
	$(call CHECK,\<813\>,quotRemInteger)
	$(call CHECK,\<60\>,quotRemInteger)
	$(call CHECK,\<219\>,divModInteger)
	$(call CHECK,\<196\>,divModInteger)
	$(call CHECK,\<1600976\>,shiftLInteger)
	$(call CHECK,\<6253\>,shiftRInteger)
	$(call CHECK,\<641\>,quotInteger)
	$(call CHECK,\<68\>,remInteger)
	$(call CHECK,\<642\>,divInteger)
	$(call CHECK,\<90\>,modInteger)
	$(call CHECK,\<200131.0\>,doubleFromInteger)
	$(call CHECK,\<200135.0\>,floatFromInteger)
	$(call CHECK,\<400276.0\>,encodeIntegerDouble)
	$(call CHECK,\<400280.0\>,encodeIntegerFloat)
	./integerConstantFolding

.PHONY: fromToInteger
fromToInteger:
	'$(TEST_HC)' -Wall -v0 -O -c fromToInteger.hs -fforce-recomp -ddump-simpl > fromToInteger.simpl
# Rules should eliminate all functions
	-grep integerToInt fromToInteger.simpl
	-grep smallInteger fromToInteger.simpl
	-grep integerToWord fromToInteger.simpl
	-grep wordToInteger fromToInteger.simpl

.PHONY: IntegerConversionRules
IntegerConversionRules:
	'$(TEST_HC)' -Wall -v0 -O -c $@.hs -fforce-recomp -ddump-simpl > $@.simpl
	-grep -q smallInteger      $@.simpl && echo "smallInteger present"
	-grep -q doubleFromInteger $@.simpl && echo "doubleFromInteger present"
	-grep -q int2Double        $@.simpl || echo "int2Double absent"
	-grep -q floatFromInteger  $@.simpl && echo "floatFromInteger present"
	-grep -q int2Float         $@.simpl || echo "int2Float absent"
	-grep -q integerToWord     $@.simpl && echo "integerToWord present"
	-grep -q int2Word          $@.simpl || echo "int2Word absent"


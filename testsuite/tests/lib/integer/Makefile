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
	./integerConstantFolding


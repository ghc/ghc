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
#	$(call CHECK,\<200007\>,plusNatural)
#	$(call CHECK,\<683234160\>,timesNatural)
#	$(call CHECK,-991\>,minusNaturalN)
#	$(call CHECK,\<989\>,minusNaturalP)
#	$(call CHECK,-200011\>,negateNatural)
#	$(call CHECK,\<200019\>,absNatural)
#	$(call CHECK,\<50024\>,gcdNatural)
#	$(call CHECK,\<1001100300\>,lcmNatural)
#	$(call CHECK,\<532\>,andNatural)
#	$(call CHECK,\<239575\>,orNatural)
#	$(call CHECK,\<239041\>,xorNatural)
#	$(call CHECK,-200059\>,complementNatural)
#	$(call CHECK,\<813\>,quotRemNatural)
#	$(call CHECK,\<60\>,quotRemNatural)
#	$(call CHECK,\<219\>,divModNatural)
#	$(call CHECK,\<196\>,divModNatural)
#	$(call CHECK,\<1600976\>,shiftLNatural)
#	$(call CHECK,\<6253\>,shiftRNatural)
#	$(call CHECK,\<641\>,quotNatural)
#	$(call CHECK,\<68\>,remNatural)
#	$(call CHECK,\<642\>,divNatural)
#	$(call CHECK,\<90\>,modNatural)
	./naturalConstantFolding


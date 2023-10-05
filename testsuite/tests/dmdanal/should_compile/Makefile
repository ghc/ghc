TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T13031:
	echo hello
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fforce-recomp T13031.hs -ddump-simpl | grep 'Arity='

# Trying to make sure the workers for g1 and g2
# take only one Int# argument
T16029:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -fforce-recomp T16029.hs -dsuppress-uniques -ddump-simpl | grep '::.*Int'

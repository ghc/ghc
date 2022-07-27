TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

jspace:
	./genJspace
	"$(TEST_HC)" $(TEST_HC_OPTS) -Wall -Werror -v0 jspace.hs -rtsopts -package ghc -threaded
	./jspace +RTS -N -hT --no-automatic-heap-samples -RTS "`'$(TEST_HC)' --print-libdir | tr -d '\r'`"

	# Should be 3200, which is 3200/(8 * 8) as ModDetails has 8 word sized fields.
	# If this number changes DO NOT ACCEPT THE TEST, you have introduced a space leak.
	cat jspace.hp | grep ModDetails


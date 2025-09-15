TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

jspace:
	./genJspace
	"$(TEST_HC)" $(TEST_HC_OPTS) -Wall -Werror -v0 jspace.hs -rtsopts -package ghc -threaded
	./jspace$(exeext) +RTS -N -hT --no-automatic-heap-samples -RTS "`'$(TEST_HC)' --print-libdir | tr -d '\r'`"


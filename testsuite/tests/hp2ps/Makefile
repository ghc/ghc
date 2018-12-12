TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T15904
T15904:
	"$(TEST_HC)" $(TEST_HC_OPTS) -rtsopts -main-is "$@" "$@.hs" -o "\"$@\""
	"./\"$@\"" '{"e": 2.72, "pi": 3.14}' "\\" "" '"' +RTS -h
	"$(HP2PS_ABS)" "\"$@\".hp"

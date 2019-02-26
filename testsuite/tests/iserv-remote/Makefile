TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

remote-iserv:
	cabal new-build all -w '$(TEST_HC)'
	'$(TEST_HC)' $(TEST_HC_OPTS)  -fexternal-interpreter -pgmi $(PWD)/iserv-wrapper Main.hs

TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

NewTyCo:
	$(RM) NewTyCo1.o NewTyCo1.hi NewTyCo2.o NewTyCo2.hi
	'$(TEST_HC)' -c NewTyCo1.hs
	'$(TEST_HC)' -c NewTyCo2.hs

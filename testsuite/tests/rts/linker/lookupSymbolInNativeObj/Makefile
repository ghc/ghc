TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

lookupSymbolInNativeObj1:
	'$(TEST_HC)' -shared -dynamic obj.c -o libobj.so
	'$(TEST_HC)' -no-hs-main -dynamic lookupSymbolInNativeObj1.c -o main
	./main


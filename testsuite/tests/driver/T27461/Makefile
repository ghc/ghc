TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# src/Bar.hs declares module Foo, which is fine for a file target, but Main's
# `import Bar` resolves to that same file and must be rejected.
T27461a :
	cp Main1.hs src/Main.hs
	! '$(TEST_HC)' $(TEST_HC_OPTS) --make -fno-code -v0 -isrc src/Main.hs src/Bar.hs

# Like T27461a, but for a module target instead of a file target.
T27461c :
	! '$(TEST_HC)' $(TEST_HC_OPTS) --make -fno-code -v0 -isrc Wrong

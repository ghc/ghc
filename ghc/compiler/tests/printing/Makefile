TOP = ../../../..
GhcRunTestRules = YES
include $(TOP)/ghc/mk/ghc.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))

print001_flags = -noC -hi
print002_flags = -noC -fno-implicit-prelude -dppr-user -ddump-rif2hs -ddump-tc -hi
print003_flags = -noC -hi
print004_flags = -noC -hi

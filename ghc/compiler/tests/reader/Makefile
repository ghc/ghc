TOP = ../../../..
GhcRunTestRules = YES
include $(TOP)/ghc/mk/ghc.mk

runtests :: $(patsubst %.hs, %.runtest, $(wildcard *.hs))

read001_flags = -noC -ddump-rif2hs
read002_flags = -noC -ddump-rif2hs
read004_flags = -noC -fno-implicit-prelude -ddump-rif2hs

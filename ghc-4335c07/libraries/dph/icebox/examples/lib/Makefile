TOPDIR=..
include $(TOPDIR)/mk/common.mk

HCFLAGS = -O -package dph-prim-par

MODULES = Benchmark Time Options
SRCS    = $(MODULES:%=Bench/%.hs)
CONF    = dph-bench.conf

DIR=$(shell pwd)

.PHONY: all configure remake clean

all: configure build install

configure: dist/setup-config

Setup: Setup.hs
	$(HC) --make $^

dist/setup-config: Setup dph-bench.cabal
	echo [] > $(CONF) 
	./Setup configure --with-compiler=$(HC) --with-hc-pkg=$(HCPKG) --package-db=$(CONF) --prefix=$(DIR)

build: dist/installed-pkg-config

dist/installed-pkg-config: dist/setup-config $(SRCS)
	./Setup build

install: $(CONF)

$(CONF): dist/installed-pkg-config
	./Setup register --inplace

clean:
	$(RM) -r dist dph-bench.conf dph-bench.conf.old Setup.hi Setup.o Setup


DPHDIR = $(TOPDIR)/..

HC = $(DPHDIR)/../../inplace/bin/ghc-stage2
HCPKG = $(DPHDIR)/../../inplace/bin/ghc-pkg

BENCH_DIR = $(TOPDIR)/lib
BENCH_FLAGS = -package dph-bench -package-conf $(BENCH_DIR)/dph-bench.conf
BENCH_DEP = $(BENCH_DIR)/dist/inplace-pkg-config

DPH_FLAGS += -Odph -threaded -rtsopts -fsimplifier-phases=4 -fstrictness-before=3 -msse2

WAYS = seq par
WAY_FLAGS = -package dph-$(WAY) -odir $(WAY) -hidir $(WAY)

CFLAGS += -O6


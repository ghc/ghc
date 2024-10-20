.PHONY: boot configure build clean test-simd000 test iserv binary-dist

FLAVOUR := devel2+vectors
GHC_BINARY := ghc
ISERV_PATH := $(shell dirname $(shell which $(GHC_BINARY)))/../lib/ghc-9.10.1/bin/ghc-iserv
EXTRA_HC_OPTS := "-fexternal-interpreter -pgmi=$(ISERV_PATH)"


boot:
	./boot

configure: boot
	CFLAGS="-march=rv64gv" configure_ghc --disable-dwarf-unwind

build: 
	hadrian/build -j --docs=none --flavour=$(FLAVOUR)

binary-dist: build 
	hadrian/build -j --docs=none --flavour=$(FLAVOUR) binary-dist

clean:
	hadrian/build clean

iserv:
	hadrian/build -j --docs=none --flavour=$(FLAVOUR) stage0:exe:iserv

test: build
	CROSS_EMULATOR=qemu-riscv64 QEMU_CPU="rv64,v=true,vlen=512,vext_spec=v1.0" EXTRA_HC_OPTS=$(EXTRA_HC_OPTS) hadrian/build -j8 --config="config.timeout=3600" --config="config.cross_cpu_features=['zvl128b', 'zvl256b', 'zvl512b']"
--docs=none --flavour=$(FLAVOUR) test

test-simd: build
	CROSS_EMULATOR=qemu-riscv64 QEMU_CPU="rv64,zba=true,zbb=true,v=true,vlen=512,vext_spec=v1.0" EXTRA_HC_OPTS=$(EXTRA_HC_OPTS) hadrian/build -j --docs=none --flavour=$(FLAVOUR) test -k --test-root-dirs="./testsuite/tests/simd/" --test-verbose=4  --config="config.cross_cpu_features=['zvl128b', 'zvl256b', 'zvl512b']"

test-simd014: build
	CROSS_EMULATOR=qemu-riscv64 hadrian/build -j --docs=none --flavour=$(FLAVOUR) test -k --only="simd014"

test-T5149: build
	CROSS_EMULATOR=qemu-riscv64 hadrian/build -j --docs=none --flavour=$(FLAVOUR) test -k --only="T5149"

test-simd_insert_baseline: build
	CROSS_EMULATOR=qemu-riscv64 hadrian/build -j --docs=none --flavour=$(FLAVOUR) test -k --only="simd_insert_baseline"

test-int8x16_basic_baseline: build
	CROSS_EMULATOR=qemu-riscv64 hadrian/build -j --docs=none --flavour=$(FLAVOUR) --freeze1 test -k --only="int8x16_basic_baseline"

test-simd000: build
	CROSS_EMULATOR=qemu-riscv64 hadrian/build -j --docs=none --flavour=$(FLAVOUR) test -k --only="simd000"

test-T25062: build
	CROSS_EMULATOR="qemu-riscv64" QEMU_CPU="rv64,zba=true,zbb=true,v=true,vlen=512,vext_spec=v1.0" hadrian/build -j --docs=none --flavour=$(FLAVOUR) test -k --only="T25062_V16 T25062_V32 T25062_V64"

test-simd009: build
	CROSS_EMULATOR=qemu-riscv64 QEMU_CPU="rv64,zba=true,zbb=true,v=true,vlen=512,vext_spec=v1.0" EXTRA_HC_OPTS=$(EXTRA_HC_OPTS) hadrian/build -j --docs=none --flavour=$(FLAVOUR) test -k --test-root-dirs="./testsuite/tests/simd/" --test-verbose=4  --config="config.cross_cpu_features=['zvl128b', 'zvl256b', 'zvl512b']" --only=simd009



.PHONY: clean_build_and_run build_and_run clean build

clean_build_and_run:
	$(MAKE) clean
	$(MAKE) build_and_run

build_and_run: build
	./main
	grep -c Lib main.hp

clean:
	$(RM) Lib.o Lib_stub.h Lib.hi Lib.a main.o main main.hp

build: Lib.a main

Lib.o: Lib.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c -O -fPIC -prof Lib.hs -o Lib.o
Lib.a: Lib.o
	llvm-ar -qcL Lib.a Lib.o 2>/dev/null || ar -qc Lib.a Lib.o

main: main.c
	"$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS)) \
		-rdynamic -no-hs-main -optc-Werror -prof \
		main.c -o main

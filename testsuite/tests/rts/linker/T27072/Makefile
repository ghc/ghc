.PHONY: clean_build_and_run build_and_run clean build

clean_build_and_run:
	$(MAKE) clean
	$(MAKE) build_and_run

build_and_run: build
	./main

clean:
	$(RM) Lib.o main.o main

build: Lib.o main

Lib.o: Lib.c
	$(CC) -c -fPIC Lib.c -o Lib.o

main: main.c
	"$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS)) \
		-no-hs-main -optc-Werror \
		main.c -o main

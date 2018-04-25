count = 1000

all: coverage literal-rule-test

literal-rule-test:
	./literal-rule-test.sh

coverage: build coverage/hpc_index.html

build: text-test-data
	cabal configure -fhpc
	cabal build

text-test-data:
	hg clone https://bitbucket.org/bos/text-test-data
	$(MAKE) -C text-test-data

coverage/text-tests.tix:
	-mkdir -p coverage
	./dist/build/text-tests/text-tests -a $(count)
	mv text-tests.tix $@

coverage/text-tests-stdio.tix:
	-mkdir -p coverage
	./scripts/cover-stdio.sh ./dist/build/text-tests-stdio/text-tests-stdio
	mv text-tests-stdio.tix $@

coverage/coverage.tix: coverage/text-tests.tix coverage/text-tests-stdio.tix
	hpc combine --output=$@ \
        --exclude=Main \
        coverage/text-tests.tix \
        coverage/text-tests-stdio.tix

coverage/hpc_index.html: coverage/coverage.tix
	hpc markup --destdir=coverage coverage/coverage.tix

clean:
	rm -rf dist coverage .hpc

.PHONY: build coverage all literal-rule-test

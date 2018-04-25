all:

bench-%: %.hs force
	ghc -O2 -DTESTING $< -I$(TOP)../include -i$(TOP).. -o $@ -outputdir tmp -rtsopts

.PRECIOUS: bench-%

bench-%.csv: bench-%
	./bench-$* "$(BENCHMARK)" -v1 --csv bench-$*.csv

.PHONY: force clean veryclean
force:

clean:
	rm -rf tmp $(patsubst %.hs, bench-%, $(wildcard *.hs))

veryclean: clean
	rm -rf *.csv

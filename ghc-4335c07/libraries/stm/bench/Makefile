GHC=ghc

# Run chanbench for 4 different channel types, 3 different benchmarks
all:
	for i in CHAN TCHAN TQUEUE TBQUEUE; do $(GHC) -D$$i -O2 -fforce-recomp chanbench.hs -o chanbench-$$i; done
	for i in 0 1 2; do echo; echo === test $$i ===; for j in CHAN TCHAN TQUEUE TBQUEUE; do printf "%-10s" $$j; time ./chanbench-$$j $$i 2000000; done; done

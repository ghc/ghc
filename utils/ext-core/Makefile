# This makefile is just for running the tests. For everything else,
# use Cabal! (The tests could be run with Cabal too, I'm just too lazy
# to figure out how.)

# The following assumes that you've built all the GHC libs with -fext-core...
libtest:
	./Driver -n `find ../../libraries -print | grep .hcr$$ | grep -v bootstrapping`

# ...or built all the nofib programs with -fext-core.
nofibtest:
	./Driver `find ../../nofib -print | grep .hcr$$`

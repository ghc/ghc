
all: runtests test

boot:
	@echo "Make boot is not needed here"

runtests:
	(cd driver && make runtests)
clean:
	(cd driver && make clean)

test: runtests
	driver/runtests --tool=../ghc/compiler/ghc-inplace \
		 --config=config/msrc/cam-02-unx.T --rootdir=tests

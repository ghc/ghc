
all: runtests normalise_errmsg test

boot:
	@echo "Make boot is not needed here"

runtests:
	(cd driver && make runtests)
normalise_errmsg:
	(cd utils/normalise_errmsg && make normalise_errmsg)

clean:
	(cd driver && make clean)
	(cd utils/normalise_errmsg && make clean)

test: runtests
	driver/runtests --tool=../ghc/compiler/ghc-inplace \
		 --config=config/msrc/cam-02-unx.T --rootdir=tests

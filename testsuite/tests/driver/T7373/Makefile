TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T7373
T7373:
	echo '[]' > package.conf
	cd pkg && '$(TEST_HC)' -v0 --make Setup
	cd pkg && ./Setup configure -v0 --with-compiler='$(TEST_HC)' --ghc-pkg-options="--global-package-db ../package.conf" --ghc-options="-package-db ../package.conf"
	cd pkg && ./Setup build -v0
	cd pkg && ./Setup register --inplace -v0
	# Pretend that B.hs hasn't been compiled yet, by removing the results
	rm pkg/dist/build/B.hi
	rm pkg/dist/build/B.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db package.conf -c D.hs -O


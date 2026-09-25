TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T27836:
	mkdir local-unit-db/
	cd local-unit-db/ && '$(GHC_PKG)' init my-test.conf
	# All these unit database paths should resolve to the same location
	# thus after the first time parsing it, we shouldn't parse it again.
	'$(TEST_HC)' $(TEST_HC_OPTS) --make \
			-package-db local-unit-db/my-test.conf -package-db local-unit-db/my-test.conf/ \
			-package-db ./local-unit-db/my-test.conf/ -package-db local-unit-db/../local-unit-db/my-test.conf/ \
			A

	'$(TEST_HC)' $(TEST_HC_OPTS) --make \
			-package-db local-unit-db/my-test.conf -package-db local-unit-db/my-test.conf/ \
			-package-db ./local-unit-db/my-test.conf/ -package-db local-unit-db/../local-unit-db/my-test.conf/ \
			A \
			-v2 -fforce-recomp 2>&1 \
		| grep 'Using binary package database:'
	echo "The package database must only be parsed once if the same database is given."


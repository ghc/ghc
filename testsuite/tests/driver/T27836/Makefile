TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T27836:
	mkdir local-unit-db/
	cd local-unit-db/ && '$(GHC_PKG)' init my-test.conf
	# All these unit database paths should resolve to the same location
	# thus after the first time parsing the unit database, we shouldn't parse it again.
	'$(TEST_HC)' $(TEST_HC_OPTS) --make \
			-unit @unitA -unit @unitB -unit @unitC \
			-v2 -fforce-recomp 2>&1 \
		| grep 'Using binary package database:'

	echo ""
	echo ""
	echo "All -package-db variations:"
	echo "- local-unit-db/my-test.conf"
	echo "- ./local-unit-db/my-test.conf"
	echo "- ./local-unit-db/my-test.conf/"
	echo "Must be parsed only once."

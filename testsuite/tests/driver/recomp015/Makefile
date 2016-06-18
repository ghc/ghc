TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f ManySections.s Main.hs
	rm -f Main$(exeext)
	rm -f Generate$(exeext)

recomp015: clean
	# Generate a file with more than 0xff00 sections to force different ELF
	# fields to be used (ELF header fields are limited to 16-bit).
	#
	# You can confirm that fields of section 0 entry are used to store the
	# number of section (size field of section 0) and the index of the .shstrtab
	# section (link field of section 0) with:
	#    readelf -t ManySections.o | less
	#  and/or
	#    readelf -t Main | less
	#
	# This test checks that GHC can read these fields correctly and avoids
	# recompilation (just like recomp011 which does the same thing for a smaller
	# number of sections)
	'$(TEST_HC)' $(TEST_HC_OPTS) Generate.hs
	./Generate > ManySections.s
	echo 'main = putStrLn "Running main..."' > Main.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ManySections.s
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -O Main.hs ManySections.o
	./Main
	sleep 1
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -O Main.hs ManySections.o
	./Main

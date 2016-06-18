TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

S01_OPTS=$(TEST_HC_OPTS) -outputdir tmp_sigof01 -i -itmp_sigof01
sigof01:
	rm -rf tmp_sigof01
	mkdir tmp_sigof01
	'$(TEST_HC)' $(S01_OPTS) -c A.hs
	'$(TEST_HC)' $(S01_OPTS) -c B.hsig -sig-of "B is main:A"
	'$(TEST_HC)' $(S01_OPTS) -c Main.hs
	'$(TEST_HC)' $(S01_OPTS) tmp_sigof01/A.o tmp_sigof01/Main.o -o tmp_sigof01/Main
	tmp_sigof01/Main

sigof01m:
	rm -rf tmp_sigof01m
	mkdir tmp_sigof01m
	'$(TEST_HC)' $(TEST_HC_OPTS) -outputdir tmp_sigof01m --make Main.hs -sig-of "B is main:A" -o tmp_sigof01m/Main
	tmp_sigof01m/Main

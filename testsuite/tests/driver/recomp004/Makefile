TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

EXECUTABLE = MainX$(exeext)
CLEAN_FILES = c.c c.o MainX.o MainX.hi $(EXECUTABLE)

clean:
	$(RM) $(CLEAN_FILES)

# Test that updates to C modules are noticed
recomp004:
	$(RM) $(CLEAN_FILES)
	$(CP) c1.c c.c
	$(CP) Main.hs MainX.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 -c c.c -o c.o
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 --make MainX.hs c.o -o $(EXECUTABLE)
	./$(EXECUTABLE)
	$(CP) c2.c c.c
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 -c c.c -o c.o
	# Hack to make sure c.o is really newer
	touch -t 01010000 MainX.hs MainX.hi MainX.o c.c $(EXECUTABLE)
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 --make MainX.hs c.o -o $(EXECUTABLE)
	./$(EXECUTABLE)


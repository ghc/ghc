TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

EXECUTABLE = Main$(exeext)
CLEAN_FILES = c.c c.o Main.o Main.hi $(EXECUTABLE)

clean:
	$(RM) $(CLEAN_FILES)

# Test that updates to C modules are noticed
recomp004:
	$(RM) $(CLEAN_FILES)
	$(CP) c1.c c.c
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -c c.c -o c.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Main.hs c.o -o $(EXECUTABLE)
	./$(EXECUTABLE)
	$(CP) c2.c c.c
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -c c.c -o c.o
	# Hack to make sure c.o is really newer
	touch -t 01010000 Main.hs Main.hi Main.o c.c $(EXECUTABLE)
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make Main.hs c.o -o $(EXECUTABLE)
	./$(EXECUTABLE)


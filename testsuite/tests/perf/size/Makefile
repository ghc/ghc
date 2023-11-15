TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

libdir_size:
	du -s `$(TEST_HC) --print-libdir` | cut -f1 > SIZE


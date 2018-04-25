TOP = ..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = \
	sieve \
	callback001 \
	callback002 \
	threads001 \
	threads003 \
	threads006 \
	threads007 \
	chan

# Not a good benchmark, can go really slowly for random reasons:
#  threads002
OTHER_SUBDIRS = stm001 stm002 systolic tchan theads002 threads004 threads005

include $(TOP)/mk/target.mk


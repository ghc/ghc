TOP=../..

ifeq "$(NOTES_UTIL)" ""
NOTES_UTIL := $(abspath $(TOP)/../inplace/bin/notes-util)
endif

uniques:
	python3 checkUniques/check-uniques.py $(TOP)/..

notes:
	(cd $(TOP)/.. && $(NOTES_UTIL) broken-refs)

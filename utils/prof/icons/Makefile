TOP=../../..
include $(TOP)/mk/boilerplate.mk

override datadir=$(libdir)/icons
INSTALL_DATAS=$(wildcard *.xbm)

binary-dist:
	$(INSTALL_DIR)                   $(BIN_DIST_DIR)/utils/prof/icons
	$(INSTALL_DATA) Makefile         $(BIN_DIST_DIR)/utils/prof/icons/
	$(INSTALL_DATA) $(INSTALL_DATAS) $(BIN_DIST_DIR)/utils/prof/icons/

include $(TOP)/mk/target.mk


TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal-flags.mk

boot:
	$(CABAL) configure --distpref dist-inplace \
	                   $(INPLACE_DIRS_CONFIGURE_FLAGS) \
	                   $(USE_BOOT_CONFIGURE_FLAGS) \
	                   $(COMMON_CONFIGURE_FLAGS)
	$(CABAL) build     --distpref dist-inplace $(BUILD_FLAGS)
	$(CABAL) install   --distpref dist-inplace

# XXX
#all:
#	$(CABAL) configure --distpref dist-install \
#	                   $(INPLACE_DIRS_CONFIGURE_FLAGS) \
#	                   $(USE_STAGE1_CONFIGURE_FLAGS) \
#	                   $(COMMON_CONFIGURE_FLAGS)
#	$(CABAL) build     --distpref dist-install $(BUILD_FLAGS)

all:

clean: distclean

distclean:
	-$(CABAL) clean --distpref dist-inplace
	-$(CABAL) clean --distpref dist-install

# XXX fix:
#binary-dist:
#	$(INSTALL_DIR)                      $(BIN_DIST_DIR)/utils/hsc2hs
#	$(INSTALL_DATA)    Makefile         $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_DATA)    hsc2hs.sh        $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_DATA)    $(INSTALL_DATAS) $(BIN_DIST_DIR)/utils/hsc2hs/
#	$(INSTALL_PROGRAM) $(HS_PROG)       $(BIN_DIST_DIR)/utils/hsc2hs/


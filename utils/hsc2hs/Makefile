
TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal-flags.mk

SRC_HC_OPTS      += -Wall

CABAL = $(FPTOOLS_TOP_ABS)/libraries/cabal-bin \
            $(GHC) $(FPTOOLS_TOP_ABS)/libraries/bootstrapping.conf

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_FLAGS =
else
INSTALL_FLAGS = --enable-shell-wrappers
endif

boot:
	$(CABAL) configure --distpref dist-inplace $(INPLACE_CONFIGURE_FLAGS)
	$(CABAL) build     --distpref dist-inplace $(BUILD_FLAGS)
	$(CABAL) install   --distpref dist-inplace $(INSTALL_FLAGS)

ifneq "$(NO_INSTALL_HSC2HS)" "YES"
# XXX
#all:
#	$(CABAL) configure --distpref dist-install $(INSTALL_CONFIGURE_FLAGS)
#	$(CABAL) build     --distpref dist-install $(BUILD_FLAGS)
endif

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


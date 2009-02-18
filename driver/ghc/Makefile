TOP=../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
C_PROG = ghc-$(ProjectVersion)$(exeext)
INSTALL_PROGS += $(C_PROG)
else
SCRIPT_DIR = $(DESTDIR)$(bindir)
SCRIPT_BASE = ghc
SCRIPT_VERSIONED_BASE = $(SCRIPT_BASE)-$(ProjectVersion)
SCRIPT           = $(SCRIPT_DIR)/$(SCRIPT_BASE)
SCRIPT_VERSIONED = $(SCRIPT_DIR)/$(SCRIPT_VERSIONED_BASE)

install::
	mv $(SCRIPT) $(SCRIPT_VERSIONED)
	ln -s $(SCRIPT_VERSIONED_BASE) $(SCRIPT)
endif

include $(TOP)/mk/bindist.mk

include $(TOP)/mk/target.mk

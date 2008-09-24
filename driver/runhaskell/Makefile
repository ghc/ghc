TOP=../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
install::
	cp $(DESTDIR)$(bindir)/runghc.exe $(DESTDIR)$(bindir)/runhaskell.exe
else
install::
	ln -sf runghc $(DESTDIR)$(bindir)/runhaskell
endif

include $(TOP)/mk/bindist.mk

include $(TOP)/mk/target.mk

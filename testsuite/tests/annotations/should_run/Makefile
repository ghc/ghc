TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

CONFIG_HS=Config.hs

config : 
	rm -f $(CONFIG_HS)
	@echo "Creating $(CONFIG_HS) ... "
	echo "module Config where" >>$(CONFIG_HS)
	echo "cTop          :: String" >> $(CONFIG_HS)
	echo 'cTop          = "$(subst \,\\,$(shell '$(TEST_HC)' --print-libdir))"' >> $(CONFIG_HS)

TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

CONFIG_HS=Config.hs

config : 
	rm -f $(CONFIG_HS)
	echo "module Config where" >>$(CONFIG_HS)
	echo "cTop          :: String" >> $(CONFIG_HS)
	echo 'cTop          = "$(subst \,\\,$(shell '$(TEST_HC)' --print-libdir))"' >> $(CONFIG_HS)

T16841:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -c T16841.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface T16841.hi | grep 'annotated by'

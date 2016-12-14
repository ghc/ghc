TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T12485 :
	@rm -rf a.db b.db
	'$(GHC_PKG)' init a.db
	'$(GHC_PKG)' -f a.db register a.pkg
	'$(GHC_PKG)' init b.db
	'$(GHC_PKG)' -f a.db/ -f b.db/ register b.pkg # register b.pkg in b.db
	# -package-db in dependency order
	'$(TEST_HC)' $(TEST_HC_OPTS) -XNoImplicitPrelude -fforce-recomp -hide-all-packages -no-user-package-db -package-db a.db -package-db b.db -package-id a-1-XXX -package-id b-1-XXX Main.hs
	# -package-db in reverse dependency order
	'$(TEST_HC)' $(TEST_HC_OPTS) -XNoImplicitPrelude -fforce-recomp -hide-all-packages -no-user-package-db -package-db b.db -package-db a.db -package-id a-1-XXX -package-id b-1-XXX Main.hs

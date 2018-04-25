TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

DIR='test,path'
PKGCONF=$(DIR)/local.package.conf
LOCAL_GHC_PKG = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF)

ifeq "$(WINDOWS)" "YES"
DLL = lib$1.dll
else ifeq "$(DARWIN)" "YES"
DLL = lib$1.dylib
else
DLL = lib$1.so
endif

ifeq "$(WINDOWS)" "YES"
EXE = $1.exe
else ifeq "$(DARWIN)" "YES"
EXE = $1
else
EXE = $1
endif

# Check if paths with commas work
# Previously, the linker would use -Wl,-rpath -Wl,test,path to pass the
# path and that would treat test and path as 2 different arguments
path_with_commas :
	@rm -rf $(DIR)
	mkdir -p $(DIR)
	$(LOCAL_GHC_PKG) init $(PKGCONF)
	# Create shared library 'foo' from Dummy.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -shared -dynamic Dummy.hs -odir $(DIR) -hidir $(DIR) -o $(DIR)/$(call DLL,foo) > /dev/null
	# Make a package that forces trying to load it
	$(LOCAL_GHC_PKG) register --force test.pkg

	@rm -rf Main.{hi,o} Main
	# Because we compile with -package testpkg, TH has to load 'foo'
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -package-db $(PKGCONF) -package testpkg Main.hs
	./$(call EXE,Main)

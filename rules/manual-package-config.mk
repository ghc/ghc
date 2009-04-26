
define manual-package-config # args: $1 = dir

$1/package.conf.inplace : $1/package.conf.in $(GHC_PKG_INPLACE)
	$(CPP) $(RAWCPP_FLAGS) -P \
		-DTOP='"$$(TOP)"' \
		$$($1_PACKAGE_CPP_OPTS) \
		-x c -I$$(GHC_INCLUDE_DIR) $$< | \
	grep -v '^#pragma GCC' | \
	sed -e 's/""//g' -e 's/:[ 	]*,/: /g' >$$@

	$(GHC_PKG_INPLACE) update --force $$@

# This is actually a real file, but we need to recreate it on every
# "make install", so we declare it as phony
.PHONY: $1/package.conf.install
$1/package.conf.install:
	$(CPP) $(RAWCPP_FLAGS) -P \
		-DINSTALLING \
		-DLIB_DIR='"$$(libdir)"' \
		-DINCLUDE_DIR='"$$(libdir)/include"' \
		$$($1_PACKAGE_CPP_OPTS) \
		-x c -I$$(GHC_INCLUDE_DIR) $1/package.conf.in | \
	grep -v '^#pragma GCC' | \
	sed -e 's/""//g' -e 's/:[ 	]*,/: /g' >$$@

clean : clean_$1
.PHONY: clean_$1
clean_$1 : clean_$1_package.conf
.PHONY: clean_$1_package.conf
clean_$1_package.conf :
	$(RM) $1/package.conf.install $1/package.conf.inplace
endef

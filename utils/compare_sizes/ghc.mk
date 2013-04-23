
utils/compare_sizes_USES_CABAL = YES
utils/compare_sizes_PACKAGE = compareSizes
utils/compare_sizes_MODULES = Main
utils/compare_sizes_dist-install_PROGNAME = compareSizes
utils/compare_sizes_dist-install_INSTALL_INPLACE = NO

$(eval $(call build-prog,utils/compare_sizes,dist-install,1))



utils/compare_sizes_USES_CABAL = YES
utils/compare_sizes_PACKAGE = compareSizes
utils/compare_sizes_MODULES = Main
utils/compare_sizes_dist_PROG = compareSizes$(exeext)

$(eval $(call build-prog,utils/compare_sizes,dist,1))


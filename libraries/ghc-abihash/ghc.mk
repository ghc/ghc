libraries/ghc-abihash_PACKAGE = ghc-abihash
libraries/ghc-abihash_dist-install_GROUP = libraries
$(if $(filter ghc-abihash,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/ghc-abihash,dist-boot,0)))
$(if $(filter ghc-abihash,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/ghc-abihash,dist-install,1)))
$(if $(filter ghc-abihash,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/ghc-abihash,dist-install,2)))

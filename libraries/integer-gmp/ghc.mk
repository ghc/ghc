libraries/integer-gmp_PACKAGE = integer-gmp
libraries/integer-gmp_dist-install_GROUP = libraries
$(if $(filter integer-gmp,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/integer-gmp,dist-boot,0)))
$(if $(filter integer-gmp,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/integer-gmp,dist-install,1)))
$(if $(filter integer-gmp,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/integer-gmp,dist-install,2)))

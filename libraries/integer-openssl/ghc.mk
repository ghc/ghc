libraries/integer-openssl_PACKAGE = integer-openssl
libraries/integer-openssl_dist-install_GROUP = libraries
$(if $(filter integer-openssl,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/integer-openssl,dist-boot,0)))
$(if $(filter integer-openssl,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/integer-openssl,dist-install,1)))
$(if $(filter integer-openssl,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/integer-openssl,dist-install,2)))

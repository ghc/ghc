libraries/libiserv_PACKAGE = libiserv
libraries/libiserv_dist-install_GROUP = libraries
$(if $(filter libiserv,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/libiserv,dist-boot,0)))
$(if $(filter libiserv,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/libiserv,dist-install,1)))
$(if $(filter libiserv,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/libiserv,dist-install,2)))

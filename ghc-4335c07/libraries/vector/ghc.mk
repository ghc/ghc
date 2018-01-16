libraries/vector_PACKAGE = vector
libraries/vector_dist-install_GROUP = libraries
$(if $(filter vector,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/vector,dist-boot,0)))
$(if $(filter vector,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/vector,dist-install,1)))
$(if $(filter vector,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/vector,dist-install,2)))

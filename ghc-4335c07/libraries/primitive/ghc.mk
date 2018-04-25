libraries/primitive_PACKAGE = primitive
libraries/primitive_dist-install_GROUP = libraries
$(if $(filter primitive,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/primitive,dist-boot,0)))
$(if $(filter primitive,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/primitive,dist-install,1)))
$(if $(filter primitive,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/primitive,dist-install,2)))

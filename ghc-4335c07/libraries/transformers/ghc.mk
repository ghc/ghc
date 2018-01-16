libraries/transformers_PACKAGE = transformers
libraries/transformers_dist-install_GROUP = libraries
$(if $(filter transformers,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/transformers,dist-boot,0)))
$(if $(filter transformers,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/transformers,dist-install,1)))
$(if $(filter transformers,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/transformers,dist-install,2)))

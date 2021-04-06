libraries/network_PACKAGE = network
libraries/network_dist-install_GROUP = libraries
$(if $(filter network,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/network,dist-boot,0)))
$(if $(filter network,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/network,dist-install,1)))
$(if $(filter network,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/network,dist-install,2)))

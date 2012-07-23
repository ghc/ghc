libraries/lwconc_PACKAGE = lwconc
libraries/lwconc_dist-install_GROUP = libraries
$(if $(filter lwconc,$(PKGS_THAT_BUILD_WITH_STAGE0)),$(eval $(call build-package,libraries/lwconc,dist-boot,0)))
$(eval $(call build-package,libraries/lwconc,dist-install,$(if $(filter lwconc,$(PKGS_THAT_BUILD_WITH_STAGE2)),2,1)))

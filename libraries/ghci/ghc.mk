libraries/ghci_PACKAGE = ghci
libraries/ghci_dist-install_GROUP = libraries
$(if $(filter ghci,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/ghci,dist-boot,0)))
$(if $(filter ghci,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/ghci,dist-install,1)))
$(if $(filter ghci,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/ghci,dist-install,2)))

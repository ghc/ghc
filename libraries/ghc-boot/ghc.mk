libraries/ghc-boot_PACKAGE = ghc-boot
libraries/ghc-boot_dist-install_GROUP = libraries
$(if $(filter ghc-boot,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/ghc-boot,dist-boot,0)))
$(if $(filter ghc-boot,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/ghc-boot,dist-install,1)))
$(if $(filter ghc-boot,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/ghc-boot,dist-install,2)))

libraries/ghc-boot/dist-boot/build/GHC/Version.hs \
libraries/ghc-boot/dist-install/build/GHC/Version.hs: mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "module GHC.Version where"                                    >> $@
	@echo                                                               >> $@
	@echo 'import Prelude -- See Note [Why do we import Prelude here?]' >> $@
	@echo                                                               >> $@
	@echo 'cProjectGitCommitId   :: String'                             >> $@
	@echo 'cProjectGitCommitId   = "$(ProjectGitCommitId)"'             >> $@
	@echo                                                               >> $@
	@echo 'cProjectVersion       :: String'                             >> $@
	@echo 'cProjectVersion       = "$(ProjectVersion)"'                 >> $@
	@echo                                                               >> $@
	@echo 'cProjectVersionInt    :: String'                             >> $@
	@echo 'cProjectVersionInt    = "$(ProjectVersionInt)"'              >> $@
	@echo                                                               >> $@
	@echo 'cProjectPatchLevel    :: String'                             >> $@
	@echo 'cProjectPatchLevel    = "$(ProjectPatchLevel)"'              >> $@
	@echo                                                               >> $@
	@echo 'cProjectPatchLevel1   :: String'                             >> $@
	@echo 'cProjectPatchLevel1   = "$(ProjectPatchLevel1)"'             >> $@
	@echo                                                               >> $@
	@echo 'cProjectPatchLevel2   :: String'                             >> $@
	@echo 'cProjectPatchLevel2   = "$(ProjectPatchLevel2)"'             >> $@
	@echo done.

libraries/ghc-boot/dist-boot/package-data.mk: \
	libraries/ghc-boot/dist-boot/build/GHC/Version.hs
libraries/ghc-boot/dist-install/package-data.mk: \
	libraries/ghc-boot/dist-install/build/GHC/Version.hs

libraries/ghc-boot/dist-boot/build/GHC/Platform/Host.hs \
libraries/ghc-boot/dist-install/build/GHC/Platform/Host.hs: mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "module GHC.Platform.Host where"                              >> $@
	@echo                                                               >> $@
	@echo 'import GHC.Platform'                                         >> $@
	@echo                                                               >> $@
	@echo 'cHostPlatformArch   :: Arch'                                 >> $@
	@echo 'cHostPlatformArch   = $(HaskellHostArch)'                    >> $@
	@echo                                                               >> $@
	@echo 'cHostPlatformOS     :: OS'                                   >> $@
	@echo 'cHostPlatformOS     = $(HaskellHostOs)'                      >> $@
	@echo                                                               >> $@
	@echo 'cHostPlatformMini :: PlatformMini'                           >> $@
	@echo 'cHostPlatformMini = PlatformMini'                            >> $@
	@echo '  { platformMini_arch = cHostPlatformArch'                   >> $@
	@echo '  , platformMini_os = cHostPlatformOS'                       >> $@
	@echo '  }'                                                         >> $@
	@echo done.

libraries/ghc-boot/dist-boot/package-data.mk: \
	libraries/ghc-boot/dist-boot/build/GHC/Platform/Host.hs
libraries/ghc-boot/dist-install/package-data.mk: \
	libraries/ghc-boot/dist-install/build/GHC/Platform/Host.hs

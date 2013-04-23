
utils/ghc-pwd_USES_CABAL = YES
utils/ghc-pwd_PACKAGE    = ghc-pwd
utils/ghc-pwd_dist-install_INSTALL_INPLACE = YES
utils/ghc-pwd_dist-install_WANT_BINDIST_WRAPPER = YES
utils/ghc-pwd_dist-install_PROGNAME = ghc-pwd

$(eval $(call build-prog,utils/ghc-pwd,dist-install,1))


#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/sh
(cd Cabal; misc/gen-extra-source-files.hs Cabal.cabal)
(cd cabal-install; ../Cabal/misc/gen-extra-source-files.hs cabal-install.cabal)

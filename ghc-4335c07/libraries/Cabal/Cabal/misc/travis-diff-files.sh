#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/sh
git status > /dev/null # See 09a71929e433f36b27fd6a4938469d3bbbd5e191
git diff-files -p --exit-code

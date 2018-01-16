#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/sh

(echo 'Benchmark;Runtime change;Original runtime'; ./bench-cmp.pl "$@") | column -ts\;

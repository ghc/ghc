# Bootstrapping hadrian

These scripts are originally from the cabal-install repo with a few
small tweaks.

This utility allows you to build hadrian without cabal-install, which can be useful
for packagers. If you are a developer then build hadrian using cabal-install.

If you want to bootstrap with ghc-8.10.5 then run the ./bootstrap script with the
`plan-bootstrap-8.10.5.json` file.

    bootstrap.py -d plan-bootstrap-8.10.5.json -w /path/to-ghc

The result of the bootstrap script will be a hadrian binary in
`_build/bin/hadrian`.

There is a script (using nix) which can be used to generate the bootstrap plans for the range
of supported GHC versions using nix.

    generate_bootstrap_plans

Otherwise you can run the commands in ./generate_bootstrap_plans directly.


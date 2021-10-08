# node.js dependencies for GHCJS

This repository contains an `npm` package description for pulling in the dependencies
of the parts of GHCJS implemented with node.js scripts (`thrunner.js for Template
Haskell and `irunner.js` for GHCJSi).

The `dist` branch contains a full snapshot of the code for these dependencies.
The `ghcjs-boot` repository has a submodule referring to a commit in that branch.

To update the dependencies:

  1. update `package.json`
  2. commit changes
  3. run `update.sh` to add a new snapshot to the `dist` branch
  4. update the `ghcjs-node` git submodule in `ghcjs-boot` to point to the new snapshot



import ghc.nix/default.nix {
#    bootghc = "ghc902";
    withDocs = false;
    withHadrianDeps = true;
    withLlvm = true;
    withIde = true;
}

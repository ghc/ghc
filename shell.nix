import ghc.nix/default.nix {
    bootghc = "ghc902";
    withDocs = true;
    withHadrianDeps = true;
    withLlvm = true;
    withIde = true;
}

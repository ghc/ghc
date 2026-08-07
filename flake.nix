{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux"];
      perSystem = {
        config,
        pkgs,
        ...
      }: {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.haskell.compiler.ghc910
            pkgs.haskell.packages.ghc910.haskell-language-server
            pkgs.haskellPackages.happy
            pkgs.haskellPackages.alex
            pkgs.cabal-install
            pkgs.autoconf
            pkgs.mold
            pkgs.automake
            pkgs.m4
            pkgs.python3
            pkgs.less
            pkgs.llvmPackages_20.llvm
            pkgs.llvmPackages_20.clang
            pkgs.gmp
            pkgs.sphinx
            (pkgs.texliveSmall.withPackages (ps: with ps; [termes-otf
                scheme-medium collection-xetex fncychap titlesec tabulary varwidth
            framed capt-of wrapfig needspace dejavu-otf helvetic upquote
             ]))
          ];
        };
      };
      flake = {};
    };
}

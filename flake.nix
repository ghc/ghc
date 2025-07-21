{
  description = "GHC development environment using ghc.nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-nix = {
      url = "gitlab:ghc/ghc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ghc-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghc-env = ghc-nix.devShells.${system}.default;
      in
      {
        devShells.default = ghc-env;
        
        # Alternative: if you want to customize the environment
        devShells.custom = pkgs.mkShell {
          inputsFrom = [ ghc-env ];
          buildInputs = with pkgs; [
            # Add any additional tools you need
            git
            vim
          ];
        };
      });
}

{
  description = "GHC boot library linting";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        packages = rec {
          check-submodules = pkgs.haskellPackages.callCabal2nix "generate-ci" ./. {};
          default = check-submodules;
        };

        devShells.default = self.packages.${system}.default.env;

        apps = rec {
          check-submodules = flake-utils.lib.mkApp {
            drv = self.packages.${system}.check-submodules;
          };
          default = check-submodules;
        };
      }
    );
}


{
  description = "MMTK-GHC development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; # We want to use packages from the binary cache
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = { url = "github:hercules-ci/gitignore.nix"; flake = false; };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        gitignoreSrc = pkgs.callPackage inputs.gitignore { };
        llvmPackages = pkgs.llvmPackages_11;
      in rec {
        devShell = pkgs.mkShell {
          CARGO_INSTALL_ROOT = "${toString ./.}/.cargo";

          LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";

          LLVM_CONFIG_PATH = "${llvmPackages.llvm.dev}/bin/llvm-config";

          buildInputs = with pkgs; [
            rustup
            git
            # For bindgen
            llvm clang
          ];
        };
      });
}

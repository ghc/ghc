{
  description = "GHC CI Generator";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        packages = rec {
          # The Haskell generator executable
          generate-ci = pkgs.haskellPackages.callCabal2nix "generate-ci" ./. {};


          # Wrapper scripts
          generate-job-metadata = pkgs.runCommand "generate-job-metadata" {
            nativeBuildInputs = with pkgs; [ makeWrapper ];
          } ''
            mkdir -p $out/bin
            makeWrapper ${./generate-job-metadata} $out/bin/generate-job-metadata \
              --prefix PATH : ${with pkgs; lib.makeBinPath [ generate-ci gitMinimal ]}
          '';

          generate-jobs = pkgs.runCommand "generate-jobs" {
            nativeBuildInputs = with pkgs; [ makeWrapper ];
          } ''
            mkdir -p $out/bin
            makeWrapper ${./generate-jobs} $out/bin/generate-jobs \
              --prefix PATH : ${with pkgs; lib.makeBinPath [ generate-ci jq gitMinimal ]}
          '';
          default = generate-jobs;
        };

        apps = rec {
          generate-jobs = flake-utils.lib.mkApp {
            drv = self.packages.${system}.generate-jobs;
          };

          generate-job-metadata = flake-utils.lib.mkApp {
            drv = self.packages.${system}.generate-job-metadata;
          };

          default = generate-jobs;
        };
      }
    );
}

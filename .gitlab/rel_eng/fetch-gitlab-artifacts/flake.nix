{
  description = "GHC release download utility";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        packages = rec {
          fetch-gitlab = 
            let
              pkg = { buildPythonPackage, python-gitlab, unzip }:
                buildPythonPackage {
                  pname = "fetch-gitlab";
                  version = "0.0.1";
                  src = ./.;
                  propagatedBuildInputs = [ python-gitlab unzip ];
                  preferLocalBuild = true;
                };
            in pkgs.python3Packages.callPackage pkg { inherit (pkgs) unzip; };
          default = fetch-gitlab;
        };
        apps = rec {
          fetch-gitlab = flake-utils.lib.mkApp { drv = self.packages.${system}.fetch-gitlab; };
          default = fetch-gitlab;
        };
      }
    );
}

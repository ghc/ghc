{ nix-gitignore, python3Packages, fetch-gitlab }:

let
  ghcup-metadata = { buildPythonPackage, python-gitlab, pyyaml }:
    buildPythonPackage {
      pname = "ghcup-metadata";
      version = "0.0.1";
      src = nix-gitignore.gitignoreSource [] ./.;
      propagatedBuildInputs = [fetch-gitlab python-gitlab pyyaml ];
      preferLocalBuild = true;
    };
in
python3Packages.callPackage ghcup-metadata { }

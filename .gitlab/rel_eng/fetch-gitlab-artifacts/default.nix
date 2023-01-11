{ nix-gitignore, python3Packages, unzip }:

let
  fetch-gitlab = { buildPythonPackage, python-gitlab, unzip }:
    buildPythonPackage {
      pname = "fetch-gitlab";
      version = "0.0.1";
      src = nix-gitignore.gitignoreSource [] ./.;
      propagatedBuildInputs = [ python3Packages.python-gitlab unzip ];
      preferLocalBuild = true;
    };
in 
python3Packages.callPackage fetch-gitlab { inherit unzip; }

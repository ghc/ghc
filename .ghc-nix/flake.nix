{
  description = "adhoc ghc.nix devShell flake";
  nixConfig = {
    bash-prompt = "\\[\\e[34;1m\\]ghc.nix ~ \\[\\e[0m\\]";
  };
  inputs.ghc-nix.url = "gitlab:ghc/ghc.nix?host=gitlab.haskell.org";
  # In case you need to fork ghc.nix:
  # inputs.ghc-nix.url = "gitlab:ghc/ghc.nix/yourbranch?host=gitlab.haskell.org";
  outputs = { ghc-nix, ... }:
    let
      userSettings = {
        # put your settings here
        withIde = true;
      };
    in
    {
      devShells = ghc-nix.lib.perSystem (system: { default = ghc-nix.legacy ({ inherit system; } // userSettings); });
    };
}

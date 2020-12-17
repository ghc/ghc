import ghc.nix/default.nix {
    version = "9.1";
    withDocs = false;
    withHadrianDeps = true;
    crossTargetArch = "mips64el-unknown-linux-gnuabi64"; # "mips64el-unknown-linux-gnu";
    nixCrossTools = "svenmips";
    withNuma = false;
    withLlvm = false;
    useClang = false;
    withDwarf = false;
    withCustomLlvm = "/home/sven/src/llvm-project/build";
    nixpkgs = import /home/sven/src/nixpkgs { };
#        overlays = [ (self: super: {
#            fun-binutils-unwrapped = super.binutils-unwrapped.overrideAttrs (oldAttrs: rec {
#                name = "fun-binutils";
#                dontStrip = true;
#                # patches = oldAttrs.patches ++ [./binutils.patch];
#                NIX_CFLAGS_COMPILE = toString (oldAttrs.NIX_CFLAGS_COMPILE or "") + " -ggdb -Og";
#                });
#
#           fun-binutils = super.pkgs.wrapBintoolsWith {
#               bintools = self.fun-binutils-unwrapped;
#           };
#      })];
#    };
}

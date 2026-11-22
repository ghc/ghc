from itertools import permutations
from pathlib import Path
import subprocess
import sys
import tempfile


def ordered(groups, order):
    return " ".join(groups[key] for key in order if key in groups)


def sources(provider, reader):
    declarations = []
    applications = []
    patterns = []
    for label, field in [("Linear", "a %1 ->"), ("Ordinary", "a ->")]:
        groups = {"C": "Eq a =>", "F": field, "I": "forall c.", "R": "forall b ->"}
        for order in permutations(groups):
            name = label + "".join(order)
            signature = ordered(groups, order)
            arguments = ordered({"F": "x", "I": "@()", "R": "(type Char)"}, order)
            pattern = ordered({"F": "x", "R": "(type _)"}, order)
            declarations.append(f"  {name} :: forall a. {signature} T a b c\n")
            applications.append(f"{name} @Double {arguments}")
            patterns.append(f"observe ({name} {pattern}) = x == x\n")

    (provider / "Provider.hs").write_text(
        "{-# LANGUAGE ExplicitForAll, GADTs, LinearTypes, RequiredTypeArguments #-}\n"
        "module Provider (T(..)) where\n\n"
        "data T a b c where\n" + "".join(declarations),
        encoding="utf-8",
    )
    (reader / "Main.hs").write_text(
        "{-# LANGUAGE ExplicitNamespaces, GADTs, RequiredTypeArguments #-}\n"
        "{-# LANGUAGE TypeAbstractions, TypeApplications #-}\n"
        "module Main where\n\n"
        "import Provider\n\n"
        "make :: Double -> [T Double Char ()]\n"
        "make x = [" + ",\n          ".join(applications) + "]\n\n"
        "observe :: T a b c -> Bool\n" + "".join(patterns) +
        "\nmain :: IO ()\n"
        "main = do\n"
        "  let finite = map observe (make 3)\n"
        "      nan = map observe (make (0 / 0))\n"
        "  print (length finite, and finite, or nan)\n",
        encoding="utf-8",
    )


compiler, libdir = sys.argv[1:]
with tempfile.TemporaryDirectory() as fresh:
    work = Path(fresh)
    for optimization in ["-O0", "-O2"]:
        provider = work / optimization / "provider"
        reader = work / optimization / "reader"
        provider.mkdir(parents=True)
        reader.mkdir()
        sources(provider, reader)
        ghc = [compiler, "-B" + libdir, "-v0", optimization, "-package-env", "-",
               "-i", "-i.", "-dcore-lint", "-dstg-lint",
               "-fomit-interface-pragmas", "-fno-write-if-simplified-core"]
        subprocess.run(ghc + ["-c", "Provider.hs"], cwd=provider, check=True)
        (reader / "Provider.hi").write_bytes((provider / "Provider.hi").read_bytes())
        subprocess.run(ghc + ["-c", "Main.hs"], cwd=reader, check=True)
        subprocess.run(ghc + ["Main.o", str(provider / "Provider.o"), "-o", "test"],
                       cwd=reader, check=True)
        subprocess.run([reader / "test"], cwd=reader, check=True)

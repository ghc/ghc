"""Check that --make restores a missing length equation during a compiler upgrade."""
from argparse import ArgumentParser
from hashlib import sha256
from pathlib import Path
from shutil import copy2
from subprocess import PIPE, STDOUT, run
from tempfile import TemporaryDirectory
import os

def products(directory: Path) -> dict[str, tuple[int, bytes]]:
    return {
        file.name: (file.stat().st_mtime_ns, sha256(file.read_bytes()).digest())
        for file in directory.iterdir() if file.suffix in (".o", ".hi")
    }

def invoke(compiler: list[str], directory: Path, *args: str, expected_error: str | None = None) -> str:
    result = run([*compiler, *args], cwd=directory, stdout=PIPE, stderr=STDOUT, text=True)
    if expected_error is None:
        if result.returncode:
            raise SystemExit(result.stdout)
    elif result.returncode == 0 or expected_error not in result.stdout:
        raise SystemExit(f"Expected {expected_error}. Compiler output:\n{result.stdout}")
    return result.stdout

def check(directory: Path, old: list[str], new: list[str], *, interfaces_only: bool, library_upgrade: bool) -> None:
    source = Path(__file__).resolve().parents[1] / "packed-vector"
    if root := os.environ.get("GHC_SOURCE_ROOT"):
        source = Path(root) / "docs/dependent-haskell/examples/packed-vector"
    for name in ("PackedVecLibrary", "RetainedCharLength", "RetainedCharProjection"):
        copy2(source / f"{name}.hs", directory)
    (directory / "RetainedCharVector.hs").write_text(
        "{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}\n"
        "module RetainedCharVector (CharVector(..)) where\n"
        "import PackedVecLibrary (Nat, Vec)\n"
        "data CharVector where\n"
        "    CharVector :: foreach (count :: Nat) -> Vec Char count -> CharVector\n"
    )
    (directory / "Control.hs").write_text("module Control where\ndata Control = Control\n")
    flags = ["-O0", "-i", "-i.", "-fhide-source-paths", "-fomit-interface-pragmas",
             "-fno-write-if-simplified-core", "-dcore-lint", "-dstg-lint"]
    if interfaces_only:
        flags.extend(["-fno-code", "-fwrite-interface"])
    invoke(old, directory, *flags, "--make", "-no-link", "Control.hs", "RetainedCharLength.hs")
    worker = "RetainedCharLength.hi"
    equation = "SourceTable T:storedLength"
    if equation in invoke(old, directory, "--show-iface", worker):
        raise SystemExit("The input compiler already exports the length equation")

    # Give the reader only the old interfaces. It cannot rebuild the worker.
    reader = directory / "reader"
    reader.mkdir()
    for interface in directory.glob("*.hi"):
        copy2(interface, reader)
    copy2(directory / "RetainedCharProjection.hs", reader)
    invoke([new[0], *old[1:]], reader, *flags, "-c", "RetainedCharProjection.hs", expected_error="[GHC-68947]")

    before = products(directory)
    roots = ["--make", "-no-link", "Control.hs", "RetainedCharProjection.hs"]
    log = invoke(new, directory, *flags, *roots)
    upgraded = products(directory)
    if equation not in invoke(new, directory, "--show-iface", worker):
        raise SystemExit("The upgraded worker omitted its length equation")
    projection = "RetainedCharProjection.hi" if interfaces_only else "RetainedCharProjection.o"
    if interfaces_only and any(name.endswith(".o") for name in upgraded):
        raise SystemExit("Typechecking produced object code")
    if before[worker] == upgraded[worker] or projection not in upgraded:
        raise SystemExit("The upgrade did not rebuild the worker and its consumer")
    for name in ("Control", "RetainedCharVector"):
        if library_upgrade:
            if f"Compiling {name} [GHC.Essentials changed]" not in log:
                raise SystemExit(f"Missing library ABI rebuild of {name}:\n{log}")
        else:
            for filename, original in before.items():
                if Path(filename).stem == name and upgraded[filename] != original:
                    raise SystemExit(f"Unexpected rebuild of {filename}")
    invoke(new, directory, *flags, *roots)
    if products(directory) != upgraded:
        raise SystemExit("The second build changed a compiler product")

if __name__ == "__main__":
    parser = ArgumentParser(description=__doc__)
    parser.add_argument("--interfaces-only", action="store_true")
    parser.add_argument("--library-upgrade", action="store_true")
    for field in ("old_ghc", "old_lib", "new_ghc", "new_lib"):
        parser.add_argument(field, type=lambda path: str(Path(path).resolve()))
    args = parser.parse_args()
    with TemporaryDirectory(prefix="ghc-length-upgrade-") as location:
        check(Path(location), [args.old_ghc, "-B" + args.old_lib, "-package-env", "-"],
              [args.new_ghc, "-B" + args.new_lib, "-package-env", "-"],
              interfaces_only=args.interfaces_only, library_upgrade=args.library_upgrade)
    print("Verified the length equation, rebuilt consumer and warm reuse.")

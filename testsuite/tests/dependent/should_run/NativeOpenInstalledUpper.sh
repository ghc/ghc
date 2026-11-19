#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -O0 -v0 -package-env - -i -dcore-lint -dstg-lint)
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{db,interfaces,exporter,client}

# Preserve the installed unit IDs and expose only .hi files for ghc-internal.
python3 - "$2/package.conf.d" "$out/db" "$out/interfaces" <<'PY'
from pathlib import Path
import re
import shlex
import shutil
import sys

original_db, copy_db, interfaces = map(Path, sys.argv[1:])
pkgroot = original_db.parent.resolve().as_posix()
[internal] = original_db.glob("ghc-internal-*.conf")
for registration in original_db.glob("*.conf"):
    contents = registration.read_text().replace("${pkgroot}", pkgroot)
    if registration == internal:
        def field(name):
            return rf"(?m)^{name}:[^\n]*(?:\n[ \t][^\n]*)*"

        paths = re.search(field("import-dirs"), contents).group().split(":", 1)[1]
        for path in map(Path, shlex.split(paths)):
            for hi in path.rglob("*.hi"):
                dest = interfaces / hi.relative_to(path)
                if not dest.exists():
                    dest.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copyfile(hi, dest)
        contents = re.sub(field("import-dirs"), lambda _: f'import-dirs: "{interfaces.as_posix()}"', contents)
        for name in ("library-dirs", "library-dirs-static", "dynamic-library-dirs"):
            contents = re.sub(field(name), "", contents)
    (copy_db / registration.name).write_text(contents)
PY
"$3" --package-db="$out/db" recache
db=(-clear-package-db -package-db "$out/db")
cd "$out/exporter"
cp "$src/UnicodeExport.hs" .
"${ghc[@]}" "${db[@]}" -fomit-interface-pragmas -c UnicodeExport.hs
"${ghc[@]}" --show-iface UnicodeExport.hi >exported.iface
grep -Fq 'GHC.Internal.Unicode.T:toUpper' exported.iface
cp UnicodeExport.hi "$out/interfaces/"

cd "$out/client"
cp "$src/NativeOpenInstalledUpper.hs" .
"${ghc[@]}" "${db[@]}" -i"$out/interfaces" -c NativeOpenInstalledUpper.hs
# Only the link step uses the original library registration and exporter object.
"${ghc[@]}" NativeOpenInstalledUpper.o ../exporter/UnicodeExport.o -o client
./client

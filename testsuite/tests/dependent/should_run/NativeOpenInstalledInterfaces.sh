# Populate out/db and out/interfaces from GHC_LIBDIR using its matching ghc-pkg.
# Preserve the installed unit IDs and expose only interfaces for ghc-internal.
python3 - "$1/package.conf.d" "$out/db" "$out/interfaces" <<'PY'
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
            for suffix in ("*.hi", "*.dyn_hi"):
                for hi in path.rglob(suffix):
                    dest = interfaces / hi.relative_to(path)
                    if not dest.exists():
                        dest.parent.mkdir(parents=True, exist_ok=True)
                        shutil.copyfile(hi, dest)
        contents = re.sub(field("import-dirs"), lambda _: f'import-dirs: "{interfaces.as_posix()}"', contents)
        for name in ("library-dirs", "library-dirs-static", "dynamic-library-dirs", "bytecode-library-dirs"):
            contents = re.sub(field(name), "", contents)
    (copy_db / registration.name).write_text(contents)
PY
"$2" --package-db="$out/db" recache
db=(-clear-package-db -package-db "$out/db")

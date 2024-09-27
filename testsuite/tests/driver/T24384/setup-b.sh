#!/usr/bin/env bash

set -eu

ghc_cmd="$1"
ghc_pkg_cmd="$2"

base="$PWD"
db="$base/db"
dep="$base/dep"
conf_dep="${dep}/dep.conf"
int="$base/int"
conf_int="${int}/int.conf"
lib_tag="ghc$($ghc_cmd --numeric-version)"

ghc_pkg()
{
  eval "${ghc_pkg_cmd@Q} --no-user-package-db --package-db=${db@Q} $@"
}

ghc()
{
  eval "${ghc_cmd@Q} $@"
}

mkdir -p "$dep" "$int" "$db"

cat > "$dep/DepA.hs" <<EOF
module DepA where
EOF

cat > "$int/DepB.hs" <<EOF
module DepB where
EOF

cat > "$conf_dep" <<'EOF'
name: dep
package-name: dep
version: 1.0
id: dep-1.0-z
key: dep-1.0-z
depends: dep-1.0-int
exposed: True
exposed-modules:
  DepA,
  DepB from dep-1.0-int:DepB
import-dirs: ${pkgroot}/dep
library-dirs: ${pkgroot}/dep
hs-libraries: HSdep-1.0-z
EOF

cat > "$conf_int" <<'EOF'
name: z-dep-z-int
package-name: dep
lib-name: int
version: 1.0
id: dep-1.0-int
key: dep-1.0-int
exposed-modules: DepB
import-dirs: ${pkgroot}/int
library-dirs: ${pkgroot}/int
hs-libraries: HSdep-1.0-int
EOF

ghc_pkg recache

ghc "-v0 -package-db ${db@Q} -hidir ${int@Q} -O0 -this-unit-id dep-1.0-int -dynamic -shared ${int@Q}/DepB.hs -o ${int@Q}/libHSdep-1.0-int-${lib_tag}.so"
ghc "-v0 -package-db ${db@Q} -hidir ${dep@Q} -O0 -this-unit-id dep-1.0-z -dynamic -shared ${dep@Q}/DepA.hs -o ${dep@Q}/libHSdep-1.0-z-${lib_tag}.so"

ghc_pkg -v0 register "${conf_int@Q}"
ghc_pkg -v0 register "${conf_dep@Q}"

cat > InternalReexport.hs <<EOF
module InternalReexport where
import DepB
EOF

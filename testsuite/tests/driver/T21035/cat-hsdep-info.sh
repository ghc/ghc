libdir=$1
base_version=$2
cat <<END
name: dep
version: 0.1
id: hsdep-0.1
key: hsdep-0.1
license: BSD3
exposed: True
exposed-modules: HsDep
hidden-modules:
trusted: False
import-dirs: "$(pwd)/hsdep"
library-dirs: "$(pwd)/$libdir" "$(pwd)"
hs-libraries: HShsdep-0.1
extra-libraries:
extra-ghci-libraries:
include-dirs:
includes:
depends: $base_version
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces:
END

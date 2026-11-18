# Derive a closed family from a checked Char case

Run `./check.sh ABS_GHC ABS_LIBDIR` with this fork's compiler and
matching library directory. The script finds the checked-in
`SimpleUpperCaseMapping.hs` relative to this directory. An optional
third argument overrides that source path. The script builds in a
temporary directory.

`GenerateUpper.hs` uses the GHC API to resolve, typecheck, and
desugar a fresh home-module copy of the Unicode worker source. It
lints the selected binding in its module scope and certifies
all 1,505 literal arms and the strict identity default. It writes the
ordinary closed family `Upper` directly from the certified Core
rows. The harness also rejects recursive and partial functions and a
nonidentity default before emitting a family.

The exporter uses `Upper c ~ uc` and `KnownChar uc`, following the
shape of `p3` in draft proposal #509. An ordinary client sees only
the family and exporter interfaces during typechecking. The link
step receives the original objects. The program checks ASCII,
Unicode and late-table mappings and the identity default. The
harness also checks a ground `Refl` and its interface dependency,
rejects a wrong literal proof, and rejects `Upper c ~ c` for a
rigid `c`. Earlier incompatible equations in this family require
that last check to remain stuck.

The rebuild changes one literal in a temporary copy of the original
worker, then runs the same Core certifier and generator again. GHC
rebuilds the ordinary family and clients. The exporter and client
source stay unchanged, and the result for `'x'` changes from `'X'`
to `'Y'`.

This example uses a generated module to carry the equations across
the interface. To check `Data.Char.toUpper c` directly, GHC still
needs to certify the package producer when it compiles the library,
associate the public function with its worker, and record the
dependency in its interface.
[Draft proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509).

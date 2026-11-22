# Rebuild the R27 package chain

Use the normal fork at `5833e38cbd0d849433badd128dc31772d8559584` or
`fd19c8fe5f39af545302cee157e7471e563087c6` and its matching `ghc-pkg`.
The comparison uses Cabal 3.18.1.0, Python 3 and GNU `patch`.
The first patch applies the fork's matchability prerequisites to
`first-class-families` and `kind-apply`. The second adds the 14
standalone kind signatures. Both patches preserve the Cabal files
and the source files' line endings.

In an empty working directory, set `replay` to the full path of this
directory and set `ghc` and `ghc_pkg` to the compiler and package tool.

```bash
for release in first-class-families-0.8.2.0 kind-apply-0.4.0.1 kind-generics-0.5.0.0; do
    curl --fail --location --remote-name "https://hackage-content.haskell.org/package/$release.tar.gz" || exit
done
sha256sum --check "$replay/sources.sha256" || exit
for archive in ./*.tar.gz; do tar -xzf "$archive" || exit; done

cat > cabal.project <<'CABAL'
packages:
  first-class-families-0.8.2.0
  kind-apply-0.4.0.1
  kind-generics-0.5.0.0
active-repositories: :none
CABAL

build=(cabal v2-build --offline --with-compiler="$ghc" --with-hc-pkg="$ghc_pkg")
targets=(lib:first-class-families lib:kind-apply lib:kind-generics)
python3 "$replay/apply.py" "$PWD" matchability || exit
"${build[@]}" --builddir=with-cusks "${targets[@]}" || exit

python3 "$replay/apply.py" "$PWD" cusks || exit
"${build[@]}" --builddir=without-cusks --ghc-options=-XNoCUSKs "${targets[@]}"
```

The first build inherits CUSKs from Haskell2010. Before applying the
second patch, `-XNoCUSKs` fails at `TyEqImpl` in `Fcf.Utils`. The
second build uses standalone signatures with CUSKs disabled. Both
builds use the compiler's existing language edition settings.

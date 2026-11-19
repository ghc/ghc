# Project a computed pair

The ordinary producer `ComputedPair.bump` returns a pair whose second
field raises an exception if evaluated. `ComputedPairProof` checks the
first field at compile time. Only the projection interface is required.
The ordinary producer and its runtime dependencies must be available
to the interpreter.

Use a fresh Hadrian build root so the library contains the generated
closure entries. This command builds the compiler and the matching
`ghc-internal` package with the default flavour.

```sh
hadrian/build --build-root=_build-readback --flavour=default --docs=none \
  stage2:exe:ghc-bin stage1:lib:ghc-internal
```

Run the example from an empty directory. Set `repo` to the repository's
absolute path.

```sh
src="$repo/docs/dependent-haskell/examples/closed-evaluation"
ghc="$repo/_build-readback/stage1/bin/ghc"
lib="$repo/_build-readback/stage1/lib"
cp "$src"/{ComputedPair,PairProjection,ComputedPairProof}.hs .
"$ghc" "-B$lib" -package-env - -O0 -c ComputedPair.hs PairProjection.hs
mkdir objects
mv PairProjection.o objects/
"$ghc" "-B$lib" -package-env - -O0 -c ComputedPairProof.hs
```

For a dynamic native compiler, use the configured interpreter way
and add `-dynamic` to both compilation commands if required. The
registered test uses the configured Template Haskell way. Its
mixed-interface object check uses `nm` and `objcopy` on Linux ELF.

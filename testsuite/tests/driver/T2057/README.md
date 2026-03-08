`T2057` checks that GHC stops after an interface-file error instead of
continuing into the linker.

The test constructs a stale package dependency on purpose.

The dependency tree is

   app/Main -> pkgB -> pkgA

where the two directories `pkgA1/` and `pkgA2/` are just two source trees
for the same package `pkgA`.

`pkgA1` defines a local type `T` and a function `f :: T -> T`.
`pkgB` builds against that package and records an unfolding `g = f` in `B.hi`.

After that, the Makefile updates the same package `pkgA` from `pkgA2/`, where
module `A` no longer exports `f`. When `Main` imports `B`, GHC has to load
`B.hi`, sees the stale reference to `f`, and must fail.

The golden [`T2057.stderr`](T2057.stderr) captures the fixed behaviour:
diagnose the missing declaration in the stale interface and then stop with
`Cannot continue after interface file error`. Any linker output would be a
regression.

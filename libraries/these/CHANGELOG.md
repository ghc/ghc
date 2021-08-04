# 1.1.1.1

- Workaround GCC-4 C-preprocessor bug

# 1.1.1

- These doesn't depend on `base-compat` anymore
- Add `NFData1/2`, `Hashable1/2`, `Eq1/2` ... instances

# 1.1

- Reverse dependency with `aeson`.
  - The `QuickCheck` instances are moved into `quickcheck-instances`
  - The `semigroupoids` instances are gone for now.

# 1.0.1

- add `partitionEithersNE :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)`

# 1

This is major package reogranisation. Old `these` were split into

- `these` providing only `These` type and some combinators
- `these-lens` providing *lens* combinators
- `semialign` providing `Semialign`, `Align`, `Zip`, `Unalign` and `Unzip` classes
- `semialign-indexed` providing `SemialignWithIndex` (`izipWith` and `ialignWith` members).
- `monad-chronicle` providing `ChronicleT` and `MonadChronicle`

Also noticeable change is `unalign :: f (These a b) -> (f a, f b)`.
For the old `f (These a b) -> (f (Maybe a), f (Maybe b))` use `unzipWith (unalign . Just)`.

- Many instances are added.
- Since annotations are removed for all but `these` package.

# 0.8.1

- Add `Semialign` `Tree`, `Tagged`, `(->) e`; `Align` `Compose` and `Proxy` instances
- Allow `semigroups-0.19` and `hashable-1.3`

# 0.8.0

- Split `align` and `alignWith` into own class: `Semialign`.
- `ialign` has default implementation
- Add `Semialign` `NonEmpty` and `Identity` instances
- Add `Swap` and `Assoc` instances (type classes from `assoc` package)
- Move optics into `Data.These.Lens` module,
  and and some combinators `Data.These.Combinators`.
  Also some combinators are renamed, so naming is now consistent.
  As the result `Data.These` has very minimal exports.
- Change type of `partitionThese` (nested pairs to triple)
- Add `partitionHereThere :: [These a b] -> ([a],[b])`

# 0.7.6

- Tigthen lower bounds
- Add dependency on `lens`
- Add `assoc`, `reassoc`, `swap` and `Swapped` instance
- Add since annotations for things added in 0.7.x
- Add `AlignWithKey ZipList` instance
- Add `Data.Align.Indexed` module.
- Add `Data.Functor.These` with `These1` data type.
- Add associativity law
- Add `toList` property to enforce "align"-feel.
- `Map` and `IntMap` `Align` instances implemented using merge combinators
  (when available)

# 0.7.5

- Add `Compose` and `(,)` `Crosswalk` instances
- Add `bitraverseThese`
- GHC-8.6 support

# 0.7.4

- `QuickCheck-2.10` support: `Arbitrary1/2` instances
- GHC-8.2 support

# 0.7.3

- Add `salign :: (Align f, Semigroup a) => f a -> f a -> f a`

# 0.7.2

- Support `aeson-1`: add `FromJSON1`, `FromJSON2` `ToJSON1`, and `ToJSON2` `These` instances.

# 0.7.1

- Add `AlignWithKey` in `Data.Align.Key` (added dependency `keys`)
- Add `These` instances for
    - `binary`: `Binary`
    - `aeson`: `FromJSON`, `ToJSON`
    - `QuickCheck`: `Arbitrary`, `CoArbitrary`, `Function`
    - `deepseq`: `NFData`

# 0.7

- Breaking change: Generalized `Monad`, `Applicative` instances of `These` and `Chronicle` to require only a `Semigroup` constraint
- More efficient `Align Seq` implementation
- Add `Crosswalk Seq` and `Vector` instances

# 0.6.2.1

- Support quickcheck-instances-0.3.12 (tests)

# 0.6.2.0

- Add support to bifunctors-5.1

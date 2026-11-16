# 1.4

- Reorder class hierarchy. `Unzip` is now at the bottom, right above `Functor`.
  There are two ways things may break:
  - You may need to add `Unzip` instance if you haven't one already, for example:
    `instance Unzip ((->) e) where unzip = unzipDefault` was added in this patch.
  - `Unzip f` doesn't imply whole hierarchy, so you may need to change `Unzip f` to `Zip f`
    in the constraints of some of your functions.
- Relax laws of Unalign allowing list-like instances.
  - Add `Unalign []` and `Unalign Vector` instances.

# 1.3.1

- Support GHC-8.6.5...GHC-9.10.1

# 1.3

- Depend on `bifunctor-classes-compat` instead of `bifunctors`
  See changelog note in `bifunctors-5.6`: https://hackage.haskell.org/package/bifunctors-5.6/changelog
  This is breaking change, but affects only GHC-8.0 and older users.
  In that case you should check various combinations of newer/older
  `bifunctors`, `these`, and `semialign` packages.

# 1.2.0.1

-  GHC-9.2 support

# 1.2

- Migrate `SemialignWithIndex` and `ZipWithIndex` to this package,
  using `FunctorWithIndex` from `indexed-traversable`.
- Add `RepeatWithIndex` type-class.
- Poly-kinded instances (notably `Tagged`)

# 1.1.0.1

- Drop `base-compat` dependency

# 1.1

- Split `Semialign` into `Semialign` and `Zip`.
- Rename old `Zip` into `Repeat`
- i.e. current main hierarchy is
- Remove `malign`, use `salign` or `alignWith mappend` where `Monoid` is necessary.
- Add `Option` instances

```haskell
instance Functor f => Semialign f where
    alignWith :: (These a b -> c) -> f a -> f b -> f c

instance Semialign f => Align f where
    nil :: f a

instance Semialign f => Zip f where
    zipWith :: (a -> b -> c) -> f a -> f b -> f c

instance Zip f => Repeat f where
    repeat :: a -> f a
```

This biased choice, that `Semialign` is a super-class of `Zip` is motivated by the fact that
- There's no `Semialign`-like class anywhere else, yet
- `Zip` and `Repeat` are `Apply` (from `semigroupoids`) and `Applicative` with slightly more laws. I
  If you need only `Repeat` class, and your type isn't `Aling`able, maybe using `Applicative` is enough?

# 1

Split out of `these` package.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
module T27424c where

-- Exact-printing regression test
-- Not every declaration there would pass renamer without errors
data G a where
  MkG1 :: (C a, D a, E a) => G a
  MkG2 :: (C a) => (D a, E a) => G a
  MkG3 :: C a => D a => E a => G a
  MkG4 :: (C a) => D a => (E a) => G a
  MkG5 :: C a => D a => a -> Int -> G a

-- Contexts on separate lines, aligned by the arrow
data A a where
  MkA1 :: C a
       => D a
       => E a
       => A a
  MkA2 :: forall a.
          (C a, D a) =>
          E a        =>
          A a
  MkA3 :: forall a
        . (C a)
       => (D a, E a)
       => a
       -> A a

-- Empty contexts
data E a where
  MkE1 :: () => E a
  MkE2 :: () => () => E a
  MkE3 :: () => C a => () => E a

-- Contexts mixed with foralls and parentheses (see #27423)
data F a where
  MkF1 :: forall a. C a => D a => F a
  MkF2 :: (forall a. C a => D a => F a)
  MkF3 :: forall a. (forall b. C a => D b => F (a, b))
  MkF4 :: forall. forall a. C a => (D a) => F a
  MkF5 :: ((forall a. (C a) => (D a) => a -> F a))
  MkF6 :: forall a -> C a => D a => F a

-- Record-style constructors with several contexts. GHC rejects these,
-- but they parse, so we check that the invalid code is exact-printed.
data R a where
  MkR1 :: C a => D a => { fld1 :: a } -> R a
  MkR2 :: forall a. (C a)
                 => (D a)
                 => { fld2 :: a } -> R a

-- Comments in and around the contexts
data K a where
  MkK1 :: -- comment before the first context
          C a => D a => K a
  MkK2 :: C a -- comment before the first arrow
       => D a
       => K a
  MkK3 :: C a => -- comment after the first arrow
          D a => K a
  MkK4 :: ( {- inside the parens -} C a) => ({- after opening paren -} D a {- before closing paren -}) => K a
  MkK5 :: C a => D a {- before the last arrow -} => {- after the last arrow -} K a

{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module T26225 where

-- Recall: ty1 is a subtype of ty2, written ty1 ⊑ ty2,
--         if we can use ty1 wherever ty2 is expected.
--         Can also read as "ty1 is more polymorphic than ty2".
-- Example: ∀ a. a -> a  ⊑  Int -> Int, meaning that we can pass
--          the identity function where one is expecting a function of type Int -> Int.

-- Int -> (∀ a. a -> a)  ⊑  α[tau]
-- Accepted by GHC.
ex0 :: ()
ex0 =
  let
    f :: Int -> (∀ a. a -> a)
    f _ = id
    g :: _α -> ()
    g _ = ()
  in g f

-- ((∀ a. a->a) -> Int) -> Bool  ⊑  α[tau]
-- Rejected by GHC up to and including 9.14.
ex1' :: ()
ex1' =
  let
    f :: ((∀ a. a -> a) -> Int) -> Bool
    f _ = False
    g :: _α -> ()
    g _ = ()
  in g f
    -- Couldn't match expected type ‘α’ with actual type ‘((∀ a. a -> a) -> Int) -> Bool’

-- ((∀ a. a->a) -> Int) -> Bool  ⊑  β[tau] Bool
-- Rejected by GHC up to and including 9.14.
ex2' :: ()
ex2' =
  let
    f :: ((∀ a. a -> a) -> Int) -> Bool
    f _ = False
    g :: _β Bool -> ()
    g _ = ()
  in g f
    -- Couldn't match expected type ‘β’ with actual type ‘(->) ((∀ a. a -> a) -> Int)’

-- ex3 ::  β[tau] Bool  ⊑  (∀ a. a->a) -> Bool
-- Rejected by GHC up to and including 9.14.
ex3 :: ()
ex3 =
  let
    f :: _β Bool
    f = undefined
    g :: ((∀ a. a -> a) -> Bool) -> ()
    g _ = ()
  in g f
    -- Couldn't match expected type ‘β’ with actual type ‘(->) (∀ a. a -> a)’

-- ex3' :: F Int Bool  ⊑  (∀ a. a->a) -> Bool, where F Int = (->) (Int -> Int)
-- Rejected by GHC up to and including 9.14.
ex3' :: ()
ex3' =
  let
    f :: F Int Bool
    f _ = False
    g :: ((∀ a. a -> a) -> Bool) -> ()
    g _ = ()
  in g f
    -- • Couldn't match type: Int -> Int
    --             with: ∀ a. a -> a
    --   Expected: (∀ a. a -> a) -> Bool
    --     Actual: F Int Bool
type family F a where { F Int = (->) (Int -> Int) }

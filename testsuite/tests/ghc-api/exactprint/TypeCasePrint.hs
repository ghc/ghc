{-# LANGUAGE DataKinds, ExperimentalDependentTypes, UnicodeSyntax #-}
module TypeCasePrint where

type F a = case a of
  True → Int  -- layout
  False -> Bool

type G a = case a of { True -> Int; {- comment -} False -> Bool; }

type H a = case a of { Bool -> Int; Ordering -> Char }
type I a = case a of { 'True -> Int; False -> Char }

type Nested d = case d of
  Just (Just field) → field  -- nested field
  Just Nothing -> Bool
  Nothing -> Char

type Open m = case m of { Just Bool -> Int; Just Ordering -> Char; Just _ -> (); Nothing -> () }

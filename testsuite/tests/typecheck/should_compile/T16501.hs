{-# Language GADTs #-} -- Only uses Existentials
module T16501 where

data T where
  Str :: Show s => { field :: s } -> T

val1 :: T
val1 = Str { field = True }

{-
val2 :: T
val2 = val1 { field = 'a' }
  • Record update for insufficiently polymorphic field: field :: s
    • In the expression: val1 {field = 'a'}
      In an equation for ‘val2’: val2 = val1 {field = 'a'}
-}

manualUpdate :: Show s => T -> s -> T
manualUpdate (Str _) s = Str s

val3 :: T
val3 = manualUpdate val1 'a'

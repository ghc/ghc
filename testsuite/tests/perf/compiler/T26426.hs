{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T26426 (
  someTypes
) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

type family Append (left :: [k]) (right :: [k]) :: [k] where
  Append '[] right = right
  Append (a : rest) right = a : Append rest right

type family TaggedTypes (tags :: [(Symbol, Type)]) :: [Type] where
  TaggedTypes '[] = '[]
  TaggedTypes ('(_, typ) : rest) = typ : TaggedTypes rest

data Types (types :: [(Symbol, Type)]) = Types

mkTypes :: forall sym val. val -> Types '[ '(sym, val) ]
mkTypes _ = Types

appendTypes ::
  -- This constraint is the one that causes the issue. If the next line is commented
  -- out, then this module compiles quickly
  Append (TaggedTypes left) (TaggedTypes right) ~ TaggedTypes (Append left right) =>
  Types left -> Types right -> Types (Append left right)
appendTypes _ _ = Types

someTypes ::
  Types
    [ '("01", Int)
    , '("02", Int)
    , '("03", Int)
    , '("04", Int)
    , '("05", Int)
    , '("06", Int)
    , '("07", Int)
    , '("08", Int)
    , '("09", Int)
    , '("10", Int)
    , '("11", Int)
    , '("12", Int)
    , '("13", Int)
    , '("14", Int)
    , '("15", Int)
    , '("16", Int)
    ]

someTypes =
  mkTypes @"01"  1 `appendTypes`
  mkTypes @"02"  2 `appendTypes`
  mkTypes @"03"  3 `appendTypes`
  mkTypes @"04"  4 `appendTypes`
  mkTypes @"05"  5 `appendTypes`
  mkTypes @"06"  6 `appendTypes`
  mkTypes @"07"  7 `appendTypes`
  mkTypes @"08"  8 `appendTypes`
  mkTypes @"09"  9 `appendTypes`
  mkTypes @"10" 10 `appendTypes`
  mkTypes @"11" 11 `appendTypes`
  mkTypes @"12" 12 `appendTypes`
  mkTypes @"13" 13 `appendTypes`
  mkTypes @"14" 14 `appendTypes`
  mkTypes @"15" 15 `appendTypes`
  mkTypes @"16" 16

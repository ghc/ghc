{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

-- -Wno-orphans is needed for things like:
-- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Base.String
    ( String
    , unsafeChr
    , ord
    , eqString
    ) where

-- ghc-prim
import GHC.Types (Char(..), Int(..), Bool(..))
import GHC.Classes (Eq(..), (&&))
import GHC.Prim (chr#, ord#)

-- | A 'String' is a list of characters.  String constants in Haskell are values
-- of type 'String'.
--
-- See "Data.List" for operations on lists.
type String = [Char]

unsafeChr :: Int -> Char
unsafeChr (I# i#) = C# (chr# i#)

-- | The 'Prelude.fromEnum' method restricted to the type 'Data.Char.Char'.
ord :: Char -> Int
ord (C# c#) = I# (ord# c#)

-- | This 'String' equality predicate is used when desugaring
-- pattern-matches against strings.
eqString :: String -> String -> Bool
eqString []       []       = True
eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
eqString _        _        = False

{-# RULES "eqString" (==) = eqString #-}
-- eqString also has a BuiltInRule in GHC.Core.Opt.ConstantFold:
--      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2)) = s1==s2


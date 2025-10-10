{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module T26480 where

import Data.Proxy
import GHC.TypeLits
import GHC.Records

import T26480_aux1 (R1)
import qualified T26480_aux2 as XXX (R2)

data S = MkS { fld_s :: Int }

data E where
  MkE :: { fld_e :: e } -> E

data Q = MkQ { fld_q :: forall a. a -> a }

data T = MkT { specificFieldName :: Int }

data G = MkG { xyzzywyzzydyzzy :: Int }

pattern P :: Int -> S
pattern P { patSynField } = MkS patSynField

-- Not a literal string
test1 :: forall (fld_s :: Symbol). Proxy fld_s -> S -> Int
test1 _ = getField @fld_s

-- Not a record type
test2 :: Int -> Int
test2 = getField @"int_fld"

-- Field out of scope: unqualified import
test3a :: R1 -> Int
test3a = getField @"f1"

-- Field out of scope: qualified import
test3b :: XXX.R2 -> Int
test3b = getField @"f2"

-- Existential record field
test4 :: E -> Int
test4 = getField @"fld_e"

-- Record field contains forall
test5 :: Q -> Bool -> Bool
test5 = getField @"fld_q"

-- Record field is misspelled
test6 :: T -> Int
test6 = getField @"specificFieldTame"

-- Record field is for a different type
test7 :: T -> Int
test7 = getField @"xyzzywyzzydyzzy"

-- Record field is misspelled and is for a different type
test8 :: T -> Int
test8 = getField @"xyzzywyzzyzyzzy"

-- Pattern synonym field
test9 :: S -> Int
test9 = getField @"patSynField"

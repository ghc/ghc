{-# LANGUAGE DataKinds, MagicHash, PartialTypeSignatures, TypeFamilies #-}
module DataToTagSolving where

import GHC.Exts
import GHC.Tuple
import Data.Void (absurd)
import Type.Reflection

tuple3ConstructorImported :: (a, b, c) -> Int#
tuple3ConstructorImported = dataToTag#

voidIsn'tImported :: _Void -> Int#
voidIsn'tImported x = case  absurd x  of
  _ -> dataToTag# x

data family Tricky a b
data instance Tricky Int  x    = TI x
data instance Tricky Bool Char = TBC1 | TBC2
data instance Tricky Char t = DataToTag t => TC t

worksWithDataInstance1 :: Tricky Int  x    -> Int#
worksWithDataInstance1 = dataToTag#

worksWithDataInstance2 :: Tricky Bool Char -> Int#
worksWithDataInstance2 = dataToTag#

worksWithLocalConstraints :: Typeable a => a -> Int#
worksWithLocalConstraints x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @(Maybe Int)
    = dataToTag# x
  | App fun arg <- typeOf x
    , Just HRefl <- fun `eqTypeRep` typeRep @(Tricky Char)
    = dataToTag# x +# case x of TC v -> dataToTag# v
  | otherwise = 15#

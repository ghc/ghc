{-# LANGUAGE RebindableSyntax, RankNTypes, TypeApplications, OverloadedStrings,
             OverloadedLists, TypeFamilies #-}

module Bug where

import qualified Prelude as P
import qualified GHC.Exts as P
import Data.List.NonEmpty ( NonEmpty )
import Data.Type.Equality ( type (~) )

fromInteger :: P.Integer -> forall a. P.Num a => a
fromInteger n = P.fromInteger n

shouldBeAnInt = 3 @P.Int

newtype RevString = RevString P.String
  deriving P.Show

instance P.IsString RevString where
  fromString str = RevString (P.reverse str)

fromString :: P.String -> forall a. P.IsString a => a
fromString str = P.fromString str

shouldBeARevString = "hello" @RevString

fromListN :: P.Int -> [elt] -> forall list. (P.IsList list, elt ~ P.Item list) => list
fromListN n l = P.fromListN n l

shouldBeANonEmpty = ['x', 'y', 'z'] @(NonEmpty P.Char)

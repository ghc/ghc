{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
module Codec.Picture.Metadata where

import Control.DeepSeq( NFData( .. ) )
import Data.Typeable( (:~:)( Refl ) )
import Data.Word( Word16 )

data ExifTag
  = TagPhotometricInterpretation
  | TagUnknown !Word16
  deriving Eq

data ExifData = ExifNone

data Keys a where
  Exif        :: !ExifTag -> Keys ExifData
  Unknown     :: !String -> Keys Value

data Value

data Elem k =
  forall a. (Show a, NFData a) => !(k a) :=> a

keyEq :: Keys a -> Keys b -> Maybe (a :~: b)
keyEq a b = case (a, b) of
  (Unknown v1, Unknown v2) | v1 == v2 -> Just Refl
  (Exif t1, Exif t2) | t1 == t2 -> Just Refl
  _ -> Nothing

newtype Metadatas = Metadatas { getMetadatas :: [Elem Keys] }

lookup :: Keys a -> Metadatas -> Maybe a
lookup k = go . getMetadatas where
  go [] = Nothing
  go ((k2 :=> v) : rest) = case keyEq k k2 of
    Nothing -> go rest
    Just Refl -> Just v

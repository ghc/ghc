{-# LANGUAGE OverloadedStrings #-}

module GHC.Utils.Indentable
  ( IndentedBuilder
  , Indentation
  , newline
  , nestBy
  , nest
  , indentTo
  , (<+>)
  , Indentable(..)
  )

where

import GHC.Prelude

import Data.ByteString.Builder
import Data.Monoid
import Data.String

type Indentation = Builder

class Indentable a where
  toIndentable :: a -> IndentedBuilder

newtype IndentedBuilder = IB (Indentation -> Builder)

newline :: IndentedBuilder
newline = IB $ \indent -> "\n" <> indent

instance Semigroup IndentedBuilder where
   IB f <> IB f' = IB $ \indent -> f indent <> f' indent

instance IsString IndentedBuilder where
   fromString s = IB $ const (fromString s)

instance Monoid IndentedBuilder where
   mempty = IB $ const mempty

nestBy :: Indentation -> IndentedBuilder -> IndentedBuilder
nestBy moreIndent (IB f) = IB (\indent -> f (indent <> moreIndent))

nest :: Int -> IndentedBuilder -> IndentedBuilder
nest k = nestBy $ fromString $ take k spaces

spaces :: String
spaces = ' ' : spaces

indentTo :: Indentation -> IndentedBuilder -> Builder
indentTo indentation (IB f) = f indentation

(<+>) :: IndentedBuilder -> IndentedBuilder -> IndentedBuilder
s <+> s' = s <> " " <> s'

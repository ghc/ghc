{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable, StandaloneDeriving #-}

-- |
-- Module      :  Documentation.Haddock.Types
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- Exposes documentation data types used for (some) of Haddock.
module Documentation.Haddock.Types where

import Data.Foldable
import Data.Traversable

instance Foldable Header where
  foldMap f (Header _ a) = f a

instance Traversable Header where
  traverse f (Header l a) = Header l `fmap` f a

data Hyperlink = Hyperlink
  { hyperlinkUrl   :: String
  , hyperlinkLabel :: Maybe String
  } deriving (Eq, Show)


data Picture = Picture
  { pictureUri   :: String
  , pictureTitle :: Maybe String
  } deriving (Eq, Show)

data Header id = Header
  { headerLevel :: Int
  , headerTitle :: id
  } deriving Functor

data Example = Example
  { exampleExpression :: String
  , exampleResult     :: [String]
  } deriving (Eq, Show)

data DocH mod id
  = DocEmpty
  | DocAppend (DocH mod id) (DocH mod id)
  | DocString String
  | DocParagraph (DocH mod id)
  | DocIdentifier id
  | DocIdentifierUnchecked mod
  | DocModule String
  | DocWarning (DocH mod id)
  | DocEmphasis (DocH mod id)
  | DocMonospaced (DocH mod id)
  | DocBold (DocH mod id)
  | DocUnorderedList [DocH mod id]
  | DocOrderedList [DocH mod id]
  | DocDefList [(DocH mod id, DocH mod id)]
  | DocCodeBlock (DocH mod id)
  | DocHyperlink Hyperlink
  | DocPic Picture
  | DocAName String
  | DocProperty String
  | DocExamples [Example]
  | DocHeader (Header (DocH mod id))
  deriving (Functor, Foldable, Traversable)

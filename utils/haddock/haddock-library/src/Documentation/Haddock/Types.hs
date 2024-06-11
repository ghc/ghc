{-# LANGUAGE DeriveTraversable #-}

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

import Control.Arrow ((***))
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable

-- | A @\@since@ declaration.
data MetaSince = MetaSince
  { sincePackage :: Maybe Package
  -- ^ optional package qualification
  , sinceVersion :: Version
  }
  deriving (Eq, Show)

-- | With the advent of 'Version', we may want to start attaching more
-- meta-data to comments. We make a structure for this ahead of time
-- so we don't have to gut half the core each time we want to add such
-- info.
data Meta = Meta
  { _metaSince :: Maybe MetaSince
  }
  deriving (Eq, Show)

data MetaDoc mod id = MetaDoc
  { _meta :: Meta
  , _doc :: DocH mod id
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor MetaDoc where
  bimap f g (MetaDoc m d) = MetaDoc m (bimap f g d)

instance Bifoldable MetaDoc where
  bifoldr f g z d = bifoldr f g z (_doc d)

instance Bitraversable MetaDoc where
  bitraverse f g (MetaDoc m d) = MetaDoc m <$> bitraverse f g d

overDoc :: (DocH a b -> DocH c d) -> MetaDoc a b -> MetaDoc c d
overDoc f d = d{_doc = f $ _doc d}

overDocF :: Functor f => (DocH a b -> f (DocH c d)) -> MetaDoc a b -> f (MetaDoc c d)
overDocF f d = (\x -> d{_doc = x}) <$> f (_doc d)

type Version = [Int]
type Package = String

data Hyperlink id = Hyperlink
  { hyperlinkUrl :: String
  , hyperlinkLabel :: Maybe id
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ModLink id = ModLink
  { modLinkName :: String
  , modLinkLabel :: Maybe id
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Picture = Picture
  { pictureUri :: String
  , pictureTitle :: Maybe String
  }
  deriving (Eq, Show)

data Header id = Header
  { headerLevel :: Int
  -- ^ between 1 and 6 inclusive
  , headerTitle :: id
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Example = Example
  { exampleExpression :: String
  , exampleResult :: [String]
  }
  deriving (Eq, Show)

data TableCell id = TableCell
  { tableCellColspan :: Int
  , tableCellRowspan :: Int
  , tableCellContents :: id
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype TableRow id = TableRow
  { tableRowCells :: [TableCell id]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Table id = Table
  { tableHeaderRows :: [TableRow id]
  , tableBodyRows :: [TableRow id]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Highlight = Highlight
  { highlightLanguage :: String
  , highlightContent  :: String
  }
  deriving (Eq, Show)

data DocH mod id
  = DocEmpty
  | DocAppend (DocH mod id) (DocH mod id)
  | DocString String
  | DocParagraph (DocH mod id)
  | DocIdentifier id
  | -- | A qualified identifier that couldn't be resolved.
    DocIdentifierUnchecked mod
  | -- | A link to a module, with an optional label.
    DocModule (ModLink (DocH mod id))
  | -- | This constructor has no counterpart in Haddock markup.
    DocWarning (DocH mod id)
  | DocEmphasis (DocH mod id)
  | DocMonospaced (DocH mod id)
  | DocBold (DocH mod id)
  | DocUnorderedList [DocH mod id]
  | DocOrderedList [(Int, DocH mod id)]
  | DocDefList [(DocH mod id, DocH mod id)]
  | DocCodeBlock (DocH mod id)
  | DocCodeBlockHighlight Highlight
  | DocHyperlink (Hyperlink (DocH mod id))
  | DocPic Picture
  | DocMathInline String
  | DocMathDisplay String
  | -- | A (HTML) anchor. It must not contain any spaces.
    DocAName String
  | DocProperty String
  | DocExamples [Example]
  | DocHeader (Header (DocH mod id))
  | DocTable (Table (DocH mod id))
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor DocH where
  bimap _ _ DocEmpty = DocEmpty
  bimap f g (DocAppend docA docB) = DocAppend (bimap f g docA) (bimap f g docB)
  bimap _ _ (DocString s) = DocString s
  bimap f g (DocParagraph doc) = DocParagraph (bimap f g doc)
  bimap _ g (DocIdentifier i) = DocIdentifier (g i)
  bimap f _ (DocIdentifierUnchecked m) = DocIdentifierUnchecked (f m)
  bimap f g (DocModule (ModLink m lbl)) = DocModule (ModLink m (fmap (bimap f g) lbl))
  bimap f g (DocWarning doc) = DocWarning (bimap f g doc)
  bimap f g (DocEmphasis doc) = DocEmphasis (bimap f g doc)
  bimap f g (DocMonospaced doc) = DocMonospaced (bimap f g doc)
  bimap f g (DocBold doc) = DocBold (bimap f g doc)
  bimap f g (DocUnorderedList docs) = DocUnorderedList (map (bimap f g) docs)
  bimap f g (DocOrderedList docs) = DocOrderedList (map (\(index, a) -> (index, bimap f g a)) docs)
  bimap f g (DocDefList docs) = DocDefList (map (bimap f g *** bimap f g) docs)
  bimap f g (DocCodeBlock doc) = DocCodeBlock (bimap f g doc)
  bimap _ _ (DocCodeBlockHighlight hl) = DocCodeBlockHighlight hl
  bimap f g (DocHyperlink (Hyperlink url lbl)) = DocHyperlink (Hyperlink url (fmap (bimap f g) lbl))
  bimap _ _ (DocPic picture) = DocPic picture
  bimap _ _ (DocMathInline s) = DocMathInline s
  bimap _ _ (DocMathDisplay s) = DocMathDisplay s
  bimap _ _ (DocAName s) = DocAName s
  bimap _ _ (DocProperty s) = DocProperty s
  bimap _ _ (DocExamples examples) = DocExamples examples
  bimap f g (DocHeader (Header level title)) = DocHeader (Header level (bimap f g title))
  bimap f g (DocTable (Table header body)) = DocTable (Table (map (fmap (bimap f g)) header) (map (fmap (bimap f g)) body))

instance Bifoldable DocH where
  bifoldr f g z (DocAppend docA docB) = bifoldr f g (bifoldr f g z docA) docB
  bifoldr f g z (DocParagraph doc) = bifoldr f g z doc
  bifoldr _ g z (DocIdentifier i) = g i z
  bifoldr f _ z (DocIdentifierUnchecked m) = f m z
  bifoldr f g z (DocWarning doc) = bifoldr f g z doc
  bifoldr f g z (DocEmphasis doc) = bifoldr f g z doc
  bifoldr f g z (DocMonospaced doc) = bifoldr f g z doc
  bifoldr f g z (DocBold doc) = bifoldr f g z doc
  bifoldr f g z (DocUnorderedList docs) = foldr (flip (bifoldr f g)) z docs
  bifoldr f g z (DocOrderedList docs) = foldr (flip (bifoldr f g)) z (map snd docs)
  bifoldr f g z (DocDefList docs) = foldr (\(l, r) acc -> bifoldr f g (bifoldr f g acc l) r) z docs
  bifoldr f g z (DocCodeBlock doc) = bifoldr f g z doc
  bifoldr f g z (DocHeader (Header _ title)) = bifoldr f g z title
  bifoldr f g z (DocTable (Table header body)) = foldr (\r acc -> foldr (flip (bifoldr f g)) acc r) (foldr (\r acc -> foldr (flip (bifoldr f g)) acc r) z body) header
  bifoldr _ _ z _ = z

instance Bitraversable DocH where
  bitraverse _ _ DocEmpty = pure DocEmpty
  bitraverse f g (DocAppend docA docB) = DocAppend <$> bitraverse f g docA <*> bitraverse f g docB
  bitraverse _ _ (DocString s) = pure (DocString s)
  bitraverse f g (DocParagraph doc) = DocParagraph <$> bitraverse f g doc
  bitraverse _ g (DocIdentifier i) = DocIdentifier <$> g i
  bitraverse f _ (DocIdentifierUnchecked m) = DocIdentifierUnchecked <$> f m
  bitraverse f g (DocModule (ModLink m lbl)) = DocModule <$> (ModLink m <$> traverse (bitraverse f g) lbl)
  bitraverse f g (DocWarning doc) = DocWarning <$> bitraverse f g doc
  bitraverse f g (DocEmphasis doc) = DocEmphasis <$> bitraverse f g doc
  bitraverse f g (DocMonospaced doc) = DocMonospaced <$> bitraverse f g doc
  bitraverse f g (DocBold doc) = DocBold <$> bitraverse f g doc
  bitraverse f g (DocUnorderedList docs) = DocUnorderedList <$> traverse (bitraverse f g) docs
  bitraverse f g (DocOrderedList docs) = DocOrderedList <$> traverseSnd (bitraverse f g) docs
    where
      traverseSnd f' = traverse (\(x, a) -> (\b -> (x, b)) <$> f' a)
  bitraverse f g (DocDefList docs) = DocDefList <$> traverse (bitraverse (bitraverse f g) (bitraverse f g)) docs
  bitraverse f g (DocCodeBlock doc) = DocCodeBlock <$> bitraverse f g doc
  bitraverse _ _ (DocCodeBlockHighlight doc) = pure (DocCodeBlockHighlight doc)
  bitraverse f g (DocHyperlink (Hyperlink url lbl)) = DocHyperlink <$> (Hyperlink url <$> traverse (bitraverse f g) lbl)
  bitraverse _ _ (DocPic picture) = pure (DocPic picture)
  bitraverse _ _ (DocMathInline s) = pure (DocMathInline s)
  bitraverse _ _ (DocMathDisplay s) = pure (DocMathDisplay s)
  bitraverse _ _ (DocAName s) = pure (DocAName s)
  bitraverse _ _ (DocProperty s) = pure (DocProperty s)
  bitraverse _ _ (DocExamples examples) = pure (DocExamples examples)
  bitraverse f g (DocHeader (Header level title)) = (DocHeader . Header level) <$> bitraverse f g title
  bitraverse f g (DocTable (Table header body)) = (\h b -> DocTable (Table h b)) <$> traverse (traverse (bitraverse f g)) header <*> traverse (traverse (bitraverse f g)) body

-- | The namespace qualification for an identifier.
data Namespace = Value | Type | None deriving (Eq, Ord, Enum, Show)

-- | Render the a namespace into the same format it was initially parsed.
renderNs :: Namespace -> String
renderNs Value = "v"
renderNs Type = "t"
renderNs None = ""

-- | 'DocMarkupH' is a set of instructions for marking up documentation.
-- In fact, it's really just a mapping from 'Doc' to some other
-- type [a], where [a] is usually the type of the output (HTML, say).
-- Use 'Documentation.Haddock.Markup.markup' to apply a 'DocMarkupH' to
-- a 'DocH'.
--
-- @since 1.4.5
data DocMarkupH mod id a = Markup
  { markupEmpty :: a
  , markupString :: String -> a
  , markupParagraph :: a -> a
  , markupAppend :: a -> a -> a
  , markupIdentifier :: id -> a
  , markupIdentifierUnchecked :: mod -> a
  , markupModule :: ModLink a -> a
  , markupWarning :: a -> a
  , markupEmphasis :: a -> a
  , markupBold :: a -> a
  , markupMonospaced :: a -> a
  , markupUnorderedList :: [a] -> a
  , markupOrderedList :: [(Int, a)] -> a
  , markupDefList :: [(a, a)] -> a
  , markupCodeBlock :: a -> a
  , markupCodeBlockHighlight :: Highlight -> a
  , markupHyperlink :: Hyperlink a -> a
  , markupAName :: String -> a
  , markupPic :: Picture -> a
  , markupMathInline :: String -> a
  , markupMathDisplay :: String -> a
  , markupProperty :: String -> a
  , markupExample :: [Example] -> a
  , markupHeader :: Header a -> a
  , markupTable :: Table a -> a
  }

--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module HaddockTypes (
  -- * Module interfaces
  NameEnv, Interface(..), ExportItem(..), ModuleMap,

  -- * User documentation strings
  DocString, GenDoc(..), Doc, ParsedDoc, DocMarkup(..),
  markup, mapIdent, 
  docAppend, docParagraph,
 ) where

import FiniteMap
import HsSyn

import Char (isSpace)

-- ---------------------------------------------------------------------------
-- Describing a module interface

type NameEnv   = FiniteMap HsName HsQName

data Interface 
  = Interface {
	iface_filename :: FilePath,
		-- ^ the filename that contains the source code for this module

	iface_env :: NameEnv,
		-- ^ environment mapping names to *original* names

	iface_exports :: [ExportItem],
		-- ^ the exports used to construct the documentation 

	iface_orig_exports :: [ExportItem],
		-- ^ the exports used to construct the documentation
		-- (with orig names, not import names)

	iface_decls :: FiniteMap HsName HsDecl,
		-- ^ decls from this module (only)
		-- restricted to only those bits exported.
		-- the map key is the "main name" of the decl.

	iface_name_docs :: FiniteMap HsName Doc,
		-- ^ maps names exported by this module to documentation.
		-- Includes not just "main names" but names of constructors,
		-- record fields, etc.

	iface_portability :: String,
	iface_stability   :: String,
	iface_maintainer  :: String,
		-- ^ information from the module header

	iface_doc	  :: Maybe Doc
		-- ^ documentation from the module header
  }

type DocString = String

data ExportItem 
  = ExportDecl
	HsDecl		-- a declaration

  | ExportGroup		-- a section heading
	Int		-- section level (1, 2, 3, ... )
	String		-- section "id" (for hyperlinks)
	Doc		-- section heading text

type ModuleMap = FiniteMap Module Interface

-- -----------------------------------------------------------------------------
-- Doc strings and formatting

data GenDoc id
  = DocEmpty 
  | DocAppend (GenDoc id) (GenDoc id)
  | DocString String
  | DocParagraph (GenDoc id)
  | DocIdentifier id
  | DocModule String
  | DocEmphasis (GenDoc id)
  | DocMonospaced (GenDoc id)
  | DocUnorderedList [GenDoc id]
  | DocOrderedList [GenDoc id]
  | DocCodeBlock (GenDoc id)
  | DocURL String

type Doc = GenDoc HsQName
type ParsedDoc = GenDoc String

-- | DocMarkup is a set of instructions for marking up documentation.
-- In fact, it's really just a mapping from 'GenDoc' to some other
-- type [a], where [a] is usually the type of the output (HTML, say).

data DocMarkup id a = Markup {
  markupEmpty         :: a,
  markupString        :: String -> a,
  markupParagraph     :: a -> a,
  markupAppend        :: a -> a -> a,
  markupIdentifier    :: id -> a,
  markupModule        :: String -> a,
  markupEmphasis      :: a -> a,
  markupMonospaced    :: a -> a,
  markupUnorderedList :: [a] -> a,
  markupOrderedList   :: [a] -> a,
  markupCodeBlock     :: a -> a,
  markupURL	      :: String -> a
  }

markup :: DocMarkup id a -> GenDoc id -> a
markup m DocEmpty		= markupEmpty m
markup m (DocAppend d1 d2)	= markupAppend m (markup m d1) (markup m d2)
markup m (DocString s)		= markupString m s
markup m (DocParagraph d)	= markupParagraph m (markup m d)
markup m (DocIdentifier i)	= markupIdentifier m i
markup m (DocModule mod)	= markupModule m mod
markup m (DocEmphasis d)	= markupEmphasis m (markup m d)
markup m (DocMonospaced d)	= markupMonospaced m (markup m d)
markup m (DocUnorderedList ds)	= markupUnorderedList m (map (markup m) ds)
markup m (DocOrderedList ds)	= markupOrderedList m (map (markup m) ds)
markup m (DocCodeBlock d)	= markupCodeBlock m (markup m d)
markup m (DocURL url)		= markupURL m url

-- | Since marking up is just a matter of mapping 'Doc' into some
-- other type, we can \'rename\' documentation by marking up 'Doc' into
-- the same thing, modifying only the identifiers embedded in it.
mapIdent f = Markup {
  markupEmpty         = DocEmpty,
  markupString        = DocString,
  markupParagraph     = DocParagraph,
  markupAppend        = DocAppend,
  markupIdentifier    = f,
  markupModule        = DocModule,
  markupEmphasis      = DocEmphasis,
  markupMonospaced    = DocMonospaced,
  markupUnorderedList = DocUnorderedList,
  markupOrderedList   = DocOrderedList,
  markupCodeBlock     = DocCodeBlock,
  markupURL	      = DocURL
  }

-- -----------------------------------------------------------------------------
-- ** Smart constructors

-- used to make parsing easier; we group the list items later
docAppend (DocUnorderedList ds1) (DocUnorderedList ds2) 
  = DocUnorderedList (ds1++ds2)
docAppend (DocUnorderedList ds1) (DocAppend (DocUnorderedList ds2) d)
  = DocAppend (DocUnorderedList (ds1++ds2)) d
docAppend (DocOrderedList ds1) (DocOrderedList ds2) 
  = DocOrderedList (ds1++ds2)
docAppend (DocOrderedList ds1) (DocAppend (DocOrderedList ds2) d)
  = DocAppend (DocOrderedList (ds1++ds2)) d
docAppend d1 d2 
  = DocAppend d1 d2

-- again to make parsing easier - we spot a paragraph whose only item
-- is a DocMonospaced and make it into a DocCodeBlock
docParagraph (DocMonospaced p)
  = DocCodeBlock p
docParagraph (DocAppend (DocString s1) (DocMonospaced p))
  | all isSpace s1
  = DocCodeBlock p
docParagraph (DocAppend (DocString s1)
		(DocAppend (DocMonospaced p) (DocString s2)))
  | all isSpace s1 && all isSpace s2
  = DocCodeBlock p
docParagraph p
  = DocParagraph p

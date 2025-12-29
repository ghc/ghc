{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Haskell.Syntax.Basic where

import Control.DeepSeq
import Data.Data (Data)
import Data.Eq
import Data.Ord
import Data.Bool
import Prelude

import GHC.Data.FastString (FastString)

{-
************************************************************************
*                                                                      *
Rules
*                                                                      *
************************************************************************
-}

type RuleName = FastString

{-
************************************************************************
*                                                                      *
\subsection[Top-level/local]{Top-level/not-top level flag}
*                                                                      *
************************************************************************
-}

data TopLevelFlag
  = TopLevel
  | NotTopLevel
  deriving Data

isTopLevel, isNotTopLevel :: TopLevelFlag -> Bool

isNotTopLevel NotTopLevel = True
isNotTopLevel TopLevel    = False

isTopLevel TopLevel     = True
isTopLevel NotTopLevel  = False

{-
************************************************************************
*                                                                      *
Boxity
*                                                                      *
************************************************************************
-}

data Boxity
  = Boxed
  | Unboxed
  deriving( Eq, Data )

isBoxed :: Boxity -> Bool
isBoxed Boxed   = True
isBoxed Unboxed = False

{-
************************************************************************
*                                                                      *
Counts and indices
*                                                                      *
************************************************************************
-}

-- | The width of an unboxed sum
type SumWidth = Int

-- | A *one-index* constructor tag
--
-- Type of the tags associated with each constructor possibility or superclass
-- selector
type ConTag = Int

{-
************************************************************************
*                                                                      *
Field Labels
*                                                                      *
************************************************************************
-}

-- | Field labels are just represented as strings;
-- they are not necessarily unique (even within a module)
newtype FieldLabelString = FieldLabelString { field_label:: FastString }
  deriving (Data, Eq, NFData)

{-
************************************************************************
*                                                                      *
Field Labels
*                                                                      *
************************************************************************
-}

-- | See Note [Roles] in GHC.Core.Coercion
--
-- Order of constructors matters: the Ord instance coincides with the *super*typing
-- relation on roles.
data Role = Nominal | Representational | Phantom
  deriving (Eq, Ord, Data)

{-
************************************************************************
*                                                                      *
Source Strictness and Unpackedness
*                                                                      *
************************************************************************
-}

-- | Source Strictness
--
-- What strictness annotation the user wrote
data SrcStrictness = SrcLazy -- ^ Lazy, ie '~'
                   | SrcStrict -- ^ Strict, ie '!'
                   | NoSrcStrict -- ^ no strictness annotation
     deriving (Eq, Data)

-- | Source Unpackedness
--
-- What unpackedness the user requested
data SrcUnpackedness = SrcUnpack -- ^ {-# UNPACK #-} specified
                     | SrcNoUnpack -- ^ {-# NOUNPACK #-} specified
                     | NoSrcUnpack -- ^ no unpack pragma
     deriving (Eq, Data)

{-
************************************************************************
*                                                                      *
Fixity
*                                                                      *
************************************************************************
-}

-- | Captures the fixity of declarations as they are parsed. This is not
-- necessarily the same as the fixity declaration, as the normal fixity may be
-- overridden using parens or backticks.
data LexicalFixity = Prefix | Infix deriving (Eq, Data)

data FixityDirection
   = InfixL
   | InfixR
   | InfixN
   deriving (Eq, Data)

instance NFData FixityDirection where
  rnf InfixL = ()
  rnf InfixR = ()
  rnf InfixN = ()

data Fixity = Fixity Int FixityDirection
  deriving (Eq, Data)

instance NFData Fixity where
  rnf (Fixity i d) = rnf i `seq` rnf d `seq` ()

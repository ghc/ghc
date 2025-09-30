{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Language.Haskell.Syntax.Basic where

import Data.Data (Data)
import Data.Eq
import Data.Ord
import Data.Bool
import Prelude

import Control.DeepSeq

import qualified Language.Haskell.Textual.Source as Source
import Language.Haskell.Textual.UTF8

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
newtype FieldLabelString = FieldLabelString { field_label :: TextUTF8 }
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

data OverlapMode  -- See Note [Rules for instance lookup] in GHC.Core.InstEnv
  = NoOverlap Source.CodeSnippet
                  -- See Note [Pragma source text]
    -- ^ This instance must not overlap another `NoOverlap` instance.
    -- However, it may be overlapped by `Overlapping` instances,
    -- and it may overlap `Overlappable` instances.


  | Overlappable Source.CodeSnippet
                  -- See Note [Pragma source text]
    -- ^ Silently ignore this instance if you find a
    -- more specific one that matches the constraint
    -- you are trying to resolve
    --
    -- Example: constraint (Foo [Int])
    --   instance                      Foo [Int]
    --   instance {-# OVERLAPPABLE #-} Foo [a]
    --
    -- Since the second instance has the Overlappable flag,
    -- the first instance will be chosen (otherwise
    -- its ambiguous which to choose)


  | Overlapping Source.CodeSnippet
                  -- See Note [Pragma source text]
    -- ^ Silently ignore any more general instances that may be
    --   used to solve the constraint.
    --
    -- Example: constraint (Foo [Int])
    --   instance {-# OVERLAPPING #-} Foo [Int]
    --   instance                     Foo [a]
    --
    -- Since the first instance has the Overlapping flag,
    -- the second---more general---instance will be ignored (otherwise
    -- it is ambiguous which to choose)

  | Overlaps Source.CodeSnippet
                  -- See Note [Pragma source text]
    -- ^ Equivalent to having both `Overlapping` and `Overlappable` flags.

  | Incoherent Source.CodeSnippet
                  -- See Note [Pragma source text]
    -- ^ Behave like Overlappable and Overlapping, and in addition pick
    -- an arbitrary one if there are multiple matching candidates, and
    -- don't worry about later instantiation
    --
    -- Example: constraint (Foo [b])
    -- instance {-# INCOHERENT -} Foo [Int]
    -- instance                   Foo [a]
    -- Without the Incoherent flag, we'd complain that
    -- instantiating 'b' would change which instance
    -- was chosen. See also Note [Incoherent instances] in "GHC.Core.InstEnv"

  | NonCanonical Source.CodeSnippet
    -- ^ Behave like Incoherent, but the instance choice is observable
    -- by the program behaviour. See Note [Coherence and specialisation: overview].
    --
    -- We don't have surface syntax for the distinction between
    -- Incoherent and NonCanonical instances; instead, the flag
    -- `-f{no-}specialise-incoherents` (on by default) controls
    -- whether `INCOHERENT` instances are regarded as Incoherent or
    -- NonCanonical.

  deriving (Eq, Data)

instance NFData OverlapMode where
  rnf (NoOverlap s) = rnf s
  rnf (Overlappable s) = rnf s
  rnf (Overlapping s) = rnf s
  rnf (Overlaps s) = rnf s
  rnf (Incoherent s) = rnf s
  rnf (NonCanonical s) = rnf s

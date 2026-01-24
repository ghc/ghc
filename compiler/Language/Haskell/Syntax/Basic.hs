{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Eq XOverlapMode, NFData OverlapMode

module Language.Haskell.Syntax.Basic where

import Data.Data (Data)
import Data.Eq
import Data.Ord
import Data.Bool
import Prelude

import GHC.Data.FastString (FastString)
import Language.Haskell.Syntax.Extension
import Control.DeepSeq

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

data OverlapMode pass -- See Note [Rules for instance lookup] in GHC.Core.InstEnv
  = NoOverlap (XOverlapMode pass)
                  -- See Note [Pragma source text]
    -- ^ This instance must not overlap another `NoOverlap` instance.
    -- However, it may be overlapped by `Overlapping` instances,
    -- and it may overlap `Overlappable` instances.


  | Overlappable (XOverlapMode pass)
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

  | Overlapping (XOverlapMode pass)
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

  | Overlaps (XOverlapMode pass)
                  -- See Note [Pragma source text]
    -- ^ Equivalent to having both `Overlapping` and `Overlappable` flags.

  | Incoherent (XOverlapMode pass)
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

  | NonCanonical (XOverlapMode pass)
    -- ^ Behave like Incoherent, but the instance choice is observable
    -- by the program behaviour. See Note [Coherence and specialisation: overview].
    --
    -- We don't have surface syntax for the distinction between
    -- Incoherent and NonCanonical instances; instead, the flag
    -- `-f{no-}specialise-incoherents` (on by default) controls
    -- whether `INCOHERENT` instances are regarded as Incoherent or
    -- NonCanonical.

  | XOverlapMode !(XXOverlapMode pass)
    -- ^ The /Trees That Grow/ extension point constructor.

deriving instance ( Eq (XOverlapMode pass)
                  , Eq (XXOverlapMode pass)) => Eq (OverlapMode pass)

instance ( NFData (XOverlapMode pass)
         , XXOverlapMode pass ~ DataConCantHappen) => NFData (OverlapMode pass) where
  rnf = \case
    NoOverlap    s -> rnf s
    Overlappable s -> rnf s
    Overlapping  s -> rnf s
    Overlaps     s -> rnf s
    Incoherent   s -> rnf s
    NonCanonical s -> rnf s

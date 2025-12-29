{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Eq XOverlapMode, NFData OverlapMode

{- |
Data-type describing the overlap annotations for instances.
-}
module Language.Haskell.Syntax.Decls.Overlap where

import Control.DeepSeq
import Data.Eq
import Prelude

import Language.Haskell.Syntax.Extension

-- | The status of overlapping instances /(including no overlap)/ for a type.
data OverlapMode pass -- See Note [Rules for instance lookup] in GHC.Core.InstEnv
  = NoOverlap (XOverlapMode pass)
    -- ^ This instance must not overlap another `NoOverlap` instance.
    -- However, it may be overlapped by `Overlapping` instances,
    -- and it may overlap `Overlappable` instances.


  | Overlappable (XOverlapMode pass)
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
    -- ^ Equivalent to having both `Overlapping` and `Overlappable` flags.

  | Incoherent (XOverlapMode pass)
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
                  , Eq (XXOverlapMode pass)
                  ) => Eq (OverlapMode pass)

instance ( NFData (XOverlapMode pass)
         , NFData (XXOverlapMode pass)
         ) => NFData (OverlapMode pass) where
  rnf = \case
    NoOverlap    s -> rnf s
    Overlappable s -> rnf s
    Overlapping  s -> rnf s
    Overlaps     s -> rnf s
    Incoherent   s -> rnf s
    NonCanonical s -> rnf s
    XOverlapMode s -> rnf s


hasIncoherentFlag :: OverlapMode p -> Bool
hasIncoherentFlag mode =
  case mode of
    Incoherent   _ -> True
    NonCanonical _ -> True
    _              -> False

hasOverlappableFlag :: OverlapMode p -> Bool
hasOverlappableFlag mode =
  case mode of
    Overlappable _ -> True
    Overlaps     _ -> True
    Incoherent   _ -> True
    NonCanonical _ -> True
    _              -> False

hasOverlappingFlag :: OverlapMode p -> Bool
hasOverlappingFlag mode =
  case mode of
    Overlapping  _ -> True
    Overlaps     _ -> True
    Incoherent   _ -> True
    NonCanonical _ -> True
    _              -> False

hasNonCanonicalFlag :: OverlapMode p -> Bool
hasNonCanonicalFlag = \case
    NonCanonical{} -> True
    _              -> False

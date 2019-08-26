{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Var.Open
  -- * TyVar's
  ( VarBndr (..)
  , binderVar
  , binderVars
  , binderArgFlag

  -- * ArgFlags
  , ArgFlag(..)
  , isVisibleArgFlag
  , isInvisibleArgFlag
  , sameVis
  , AnonArgFlag(..)
  , ForallVisFlag(..)
  , argToForallVisFlag

  ) where

import GhcPrelude

import Binary
import Outputable

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import qualified Data.Semigroup as S

{- *********************************************************************
*                                                                      *
*                   ArgFlag
*                                                                      *
********************************************************************* -}

-- | Argument Flag
--
-- Is something required to appear in source Haskell ('Required'),
-- permitted by request ('Specified') (visible type application), or
-- prohibited entirely from appearing in source Haskell ('Inferred')?
-- See Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in TyCoRep
data ArgFlag = Inferred | Specified | Required
  deriving (Eq, Ord, Data)
  -- (<) on ArgFlag means "is less visible than"

-- | Does this 'ArgFlag' classify an argument that is written in Haskell?
isVisibleArgFlag :: ArgFlag -> Bool
isVisibleArgFlag Required = True
isVisibleArgFlag _        = False

-- | Does this 'ArgFlag' classify an argument that is not written in Haskell?
isInvisibleArgFlag :: ArgFlag -> Bool
isInvisibleArgFlag = not . isVisibleArgFlag

-- | Do these denote the same level of visibility? 'Required'
-- arguments are visible, others are not. So this function
-- equates 'Specified' and 'Inferred'. Used for printing.
sameVis :: ArgFlag -> ArgFlag -> Bool
sameVis Required Required = True
sameVis Required _        = False
sameVis _        Required = False
sameVis _        _        = True

instance Outputable ArgFlag where
  ppr Required  = text "[req]"
  ppr Specified = text "[spec]"
  ppr Inferred  = text "[infrd]"

instance Binary ArgFlag where
  put_ bh Required  = putByte bh 0
  put_ bh Specified = putByte bh 1
  put_ bh Inferred  = putByte bh 2

  get bh = do
    h <- getByte bh
    case h of
      0 -> return Required
      1 -> return Specified
      _ -> return Inferred

-- | The non-dependent version of 'ArgFlag'.

-- Appears here partly so that it's together with its friend ArgFlag,
-- but also because it is used in IfaceType, rather early in the
-- compilation chain
-- See Note [AnonArgFlag vs. ForallVisFlag]
data AnonArgFlag
  = VisArg    -- ^ Used for @(->)@: an ordinary non-dependent arrow.
              --   The argument is visible in source code.
  | InvisArg  -- ^ Used for @(=>)@: a non-dependent predicate arrow.
              --   The argument is invisible in source code.
  deriving (Eq, Ord, Data)

instance Outputable AnonArgFlag where
  ppr VisArg   = text "[vis]"
  ppr InvisArg = text "[invis]"

instance Binary AnonArgFlag where
  put_ bh VisArg   = putByte bh 0
  put_ bh InvisArg = putByte bh 1

  get bh = do
    h <- getByte bh
    case h of
      0 -> return VisArg
      _ -> return InvisArg

-- | Is a @forall@ invisible (e.g., @forall a b. {...}@, with a dot) or visible
-- (e.g., @forall a b -> {...}@, with an arrow)?

-- See Note [AnonArgFlag vs. ForallVisFlag]
data ForallVisFlag
  = ForallVis   -- ^ A visible @forall@ (with an arrow)
  | ForallInvis -- ^ An invisible @forall@ (with a dot)
  deriving (Eq, Ord, Data)

instance Outputable ForallVisFlag where
  ppr f = text $ case f of
                   ForallVis   -> "ForallVis"
                   ForallInvis -> "ForallInvis"

-- | Convert an 'ArgFlag' to its corresponding 'ForallVisFlag'.
argToForallVisFlag :: ArgFlag -> ForallVisFlag
argToForallVisFlag Required  = ForallVis
argToForallVisFlag Specified = ForallInvis
argToForallVisFlag Inferred  = ForallInvis

{-
Note [AnonArgFlag vs. ForallVisFlag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The AnonArgFlag and ForallVisFlag data types are quite similar at a first
glance:

  data AnonArgFlag   = VisArg    | InvisArg
  data ForallVisFlag = ForallVis | ForallInvis

Both data types keep track of visibility of some sort. AnonArgFlag tracks
whether a FunTy has a visible argument (->) or an invisible predicate argument
(=>). ForallVisFlag tracks whether a `forall` quantifier is visible
(forall a -> {...}) or invisible (forall a. {...}).

Given their similarities, it's tempting to want to combine these two data types
into one, but they actually represent distinct concepts. AnonArgFlag reflects a
property of *Core* types, whereas ForallVisFlag reflects a property of the GHC
AST. In other words, AnonArgFlag is all about internals, whereas ForallVisFlag
is all about surface syntax. Therefore, they are kept as separate data types.
-}

{- *********************************************************************
*                                                                      *
*                   VarBndr
*                                                                      *
********************************************************************* -}

-- | Variable Binder
--
-- VarBndr is polymorphic in both var and visibility fields.
-- Currently there are six different uses of 'VarBndr':
--   * Var.TyVarBinder   = VarBndr TyVar ArgFlag
--   * Var.TyCoVarBinder = VarBndr TyCoVar ArgFlag
--   * TyCon.TyConBinder     = VarBndr TyVar TyConBndrVis
--   * TyCon.TyConTyCoBinder = VarBndr TyCoVar TyConBndrVis
--   * IfaceType.IfaceForAllBndr  = VarBndr IfaceBndr ArgFlag
--   * IfaceType.IfaceTyConBinder = VarBndr IfaceBndr TyConBndrVis
data VarBndr var argf = Bndr var argf
  deriving ( Eq, Data
           , Functor, Foldable, Traversable
           )

binderVar :: VarBndr tv argf -> tv
binderVar (Bndr v _) = v

binderVars :: [VarBndr tv argf] -> [tv]
binderVars tvbs = map binderVar tvbs

binderArgFlag :: VarBndr tv argf -> argf
binderArgFlag (Bndr _ argf) = argf

instance Bifunctor VarBndr where
  bimap f g (Bndr v a) = Bndr (f v) (g a)

instance Bifoldable VarBndr where
  bifoldMap f g (Bndr v a) = f v S.<> g a

instance Bitraversable VarBndr where
  bitraverse f g (Bndr v a) = Bndr <$> f v <*> g a

instance (Binary tv, Binary vis) => Binary (VarBndr tv vis) where
  put_ bh (Bndr tv vis) = do { put_ bh tv; put_ bh vis }

  get bh = do { tv <- get bh; vis <- get bh; return (Bndr tv vis) }

instance Outputable tv => Outputable (VarBndr tv ArgFlag) where
  ppr (Bndr v Required)  = ppr v
  ppr (Bndr v Specified) = char '@' <> ppr v
  ppr (Bndr v Inferred)  = braces (ppr v)

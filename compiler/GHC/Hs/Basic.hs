{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable, Binary
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Fixity
module GHC.Hs.Basic
   ( module Language.Haskell.Syntax.Basic,
   TyConFlavour(..), TypeOrData(..), tyConFlavourAssoc_maybe
   ) where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Utils.Binary

import Data.Data (Data)
import Control.DeepSeq ( NFData(..) )

import Language.Haskell.Syntax.Basic

instance Outputable LexicalFixity where
  ppr Prefix = text "Prefix"
  ppr Infix  = text "Infix"

instance Outputable FixityDirection where
    ppr InfixL = text "infixl"
    ppr InfixR = text "infixr"
    ppr InfixN = text "infix"

instance Outputable Fixity where
    ppr (Fixity prec dir) = hcat [ppr dir, space, int prec]


instance Binary Fixity where
    put_ bh (Fixity aa ab) = do
            put_ bh aa
            put_ bh ab
    get bh = do
          aa <- get bh
          ab <- get bh
          return (Fixity aa ab)

------------------------

instance Binary FixityDirection where
    put_ bh InfixL =
            putByte bh 0
    put_ bh InfixR =
            putByte bh 1
    put_ bh InfixN =
            putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> return InfixL
              1 -> return InfixR
              _ -> return InfixN


{- *********************************************************************
*                                                                      *
TyConFlavour
*                                                                      *
********************************************************************* -}
-- you might wonder why these definitions are here, well really its just
-- because we need Boxity which comes from L.H.S.Basic, but we can't import
-- to GHC.Types.Basic because then we cycle (for now).

-- | Paints a picture of what a 'TyCon' represents, in broad strokes.
-- This is used towards more informative error messages.
data TyConFlavour tc
  = ClassFlavour
  | TupleFlavour Boxity
  | SumFlavour
  | DataTypeFlavour
  | NewtypeFlavour
  | AbstractTypeFlavour
  | OpenFamilyFlavour TypeOrData (Maybe tc) -- Just tc <=> (tc == associated class)
  | ClosedTypeFamilyFlavour
  | TypeSynonymFlavour
  | BuiltInTypeFlavour -- ^ e.g., the @(->)@ 'TyCon'.
  | PromotedDataConFlavour
  deriving (Eq, Data, Functor)


-- | Whether something is a type or a data declaration,
-- e.g. a type family or a data family.
data TypeOrData
  = IAmData
  | IAmType
  deriving (Eq, Data)

-- | Get the enclosing class TyCon (if there is one) for the given TyConFlavour
tyConFlavourAssoc_maybe :: TyConFlavour tc -> Maybe tc
tyConFlavourAssoc_maybe (OpenFamilyFlavour _ mb_parent) = mb_parent
tyConFlavourAssoc_maybe _                               = Nothing



instance Outputable (TyConFlavour tc) where
  ppr = text . go
    where
      go ClassFlavour = "class"
      go (TupleFlavour boxed) | isBoxed boxed = "tuple"
                              | otherwise     = "unboxed tuple"
      go SumFlavour              = "unboxed sum"
      go DataTypeFlavour         = "data type"
      go NewtypeFlavour          = "newtype"
      go AbstractTypeFlavour     = "abstract type"
      go (OpenFamilyFlavour type_or_data mb_par)
        = assoc ++ t_or_d ++ " family"
        where
          assoc = case mb_par of Just _ -> "associated " ; Nothing -> ""
          t_or_d = case type_or_data of { IAmType -> "type"; IAmData -> "data" }
      go ClosedTypeFamilyFlavour = "type family"
      go TypeSynonymFlavour      = "type synonym"
      go BuiltInTypeFlavour      = "built-in type"
      go PromotedDataConFlavour  = "promoted data constructor"

instance NFData tc => NFData (TyConFlavour tc) where
  rnf ClassFlavour = ()
  rnf (TupleFlavour !_) = ()
  rnf SumFlavour = ()
  rnf DataTypeFlavour = ()
  rnf NewtypeFlavour = ()
  rnf AbstractTypeFlavour = ()
  rnf (OpenFamilyFlavour !_ mb_tc) = rnf mb_tc
  rnf ClosedTypeFamilyFlavour = ()
  rnf TypeSynonymFlavour = ()
  rnf BuiltInTypeFlavour = ()
  rnf PromotedDataConFlavour = ()

instance Outputable TypeOrData where
  ppr IAmData = text "data"
  ppr IAmType = text "type"

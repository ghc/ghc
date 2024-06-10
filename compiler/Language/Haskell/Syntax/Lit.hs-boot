{-# LANGUAGE UndecidableInstances #-}
module Language.Haskell.Syntax.Lit where

import GHC.Data.FastString
import Language.Haskell.Syntax.Extension
import Prelude

data IntegralLit pass = IL
   { il_text  :: XIntegralLit pass
   , il_neg   :: Bool
   , il_value :: Integer
   }

instance Eq (IntegralLit pass)
instance Ord (IntegralLit pass)

data FractionalLit pass = FL
    { fl_text :: XFractionalLit pass
    , fl_neg :: Bool
    , fl_signi :: Rational
    , fl_exp :: Integer
    , fl_exp_base :: FractionalExponentBase
    }

instance Eq (FractionalLit pass)
instance Ord (FractionalLit pass)

data FractionalExponentBase
  = Base2
  | Base10

instance Eq FractionalExponentBase
instance Ord FractionalExponentBase

data StringLit pass = SL
  { sl_st :: XStringLit pass,

    sl_fs :: FastString
  }
instance Eq (StringLit pass)
instance Ord (StringLit pass)

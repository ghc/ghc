{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.Utils.Outputable.Instances where

import GHC.Prelude
import GHC.Types.SourceText (SourceText(..), pprWithSourceText)
import GHC.Types.SrcLoc (NoCommentsLocation)
import GHC.Utils.Outputable
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Lit

instance (XIntegralLit pass ~ SourceText) => Outputable (IntegralLit pass) where
  ppr (IL (SourceText src) _ _) = ftext src
  ppr (IL NoSourceText _ value) = text (show value)

instance (XFractionalLit pass ~ SourceText)
  => Outputable (FractionalLit pass) where
  ppr (fl@(FL {})) =
    pprWithSourceText (fl_text fl) $
      rational $ mkRationalWithExponentBase (fl_signi fl) (fl_exp fl) (fl_exp_base fl)

instance (XStringLit pass ~ (SourceText, Maybe NoCommentsLocation))
  => Outputable (StringLit pass) where
  ppr sl = pprWithSourceText (fst (sl_st sl)) (doubleQuotes $ ftext $ sl_fs sl)

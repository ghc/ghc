module TH_spliceDecl3_Lib
where

import Language.Haskell.THSyntax

rename' :: Dec -> Q [Dec]
rename' (DataDec ctxt tyName tyvars cons derivs) =
  return [DataDec ctxt (stripMod tyName ++ "'") tyvars (map renameCons cons) derivs]
  where
    renameCons (NormalCon conName tys) = NormalCon (stripMod conName ++ "'") tys
    --
    stripMod = tail . snd . break (== ':')

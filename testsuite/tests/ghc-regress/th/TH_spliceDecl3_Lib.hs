module TH_spliceDecl3_Lib
where

import Language.Haskell.THSyntax

rename' :: Dec -> Q [Dec]
rename' (Data tyName tyvars cons derivs) =
  return [Data (stripMod tyName ++ "'") tyvars (map renameCons cons) derivs]
  where
    renameCons (Constr conName tys) = Constr (stripMod conName ++ "'") tys
    --
    stripMod = tail . snd . break (== ':')

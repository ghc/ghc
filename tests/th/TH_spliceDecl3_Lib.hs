module TH_spliceDecl3_Lib
where

import Language.Haskell.TH

rename' :: Dec -> Q [Dec]
rename' (DataD ctxt tyName tyvars ksig cons derivs) =
  return [DataD ctxt (stripMod tyName) tyvars ksig
          (map renameCons cons) derivs]
  where
    renameCons (NormalC conName tys) = NormalC (stripMod conName) tys
    --
    stripMod v = mkName (nameBase v ++ "'")

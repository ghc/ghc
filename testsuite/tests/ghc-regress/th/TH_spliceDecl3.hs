-- test splicing of reified and renamed data declarations

module TH_spliceDecl3
where

import Language.Haskell.THSyntax

data T = C

$(rename (reifyDecl T))

rename :: Dec -> Q [Dec]
rename (Data tyName tyvars cons derivs) =
  Data (tyName ++ "'") tyvars (renameCons cons) derivs
  where
    renameCons (Constr conName tys) = Constr (conName ++ "'") tys



module TH_genExLib where

import Language.Haskell.TH

genAny :: Q Info -> Q [Dec]
genAny decl = do { d <- decl
		 ; case d of
		    ClassI (ClassD _ name _ _ decls) _ -> return [genAnyClass name decls]
		    _ -> error "genAny can be applied to classes only"
	}

genAnyClass :: Name -> [Dec] -> Dec
genAnyClass name decls
  = DataD [] anyName [] [constructor] []
  where
    anyName = mkName ("Any" ++ nameBase name ++ "1111")
    constructor = ForallC [PlainTV var_a] [AppT (ConT name) (VarT var_a)] $
		  NormalC anyName [(NotStrict, VarT var_a)]
    var_a = mkName "a"

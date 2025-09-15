module T24944 where

import T24944a

data DataCon = DC TyCon

data AltCon = DataAlt DataCon | LitAlt

data GenStgAlt pass = GenStgAlt
  { alt_con :: !AltCon
  }

data Type = TyVarTy | FunTy | TyConApp TyCon

mkStgAltTypeFromStgAlts :: Type -> [GenStgAlt Int] -> Maybe TyCon
mkStgAltTypeFromStgAlts bndr_ty alts
  = let may = case bndr_ty of
                TyConApp tc -> Just tc
                FunTy -> Just myTyCon
                TyVarTy -> Nothing
    in case may of
          Just (TyCon { tyConDetails = AlgTyCon True })
             -> case alt_con <$> alts of
                  DataAlt (DC con) : _ -> Just con
                  _ -> Nothing
          _ -> Nothing

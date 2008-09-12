module Language.Core.CoreUtils where

import Language.Core.Core
import Language.Core.Utils

import Data.Generics
import Data.List

splitDataConApp_maybe :: Exp -> Maybe (Qual Dcon, [Ty], [Exp])
splitDataConApp_maybe (Dcon d) = Just (d, [], [])
splitDataConApp_maybe (Appt rator t) = 
   case splitDataConApp_maybe rator of
     Just (r, ts, rs) -> Just (r, ts ++ [t], rs)
     Nothing          -> Nothing
splitDataConApp_maybe (App rator rand) =
  case splitDataConApp_maybe rator of
    Just (r, ts, rs) -> Just (r, ts, rs++[rand])
    Nothing -> Nothing
splitDataConApp_maybe _ = Nothing

splitApp :: Exp -> (Exp, [Exp])
splitApp (Appt rator _) = splitApp rator
splitApp (App rator rand) =
  case splitApp rator of
    (r, rs) -> (r, rs++[rand])
splitApp e = (e, [])

splitAppIgnoreCasts :: Exp -> (Exp, [Exp])
splitAppIgnoreCasts (Appt rator _) = splitApp rator
splitAppIgnoreCasts (App (Cast rator _) rand) = splitApp (App rator rand)
splitAppIgnoreCasts (App rator rand) =
  case splitApp rator of
    (r, rs) -> (r, rs++[rand])
splitAppIgnoreCasts e = (e, [])

splitFunTy_maybe :: Ty -> Maybe ([Ty], Ty)
splitFunTy_maybe (Tforall _ t) = splitFunTy_maybe t
splitFunTy_maybe t = 
  case splitFunTy2_maybe t of
    Just (rator, rand) -> case splitFunTy_maybe rand of
                            Just (r,s) -> Just (rator:r, s)
                            Nothing -> Just ([rator], rand)
    Nothing -> Nothing

splitFunTy2_maybe :: Ty -> Maybe (Ty,Ty)
splitFunTy2_maybe (Tapp (Tapp (Tcon c) t) u) | c == tcArrow = Just (t, u)
splitFunTy2_maybe _ = Nothing

vdefNamesQ :: [Vdef] -> [Qual Var]
vdefNamesQ = map (\ (Vdef (v,_,_)) -> v)

vdefNames :: [Vdef] -> [Var]
vdefNames = snd . unzip . vdefNamesQ

vdefTys :: [Vdef] -> [Ty]
vdefTys = map (\ (Vdef (_,t,_)) -> t)

vdefgNames :: Vdefg -> [Var]
vdefgNames (Rec vds) = map (\ (Vdef ((_,v),_,_)) -> v) vds
vdefgNames (Nonrec (Vdef ((_,v),_,_))) = [v]
vdefgTys :: Vdefg -> [Ty]
vdefgTys (Rec vds) = map (\ (Vdef (_,t,_)) -> t) vds
vdefgTys (Nonrec (Vdef (_,t,_))) = [t]

vbNames :: [Vbind] -> [Var]
vbNames = fst . unzip

-- assumes v is not bound in e
substIn :: Data a => Var -> Var -> a -> a
substIn v newV = everywhereExcept (mkT frob)
  where frob (Var (Nothing,v1)) | v == v1   = Var (Nothing,newV)
        frob e                              = e

substVars :: Data a => [Var] -> [Var] -> a -> a
substVars oldVars newVars e = foldl' (\ e1 (old,new) -> substIn old new e1) 
  e (zip oldVars newVars)


tdefNames :: [Tdef] -> [Qual Var]
tdefNames = concatMap doOne
  where doOne (Data qtc _ cds) = qtc:(concatMap doCdef cds)
        doOne (Newtype qtc qtc1 _ _) = [qtc, qtc1]
        doCdef (Constr qdc _ _) = [qdc]


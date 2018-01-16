module Substitution   (Sub, applySub, lookupSub, emptySub, extendSub,
                       makeSub, thenSub, domSub, unifySub)
                      where

import Type           (TVarId, TConId, MonoType (TVar, TCon), freeTVarMono)
import FiniteMap      (FM, emptyFM, lookupElseFM, makeFM, extendFM,
                       thenFM, mapFM, domFM, ranFM)
import MaybeM         (Maybe, thenM, returnM, failM, guardM)
data  Sub  =  MkSub (FM TVarId MonoType)
rep                           ::  Sub -> FM TVarId MonoType
rep (MkSub f)                 =   f
applySub                      ::  Sub -> MonoType -> MonoType
applySub s (TVar x)           =   lookupSub s x
applySub s (TCon k ts)        =   TCon k (map (applySub s) ts)
lookupSub                     ::  Sub -> TVarId -> MonoType
lookupSub s x                 =   lookupElseFM (TVar x) (rep s) x
unitSub                       ::  TVarId -> MonoType -> Sub
unitSub x t                   =   MkSub (makeFM [(x,t)])
emptySub                      ::  Sub
emptySub                      =   MkSub emptyFM
makeSub                       ::  [(TVarId, MonoType)] -> Sub
makeSub xts                   =   MkSub (makeFM xts)
extendSub                     ::  Sub -> TVarId -> MonoType -> Sub
extendSub s x t               =   s `thenSub` unitSub x (applySub s t)
thenSub                       ::  Sub -> Sub -> Sub
r `thenSub` s                 =   MkSub (mapFM (applySub s) (rep r) `thenFM` rep s)
domSub                        ::  Sub -> [TVarId]
domSub s                      =   domFM (rep s)
unifySub                              =  unify
unify                                 :: MonoType -> MonoType -> Sub -> Maybe Sub
unify (TVar x) u s                    =  unifyTVar x u s
unify t (TVar y) s                    =  unifyTVar y t s
unify (TCon j ts) (TCon k us) s       =  (j == k) `guardM` unifies ts us s
unifies                               :: [MonoType] -> [MonoType] -> Sub -> Maybe Sub
unifies [] [] s                       =  returnM s
unifies (t:ts) (u:us) s               =  unify t u s `thenM` (\s' -> unifies ts us s')
unifyTVar                             :: TVarId -> MonoType -> Sub -> Maybe Sub
unifyTVar x t s | x `elem` domSub s     =  unify (lookupSub s x) t s
                | TVar x == t         =  returnM s
                | x `elem` freeVars t   =  failM
                | otherwise           =  returnM (extendSub s x t)    
freeVars                              =  freeTVarMono

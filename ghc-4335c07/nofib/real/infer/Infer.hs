module Infer (inferTerm) where

import Data.List(nub)

import  MyList                  (minus)
import  Type                  (TVarId, TConId, MonoType (..), PolyType (All),
                               arrow, freeTVarMono)
import  Term                  (VarId, Term (Var, Abs, App, Let))
import  Substitution          (Sub, applySub, lookupSub, makeSub)
import  Environment           (Env, lookupEnv, extendLocal, extendGlobal,
                               domEnv, freeTVarEnv)
import  InferMonad            (Infer, thenI, returnI, guardI, getSubI,
                               freshI, freshesI, unifyI, substituteI)
import  MaybeM

specialiseI                   :: PolyType -> Infer MonoType
specialiseI (All xxs tt)      =  freshesI (length xxs) `thenI` (\yys ->
                                 returnI (applySubs xxs yys tt))
applySubs                     :: [TVarId] -> [MonoType] -> MonoType -> MonoType
applySubs xxs yys tt          =  applySub (makeSub (zip xxs yys)) tt
generaliseI                   :: Env -> MonoType -> Infer PolyType
generaliseI aa tt             =  getSubI `thenI` (\s ->
 				 let aaVars = nub (freeTVarSubEnv s aa) in
				 let ttVars = nub (freeTVarMono tt) in
				 let xxs    = ttVars `minus` aaVars in
                                 returnI (All xxs tt)
                                 )
freeTVarSubEnv                :: Sub -> Env -> [TVarId]
freeTVarSubEnv s aa           =  concat (map (freeTVarMono . lookupSub s)
                                             (freeTVarEnv aa))
inferTerm  ::  Env -> Term -> Infer MonoType
inferTerm aa (Var x)  =
      (x `elem` domEnv aa)                      `guardI` (
      let ss = lookupEnv aa x in
      specialiseI ss                          `thenI`  (\tt ->
      substituteI tt                          `thenI`  (\uu  ->
                                              returnI  uu)))
inferTerm aa (Abs x v)  =
      freshI                                  `thenI` (\xx ->
      inferTerm (extendLocal aa x xx) v       `thenI` (\vv ->
      substituteI xx                          `thenI` (\uu ->
                                              returnI (uu `arrow` vv))))
inferTerm aa (App t u)  =
      inferTerm aa t                          `thenI` (\tt ->
      inferTerm aa u                          `thenI` (\uu ->
      freshI                                  `thenI` (\xx ->
      unifyI tt (uu `arrow` xx)               `thenI` (\() ->
      substituteI xx                          `thenI` (\vv ->
                                              returnI vv)))))
inferTerm aa (Let x u v)  =
      inferTerm aa u                          `thenI` (\uu ->
      generaliseI aa uu                       `thenI` (\ss ->
      inferTerm (extendGlobal aa x ss) v      `thenI` (\vv ->
                                              returnI vv)))

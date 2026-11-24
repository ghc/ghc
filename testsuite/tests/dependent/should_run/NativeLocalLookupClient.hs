{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, StandaloneKindSignatures, TypeOperators #-}
module Main where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import qualified NativeLocalLookup as P

type Found :: forall (n :: P.Nat). P.Env n -> P.Fin n -> P.Clos -> Type
data Found e x c where
  ELZ :: Found ('P.ECons c e) 'P.FZ c
  ELS :: Found e x c -> Found ('P.ECons other e) ('P.FS x) c

lookupAgrees :: Found e x c -> P.lookupEnv e x :~: c
lookupAgrees ELZ     = Refl
lookupAgrees (ELS p) = lookupAgrees p

#ifdef USE_HIDDEN_EVAR
hiddenCon :: P.EvalBig ('P.ECons ('P.MkClos ('P.ULam ('P.UVar 'P.FZ)) 'P.ENil) 'P.ENil)
                       ('P.UVar 'P.FZ)
                       ('P.MkClos ('P.ULam ('P.UVar 'P.FZ)) 'P.ENil)
hiddenCon = P.EVar P.ELam
#endif

#ifndef ABSTRACT_CLIENT
outerStep :: P.EvalBig ('P.ECons ('P.MkClos ('P.ULam ('P.ULam ('P.UVar 'P.FZ))) 'P.ENil)
                         ('P.ECons ('P.MkClos ('P.ULam ('P.UVar 'P.FZ)) 'P.ENil) 'P.ENil))
                       ('P.UVar ('P.FS 'P.FZ))
                       ('P.MkClos ('P.ULam ('P.UVar 'P.FZ)) 'P.ENil)
outerStep = P.EVar P.ELam
#endif

main :: IO ()
main = do
  let c0 = P.MkClos (P.ULam (P.UVar P.FZ)) P.ENil
      env = P.ECons (error "unused head closure") (P.ECons c0 (error "unused tail env"))
      hit = case P.lookupEnv env (P.FS P.FZ) of
              P.MkClos (P.ULam (P.UVar P.FZ)) P.ENil -> True
              _ -> False
      eqOk = case lookupAgrees (ELS ELZ :: Found ('P.ECons ('P.MkClos ('P.ULam ('P.ULam ('P.UVar 'P.FZ))) 'P.ENil)
                                                   ('P.ECons ('P.MkClos ('P.ULam ('P.UVar 'P.FZ)) 'P.ENil) 'P.ENil))
                                                 ('P.FS 'P.FZ)
                                                 ('P.MkClos ('P.ULam ('P.UVar 'P.FZ)) 'P.ENil)) of
               Refl -> True
      kOk = case P.evalBigIsLam P.evalKIdOmega of P.MkIsLamClos -> True
#ifdef ABSTRACT_CLIENT
  print (hit, eqOk, kOk)
#else
  let stepOk = case P.evalDeterm outerStep outerStep of Refl -> True
  print (hit, eqOk, kOk, stepOk)
#endif

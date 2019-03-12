{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module T11629 where

import Control.Monad
import Language.Haskell.TH

class C (a :: Bool)
class D (a :: (Bool, Bool))
class E (a :: [Bool])

instance C True
instance C 'False

instance D '(True, False)
instance D '(False, True)

instance E '[True, False]
instance E '[False, True]

do
  let getType (InstanceD _ _ ty _) = ty
      getType _                    = error "getType: only defined for InstanceD"

      failMsg a ty1 ty2 = fail $ "example " ++ a
        ++ ": ty1 /= ty2, where\n ty1 = "
        ++ show ty1 ++ "\n ty2 = " ++ show ty2

      withoutSig (ForallT tvs cxt ty) = ForallT tvs cxt (withoutSig ty)
      withoutSig (AppT ty1 ty2)       = AppT (withoutSig ty1) (withoutSig ty2)
      withoutSig (SigT ty ki)         = withoutSig ty
      withoutSig ty                   = ty

  -- test #1: type quotations and reified types should agree.
  ty1 <- [t| C True |]
  ty2 <- [t| C 'False |]
  ClassI _ insts <- reify ''C
  let [ty1', ty2'] = map getType insts

  when (ty1 /= ty1') $ failMsg "A" ty1 ty1'
  when (ty2 /= ty2') $ failMsg "B" ty2 ty2'

  -- test #2: type quotations and reified types should agree wrt
  -- promoted tuples.
  ty3 <- [t| D '(True, False) |]
  ty4 <- [t| D (False, True)  |]
  ClassI _ insts <- reify ''D
  let [ty3', ty4'] = map (withoutSig . getType) insts

  when (ty3 /= ty3') $ failMsg "C" ty3 ty3'
  -- The following won't work. See https://gitlab.haskell.org/ghc/ghc/issues/12853
  -- when (ty4 /= ty4') $ failMsg "D" ty4 ty4'

  -- test #3: type quotations and reified types should agree wrt to
  -- promoted lists.
  ty5 <- [t| E '[True, False] |]
  ty6 <- [t| E [False, True]  |]

  ClassI _ insts <- reify ''E
  let [ty5', ty6'] = map (withoutSig . getType) insts

  when (ty5 /= ty5') $ failMsg "C" ty5 ty5'
  when (ty6 /= ty6') $ failMsg "D" ty6 ty6'

  return []

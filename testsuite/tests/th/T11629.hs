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

      -- CQ[reify-inst-order]
      -- Q: Why check membership instead of matching the reified instances
      --    positionally?
      -- A~ reify lists a class's instances in RoughMap order (GHC.Core.RoughMap):
      --    instances with different head tycons come out in insertion order,
      --    instances sharing a head come out of one leaf list, so the order
      --    depends on the order in which the typechecker added them.
      checkReified a ty tys = when (ty `notElem` tys) $ fail $ "example " ++ a
        ++ ": ty not among reified instance types, where\n ty = "
        ++ show ty ++ "\n reified = " ++ show tys

      withoutSig (ForallT tvs cxt ty) = ForallT tvs cxt (withoutSig ty)
      withoutSig (AppT ty1 ty2)       = AppT (withoutSig ty1) (withoutSig ty2)
      withoutSig (SigT ty ki)         = withoutSig ty
      withoutSig ty                   = ty

  -- test #1: type quotations and reified types should agree.
  ty1 <- [t| C True |]
  ty2 <- [t| C 'False |]
  ClassI _ insts <- reify ''C
  let tys' = map getType insts

  checkReified "A" ty1 tys'
  checkReified "B" ty2 tys'

  -- test #2: type quotations and reified types should agree wrt
  -- promoted tuples.
  ty3 <- [t| D '(True, False) |]
  ty4 <- [t| D (False, True)  |]
  ClassI _ insts <- reify ''D
  let tys' = map (withoutSig . getType) insts

  checkReified "C" ty3 tys'
  -- The following won't work. See https://gitlab.haskell.org/ghc/ghc/issues/12853
  -- checkReified "D" ty4 tys'

  -- test #3: type quotations and reified types should agree wrt to
  -- promoted lists.
  ty5 <- [t| E '[True, False] |]
  ty6 <- [t| E [False, True]  |]

  ClassI _ insts <- reify ''E
  let tys' = map (withoutSig . getType) insts

  checkReified "E" ty5 tys'
  checkReified "F" ty6 tys'

  return []

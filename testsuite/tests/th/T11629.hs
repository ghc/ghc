{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module T11629 where

import Control.Monad
import Language.Haskell.TH
import Data.List (sort)

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

      compareInstances :: String -> Name -> [Q Type] -> Q ()
      compareInstances label cls expectedM = do
        ClassI _ insts <- reify cls
        -- N.B. Sort as the order of instances provided by TH is
        -- non-deterministic.
        let actual = sort $ map (withoutSig . getType) insts
        expected <- sort <$> sequence expectedM
        let check (a,e)
              | a /= e = runIO $ print $ label ++ ": mismatch: " ++ show a ++ " /= " ++ show e
              | otherwise = return ()

        mapM_ check (zip actual expected)

  -- test #1: type quotations and reified types should agree.
  compareInstances "C" ''C
    [ [t| C True |]
    , [t| C 'False |]
    ]

  -- test #2: type quotations and reified types should agree wrt
  -- promoted tuples.
  --
  -- This doesn't work due to https://gitlab.haskell.org/ghc/ghc/issues/12853
  --
  --compareInstances "D" ''D
  --  [ [t| D '(True, False) |]
  --  , [t| D '(False, True)  |]
  --  ]

  -- test #3: type quotations and reified types should agree wrt to
  -- promoted lists.
  compareInstances "E" ''E
    [ [t| E '[True, False] |]
    , [t| E [False, True]  |]
    ]

  return []

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}
module T13942 where

$([d| f :: Either Int (Int -> Int)
      f = undefined
    |])

{-

Note: to debug

~/inplace/bin/ghc-stage2 --interactive
load the following
--------------------------------------
import Language.Haskell.TH

foo :: IO ()
foo = do
  r <- runQ ([d| f :: Either Int (Int -> Int)
                 f = undefined
             |])
  print r

----------------------------------------
foo
[SigD f_0 (AppT (AppT (ConT Data.Either.Either) (ConT GHC.Types.Int)) (AppT (AppT ArrowT (ConT GHC.Types.Int)) (ConT GHC.Types.Int)))
,ValD (VarP f_0) (NormalB (VarE GHC.Err.undefined)) []]

[SigD f_0
  (AppT (AppT (ConT Data.Either.Either)
              (ConT GHC.Types.Int))
        (AppT (AppT ArrowT
                    (ConT GHC.Types.Int))
              (ConT GHC.Types.Int)))
-}

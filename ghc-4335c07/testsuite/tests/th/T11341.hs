{-# LANGUAGE GADTs, TemplateHaskell #-}
module T11341 where

import Language.Haskell.TH
import System.IO

type S1 = T1
data T1 a where
  MkT1 :: S1 Int

type S2 a = T2
data T2 a where
  MkT2 :: S2 Char Int

type Id a = a
type S3 a = T3
data T3 a where
      MkT3 :: Id (S3 Char Int)

$( do  -- test reification
  { TyConI dec <- runQ $ reify (mkName "T1")
  ; runIO $ putStrLn (pprint dec) >> hFlush stdout

  ; TyConI dec <- runQ $ reify (mkName "T2")
  ; runIO $ putStrLn (pprint dec) >> hFlush stdout

  ; TyConI dec <- runQ $ reify (mkName "T3")
  ; runIO $ putStrLn (pprint dec) >> hFlush stdout

  ; return [] } )

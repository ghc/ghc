{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module T7667 where

import Language.Haskell.TH

$( return [ TySynD (mkName "+") [PlainTV (mkName "a") BndrReq, PlainTV (mkName "b") BndrReq]
                   (AppT (AppT (ConT ''Either) (VarT $ mkName "a")) (VarT $ mkName "b")) ] )

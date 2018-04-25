{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module T7667 where

import Language.Haskell.TH

$( return [ TySynD (mkName "+") [PlainTV (mkName "a"), PlainTV (mkName "b")]
                   (AppT (AppT (ConT ''Either) (VarT $ mkName "a")) (VarT $ mkName "b")) ] )
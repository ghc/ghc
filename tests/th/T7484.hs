{-# LANGUAGE TemplateHaskell #-}

module T7484 where

import Language.Haskell.TH

$( return [ ValD (VarP (mkName "a ")) (NormalB (LitE (IntegerL 5))) [] ] )

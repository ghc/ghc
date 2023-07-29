{-# LANGUAGE TemplateHaskell #-}

module T23719 where

import Language.Haskell.TH as TH

$(TH.varP (TH.mkName "forall")) = ()
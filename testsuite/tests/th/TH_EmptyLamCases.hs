{-# LANGUAGE TemplateHaskell #-}

module TH_EmptyLamCases where

import Language.Haskell.TH

f = $( lamCasesE [] )
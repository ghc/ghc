{-# LANGUAGE TemplateHaskell #-}

-- Trac #2674

module ShouldFail where

import Language.Haskell.TH

$(return [FunD (mkName "foo") []])

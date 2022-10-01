{-# LANGUAGE TemplateHaskell #-}

module ShouldFail where

import Language.Haskell.TH

$(return [FunD (mkName "foo") [Clause [] (GuardedB []) []]])

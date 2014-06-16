{-# LANGUAGE TemplateHaskell, TypeFamilies, RankNTypes, FlexibleContexts #-}

module T4056 where
import Language.Haskell.TH

astTest :: Q [Dec]
astTest = [d|
    class C t where
        op :: [t] -> [t]
        op = undefined
  |]

class D t where
  bop :: [t] -> [t]
  bop = undefined

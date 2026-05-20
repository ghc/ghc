{-# LANGUAGE TemplateHaskell #-}

module TH_cvt_InvalidTyFamInstLHS where

import Language.Haskell.TH

$(return [TySynInstD (TySynEqn Nothing WildCardT (ConT (mkName "Int")))])

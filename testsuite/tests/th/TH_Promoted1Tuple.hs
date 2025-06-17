{-# LANGUAGE TemplateHaskell, GHC2021 #-}

module TH_Promoted1Tuple where

import Language.Haskell.TH

$(sequence [tySynD (mkName "F") [] (appT (promotedTupleT 1) (conT ''Int))])

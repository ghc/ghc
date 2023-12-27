{-# LANGUAGE TemplateHaskell #-}
module TH where

import Data.Maybe
import Language.Haskell.TH

x, y :: Q [Dec]
x = [d|{-# SCC f #-}; f = 1|]
y = [d|{-# SCC g "custom_name_g" #-}; g = 1|]

gen :: Q [Dec]
gen = do
  a <- fromJust <$> lookupValueName "a"
  b <- fromJust <$> lookupValueName "b"
  pure
    [ PragmaD $ SCCP a Nothing
    , PragmaD $ SCCP b (Just "custom_name_b")
    ]

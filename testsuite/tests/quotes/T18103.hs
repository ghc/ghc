{-# LANGUAGE TemplateHaskellQuotes #-}
module T18103 where

import Language.Haskell.TH

ex :: IO [Dec]
ex = [d| foo x = x |]

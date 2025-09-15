{-# LANGUAGE TemplateHaskell #-}

module T9262 where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr
import System.IO

$(do insts <- reifyInstances ''Eq [ListT `AppT` VarT (mkName "a")]
     runIO $ putStrLn $ pprint insts
     runIO $ hFlush stdout
     return [] )

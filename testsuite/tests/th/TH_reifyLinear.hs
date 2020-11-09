{-# LANGUAGE LinearTypes #-}
module TH_reifyLinear where

import Language.Haskell.TH
import System.IO

type T = Int %1 -> Int

$(
    do x <- reify ''T
       runIO $ hPutStrLn stderr $ pprint x
       return []
 )

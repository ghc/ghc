{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T15845 where

import Language.Haskell.TH
import System.IO

data family F1 a b
data instance F1 [a] b = MkF1

data family F2 a
data instance F2 a = MkF2

$(do i1 <- reify ''F1
     i2 <- reify ''F2
     runIO $ mapM_ (hPutStrLn stderr . pprint) [i1, i2]
     pure [])

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
module T22818 where

import Language.Haskell.TH
import System.IO

type data T = MkT

$(pure [])

$(do i <- reify ''MkT
     runIO $ do
       hPutStrLn stderr $ pprint i
       hFlush stderr
     pure [])

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T12646 where

import Language.Haskell.TH
import System.IO

type family F (a :: k) :: * where
    F (a :: * -> *) = Int
    F (a :: k)      = Char

$(do info <- reify ''F
     runIO $ putStrLn $ pprint info
     runIO $ hFlush stdout
     return [])

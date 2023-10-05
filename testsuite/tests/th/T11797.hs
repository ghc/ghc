{-# LANGUAGE TemplateHaskell #-}

module T11797 where

import Language.Haskell.TH
import System.IO

$(do dec <- [d| class Foo a where
                  meth :: a -> b -> a |]
     runIO $ do putStrLn $ pprint dec
                hFlush stdout
     return [] )

-- the key bit is the forall b. in the type of the method

{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module T15738 where

import Language.Haskell.TH
import System.IO

data Foo x = MkFoo x

$(do d <- [d| f :: (forall a. Eq (Foo a)) => Foo x -> Foo x -> Bool
              f = (==) |]
     runIO $ hPutStrLn stderr $ pprint d
     pure d)

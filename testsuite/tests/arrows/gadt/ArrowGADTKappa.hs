{-# LANGUAGE Arrows, GADTs #-}

module ShouldFail where

import Control.Arrow

data G where
  MkG :: Show a => a -> G

handleG :: ( (a, ()) -> b )
        -> ( (a, (G, ())) -> b )
        -> ( (a, ()) -> b )
handleG = undefined

foo :: String -> String
foo = proc x -> do
  (id -< x) `handleG` \ (MkG g) -> show -< g

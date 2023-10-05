{-# LANGUAGE Arrows, ViewPatterns #-}

module T3964 where

import Control.Arrow

testF :: Eq a => a -> (Maybe (Maybe a)) -> Maybe a
testF v = proc x -> case x of
     Just (Just ((==v) -> True)) -> returnA -< Just v
     _                           -> returnA -< Nothing

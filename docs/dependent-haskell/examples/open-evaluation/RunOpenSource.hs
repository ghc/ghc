{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import qualified OpenSource

main :: IO ()
main = do
    print (OpenSource.x :: Int, OpenSource.y :: Int, OpenSource.z :: Int)
    print ( OpenSource.first False (42 :: Int)
          , OpenSource.repeated (OpenSource.second False (42 :: Int)) )
    print ( OpenSource.routedFirst False (42 :: Int)
          , OpenSource.routedSecond False (42 :: Int) )
    print [ OpenSource.tupleFirst 'x' False
          , OpenSource.tupleSecond False 'y'
          , OpenSource.workerFirst 'z' True
          , OpenSource.ignoresRigidPair (Proxy :: Proxy '(Int, Bool)) '.' ]

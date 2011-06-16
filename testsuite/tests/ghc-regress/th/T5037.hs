{-# LANGUAGE TemplateHaskell #-}
module T5037 where
import Language.Haskell.TH
import System.IO

$( do ds <- [d|  f :: Maybe Int -> Int
                 f Nothing = 3
                 f (Just x) = $(varE (mkName "x"))
            |]
      runIO $ (putStrLn (pprint ds) >> hFlush stdout)
      return ds )

{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Maybe (Maybe(..))
import Data.Ord (Ord)
import Language.Haskell.TH (mkName, nameSpace)

main :: IO ()
main = mapM_ (print . nameSpace)
             [ 'Prelude.id
             , mkName "id"
             , 'Data.Maybe.Just
             , ''Data.Maybe.Maybe
             , ''Data.Ord.Ord
             ]

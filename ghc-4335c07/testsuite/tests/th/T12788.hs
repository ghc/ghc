{-# LANGUAGE TemplateHaskell #-}
module T12788 where

import Language.Haskell.TH
import T12788_Lib

data Bad = Bad { _bad :: String } deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions{} ''Bad)

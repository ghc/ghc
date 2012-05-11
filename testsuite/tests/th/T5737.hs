{-# LANGUAGE TemplateHaskell #-}
module T5737 where

import Language.Haskell.TH
makeAlpha n = [d| data Alpha = Alpha $(conT n) deriving (Show, Read) |]  

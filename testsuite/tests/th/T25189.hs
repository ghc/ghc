{-# LANGUAGE TemplateHaskell #-}
module T25189 where

import Language.Haskell.TH

foreign import ccall foo :: IO ()

$(runIO foo >> pure [])

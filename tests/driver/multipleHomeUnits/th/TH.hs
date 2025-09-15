{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath

th = $(makeRelativeToProject "data" >>= runIO . readFile >> [| () |])

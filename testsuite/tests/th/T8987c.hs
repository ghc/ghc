{-# LANGUAGE TemplateHaskell #-}

module T8987c where
import Language.Haskell.TH
import System.IO.Error
import Control.Exception

$(fail ['1', throw (userError "IOError in fail")] >> return [])

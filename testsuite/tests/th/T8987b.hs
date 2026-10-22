{-# LANGUAGE TemplateHaskell #-}

module T8987b where
import Language.Haskell.TH
import System.IO.Error
import Control.Exception

$(reportWarning ['1', throw (userError "IOError in warning")] >> return [])

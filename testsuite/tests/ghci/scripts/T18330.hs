{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Monad.IO.Class
import Language.Haskell.TH.Syntax

$(liftIO (putStrLn "Bip") >> return [])

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.Base (
    ConfigKey (..),
    askConfigWithDefault, askConfig,
    Condition (..)
    ) where

import Base
import Development.Shake.Classes

type Condition = Action Bool

newtype ConfigKey = ConfigKey String deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

askConfigWithDefault :: String -> Action String -> Action String
askConfigWithDefault key defaultAction = do
    maybeValue <- askOracle $ ConfigKey key 
    case maybeValue of
        Just value -> return value
        Nothing    -> defaultAction

askConfig :: String -> Action String
askConfig key = askConfigWithDefault key $
    error $ "\nCannot find key '" ++ key ++ "' in configuration files."

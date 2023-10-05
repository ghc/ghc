{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module T15713 where

import Data.String

data Expr = App Expr Expr | Var String
  deriving (Eq)

instance IsString Expr where
  fromString = Var . fromString

go = \case
  App ( App ( App "refWithFile" identM ) filenameM) exceptionMayM -> Just 2
  App ( App "and" a ) b -> Just 3
  App ( App "or" a ) b -> Just 4
  _ -> Nothing

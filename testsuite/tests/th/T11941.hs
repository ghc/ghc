{-# LANGUAGE TemplateHaskell #-}

module T11941 where

import Data.Monoid

const (return []) $ mempty { getFrst = Just () }

{-# LANGUAGE ExplicitNamespaces #-}

module DodgyImports04 where

import Control.Applicative (type Applicative (type ..))  -- dodgy: no associated types
import Data.Either (type Either (type ..))               -- dodgy: not a class

import Data.Proxy (type Proxy(data ..))                  -- ok
import Data.Proxy (type Proxy(type ..))                  -- dodgy: not a class
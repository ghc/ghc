{-# LANGUAGE ExplicitNamespaces #-}

module Test27291
    ( C(type ..)       -- exports class C and data family D
    , C(data ..)       -- exports class C and method m
    , D(type ..)       -- exports data family D
    , type T (..)      -- exports type T and all its data constructors D, D2
    , type T (type ..) -- exports type T
    , type K (type ..) -- exports type K and its constructor K1
    ) where

import Control.Applicative qualified as A (type Applicative (data ..))
import Data.Either qualified as E (type Either (data ..))

import Data.Bool (data True (..))
import Data.Bool (data True( data .. ) )
import Data.Bool (data True( type ..))

import DodgyImports03_helper (C(  .. ))
import DodgyImports03_helper (C  (data .. ))
import DodgyImports03_helper (C(  type ..) )

import DodgyImports03_helper (T ( .. ) )
import DodgyImports03_helper (T(data ..))
import DodgyImports03_helper (T(type ..))

import Control.Applicative (type Applicative (type ..))  -- dodgy: no associated types
import Data.Either (type Either (type ..))               -- dodgy: not a class

import Data.Proxy (type Proxy(data ..))                  -- ok
import Data.Proxy (type Proxy(type ..))                  -- dodgy: not a class

import T25901_sub_g_helper qualified as T1 (T (data ..)) -- T and MkT
import T25901_sub_g_helper qualified as T2 (T (type ..)) -- T only
import T25901_sub_g_helper qualified as T3 (type T (..)) -- T and MkT

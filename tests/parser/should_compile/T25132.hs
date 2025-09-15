{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T25132 where

import Data.Proxy
import GHC.TypeLits

singleTypeMultiVal :: Proxy "this is a\nmultiline\nstring"
singleTypeMultiVal = Proxy @"""
                            this is a
                            multiline
                            string
                            """

multiTypeSingleVal :: Proxy """
                            this is a
                            multiline
                            string
                            """
multiTypeSingleVal = Proxy @"this is a\nmultiline\nstring"

multiTypeMultiVal :: Proxy """
                           this is a
                           multiline
                           string
                           """
multiTypeMultiVal = Proxy @"""
                           this is a
                           multiline
                           string
                           """

k1 :: ()
k1 = test where
  test :: "string" ~ """string""" => ()
  test = ()

k2 :: ()
k2 = test where
  test :: ConsSymbol 's' "tring" ~ """string""" => ()
  test = ()

k3 :: UnconsSymbol "string" ~ Just '( 's', x) => Proxy x
k3 = test where
  test :: Proxy """tring"""
  test = Proxy

k4 :: Proxy "string"
k4 = Proxy @"""string"""

k5 :: ()
k5 = test """string""" where
  test :: forall a -> ()
  test _ = ()

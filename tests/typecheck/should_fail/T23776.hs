{-# LANGUAGE GHC2021 #-}

module T23776 where

import Data.Kind

foo :: Maybe a -> Maybe a
foo (Just @b x) = Just @b x
foo _ = Nothing

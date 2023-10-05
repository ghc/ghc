{-# LANGUAGE DataKinds #-}
module WCompatWarningsOnOff where

import Data.Proxy
import GHC.Types
import Data.List
import Data.Kind

type T1 = 'Nothing :: Maybe a

foo :: Maybe a -> Maybe a
foo (Just @b x) = Just @b x
foo _ = Nothing

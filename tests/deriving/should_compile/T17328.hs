{-# LANGUAGE DerivingStrategies, StandaloneDeriving, GeneralizedNewtypeDeriving,
             DerivingVia #-}

module T17328 where

import T17328a ( N1, N2(..) )

import Data.Coerce

deriving newtype instance Eq N1


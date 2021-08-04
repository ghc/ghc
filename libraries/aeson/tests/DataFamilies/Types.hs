-- DataKinds is needed for deriveAll0 calls on GHC 8
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DataFamilies.Types (module DataFamilies.Types) where

import Prelude.Compat

import Generics.Deriving.TH (deriveAll0)
import Types (ApproxEq(..))

data family Nullary a
data instance Nullary Int  = C1 | C2 | C3 deriving (Eq, Show)
data instance Nullary Char = C4           deriving (Eq, Show)

data family SomeType a b c
data instance SomeType c () a = Nullary
                              | Unary Int
                              | Product String (Maybe Char) a
                              | Record { testOne   :: Double
                                       , testTwo   :: Maybe Bool
                                       , testThree :: Maybe a
                                       }
                              | List [a]
    deriving (Eq, Show)

data family Approx a
newtype instance Approx a = Approx { fromApprox :: a }
    deriving (Show, ApproxEq, Num)

instance (ApproxEq a) => Eq (Approx a) where
    Approx a == Approx b = a =~ b

data family GADT a
data instance GADT a where
    GADT :: { gadt :: String } -> GADT String

deriving instance Eq   (GADT a)
deriving instance Show (GADT a)

-- We use generic-deriving to be able to derive Generic instances for
-- data families on GHC 7.4.

$(deriveAll0 'C1)
$(deriveAll0 'C4)
$(deriveAll0 'Approx)
$(deriveAll0 'Nullary)

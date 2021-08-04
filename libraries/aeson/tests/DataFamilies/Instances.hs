{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module DataFamilies.Instances () where

import Prelude.Compat

import Data.Aeson.TH
import Data.Aeson.Types (FromJSON(..))
import DataFamilies.Types
import Test.QuickCheck (Arbitrary(..), elements, oneof)

instance (Arbitrary a) => Arbitrary (Approx a) where
    arbitrary = Approx <$> arbitrary

instance Arbitrary (Nullary Int) where
    arbitrary = elements [C1, C2, C3]

instance Arbitrary a => Arbitrary (SomeType c () a) where
    arbitrary = oneof [ pure Nullary
                      , Unary   <$> arbitrary
                      , Product <$> arbitrary <*> arbitrary <*> arbitrary
                      , Record  <$> arbitrary <*> arbitrary <*> arbitrary
                      , List    <$> arbitrary
                      ]

instance Arbitrary (GADT String) where
    arbitrary = GADT <$> arbitrary

deriveJSON defaultOptions 'C1
deriveJSON defaultOptions 'Nullary
deriveJSON defaultOptions 'Approx

deriveToJSON defaultOptions 'GADT
-- We must write the FromJSON instance head ourselves
-- due to the refined GADT return type
instance FromJSON (GADT String) where
    parseJSON = $(mkParseJSON defaultOptions 'GADT)

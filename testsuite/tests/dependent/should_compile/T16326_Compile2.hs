{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T16326_Compile2 where

import Data.Kind

type family Wat :: forall a. a
type Lol = Wat @(forall a -> a) (forall a -> a) (forall a. a) @(forall a. a)
               Type Bool

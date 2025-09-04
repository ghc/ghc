
{-# LANGUAGE TypeFamilies #-}

module T26154_A where

import {-# SOURCE #-} T26154_B

type family F a b
type instance F a b = b

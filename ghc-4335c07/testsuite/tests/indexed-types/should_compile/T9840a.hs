{-# LANGUAGE TypeFamilies #-}

module T9840a where

import {-# SOURCE #-} T9840

type family G a where

bar :: X a -> X a
bar = id

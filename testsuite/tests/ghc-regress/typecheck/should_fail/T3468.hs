{-# LANGUAGE ExistentialQuantification #-}

module T3468 where

import {-# SOURCE #-} T3468

data Tool d = forall a r . F a


{-# LANGUAGE ExistentialQuantification #-}

module T3468 where

import T3468a

data Tool d = forall a r . F a


 {-# LANGUAGE TypeFamilies #-}

module T8500 where

import T8500a

instance C Int where
   type F Int = Double

instance C Bool

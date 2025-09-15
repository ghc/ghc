{-# LANGUAGE TypeFamilies #-}
module ClassDefaultInHsBootA2 where

import ClassDefaultInHsBootA1

data I = I Int

instance C I where
  type T I = ()
  def = ()

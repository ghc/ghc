{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- Analogous to module Class from tests/backpack/should_compile/bkp58.bkp
module ClassDefaultInHsBootA1 where

class Show (T x) => C x where
  type T x
  type T x = Int
  def :: T x

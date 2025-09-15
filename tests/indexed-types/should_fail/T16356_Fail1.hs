{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T16356_Fail1 where

import Data.Kind

class C (a :: j) where
  type T (a :: j)
  type T @Type a = Maybe a

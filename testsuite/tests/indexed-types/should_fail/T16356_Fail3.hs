{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T16356_Fail3 where

import Data.Kind

class C a where
  type T1 a
  type T1 @Type a = a

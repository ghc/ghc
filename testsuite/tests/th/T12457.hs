{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

import Language.Haskell.TH
import GHC.Internal.TH.Lib
import T12457_aux

instance C Int where
  type Assoc Int = Bool
  m _ = 1
  n _ = 2

newtype T a = T [a] deriving th C

main = do
  print $ m (T [1,2,3::Int])

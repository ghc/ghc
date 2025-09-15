{-# LANGUAGE DeriveGeneric #-}
module T9630 where
import T9630a
import GHC.Generics
import Control.Applicative

data T = T () () () ()
          ()()()()()()()
          ()()()()()()()()()()()()()()()()
          ()()()()()()()()()()()()()()()()
          ()()()()()()()()()()()()()()()()
          ()()()()()()()()()()()()()()()()
          ()()()()()()()()()()()()()()()()
          ()()()()()()()()()()()()()()()()
          ()()()()()()()()()()()()()()()()
          ()()()()()()()()()()()()()()()()
  deriving Generic

instance Serialize T where
  get = to <$> gGet
  put = gPut . from

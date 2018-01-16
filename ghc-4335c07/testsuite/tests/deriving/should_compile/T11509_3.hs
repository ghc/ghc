{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module T11509_3 where

import System.IO (Handle) -- A data type whose constructors are hidden

class C a where

deriving instance C Handle

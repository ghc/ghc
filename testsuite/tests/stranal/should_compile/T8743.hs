module T8743 where

-- Without the following import, it does not fail
import {-# SOURCE #-} T8743 ()

-- [()] required, () does not work.
class ToRow a where toRow :: a -> [()]

instance ToRow (Maybe a) where
    toRow Nothing  = [()]
    toRow (Just _) = [()]

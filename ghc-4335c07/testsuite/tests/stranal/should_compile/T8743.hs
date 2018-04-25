module T8743 where

-- Without the following import, it does not fail
import T8743a ()

-- [()] required, () does not work.
class ToRow a where toRow :: a -> [()]

instance ToRow (Maybe a) where
    toRow Nothing  = [()]
    toRow (Just _) = [()]

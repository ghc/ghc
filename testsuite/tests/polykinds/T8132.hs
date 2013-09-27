{-# LANGUAGE MagicHash #-}
import Data.Typeable.Internal

data K = K

instance Typeable K where typeRep# _ = undefined

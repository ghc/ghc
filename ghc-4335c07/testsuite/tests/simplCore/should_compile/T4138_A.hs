
module T4138_A where

class NFData a where
    rnf :: a -> ()
    rnf a = a `seq` ()

instance NFData Float

instance (NFData a, NFData b) => NFData (a,b) where
    rnf (x,y) = rnf x `seq` rnf y

{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Coerce (Coercible)

instance Coercible () ()

main = return ()

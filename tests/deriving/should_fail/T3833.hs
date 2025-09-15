{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid

newtype DecodeMap e = DecodeMap [e]

deriving instance Monoid (DecodeMap e)

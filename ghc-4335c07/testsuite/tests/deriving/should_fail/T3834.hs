
{-# LANGUAGE StandaloneDeriving #-}

class C a
instance C Int

newtype T = T Int
deriving instance C T


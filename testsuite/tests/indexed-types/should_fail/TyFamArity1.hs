{-# LANGUAGE TypeFamilies #-}

type family T a b :: *
type instance T Int = IO   -- must fail: too few args

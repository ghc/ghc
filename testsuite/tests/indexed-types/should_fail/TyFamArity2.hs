{-# LANGUAGE TypeFamilies #-}

type family T a :: * -> *
type instance T Int Float = Char   -- must fail: extra arguments

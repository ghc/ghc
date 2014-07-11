{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T6018failclosed3 where

-- this is not injective - not all injective type variables mentioned
-- on LHS are mentioned on RHS
type family JClosed a b c = r | r -> a b where
    JClosed Int b c = Char

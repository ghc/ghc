{-# LANGUAGE FlexibleInstances #-}

-- This just tests what the kind error message looks like
-- #1633

module T1633 where

instance Functor Bool

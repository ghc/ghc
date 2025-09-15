{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
{-# LANGUAGE GADTs #-}

module T2006 where

data Expr a vs where
    EPrim   :: String -> a -> Expr a vs
    EVar    :: Expr a (a,vs)

interpret :: Expr a () -> a
interpret (EPrim _ a) = a
-- interpret EVar = error "unreachable"


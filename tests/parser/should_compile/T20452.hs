{-# LANGUAGE DataKinds #-}
{-# OPTIONS -ddump-parsed-ast #-}
module T20452 where

data Proxy1  (a :: k)  = Proxy1
data Proxy2 ((a :: k)) = Proxy2

class Prods1  (lhs :: Int)   (name :: String)   (rhs :: [(String, String)])  where {}
class Prods2 ((lhs :: Int)) ((name :: String)) ((rhs :: [(String, String)])) where {}

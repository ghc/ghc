{-# LANGUAGE ExplicitNamespaces #-}

module T24396a where

class C1
data D1 = C1

data D2 = D2

{-# DEPRECATED data C1, D2 "Data deprecation" #-}
{-# DEPRECATED type C1, D2 "Type deprecation" #-}

data D3 = D3
{-# DEPRECATED D3 "Both namespace deprecation" #-}

class C2
data D4 = C2

data D5 = D5

{-# WARNING data C2, D5 "Data warning" #-}
{-# WARNING type C2, D5 "Type warning" #-}

data D6 = D6
{-# WARNING D6 "Both namespace warning" #-}

($) :: (a -> b) -> a -> b
f $ x = f x

type f $ x = f x

{-# WARNING data ($) "Value operator warning" #-}
{-# WARNING type ($) "Type operator warning" #-}

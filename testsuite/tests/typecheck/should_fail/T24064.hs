{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module T24064 where

class C1 b where
  type F1 b

class C2 (m :: * -> *) where
  type F2 m

class C3 r where
  type F3 r

class G t m where
  g :: m a -> t m a

data Y

data X e a

data H a

data S a

fun1 :: X e ()
fun1 = undefined

fun2 :: S ()
fun2 = undefined

fun3 :: H ()
fun3 = undefined

fun4 :: (F3 r ~ F1 (F2 m)) => r -> m ()
fun4 = undefined

test :: (C2 m, F2 m ~ Y) => m ()
test = do
  fun1
  fun2
  g fun3
  fun4 undefined

main :: IO ()
main = pure ()

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- The original test case for #12957

module T12957a where

data T = A | B

data Fields (t :: T) where
  BFields :: { list :: [()] } -> Fields 'B

  AFields :: Fields 'A

  EmptyFields :: Fields t

emptyA :: Fields 'A
emptyA = AFields

data S t = S { sFields :: Fields t }

f :: () -> S 'A
f a = (S EmptyFields) { sFields = emptyA { list = [ a ] } }

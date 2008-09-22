{-# LANGUAGE NewQualifiedOperators,TypeOperators #-}
-- tests for new qualified operator syntax

module Test where

-- test the different combinations of qualification/infix for varsyms
(*!*) :: Int -> Int -> Int
0 *!* b = b
(*!*) a b = a + b

f3 = ( 
       1 *!* 2,
       (*!*) 1 2,
       Test.(*!*) 1 2,
--       1 `(*!*)` 2,
       1 `Test.(*!*)` 2
      )

-- test the different combinations of qualification/infix for type and
-- data constructors.
data a :- b = a :- b
data (:--) a b = (:--) a b

f5 :: (
       Int :-- Float,
       (:--) Int Float,
       Test.(:--) Int Float,
--       Int `(:--)` Float,
       Int `Test.(:--)` Float
       )
f5 = (
       1 :-- 1.0, 
       (:--) 1 1.0, 
       Test.(:--) 1 1.0, 
--       1 `(:--)` 1.0,
       1 `Test.(:--)` 1.0
     )

-- sections
subtract :: Int -> Int -> Int
subtract y = (`Prelude.(-)` y)

minus :: Int -> Int -> Int
minus y = (y `Prelude.(-)`)

-- infix qualified ops retain their precedence and associativity
f1 :: Int -> Int -> Int
f1 x y = x `Prelude.(*)` y `Prelude.(+)` x `Prelude.(*)` y

-- misc tests
f2 = [False..] -- means qualified '.' in Haskell 98

{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, ApplicativeDo #-}
module Main where

import Control.Applicative
import Text.PrettyPrint as PP

(a:b:c:d:e:f:g:h:_) = map (\c -> doc [c]) ['a'..]

-- a | b
test1 :: M ()
test1 = do
  x1 <- a
  x2 <- b
  const (return ()) (x1,x2)

-- no parallelism
test2 :: M ()
test2 = do
  x1 <- a
  x2 <- const g x1
  const (return ()) (x1,x2)

-- a | (b;g) | e
test3 :: M ()
test3 = do
  x1 <- a
  x2 <- b
  x3 <- const g x2
  x4 <- e
  return () `const` (x1,x2,x3,x4)

-- (a ; (b | g)) | c
-- or
-- ((a | b); g) | c
test4 :: M ()
test4 = do
  x1 <- a
  x2 <- b
  x3 <- const g x1
  x4 <- c
  return () `const` (x2,x3,x4)

-- (a | b | c); (g | h)
test5 :: M ()
test5 = do
  x1 <- a
  x2 <- b
  x3 <- c
  x4 <- const g x1
  x5 <- const h x3
  return () `const` (x3,x4,x5)

-- b/c in parallel, e/f in parallel
-- a; (b | (c; (d; (e | (f; g)))))
test6 :: M ()
test6 = do
  x1 <- a
  x2 <- const b x1
  x3 <- const c x1
  x4 <- const d x3
  x5 <- const e x4
  x6 <- const f x4
  x7 <- const g x6
  return () `const` (x1,x2,x3,x4,x5,x6,x7)

-- (a | b); (c | d)
test7 :: M ()
test7 = do
  x1 <- a
  x2 <- b
  x3 <- const c x1
  x4 <- const d x2
  return () `const` (x3,x4)

-- a; (b | c | d)
--
-- alternative (but less good):
-- ((a;b) | c); d
test8 :: M ()
test8 = do
  x1 <- a
  x2 <- const b x1
  x3 <- c
  x4 <- const d x1
  return () `const` (x2,x3,x4)

-- test that Lets don't get in the way
-- ((a | (b; c)) | d) | e
test9 :: M ()
test9 = do
  x1 <- a
  let x = doc "x"  -- this shouldn't get in the way of grouping a/b
  x2 <- b
  x3 <- const c x2
  x4 <- d
  x5 <- e
  let y = doc "y"
  return ()

-- ((a | b) ; (c | d)) | e
test10 :: M ()
test10 = do
  x1 <- a
  x2 <- b
  let z1 = (x1,x2)
  x3 <- const c x1
  let z2 = (x1,x2)
  x4 <- const d z1
  x5 <- e
  return (const () (x3,x4,x5))

-- (a | b)
-- This demonstrated a bug in RnExpr.segments (#11612)
test11 :: M ()
test11 = do
  x1 <- a
  let x2 = x1
  x3 <- b
  let x4 = c
      x5 = x4
  return (const () (x1,x2,x3,x4))

-- (a | (b ; c))
-- The strict pattern match forces (b;c), but a can still be parallel (#13875)
test12 :: M ()
test12 = do
  x1 <- a
  () <- b
  x2 <- c
  return (const () (x1,x2))

main = mapM_ run
 [ test1
 , test2
 , test3
 , test4
 , test5
 , test6
 , test7
 , test8
 , test9
 , test10
 , test11
 , test12
 ]

-- Testing code, prints out the structure of a monad/applicative expression

newtype M a = M (Bool -> (Maybe Doc, a))

maybeParen True d = parens d
maybeParen _ d = d

run :: M a -> IO ()
run (M m) = print d where (Just d,_) = m False

instance Functor M where
  fmap f m = m >>= return . f

instance Applicative M where
  pure a = M $ \_ -> (Nothing, a)
  M f <*> M a = M $ \p ->
    let (Just d1, f') = f True
        (Just d2, a') = a True
    in
        (Just (maybeParen p (d1 <+> char '|' <+> d2)), f' a')

instance Monad M where
  return = pure
  M m >>= k = M $ \p ->
    let (d1, a) = m True
        (d2, b) = case k a of M f -> f True
    in
    case (d1,d2) of
      (Nothing,Nothing) -> (Nothing, b)
      (Just d, Nothing) -> (Just d, b)
      (Nothing, Just d) -> (Just d, b)
      (Just d1, Just d2) -> (Just (maybeParen p (d1 PP.<> semi <+> d2)), b)

doc :: String -> M ()
doc d = M $ \_ -> (Just (text d), ())

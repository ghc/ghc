{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, ApplicativeDo #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}
module Main where

import Control.Applicative
import Text.PrettyPrint as PP

(a:b:c:d:e:f:g:h:_) = map (\c -> doc [c]) ['a'..]

-- This one requires -foptimal-applicative-do to find the best solution
-- ((a; b) | (c; d)); e
test1 :: M ()
test1 = do
  x1 <- a
  x2 <- const b x1
  x3 <- c
  x4 <- const d x3
  x5 <- const e (x1,x4)
  return (const () x5)

-- (a | c); (b | d); e
test2 :: M ()
test2 = do
  x1 <- a
  x3 <- c
  x2 <- const b x1
  x4 <- const d x3
  x5 <- const e (x1,x4)
  return (const () x5)

main = mapM_ run
 [ test1
 , test2
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

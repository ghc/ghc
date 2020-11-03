{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, ApplicativeComprehensions,
             RebindableSyntax  #-}
{- This module is mostly a copy of ado001 but tests that all those
   functions work when we have RebindableSyntax enabled
-}
module Main where

import Prelude hiding (return, (>>=), pure, (<*>), fmap)
import Data.Foldable (traverse_)
import Text.PrettyPrint as PP

(a:b:c:d:e:f:g:h:_) = map (\c -> doc [c]) ['a'..]

main = traverse_ run
 [ -- a | b
   [const () (x1, x2) | x1 <- a, x2 <- b]

 , -- monadic
   -- a
   [const () x2 | x1 <- pure a, x2 <- x1]

 , -- no parallelism
   -- a; g
   [const () (x1, x2) | x1 <- a, x2 <- const g x1]

 , -- a | (b;g) | e
   [const () (x1, x2, x3, x4) | x1 <- a, x2 <- b, x3 <- const g x2, x4 <- e]

 , -- (a ; (b | g)) | c
   -- or
   -- ((a | b); g) | c
   [const () (x2, x3, x4) | x1 <- a, x2 <- b, x3 <- const g x1, x4 <- c]

 , -- (a | b | c); (g | h)
   [const () (x3, x4, x5) | x1 <- a, x2 <- b, x3 <- c, x4 <- const g x1, x5 <- const h x3]

 , -- b/c in parallel, e/f in parallel
   -- a; (b | (c; (d; (e | (f; g)))))
   [const () (x1, x2, x3, x4, x5, x6, x7)
   | x1 <- a, x2 <- const b x1, x3 <- const c x1, x4 <- const d x3
   , x5 <- const e x4, x6 <- const f x4, x7 <- const g x6]

 , -- (a | b); (c | d)
   [const () (x3, x4) | x1 <- a, x2 <- b, x3 <- const c x1, x4 <- const d x2]

 , -- a; (b | c | d)
   --
   -- alternative (but less good):
   -- ((a;b) | c); d
   [const () (x2, x3, x4) | x1 <- a, x2 <- const b x1, x3 <- c, x4 <- const d x1]

 , -- test that Lets don't get in the way
   -- ((a | (b; c)) | d) | e
   [()
   | x1 <- a
   , let x = doc "x"  -- this shouldn't get in the way of grouping a/b
   , x2 <- b, x3 <- const c x2, x4 <- d, x5 <- e, let y = doc "y"]

 , -- ((a | b) ; (c | d)) | e
   [const () (x3, x4, x5)
   | x1 <- a, x2 <- b, let z1 = (x1,x2)
   , x3 <- const c x1, let z2 = (x1,x2), x4 <- const d z1, x5 <- e]

 , -- (a | b)
   -- This demonstrated a bug in RnExpr.segments (#11612)
   [const () (x1, x2, x3, x4)
   | x1 <- a, let x2 = x1, x3 <- b
   , let x4 = c
         x5 = x4]

 , -- (a | (b ; c))
   -- The strict pattern match forces (b;c), but a can still be parallel (#13875)
   [const () (x1, x2) | x1 <- a, () <- b, x2 <- c]
 ]

-- Testing code, prints out the structure of a monad/applicative expression

newtype M a = M (Bool -> (Maybe Doc, a))

maybeParen True d = parens d
maybeParen _ d = d

run :: M a -> IO ()
run (M m) = print d where (Just d,_) = m False

fmap f = (>>= pure . f)

join :: M (M a) -> M a
join = (>>= id)

pure a = M $ \_ -> (Nothing, a)

return = pure

M f <*> M a = M $ \p ->
  let (Just d1, f') = f True
      (Just d2, a') = a True
  in
      (Just (maybeParen p (d1 <+> char '|' <+> d2)), f' a')

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

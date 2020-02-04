{-# LANGUAGE ApplicativeComprehensions #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Applicative
import Data.Foldable
import Text.PrettyPrint as PP

(a:b:c:d:e:f:g:h:_) = doc . pure <$> ['a'..]

main = traverse_ run
 [ -- a | b
   [pure () (x1, x2) | x1 <- a, x2 <- b]

 , -- monadic
   -- a
   [pure () x2 | x1 <- pure a, x2 <- x1]

 , -- no parallelism
   -- a; g
   [pure () (x1, x2) | x1 <- a, x2 <- pure g x1]

 , -- a | (b;g) | e
   [pure () (x1, x2, x3, x4) | x1 <- a, x2 <- b, x3 <- pure g x2, x4 <- e]

 , -- (a ; (b | g)) | c
   -- or
   -- ((a | b); g) | c
   [pure () (x2, x3, x4) | x1 <- a, x2 <- b, x3 <- pure g x1, x4 <- c]

 , -- (a | b | c); (g | h)
   [pure () (x3, x4, x5) | x1 <- a, x2 <- b, x3 <- c, x4 <- pure g x1, x5 <- pure h x3]

 , -- b/c in parallel, e/f in parallel
   -- a; (b | (c; (d; (e | (f; g)))))
   [pure () (x1, x2, x3, x4, x5, x6, x7)
   | x1 <- a, x2 <- pure b x1, x3 <- pure c x1, x4 <- pure d x3
   , x5 <- pure e x4, x6 <- pure f x4, x7 <- pure g x6]

 , -- (a | b); (c | d)
   [pure () (x3, x4) | x1 <- a, x2 <- b, x3 <- pure c x1, x4 <- pure d x2]

 , -- a; (b | c | d)
   --
   -- alternative (but less good):
   -- ((a;b) | c); d
   [pure () (x2, x3, x4) | x1 <- a, x2 <- const b x1, x3 <- c, x4 <- const d x1]

 , -- test that Lets don't get in the way
   -- ((a | (b; c)) | d) | e
   [()
   | x1 <- a
   , let x = doc "x"  -- this shouldn't get in the way of grouping a/b
   , x2 <- b, x3 <- const c x2, x4 <- d, x5 <- e, let y = doc "y"]

 , -- ((a | b) ; (c | d)) | e
   [pure () (x3, x4, x5)
   | x1 <- a, x2 <- b, let z1 = (x1,x2)
   , x3 <- const c x1, let z2 = (x1,x2), x4 <- const d z1, x5 <- e]

 , -- (a | b)
   -- This demonstrated a bug in RnExpr.segments (#11612)
   [pure () (x1, x2, x3, x4)
   | x1 <- a, let x2 = x1, x3 <- b
   , let x4 = c
         x5 = x4]

 , -- (a | (b ; c))
   -- The strict pattern match forces (b;c), but a can still be parallel (#13875)
   [pure () (x1, x2) | x1 <- a, () <- b, x2 <- c]
 ]

-- Testing code, prints out the structure of a monad/applicative expression

newtype M a = M (Bool -> (Maybe Doc, a))
  deriving (Functor)

maybeParen True d = parens d
maybeParen _ d = d

run :: M a -> IO ()
run (M m) = print d where (Just d,_) = m False

instance Applicative M where
  pure a = M $ \_ -> (Nothing, a)
  M f <*> M a = M $ \p ->
    let (Just d1, f') = f True
        (Just d2, a') = a True
    in
        (Just (maybeParen p (d1 <+> char '|' <+> d2)), f' a')

instance Monad M where
  M m >>= k = M $ \p ->
    let (d1, a) = m True
        (d2, b) = case k a of M f -> f True
    in
    (case (d1,d2) of
      (Nothing,Nothing) -> Nothing
      (Just d, Nothing) -> Just d
      (Nothing, Just d) -> Just d
      (Just d1, Just d2) -> Just (maybeParen p $ d1 PP.<> semi <+> d2), b)

doc :: String -> M ()
doc d = M $ \_ -> (Just (text d), ())

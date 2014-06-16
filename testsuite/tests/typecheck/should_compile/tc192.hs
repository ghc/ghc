{-# LANGUAGE Arrows, CPP, TypeOperators #-}

-- Test infix type notation and arrow notation

module Test where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow

-- For readability, I use infix notation for arrow types.  I'd prefer the
-- following, but GHC doesn't allow operators like "-=>" as type
-- variables.
-- 
-- comp1 :: Arrow (-=>) => b-=>c -> c-=>d -> b-=>d


comp1 :: Arrow to => b `to` c -> c `to` d -> b `to` d
comp1 f g = proc x -> do
            b <- f -< x
            g -< b

-- arrowp produces
-- comp1 f g = (f >>> g)

comp :: Arrow to => (b `to` c, c `to` d) `to` (b `to` d)
comp = arr (uncurry (>>>))

-- app :: Arrow to => (b c, b) `to` c

type R = Float
type I = Int

z1,z2 :: Arrow to => I `to` (R `to` R)
z1 = undefined 
z2 = z1

z3 :: Arrow to => (I,I) `to` (R `to` R,R `to` R)
z3 = z1 *** z2

z4 :: Arrow to => (I,I) `to` (R `to` R)
z4 = z3 >>> comp

comp4,comp5 :: Arrow to =>
  b `to` (c `to` d) -> e `to` (d `to` f) -> (b,e) `to` (c `to` f)

comp4 g f = proc (b,e) -> do
            g' <- g -< b
            f' <- f -< e
            returnA -< (g' >>> f')

comp5 g f = (g *** f) >>> comp

lam,lam2 :: Arrow to => (e,b) `to` c -> e `to` (b `to` c)

lam f = arr $ \ e -> arr (pair e) >>> f

pair a b = (a,b)

-- I got the definition lam above by starting with

lam2 f = proc e ->
         returnA -< (proc b -> do
                     c <- f -< (e,b)
                     returnA -< c)

-- I desugared with  the arrows preprocessor, removed extra parens and
-- renamed "arr" to "pure", to get
-- 
--  lam f = pure (\ e -> pure (\ b -> (e, b)) >>> f)

-- Note that lam is arrow curry

-- curry :: ((e,b) -> c) -> (e -> b -> c)

-- All equivalent:

curry1 f e b = f (e,b)

curry2 f = \ e -> \ b -> f (e,b)

curry3 f = \ e -> f . (\ b -> (e,b))

curry4 f = \ e -> f . (pair e)



comp6 :: Arrow to => b `to` (c `to` d) -> e `to` (d `to` f)
                  -> b `to` (e `to` (c `to` f))
comp6 g f = lam $ comp5 g f 

-- What about uncurrying?

-- uncurryA :: Arrow to => b `to` (c `to` d)
--                     -> (b,c) `to` d
-- uncurryA f = proc (b,c) -> do
--              f' <- f -< b
--              returnA -< f' c

-- Why "lam" instead of "curryA" (good name also): so I can use Arrows
-- lambda notation, similar to

compF g f = \ b e -> g b . f e

-- But I haven't figured out how to.

-- comp7 :: Arrow to => b `to` (c `to` d) -> e `to` (d `to` f)
--                   -> b `to` (e `to` (c `to` f))
-- comp7 g f = proc b -> proc e -> do
--             g' <- g -< b
--             f' <- f -< e
--             returnA -< (g' >>> f')

-- Try "(| lam \ b -> ... |)" in the FOP arrows chapter
-- cmd ::= form exp cmd1 ... cmdn.  Parens if nec

-- (| lam (\ b -> undefined) |)

-- Oh!  The arrow syntax allows bindings with *infix* operators.  And I
-- don't know how to finish comp7.

-- Uncurried forms:

comp8 :: Arrow to => (b,c) `to` d -> (e,d) `to` k -> (b,c,e) `to` k
comp8 g f = proc (b,c,e) -> do
            d <- g -< (b,c)
            f -< (e,d)

-- This looks like straightforward `to` translation.  With insertions of
-- curry & uncurry operators, it'd probably be easy to handle curried
-- definitions as well.

-- Simpler example, for experimentation

comp9 :: Arrow to => (c,d) `to` e -> b `to` d -> (b,c) `to` e
comp9 g f = proc (b,c) -> do
            d <- f -< b
            g -< (c,d)
 
-- Desugared:

comp9' :: Arrow to => (c,d) `to` e -> b `to` d -> (b,c) `to` e
comp9' g f = first f >>> arr (\ (d,c) -> (c,d)) >>> g



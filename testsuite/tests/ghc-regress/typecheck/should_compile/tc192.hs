{-# OPTIONS -fglasgow-exts -farrows #-}

-- Test infix type notation and arrow notation

module Test where

import Control.Arrow

-- For readability, I use infix notation for arrow types.  I'd prefer the
-- following, but GHC doesn't allow operators like "-=>" as type
-- variables.
-- 
-- comp1 :: Arrow (-=>) => b-=>c -> c-=>d -> b-=>d


comp1 :: Arrow (~>) => b~>c -> c~>d -> b~>d
comp1 f g = proc x -> do
            b <- f -< x
            g -< b

-- arrowp produces
-- comp1 f g = (f >>> g)

comp :: Arrow (~>) => (b~>c, c~>d)~>(b~>d)
comp = pure (uncurry (>>>))

-- app :: Arrow (~>) => (b c, b)~>c

type R = Float
type I = Int

z1,z2 :: Arrow (~>) => I~>(R~>R)
z1 = undefined 
z2 = z1

z3 :: Arrow (~>) => (I,I)~>(R~>R,R~>R)
z3 = z1 *** z2

z4 :: Arrow (~>) => (I,I)~>(R~>R)
z4 = z3 >>> comp

comp4,comp5 :: Arrow (~>) =>
  b~>(c~>d) -> e~>(d~>f) -> (b,e)~>(c~>f)

comp4 g f = proc (b,e) -> do
            g' <- g -< b
            f' <- f -< e
            returnA -< (g' >>> f')

comp5 g f = (g *** f) >>> comp

lam,lam2 :: Arrow (~>) => (e,b)~>c -> e~>(b~>c)

lam f = pure $ \ e -> pure (pair e) >>> f

pair = (,)

-- I got the definition lam above by starting with

lam2 f = proc e ->
         returnA -< (proc b -> do
                     c <- f -< (e,b)
                     returnA -< c)

-- I desugared with  the arrows preprocessor, removed extra parens and
-- renamed "arr" (~>) "pure", (~>) get
-- 
--  lam f = pure (\ e -> pure (\ b -> (e, b)) >>> f)

-- Note that lam is arrow curry

-- curry :: ((e,b) -> c) -> (e -> b -> c)

-- All equivalent:

curry1 f e b = f (e,b)

curry2 f = \ e -> \ b -> f (e,b)

curry3 f = \ e -> f . (\ b -> (e,b))

curry4 f = \ e -> f . (pair e)



comp6 :: Arrow (~>) => b~>(c~>d) -> e~>(d~>f)
                  -> b~>(e~>(c~>f))
comp6 g f = lam $ comp5 g f 

-- What about uncurrying?

-- uncurryA :: Arrow (~>) => b~>(c~>d)
--                     -> (b,c)~>d
-- uncurryA f = proc (b,c) -> do
--              f' <- f -< b
--              returnA -< f' c

-- Why "lam" instead of "curryA" (good name also): so I can use Arrows
-- lambda notation, similar (~>)

compF g f = \ b e -> g b . f e

-- But I haven't figured out how (~>).

-- comp7 :: Arrow (~>) => b~>(c~>d) -> e~>(d~>f)
--                   -> b~>(e~>(c~>f))
-- comp7 g f = proc b -> proc e -> do
--             g' <- g -< b
--             f' <- f -< e
--             returnA -< (g' >>> f')

-- Try "(| lam \ b -> ... |)" in the FOP arrows chapter
-- cmd ::= form exp cmd1 ... cmdn.  Parens if nec

-- (| lam (\ b -> undefined) |)

-- Oh!  The arrow syntax allows bindings with *infix* operators.  And I
-- don't know how (~>) finish comp7.

-- Uncurried forms:

comp8 :: Arrow (~>) => (b,c)~>d -> (e,d)~>k -> (b,c,e)~>k
comp8 g f = proc (b,c,e) -> do
            d <- g -< (b,c)
            f -< (e,d)

-- This looks like straightforward~>translation.  With insertions of
-- curry & uncurry operators, it'd probably be easy (~>) handle curried
-- definitions as well.

-- Simpler example, for experimentation

comp9 :: Arrow (~>) => (c,d)~>e -> b~>d -> (b,c)~>e
comp9 g f = proc (b,c) -> do
            d <- f -< b
            g -< (c,d)
 
-- Desugared:

comp9' :: Arrow (~>) => (c,d)~>e -> b~>d -> (b,c)~>e
comp9' g f = first f >>> arr (\ (d,c) -> (c,d)) >>> g



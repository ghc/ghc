{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -ddump-types -dsuppress-module-prefixes #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

-- See #1456

-- The key thing here is that foo should get the type
--     foo :: forall s b chain. (Zork s (Z [Char]) b)
--         => Q s (Z [Char]) chain -> ST s ()

-- Note the quantification over 'b', which was previously
-- omitted; see Note [Important subtlety in oclose] in GHC.Tc.Instance.FunDeps
-- (Note removed in ecddaca17dccbe1d0b56220d838fce8bc4b97884, but you can
-- find it in the history)

-- June 2021: marking this test as should_fail again.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/20064

module ShouldCompile where

import GHC.ST

data Q s a chain = Node s a chain

data Z a = Z a

s :: Q s (Z [Char]) chain -> Q s (Z [Char]) chain
s = undefined

class  Zork s a b | a -> b where
  huh :: Q s a chain -> ST s ()

--foo :: Zork s (Z [Char]) b => Q s (Z [Char]) chain -> ST s ()
foo b = huh (s b)

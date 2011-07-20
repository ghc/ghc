{-# OPTIONS_GHC -ddump-types #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- See Trac #1456

-- The key thing here is that foo should get the type
--     foo :: forall b s t1. (Zork s (Z [Char]) b)
--	   => Q s (Z [Char]) t1 -> ST s ()

-- Note the quantification over 'b', which was previously
-- omitted; see Note [Important subtlety in oclose] in FunDeps


module ShouldCompile where

import GHC.ST

data Q s a chain = Node s a chain

data Z a  = Z  a

s :: Q t (Z [Char]) t1 -> Q t (Z [Char]) t1
s = undefined

class  Zork s a b | a -> b where
  huh :: Q s a chain ->  ST s ()

foo b = huh (s b)


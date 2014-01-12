{-# LANGUAGE NoMonoPatBinds #-}
	-- Disable experimetal monomorphic pattern bindings

-- Nasty test for type signatures
-- In both groups of declarations below, the type variables 'a' and 'b'
-- end up being unified together.

module ShouldCompile where

-------------
  x :: a
  x = z `asTypeOf` y

  y :: b
  y = z

  z = x
-------------
  p :: [a]
  q :: b
  (p,q,r) = ([q,r], r, head p)

-------------
  t :: a
  u :: b
  (t,u,v) = (v,v,t)

--!!! cc004 -- ccall with synonyms, polymorphic type variables and user type variables.
module Test where

import PreludeGlaST

-- Since I messed up the handling of polymorphism originally, I'll
-- explicitly test code with UserSysTyVar (ie an explicit polymorphic
-- signature)

foo = _ccall_ f	`thenADR` \ a -> returnPrimIO (a + 1)
 where 
   thenADR :: PrimIO a -> (a -> PrimIO b) -> PrimIO b
   m `thenADR` k  = \ s -> case m s of
                         (a,t) -> k a t

-- and with a PolySysTyVar (ie no explicit signature)

bar = _ccall_ f	`thenADR` \ a -> returnPrimIO (a + 1)
 where 
   -- thenADR :: PrimIO a -> (a -> PrimIO b) -> PrimIO b
   m `thenADR` k  = \ s -> case m s of
                         (a,t) -> k a t

-- and with a type synonym

type INT = Int
barfu :: PrimIO INT
barfu = _ccall_ b


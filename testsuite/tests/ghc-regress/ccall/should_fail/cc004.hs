-- !!! cc004 -- ccall with synonyms, polymorphic type variables and user type variables.
module Test where

-- Since I messed up the handling of polymorphism originally, I'll
-- explicitly test code with UserSysTyVar (ie an explicit polymorphic
-- signature)

foo = _ccall_ f	`thenADR` \ a -> return (a + 1)
 where 
   thenADR :: IO a -> (a -> IO b) -> IO b
   thenADR = (>>=)

-- and with a PolySysTyVar (ie no explicit signature)

bar = _ccall_ f	`thenADR` \ a -> return (a + 1)
 where 
   -- thenADR :: IO a -> (a -> IO b) -> IO b
   thenADR = (>>=)

-- and with a type synonym

type INT = Int
barfu :: IO INT
barfu = _ccall_ b

-- !! INLINE on recursive functions.
{-
Date: Thu, 8 Dec 94 11:38:24 GMT
From: Julian Seward (DRL PhD) <sewardj@computer-science.manchester.ac.uk>
Message-Id: <9412081138.AA16652@rdf009.cs.man.ac.uk>
To: partain@dcs.gla.ac.uk
-}
module ShouldCompile where

type IMonad a
   = IMonadState -> IMonadReturn a

data IMonadReturn a
   = IMonadOk	IMonadState a
   | IMonadFail	IMonadState String

type IMonadState
   = Int


returnI r = \s0 -> IMonadOk   s0 r

failI msg = \s0 -> IMonadFail s0 msg

thenI m k
   = \s0 -> case m s0 of
               IMonadFail s1 msg -> IMonadFail s1 msg
               IMonadOk s1 r1    -> k r1 s1
   
tickI n = \s0 -> IMonadOk (s0+n) ()

mapI f [] = returnI []
mapI f (x:xs) = f x           `thenI` ( \ fx ->
                mapI f xs     `thenI` ( \ fxs ->
                returnI (fx:fxs)
                ))

{-# INLINE returnI #-}
{-# INLINE failI #-}
{-# INLINE thenI #-}
{-# INLINE tickI #-}
-- {-# INLINE mapI #-}

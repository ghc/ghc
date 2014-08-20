{-# LANGUAGE RankNTypes, GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -O2 #-}

-- This one make SpecConstr generate bogus code (hence -O2), 
-- with a lint error, in GHC 6.4.1
-- C.f. http://ghc.haskell.org/trac/ghc/ticket/737

module ShouldCompile where

 data IHandler st where
     IHandler :: forall st ev res. 
		 Serialize (TxContext ev) => String -> IO ev 
		 -> (res -> IO ()) -> Ev st ev res -> IHandler st

 data Ev st ev res  = Ev
 data TxContext evt = TxContext
 data TxConfig      = TxConfig
 data M    a        = M a

 class Serialize a where
 instance Serialize a => Serialize (TxContext a)
 instance Serialize Int
 instance Serialize ()

 data IHR st = forall res ev. Serialize (TxContext ev) => IHR (TxContext ev)


 runHandler :: M (IHR st) ->  IHandler st -> IO ()
 runHandler queue ih@(IHandler tstring inp out run) = runHandler queue ih


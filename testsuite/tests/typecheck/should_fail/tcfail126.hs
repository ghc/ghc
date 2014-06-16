{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

-- An interesting interaction of universals and existentials, prompted by 
-- http://www.haskell.org/pipermail/haskell-cafe/2004-October/007160.html
--
-- Note the nested pattern-match in runProg; tc183 checks the
-- non-nested version

-- 3 Sept 2010: with the new typechecker, this one succeeds

module Foo  where

import Control.Monad.Trans

data Bar m 
  = forall t. (MonadTrans t, Monad (t m)) 
	   => Bar (t m () -> m ()) (t m Int)

data Foo = Foo (forall m. Monad m => Bar m)

runProg :: Foo -> IO ()
runProg (Foo (Bar run op)) = run (prog op)
-- This nested match "ought" to work; because
--    runProg (Foo b) = case b of
--			    Bar run op -> run (prog op)
-- does work. But the interactions with GADTs and
-- desugaring defeated me, and I removed (in GHC 6.4) the ability
-- to instantiate functions on the left

prog :: (MonadTrans t, Monad (t IO)) => a -> t IO ()
prog x = error "urk"

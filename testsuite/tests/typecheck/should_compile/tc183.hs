{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

-- An interesting interaction of universals and existentials, prompted by 
-- http://www.haskell.org/pipermail/haskell-cafe/2004-October/007160.html
--
-- Note the non-nested pattern-match in runProg; tcfail126 checks the
-- nested pattern match

module Foo  where

import Control.Monad.Trans

data Bar m 
  = forall t. (MonadTrans t, Monad (t m)) 
	   => Bar (t m () -> m ()) (t m Int)

data Foo = Foo (forall m. Monad m => Bar m)

runProg :: Foo -> IO ()
runProg (Foo b) = case b of
		    Bar run op -> run (prog op)
	-- You can't say runProg (Foo (Bar run op));
	-- see tcfail126

prog :: (MonadTrans t, Monad (t IO)) => a -> t IO ()
prog x = error "urk"

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             ExistentialQuantification, FlexibleContexts #-}

{-	Email 30 Jan 2006

> the attached program compiles under GHC, but not with Hugs. as far as
> i see, Hugs don't use dependencies in class headers to figure out that
> there is only one "vMkIOError" that can be called in the last
> definition

OK, I think it's a bug (though the example is bizarre).  Sadly Hugs's
support for FDs is rough around the edges (and unlikely to improve
soon).

-}

module ShoudlCompile where

	class (Monad m) => Stream m h | h->m where
		vMkIOError :: h -> Int

	data BinHandle = forall h . Stream IO h => BinH h

	instance Stream IO BinHandle where
		vMkIOError (BinH h) = vMkIOError h

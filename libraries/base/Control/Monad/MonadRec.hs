-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.MonadRec
-- Copyright   :  (c) Oregon Graduate Institute of Science and Technology, 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org, erkok@cse.ogi.edu
-- Stability   :  experimental
-- Portability :  portable 
--
-- Declaration of the MonadRec class, and instances for
-- maybe, list, IO, strict state, and lazy state monads
--
-- Note	: There's a clear overlap with the Control.Monad.Fix
--        module, as they basically define the same structure
--        with different names. The "MonadRec" name is kept
--        here basically for compatibility with the current Hugs
--        implementation. (Note that this duplication also exist 
--        in the current Hugs release as well.)
--
-----------------------------------------------------------------------------

module Control.Monad.MonadRec (
	MonadRec(mfix)
  ) where
    
import Prelude 
import qualified Control.Monad.ST.Lazy as LazyST
import qualified Control.Monad.ST as ST
import System.IO

fix :: (a -> a) -> a
fix f = let a = f a in a

-- The MonadRec class definition

class Monad m => MonadRec m where
    mfix :: (a -> m a) -> m a 

-- Instances of MonadRec

-- Maybe:
instance MonadRec Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x

-- List:
instance MonadRec [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (tail . f)

-- IO:
instance MonadRec IO where
    mfix = fixIO 

-- Lazy State:
instance MonadRec (LazyST.ST s) where
    mfix = LazyST.fixST
    
-- Strict State:
instance MonadRec (ST.ST s) where
    mfix = ST.fixST

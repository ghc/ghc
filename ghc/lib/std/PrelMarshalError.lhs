% -----------------------------------------------------------------------------
% $Id: PrelMarshalError.lhs,v 1.1 2001/01/11 17:25:57 simonmar Exp $
%
% (c) The FFI task force, 2000
%

Marshalling support: Handling of common error conditions

\begin{code}

module PrelMarshalError (

  -- throw an exception on specific return values
  --
  throwIf,       -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO a
  throwIf_,      -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO ()
  throwIfNeg,    -- :: (Ord a, Num a) 
	         -- =>                (a -> String) -> IO a       -> IO a
  throwIfNeg_,   -- :: (Ord a, Num a)
	         -- =>                (a -> String) -> IO a       -> IO ()
  throwIfNull,   -- ::                String        -> IO (Ptr a) -> IO (Ptr a)

  -- discard return value
  --
  void           -- IO a -> IO ()
) where

import PrelPtr
import PrelBase

-- exported functions
-- ------------------

-- guard an IO operation and throw an exception if the result meets the given
-- predicate 
--
-- * the second argument computes an error message from the result of the IO
--   operation
--
throwIf                 :: (a -> Bool) -> (a -> String) -> IO a -> IO a
throwIf pred msgfct act  = 
  do
    res <- act
    (if pred res then ioError . userError . msgfct else return) res

-- like `throwIf', but discarding the result
--
throwIf_                 :: (a -> Bool) -> (a -> String) -> IO a -> IO ()
throwIf_ pred msgfct act  = void $ throwIf pred msgfct act

-- guards against negative result values
--
throwIfNeg :: (Ord a, Num a) => (a -> String) -> IO a -> IO a
throwIfNeg  = throwIf (< 0)

-- like `throwIfNeg', but discarding the result
--
throwIfNeg_ :: (Ord a, Num a) => (a -> String) -> IO a -> IO ()
throwIfNeg_  = throwIf_ (< 0)

-- guards against null pointers
--
throwIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNull  = throwIf (== nullPtr) . const

-- discard the return value of an IO action
--
void     :: IO a -> IO ()
void act  = act >> return ()

\end{code}

{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

module T21654 ( patternToQ ) where

import Data.Functor.Identity

mergeNullViews :: () -> ()
mergeNullViews _ = ()

patternToQ :: ()
patternToQ = runIdentity $ combineSeq
  where
  combineSeq :: Identity ()
  combineSeq = mdo  -- changing this to 'do' fixes the problem
    q <- Identity ()
    return $ mergeNullViews q

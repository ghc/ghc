module Supercompile.Termination.Generaliser where

import Supercompile.Core.Syntax   (Var)
import Supercompile.Core.Renaming (Out)

import Supercompile.Evaluator.Syntax

import Supercompile.Utilities (Nat, Tagged, tag, Tag, tagInt, injectTag, FinMap, FinSet)

import Outputable

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS


data Generaliser = Generaliser {
    generaliseStackFrame  :: Tagged StackFrame -> Bool,
    generaliseHeapBinding :: Out Var -> HeapBinding -> Bool
  }

generaliseNothing :: Generaliser
generaliseNothing = Generaliser (\_ -> False) (\_ _ -> False)

generaliserFromGrowing :: (a -> FinMap Nat) -> a -> a -> Generaliser
generaliserFromGrowing extract x y | IS.null generalise_what = pprTrace "no growth" (ppr generalise_what) generaliseNothing
                                   | otherwise               = generaliserFromFinSet generalise_what
  where generalise_what = IM.keysSet (IM.filter id (IM.intersectionWith (<) (extract x) (extract y)))

generaliserFromFinSet :: FinSet -> Generaliser
generaliserFromFinSet generalise_what = Generaliser {
      generaliseStackFrame  = \kf   -> should_generalise (stackFrameTag' kf),
      generaliseHeapBinding = \_ hb -> maybe False (should_generalise . pureHeapBindingTag') $ heapBindingTag hb
    }
  where should_generalise tg = IS.member (tagInt tg) generalise_what


{-# INLINE pureHeapBindingTag' #-}
pureHeapBindingTag' :: Tag -> Tag
pureHeapBindingTag' = injectTag 5

{-# INLINE stackFrameTag' #-}
stackFrameTag' :: Tagged StackFrame -> Tag
stackFrameTag' = injectTag 3 . tag

{-# INLINE qaTag' #-}
qaTag' :: Anned QA -> Tag
qaTag' = injectTag 2 . annedTag

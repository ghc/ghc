module Supercompile.Termination.TagBag (
        embedWithTagBags,
        TagBag, stateTags
    ) where

import Supercompile.Termination.Combinators
import Supercompile.Termination.Generaliser

import Supercompile.Evaluator.Syntax

import Supercompile.Utilities
import Supercompile.StaticFlags (TagBagType(..))

import qualified Data.Foldable as Foldable
import qualified Data.IntMap as IM
import qualified Data.Map as M


type TagBag = FinMap Nat

embedWithTagBags :: TagBagType -> (TTest State, State -> State -> Generaliser)
embedWithTagBags tbt = if tagBagPairwiseGrowth tbt then embedWithTagBags' (zippableT wellOrderedT) else embedWithTagBags' (cofmap Foldable.sum wellOrderedT)

embedWithTagBags' :: (forall f. (Foldable f, Zippable f, Finite (f ())) => TTest (f Nat))
                  -> (TTest State, State -> State -> Generaliser)
embedWithTagBags' nats = (cofmap stateTags (equalDomainT nats), generaliserFromGrowing stateTags)

-- NB: I try very hard to avoid creating intermediate tag bags in this function because it
-- accounts for a staggering fraction of the supercompiler's total allocation
stateTags :: State -> TagBag
stateTags (_, Heap h _, k, qa) = -- traceRender ("stateTags (TagBag)", M.map heapBindingTagBag h, map stackFrameTag' k, qaTag' qa) $
                                      -- traceRender ("stateTags:heap (TagBag)", M.map heapBindingTag h) $
                                      -- (\res -> traceRender ("stateTags (TagBag)", res) res) $
                                      pureHeapTagBag h (stackTagBag k (singletonTagBag (qaTag' qa) IM.empty))
  where
    heapBindingTagBag :: HeapBinding -> TagBag -> TagBag
    heapBindingTagBag = maybe id (singletonTagBag . pureHeapBindingTag') . heapBindingTag
      
    pureHeapTagBag :: PureHeap -> TagBag -> TagBag
    pureHeapTagBag = flip $ M.fold heapBindingTagBag -- NB: really a foldr, but the M.foldr synonym was added in a later version of containers
 
    stackTagBag :: Stack -> TagBag -> TagBag
    stackTagBag = flip $ trainCarFoldr stackFrameTagBag

    stackFrameTagBag :: Tagged StackFrame -> TagBag -> TagBag
    stackFrameTagBag = singletonTagBag . stackFrameTag'
 
    singletonTagBag :: Tag -> TagBag -> TagBag
    singletonTagBag (TG i occs) = IM.insert (unFin i) occs

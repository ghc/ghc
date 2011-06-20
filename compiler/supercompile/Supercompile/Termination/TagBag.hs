module Supercompile.Termination.TagBag (
        embedWithTagBags
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

embedWithTagBags' :: (forall f. (Foldable.Foldable f, Zippable f, Finite (f ())) => TTest (f Nat))
                  -> (TTest State, State -> State -> Generaliser)
embedWithTagBags' nats = (cofmap stateTags (equalDomainT nats), generaliserFromGrowing stateTags)
  where
    stateTags :: State -> TagBag
    stateTags (_, Heap h _, k, qa) = -- traceRender ("stateTags (TagBag)", M.map heapBindingTagBag h, map stackFrameTag' k, qaTag' qa) $
                                          -- traceRender ("stateTags:heap (TagBag)", M.map heapBindingTag h) $
                                          -- (\res -> traceRender ("stateTags (TagBag)", res) res) $
                                          pureHeapTagBag h `plusTagBag` stackTagBag k `plusTagBag` tagTagBag (qaTag' qa)
      where
        heapBindingTagBag :: HeapBinding -> TagBag
        heapBindingTagBag = maybe (mkTagBag []) (tagTagBag . pureHeapBindingTag') . heapBindingTag
          
        pureHeapTagBag :: PureHeap -> TagBag
        pureHeapTagBag = plusTagBags . map heapBindingTagBag . M.elems
     
        stackTagBag :: Stack -> TagBag
        stackTagBag = mkTagBag . map stackFrameTag'
     
        tagTagBag :: Tag -> TagBag
        tagTagBag = mkTagBag . return
        
        mkTagBag :: [Tag] -> TagBag
        mkTagBag = plusTagBags . map (\(TG i occs) -> IM.singleton (unFin i) occs)
        
        plusTagBag :: TagBag -> TagBag -> TagBag
        plusTagBag = IM.unionWith (+)
        
        plusTagBags :: [TagBag] -> TagBag
        plusTagBags = foldr plusTagBag IM.empty

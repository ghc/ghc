module Supercompile.Termination.TagBag (
        embedWithTagBags,
        TagBag, tagBagTagSet, stateTags
    ) where

import Supercompile.Termination.Combinators
import Supercompile.Termination.Generaliser

import Supercompile.Evaluator.Syntax

import Supercompile.Utilities
import Supercompile.StaticFlags (TagBagType(..))

import Unique (mkUniqueGrimily, unpkUnique)
import Util

import Data.Char
import qualified Data.Foldable as Foldable
import qualified Data.IntMap as IM
import qualified Data.Map as M


newtype TagBag = TagBag { unTagBag :: FinMap Nat }

instance Outputable TagBag where
    ppr tb = hsep [ pPrintTag tag <> (if n == 1 then empty else braces (ppr n))
                  | (tag, n) <- IM.toList (unTagBag tb) ]

tagBagTagSet :: TagBag -> FinSet
tagBagTagSet = IM.keysSet . unTagBag

pPrintTag :: Int -> SDoc
pPrintTag n = (try 'h' 5 `mplus` try 'k' 3 `mplus` try 'q' 2 `mplus` uniq '?' n) `orElse` nat '?' n
  where try w divisor = do
          (q, 0) <- return $ n `quotRem` divisor
          uniq w q
        
        uniq w m
          | 64 < ord_c && ord_c < 128 = Just $ char w <> ppr u
          | otherwise                 = Nothing
          where u = mkUniqueGrimily m
                (c, _) = unpkUnique u
                ord_c = ord c
        
        nat w m | m < 0     = char w <> char '-' <> text (iToBase62 (negate m))
                | otherwise = char w <>             text (iToBase62 m)

embedWithTagBags :: TagBagType -> (TTest State, State -> State -> Generaliser)
embedWithTagBags tbt = if tagBagPairwiseGrowth tbt then embedWithTagBags' (zippableT wellOrderedT) else embedWithTagBags' (cofmap Foldable.sum wellOrderedT)

embedWithTagBags' :: (forall f. (Foldable f, Zippable f, Finite (f ())) => TTest (f Nat))
                  -> (TTest State, State -> State -> Generaliser)
embedWithTagBags' nats = (cofmap (unTagBag . stateTags) (equalDomainT nats), generaliserFromGrowing (unTagBag . stateTags))

-- NB: I try very hard to avoid creating intermediate tag bags in this function because it
-- accounts for a staggering fraction of the supercompiler's total allocation
stateTags :: State -> TagBag
stateTags (_, Heap h _, k, qa) = -- traceRender ("stateTags (TagBag)", M.map heapBindingTagBag h, map stackFrameTag' k, qaTag' qa) $
                                      -- traceRender ("stateTags:heap (TagBag)", M.map heapBindingTag h) $
                                      -- (\res -> traceRender ("stateTags (TagBag)", res) res) $
                                      TagBag $ pureHeapTagBag h (stackTagBag k (singletonTagBag (qaTag' qa) IM.empty))
  where
    heapBindingTagBag :: HeapBinding -> FinMap Nat -> FinMap Nat
    heapBindingTagBag = maybe id (singletonTagBag . pureHeapBindingTag') . heapBindingTag
      
    pureHeapTagBag :: PureHeap -> FinMap Nat -> FinMap Nat
    pureHeapTagBag = flip $ M.fold heapBindingTagBag -- NB: really a foldr, but the M.foldr synonym was added in a later version of containers
 
    stackTagBag :: Stack -> FinMap Nat -> FinMap Nat
    stackTagBag = flip $ trainCarFoldr stackFrameTagBag

    stackFrameTagBag :: Tagged StackFrame -> FinMap Nat -> FinMap Nat
    stackFrameTagBag = singletonTagBag . stackFrameTag'
 
    singletonTagBag :: Tag -> FinMap Nat -> FinMap Nat
    singletonTagBag (TG i occs) = IM.insert (unFin i) occs

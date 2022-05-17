{-|
Module      : GHC.Wasm.ControlFlow.FromCmm
Description : Translation of (reducible) Cmm control flow to WebAssembly

Code in this module can translate any _reducible_ Cmm control-flow
graph to the structured control flow that is required by WebAssembly.
The algorithm is subtle and is described in detail in a draft paper
to be found at https://www.cs.tufts.edu/~nr/pubs/relooper.pdf.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Wasm.ControlFlow.FromCmm
  ( structuredControl
  )
where

import GHC.Prelude hiding (succ)

import Data.Function
import Data.List
import Data.Semigroup
import qualified Data.Tree as Tree

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dominators
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Switch

import GHC.Platform

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable ( Outputable, text, (<+>), ppr
                            , pprWithCommas
                            , showSDocUnsafe
                            )

import GHC.Wasm.ControlFlow

--------------------- Abstraction of Cmm control flow -----------------------

-- | Abstracts the kind of control flow we understand how to convert.
-- A block can be left in one of four ways:
--
--   * Unconditionally
--
--   * Conditionally on a predicate of type `e`
--
--   * To a location determined by the value of a scrutinee of type `e`
--
--   * Not at all.


data ControlFlow e = Unconditional Label
                   | Conditional e Label Label
                   | Switch { _scrutinee :: e
                            , _range :: BrTableInterval
                            , _targets :: [Maybe Label] -- from 0
                            , _defaultTarget :: Maybe Label
                            }
                   | TerminalFlow

flowLeaving :: Platform -> CmmBlock -> ControlFlow CmmExpr
flowLeaving platform b =
    case lastNode b of
      CmmBranch l -> Unconditional l
      CmmCondBranch c t f _ -> Conditional c t f
      CmmSwitch e targets ->
          let (offset, target_labels) = switchTargetsToTable targets
              (lo, hi) = switchTargetsRange targets
              default_label = switchTargetsDefault targets
              scrutinee = smartPlus platform e offset
              range = inclusiveInterval (lo+toInteger offset) (hi+toInteger offset)
          in  Switch scrutinee range (atMost brTableLimit target_labels) default_label

      CmmCall { cml_cont = Just l } -> Unconditional l
      CmmCall { cml_cont = Nothing } -> TerminalFlow
      CmmForeignCall { succ = l } -> Unconditional l

  where atMost :: Int -> [a] -> [a]
        atMost k xs = if xs `hasAtLeast` k then
                          panic "switch table is too big for WebAssembly"
                      else
                          xs

        hasAtLeast :: [a] -> Int -> Bool
        hasAtLeast _ 0 = True
        hasAtLeast [] _ = False
        hasAtLeast (_:xs) k = hasAtLeast xs (k - 1)


----------------------- Evaluation contexts ------------------------------

-- | The syntactic constructs in which Wasm code may be contained.
-- A list of these constructs represents an evaluation context,
-- which is used to determined what level of `br` instruction
-- reaches a given label.

data ContainingSyntax
    = BlockFollowedBy Label
    | LoopHeadedBy Label
    | IfThenElse (Maybe Label) -- ^ Carries the label that follows `if...end`, if any

matchesFrame :: Label -> ContainingSyntax -> Bool
matchesFrame label (BlockFollowedBy l) = label == l
matchesFrame label (LoopHeadedBy l) = label == l
matchesFrame label (IfThenElse (Just l)) = label == l
matchesFrame _ _ = False

data Context = Context { enclosing :: [ContainingSyntax]
                       , fallthrough :: Maybe Label  -- the label can
                                                     -- be reached just by "falling through"
                                                     -- the hole
                       }

instance Outputable Context where
  ppr c | Just l <- fallthrough c =
                    pprWithCommas ppr (enclosing c) <+> text "fallthrough to" <+> ppr l
        | otherwise = pprWithCommas ppr (enclosing c)

emptyContext :: Context
emptyContext = Context [] Nothing

inside :: ContainingSyntax -> Context -> Context
withFallthrough :: Context -> Label -> Context

inside frame c = c { enclosing = frame : enclosing c }
withFallthrough c l = c { fallthrough = Just l }

type CmmStmts = Block CmmNode O O
type CfgNode = CmmBlock



----------------------- Translation ------------------------------

-- | Convert a Cmm CFG to WebAssembly's structured control flow.

structuredControl :: forall expr stmt .
                     Platform  -- ^ needed for offset calculation
                  -> (Label -> CmmExpr -> expr) -- ^ translator for expressions
                  -> (Label -> CmmStmts -> stmt) -- ^ translator for straight-line code
                  -> CmmGraph -- ^ CFG to be translated
                  -> WasmControl stmt expr
structuredControl platform txExpr txBlock g =
   doTree dominatorTree emptyContext
 where
   dominatorTree :: Tree.Tree CfgNode -- ^ Dominator tree in which children are sorted
                                       -- with highest reverse-postorder number first
   dominatorTree = fmap blockLabeled $ sortTree $ gwdDominatorTree gwd

   doTree     :: Tree.Tree CfgNode -> Context -> WasmControl stmt expr
   nodeWithin :: CfgNode -> [Tree.Tree CfgNode] -> Maybe Label
                                    -> Context -> WasmControl stmt expr
   doBranch   :: Label -> Label     -> Context -> WasmControl stmt expr

   doTree (Tree.Node x immediateDominatees) context =
       let codeForX = nodeWithin x dominatees Nothing
       in  if isHeader x then
             WasmLoop (codeForX loopContext)
           else
             codeForX context
     where dominatees = case lastNode x of
                          CmmSwitch {} -> immediateDominatees
                                 -- N.B. Unlike `if`, translation of Switch uses only labels.
                          _ -> filter isMergeTree $ immediateDominatees
           loopContext = LoopHeadedBy (entryLabel x) `inside`
                           (context `withFallthrough` (entryLabel x))

   nodeWithin x (y_n:ys) (Just zlabel) context =
       WasmBlock $ nodeWithin x (y_n:ys) Nothing context'
     where context' = BlockFollowedBy zlabel `inside` context
   nodeWithin x (y_n:ys) Nothing context =
       nodeWithin x ys (Just ylabel) (context `withFallthrough` ylabel) <> doTree y_n context
     where ylabel = treeEntryLabel y_n
   nodeWithin x [] (Just zlabel) context
     | not (generatesIf x) =
         WasmBlock (nodeWithin x [] Nothing context')
     where context' = BlockFollowedBy zlabel `inside` context
   nodeWithin x [] maybeMarks context =
       translationOfX context
     where xlabel = entryLabel x

           translationOfX :: Context -> WasmControl stmt expr
           translationOfX context =
             WasmAction (txBlock xlabel $ nodeBody x) <>
             case flowLeaving platform x of
               Unconditional l -> doBranch xlabel l context
               Conditional e t f ->
                 WasmIf (txExpr xlabel e)
                        (doBranch xlabel t (IfThenElse maybeMarks `inside` context))
                        (doBranch xlabel f (IfThenElse maybeMarks `inside` context))
               TerminalFlow -> WasmReturn
               Switch e range targets default' ->
                   WasmBrTable (txExpr xlabel e)
                               range
                               (map switchIndex targets)
                               (switchIndex default')
            where switchIndex :: Maybe Label -> Int
                  switchIndex Nothing = 0 -- arbitrary; GHC won't go here
                  switchIndex (Just lbl) = index lbl (enclosing context)

   doBranch from to context
      | to `elem` fallthrough context = mempty -- optimization: `br` not needed
      | isBackward from to = WasmBr i -- continue
      | isMergeLabel to = WasmBr i -- exit
      | otherwise = doTree (subtreeAt to) context -- inline the code here
     where i = index to (enclosing context)

   generatesIf x = case flowLeaving platform x of Conditional {} -> True
                                                  _ -> False

   ---- everything else is utility functions

   treeEntryLabel = entryLabel . Tree.rootLabel

   sortTree :: Tree.Tree Label -> Tree.Tree Label
     -- ^ Sort highest rpnum first
   sortTree (Tree.Node label children) =
      Tree.Node label $ sortBy (flip compare `on` (rpnum . Tree.rootLabel)) $
                        map sortTree children

   subtreeAt :: Label -> Tree.Tree CfgNode
   blockLabeled :: Label -> CfgNode
   rpnum :: Label -> RPNum -- ^ reverse postorder number of the labeled block
   isMergeTree :: Tree.Tree CfgNode -> Bool
   isMergeLabel :: Label -> Bool
   isMergeNode :: CfgNode -> Bool
   isHeader :: CfgNode -> Bool -- ^ identify loop headers
     -- ^ all nodes whose immediate dominator is the given block.
     -- They are produced with the largest RP number first,
     -- so the largest RP number is pushed on the context first.
   dominates :: Label -> Label -> Bool
     -- ^ Domination relation (not just immediate domination)

   GMany NothingO blockmap NothingO = g_graph g

   blockLabeled l = findLabelIn l blockmap

   rpblocks :: [CfgNode]
   rpblocks = revPostorderFrom blockmap (g_entry g)

   foldEdges :: forall a . (Label -> Label -> a -> a) -> a -> a
   foldEdges f a =
     foldl (\a (from, to) -> f from to a)
           a
           [(entryLabel from, to) | from <- rpblocks, to <- successors from]

   isMergeLabel l = setMember l mergeBlockLabels
   isMergeNode = isMergeLabel . entryLabel
   isMergeTree = isMergeNode . Tree.rootLabel

   isBackward from to = rpnum to <= rpnum from -- self-edge counts as a backward edge

   subtreeAt label = findLabelIn label subtrees
   subtrees :: LabelMap (Tree.Tree CfgNode)
   subtrees = addSubtree mapEmpty dominatorTree
     where addSubtree map (t@(Tree.Node root children)) =
               foldl addSubtree (mapInsert (entryLabel root) t map) children

   mergeBlockLabels :: LabelSet
   -- N.B. A block is a merge node if it is where control flow merges.
   -- That means it is entered by multiple control-flow edges, _except_
   -- back edges don't count.  There must be multiple paths that enter the
   -- block _without_ passing through the block itself.
   mergeBlockLabels =
       setFromList [entryLabel n | n <- rpblocks, big (forwardPreds (entryLabel n))]
    where big [] = False
          big [_] = False
          big (_ : _ : _) = True

          forwardPreds :: Label -> [Label] -- reachable predecessors of reachable blocks,
                                           -- via forward edges only
          forwardPreds = \l -> mapFindWithDefault [] l predmap
              where predmap :: LabelMap [Label]
                    predmap = foldEdges addForwardEdge mapEmpty
                    addForwardEdge from to pm
                        | isBackward from to = pm
                        | otherwise = addToList (from :) to pm

   isHeader = isHeaderLabel . entryLabel
   isHeaderLabel = \l -> setMember l headers  -- loop headers
      where headers :: LabelSet
            headers = foldMap headersPointedTo blockmap
            headersPointedTo block =
                setFromList [label | label <- successors block,
                                              dominates label (entryLabel block)]

   index _ [] = panic "destination label not in evaluation context"
   index label (frame : context)
       | label `matchesFrame` frame = 0
       | otherwise = 1 + index label context

   gwd = graphWithDominators g
   rpnum = gwdRPNumber gwd
   dominates lbl blockname =
       lbl == blockname || dominatorsMember lbl (gwdDominatorsOf gwd blockname)



nodeBody :: CfgNode -> CmmStmts
nodeBody (BlockCC _first middle _last) = middle


smartPlus :: Platform -> CmmExpr -> Int -> CmmExpr
smartPlus _ e 0 = e
smartPlus platform e k =
    CmmMachOp (MO_Add width) [e, CmmLit (CmmInt (fromIntegral k) width)]
  where width = cmmExprWidth platform e

addToList :: (IsMap map) => ([a] -> [a]) -> KeyOf map -> map [a] -> map [a]
addToList consx = mapAlter add
    where add Nothing = Just (consx [])
          add (Just xs) = Just (consx xs)

------------------------------------------------------------------
--- everything below here is for diagnostics in case of panic

instance Outputable ContainingSyntax where
    ppr (BlockFollowedBy l) = text "node" <+> ppr l
    ppr (LoopHeadedBy l) = text "loop" <+> ppr l
    ppr (IfThenElse l) = text "if-then-else" <+> ppr l

findLabelIn :: HasDebugCallStack => Label -> LabelMap a -> a
findLabelIn lbl = mapFindWithDefault failed lbl
  where failed =
            panic $ "label " ++ showSDocUnsafe (ppr lbl) ++ " not found in control-flow graph"

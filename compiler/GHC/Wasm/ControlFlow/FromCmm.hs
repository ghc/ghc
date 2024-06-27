{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module GHC.Wasm.ControlFlow.FromCmm
  ( structuredControl
  )
where

import GHC.Prelude hiding (succ)

import Data.Function
import Data.List (sortBy)
import qualified Data.Tree as Tree

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dominators
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Reducibility
import GHC.Cmm.Switch

import GHC.Data.Graph.Collapse (MonadUniqDSM (liftUniqDSM))
import GHC.CmmToAsm.Wasm.Types

import GHC.Platform
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable ( Outputable, text, (<+>), ppr
                            , pprWithCommas
                            )

import GHC.Wasm.ControlFlow


{-|
Module      : GHC.Wasm.ControlFlow.FromCmm
Description : Translation of (reducible) Cmm control flow to WebAssembly

Code in this module can translate any _reducible_ Cmm control-flow
graph to the structured control flow that is required by WebAssembly.
The algorithm is subtle and is described in detail in a draft paper
to be found at https://www.cs.tufts.edu/~nr/pubs/relooper.pdf.
-}

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
                   | TailCall e

flowLeaving :: Platform -> CmmBlock -> ControlFlow CmmExpr
flowLeaving platform b =
    case lastNode b of
      CmmBranch l -> Unconditional l
      CmmCondBranch c t f _ -> Conditional c t f
      CmmSwitch e targets ->
          let (offset, target_labels) = switchTargetsToTable targets
              (lo, hi) = switchTargetsRange targets
              default_label = switchTargetsDefault targets
              scrutinee = smartExtend platform $ smartPlus platform e offset
              range = inclusiveInterval (lo+toInteger offset) (hi+toInteger offset)
          in  Switch scrutinee range target_labels default_label
      CmmCall { cml_target = e } -> TailCall e
      _ -> panic "flowLeaving: unreachable"

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

type CmmActions = Block CmmNode O O

type FT pre post = WasmFunctionType pre post

returns :: FT '[] '[ 'I32]
doesn'tReturn :: FT '[] '[]

returns = WasmFunctionType TypeListNil (TypeListCons TagI32 TypeListNil)
doesn'tReturn = WasmFunctionType TypeListNil TypeListNil

emptyPost :: FT pre post -> Bool
emptyPost (WasmFunctionType _ TypeListNil) = True
emptyPost _ = False

----------------------- Translation ------------------------------

-- | Convert a Cmm CFG to WebAssembly's structured control flow.

structuredControl :: forall expr stmt m .
                     MonadUniqDSM m
                  => Platform  -- ^ needed for offset calculation
                  -> (Label -> CmmExpr -> m expr) -- ^ translator for expressions
                  -> (Label -> CmmActions -> m stmt) -- ^ translator for straight-line code
                  -> CmmGraph -- ^ CFG to be translated
                  -> m (WasmControl stmt expr '[] '[ 'I32])
structuredControl platform txExpr txBlock g' = do
  gwd :: GraphWithDominators CmmNode <-
    liftUniqDSM $ asReducible $ graphWithDominators g'

  let
   g :: CmmGraph
   g = gwd_graph gwd

   dominatorTree :: Tree.Tree CmmBlock-- Dominator tree in which children are sorted
                                       -- with highest reverse-postorder number first
   dominatorTree = fmap blockLabeled $ sortTree $ gwdDominatorTree gwd

   doTree     :: FT '[] post -> Tree.Tree CmmBlock -> Context -> m (WasmControl stmt expr '[] post)
   nodeWithin :: forall post .
                 FT '[] post -> CmmBlock -> [Tree.Tree CmmBlock] -> Maybe Label
                                                   -> Context -> m (WasmControl stmt expr '[] post)
   doBranch   :: FT '[] post -> Label -> Label     -> Context -> m (WasmControl stmt expr '[] post)

   doTree fty (Tree.Node x children) context =
       let codeForX = nodeWithin fty x selectedChildren Nothing
       in  if isLoopHeader x then
             WasmLoop fty <$> codeForX loopContext
           else
             codeForX context
     where selectedChildren = case lastNode x of
                                CmmSwitch {} -> children
                                   -- N.B. Unlike `if`, translation of Switch uses only labels.
                                _ -> filter hasMergeRoot children
           loopContext = LoopHeadedBy (entryLabel x) `inside` context
           hasMergeRoot = isMergeNode . Tree.rootLabel

   nodeWithin fty x (y_n:ys) (Just zlabel) context =
       WasmBlock fty <$> nodeWithin fty x (y_n:ys) Nothing context'
     where context' = BlockFollowedBy zlabel `inside` context
   nodeWithin fty x (y_n:ys) Nothing context =
       nodeWithin doesn'tReturn x ys (Just ylabel) (context `withFallthrough` ylabel) <<>>
       doTree fty y_n context
     where ylabel = treeEntryLabel y_n
   nodeWithin fty x [] (Just zlabel) context
     | not (generatesIf x) =
         WasmBlock fty <$> nodeWithin fty x [] Nothing context'
     where context' = BlockFollowedBy zlabel `inside` context
   nodeWithin fty x [] maybeMarks context =
       translationOfX context
     where xlabel = entryLabel x

           translationOfX :: Context -> m (WasmControl stmt expr '[] post)
           translationOfX context =
             (WasmActions <$> txBlock xlabel (nodeBody x)) <<>>
             case flowLeaving platform x of
               Unconditional l -> doBranch fty xlabel l context
               Conditional e t f ->
                 WasmIf fty
                        <$> txExpr xlabel e
                        <*> doBranch fty xlabel t (IfThenElse maybeMarks `inside` context)
                        <*> doBranch fty xlabel f (IfThenElse maybeMarks `inside` context)
               TailCall e -> WasmTailCall <$> txExpr xlabel e
               Switch e range targets default' ->
                   WasmBrTable <$>  txExpr xlabel e
                               <$~> range
                               <$~> map switchIndex targets
                               <$~> switchIndex default'
            where switchIndex :: Maybe Label -> Int
                  switchIndex Nothing = 0 -- arbitrary; GHC won't go here
                  switchIndex (Just lbl) = index lbl (enclosing context)

   doBranch fty from to context
      | to `elem` fallthrough context && emptyPost fty = pure WasmFallthrough
                -- optimization: `br` is not needed, but it typechecks
                -- only if nothing is expected to be left on the stack

      | isBackward from to = pure $ WasmBr i -- continue
      | isMergeLabel to = pure $ WasmBr i -- exit
      | otherwise = doTree fty (subtreeAt to) context -- inline the code here
     where i = index to (enclosing context)

   generatesIf :: CmmBlock -> Bool
   generatesIf x = case flowLeaving platform x of Conditional {} -> True
                                                  _ -> False

   ---- everything else is utility functions

   treeEntryLabel :: Tree.Tree CmmBlock -> Label
   treeEntryLabel = entryLabel . Tree.rootLabel

   sortTree :: Tree.Tree Label -> Tree.Tree Label
    -- Sort highest rpnum first
   sortTree (Tree.Node label children) =
      Tree.Node label $ sortBy (flip compare `on` (rpnum . Tree.rootLabel)) $
                        map sortTree children

   subtreeAt :: Label -> Tree.Tree CmmBlock
   blockLabeled :: Label -> CmmBlock
   rpnum :: Label -> RPNum-- reverse postorder number of the labeled block
   isMergeLabel :: Label -> Bool
   isMergeNode :: CmmBlock -> Bool
   isLoopHeader :: CmmBlock -> Bool-- identify loop headers
    -- all nodes whose immediate dominator is the given block.
     -- They are produced with the largest RP number first,
     -- so the largest RP number is pushed on the context first.
   dominates :: Label -> Label -> Bool
    -- Domination relation (not just immediate domination)

   blockmap :: LabelMap CmmBlock
   GMany NothingO blockmap NothingO = g_graph g

   blockLabeled l = findLabelIn l blockmap

   rpblocks :: [CmmBlock]
   rpblocks = revPostorderFrom blockmap (g_entry g)

   foldEdges :: forall a . (Label -> Label -> a -> a) -> a -> a
   foldEdges f a =
     foldl (\a (from, to) -> f from to a)
           a
           [(entryLabel from, to) | from <- rpblocks, to <- successors from]

   isMergeLabel l = setMember l mergeBlockLabels
   isMergeNode = isMergeLabel . entryLabel

   isBackward :: Label -> Label -> Bool
   isBackward from to = rpnum to <= rpnum from -- self-edge counts as a backward edge

   subtreeAt label = findLabelIn label subtrees
   subtrees :: LabelMap (Tree.Tree CmmBlock)
   subtrees = addSubtree mapEmpty dominatorTree
     where addSubtree map t@(Tree.Node root children) =
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

   isLoopHeader = isHeaderLabel . entryLabel
   isHeaderLabel = (`setMember` headers)  -- loop headers
      where headers :: LabelSet
            headers = foldMap headersPointedTo blockmap
            headersPointedTo block =
                setFromList [label | label <- successors block,
                                              dominates label (entryLabel block)]

   index :: Label -> [ContainingSyntax] -> Int
   index _ [] = panic "destination label not in evaluation context"
   index label (frame : context)
       | label `matchesFrame` frame = 0
       | otherwise = 1 + index label context

   rpnum = gwdRPNumber gwd
   dominates lbl blockname =
       lbl == blockname || dominatorsMember lbl (gwdDominatorsOf gwd blockname)

  doTree returns dominatorTree emptyContext

nodeBody :: CmmBlock -> CmmActions
nodeBody (BlockCC _first middle _last) = middle

-- | A CmmSwitch scrutinee may have any width, but a br_table operand
-- must be exactly word sized, hence the extension here. (#22871)
smartExtend :: Platform -> CmmExpr -> CmmExpr
smartExtend p e | w0 == w1 = e
                | otherwise = CmmMachOp (MO_UU_Conv w0 w1) [e]
  where
    w0 = cmmExprWidth p e
    w1 = wordWidth p

smartPlus :: Platform -> CmmExpr -> Int -> CmmExpr
smartPlus _ e 0 = e
smartPlus platform e k =
    CmmMachOp (MO_Add width) [e, CmmLit (CmmInt (toInteger k) width)]
  where width = cmmExprWidth platform e

addToList :: ([a] -> [a]) -> Label -> LabelMap [a] -> LabelMap [a]
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
            pprPanic "label not found in control-flow graph" (ppr lbl)


infixl 4 <$~>
(<$~>) :: Functor m => m (a -> b) -> a -> m b
(<$~>) f x = fmap ($ x) f

(<<>>) :: forall m s e pre mid post
       . Applicative m
       => m (WasmControl s e pre mid)
       -> m (WasmControl s e mid post)
       -> m (WasmControl s e pre post)
(<<>>) = liftA2 (<>)

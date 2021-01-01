{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Stg.InferTags.Analysis

where

#include "HsVersions.h"

#if defined(DEBUG)
#define WITH_NODE_DESC
#endif

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Id
import GHC.Types.RepType
import GHC.Unit.Types (Module)

import GHC.Types.Demand ( Divergence ( Absent ), splitStrictSig )
import GHC.Types.Unique
import GHC.Types.Unique.FM

import GHC.Core.DataCon
import GHC.Core (AltCon(..))
import GHC.Core.TyCon (tyConDataCons)
import GHC.Core.Type

import GHC.Utils.Outputable
import GHC.Stg.Syntax as StgSyn hiding (AlwaysEnter)
import GHC.Stg.Utils

import GHC.StgToCmm.Types ( LambdaFormInfo(..) )
import GHC.Builtin.Names
import GHC.Builtin.Types.Prim (addrPrimTy)

import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Utils.Monad
import GHC.Utils.Error
import GHC.Types.Var.Env

import GHC.Data.Graph.Directed

-- import Data.Array.IArray as U
-- import Data.Array.MArray as M
import Data.Array.IO as IOA
import Data.IORef

import GHC.Utils.Panic
import GHC.Driver.Ppr (pprTraceM)

import Control.Monad

-- Both of these are only used for dumping nodes with -ddump-stg-tag-nodes
import GHC.Driver.Session
import System.IO.Unsafe

import GHC.Stg.InferTags.Types

{-
Note [The strict field invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The purpose of this invariant is that it allows us to eliminate
the tag check when casing on values coming out of strict fields.

To achieve this the code in this module transforms the STG AST such
that all strict fields of allocated constructors will only contain
tagged values.

We do allow one exceptions to this invariant.
* Values representing an absentError. See [Taggedness of absentError]
TODO: Make sure functions are forced as expected!

See also Note [Taggedness of absentError] for more information on
the later.

We uphold this invariant by inserting code for evaluation around
constructor allocation where needed. In practice this means sometimes
we will turn this RHS:

    StrictTuple foo bar

into this:

    case foo of taggedFoo ->
    case bar of taggedBar ->
        StrictTuple taggedFoo taggedBar

This is sometimes needed to ensure arguments used as strict
constructor fields are tagged. If we were to blindly rewrite
constructor allocations in this fashion we wouldn't get much
benefit out of the invariant.

Luckily *most* bindings which will be put into strict fields are already
represented by tagged pointers. So we run the tag inference analysis first.
This tries to infer if bindings like "foo" and "bar in the example above are
already tagged. If they are we can avoid inserting additional cases.

Having established the invariant we can then rely on it to eliminate tag checks
when scrutinizing bindings coming out of strict fields.

In particular when traversing the strict spine of a data structure
this eliminates a significant amount of code being executed. Leading
to significant speedups. (10% and up for some traversals!)

For a full example consider:

case x of
  StrictTuple a b -> case a of
                        True -> ...
                        False -> ...

Assuming x was inferred to be a thunk we can't optimize anything about
the outer case. Looking at the inner case we can omit the tag-check part
of the case. (Since a comes out of a strict field.)

That is to say usually we would generate code like this for the inner
case above without the strict field invariant:

    //Start of the tag-check
    c14F:
        I64[Sp - 8] = c14w;
        R1 = R2;
        Sp = Sp - 8;
        if (R1 & 7 != 0) goto c14w; else goto c14x;
    c14x:
        call (I64[R1])(R1) returns to c14w, args: 8, res: 8, upd: 8;
    //End of the tag-check
    c14w:
    //Branch on the values tag.
        if (R1 & 7 != 1) goto <True rhs>; else goto <False rhs>;

But since we know that a is tagged (coming out of a strict field and having the invariant) we
can omit the whole tag-check and are left only with the branching on the tag:

    //Branch on the tag
    c14w:
        if (R2 & 7 != 1) goto <True rhs>; else goto <False rhs>;

Note [Optimizing function closures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A binding two ways:

* By applying it to arguments and forcing the result:
    case (f x y) of ...
* By scrutinizing it as a value, potentially also branchon on the result:
    case x of ...

We optimize for the second case for a number of reasons:
* It's the case that inspired this analysis
* Scrutinizing of tagged value bindings happens more often
  than application of unknown function bindings.
* To fully optimize function bindings we would want to keep
  track of their arity as well.

Most of all it's a matter of not having been worked on much. There is no
theoretical reason why this shouldn't "just" work for function bindings.
But there might be a few details about partial applications and other cases
that need to be worked out.

Note [Taggedness of absentError]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WorkerWrapper -  under certain circumstances - will determine
that a strict field in a constructor is not used and put a bottom
expression in there as placeholder which is not supposed to be
evaluated. See Note [Absent errors] in the WW code for why we do this.

We have to take great care to avoid evaluating these absentError
expresions when upholding the strict field invariant. Because
evaluating them would trigger a panic in the runtime.

For this purpose we treat bindings representing an absentError value
as being tagged. We do this with the help of strictness analysis which
will assign a special kind of divergence value (Absent) to such bindings.

This is similar to the simplifier treating absentError as a constructor.
See Note [aBSENT_ERROR_ID] for a description of the simplifier case.

This is alright, because in the absence of bugs in the rest of GHC any such value
should never be evaluated, and therefore can be treated as already
evaluated without affecting correctness.

This means we might end up with code like this:

    data T a b c = MkT a b !c

    $wf :: a -> b -> Result
    $wf a b = let c = absentError "ww_absent"
                in  g (MkT a b c)

Where we know g will not use the strict field c.

We catch this kind of situation in two ways:
* Checking the RHS for absentError applications in the current module.
* Inspecting the strictness of imported ids.

We check for absentError applications in the current module because:
    * It works with -O0 in which case the demand analyser isn't run.
    * It is more performant to inspect the functions unique than to
        compare the strictness.

We have to rely on the strictness analysis for imported id's since not
all ids provide us with an unfolding to inspect.

If we come across a let binding of absentError we simply treat the binding as
if it's represented by a tagged pointer.

Checking the strictness properties for imported ids is important.
What if we inline $wf into a module B, but don't do so for `c` which might
get floated out? We have to ensure that c is still treated as tagged.

Checking the RHS of local functions, and strictness of imported ones is sufficient
to make this work.
* Consider c (binding absentError) to be defined in module A
* $wf to be inlined into module B from module A.

This gives us four cases to consider:

A & B not optimized:
    Since ww will not run on either, c will never be generated by the compiler.
A & B optimized:
    A will be analyzed, the demand analyzer will recognise c as an absent error
    which allows us to treat `c` as tagged inside module B.
A optimized, B not optimized:
    If B is not optimized then the body of $wf can't end up in B, so this works out.
A not optimized, B not optimized:
    Since A is not optimised GHC won't expose any unfoldings, so again $wf and c can't
    end up in different modules and things work out.


Note [nesting Limit]
~~~~~~~~~~~~~~~~~~~~
When analysing a function like `f x = x : f x` we need to widen the result to
ensure termination. We achieve this by analysing results only up to a depth of
set by the nestingLimit variable.

Analysing results nested past a few levels does not convey a real performance
improvement, but it does affect compile times significantly for certain code.

When viewing the lattice elments as tree, the nesting level is the depth of the
tree. That is a simple lattice of the form "NeverEnter <noFields>" has a nesting
level of 1. NeverEnter <NeverEnter> would have two, and so on.

Note [Useless Bangs]
~~~~~~~~~~~~~~~~~~~~
Ghcs state monad is lazy. So to avoid space leaks I've added bangs
very liberally in this module. Some are bound to be useless, but this
still beats having space leaks.

The only place in this module where we explicitly depend on lazyness is the
(unused) ty for case alternatives. (See the calls to mkSeqs). There we pass
a panic value which (eventually) get's used as an argument for StgConApp's
[Type] argument. The argument is only relevant for unarise and unused after
making this alright to do.
So there is no harm in excessive bang annotations, at least not compared to space leaks.

-}



-- See Note [nesting Limit]
nestingLimit :: Int
nestingLimit = 10



{-
Note [Field information of function ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this code:

f x :: a -> (Maybe Int,a)
f x = (Just 1, a)

We can infer the taggedness of all but the second field of the tuple.

However if we have code like this:

f x :: a -> (Maybe Int,a)
f x = (Just 1, a)

g _ =
    let v = Just f
        ...
    in
    case v of
        Just f' -> f' True
        Nothing -> f False

If we keep the field information of f when stored inside a constructor then
we can eventually figure out that f' == f, so all branches have the same return taggedness info
which is the same as the one of f. So we could infer as the taggedness of g's result:

    enterInfo(g) = NeverEnter < NeverEnter < NeverEnter >, MaybeEnter>
                    ^ func        ^ Just       ^ 1           ^ "a"

However currently when when we bind a function to a variable we throw away any field information
of the functions result. We do not *have* to strip this information, it's merely done for performance.

TODO:
    This could be done, at the expense of compile time. We should benchmark this as future work
    to figure out of it's worth and do it if useful.

    Same thing for keeping track of the original function. Potentially removing some unknown calls.

-}

-- | If we use a *function* as an unapplied argument to a constructor we throw
-- away nested information and make do with NeverEnter Top for now.
-- See Note [Field information of function ids]
getConArgNodeId :: HasDebugCallStack => ContextStack -> StgArg -> AM NodeId
getConArgNodeId _    (StgLitArg _ ) = return litNodeId
getConArgNodeId ctxt (StgVarArg v )
    | isFunTy (unwrapType $ idType v)
    = return neverNodeId
    | otherwise
    = getIdNodeId ctxt v



-- `isRecTail f ctxt`
-- Determine if f called in context ctxt is a recursive tail call.
-- If so it's result will be NoValue.
-- See Note [Recursive Functions]
isRecTail :: Id -> ContextStack -> Bool
isRecTail _f (CTopLevel _ : _) = False
isRecTail  f (CLetRec bnds _: _)
    | isSingleMapOf f bnds
    = True
isRecTail f (CLetRec _ LNE    : ctxt) = isRecTail f ctxt
isRecTail _f (CLetRec _ NotLNE : _   ) = False
isRecTail f (CLetRecBody bnds _ :ctxt)
    -- `let x = e in .. -> x`
    -- is not a recursive tail call
    | f `elemVarEnv` bnds = False
    | otherwise = isRecTail f ctxt
isRecTail f (CLet _ LNE    : ctxt) = isRecTail f ctxt
isRecTail _f (CLet _ NotLNE : _) = False
isRecTail f (CLetBody bnds _ : ctxt)
    | elemVarEnv f bnds = False
    | otherwise = isRecTail f ctxt
isRecTail f (CClosureBody args : ctxt)
    | f `elemVarEnv` args = False
    | otherwise = isRecTail f ctxt
isRecTail _f (CCaseScrut : _) = False
isRecTail f (CCaseBndr bnd : ctxt )
    | elemVarEnv f bnd
    = False
    | otherwise
    = isRecTail f ctxt
isRecTail f (CAlt bnds : ctxt)
    | f `elemVarEnv` bnds = False
    | otherwise = isRecTail f ctxt
isRecTail f x = pprPanic "Incomplete" $ ppr (f,x)

-- | isSingleMapOf v env == fromList [(v',_)] && v == v'
--
-- Used to determine recursive calls.
isSingleMapOf :: Id -> VarEnv NodeId -> Bool
isSingleMapOf v env =
    isSingletonUFM env && elemVarEnv v env

-- | Create a data flow note which combines multiple branches.
-- See Note [The lattice element combinators]
mkJoinNode :: [NodeId] -> AM NodeId
mkJoinNode []     = return unknownNodeId
mkJoinNode [node] = return node
mkJoinNode inputs = {-# SCC joinNode #-} do
    node_id <- mkUniqueId
    let updater = do
            input_results <- mapM lookupNodeResult inputs
            let result = foldl1' combineLattices input_results
            if isFinalValue result
                then do
                    node <- getNode node_id
                    markDone $ node { node_result = result }
                else updateNodeResult node_id result
            return $! result

    addNode notDone $ setNodeDesc (text "branches")
                    $ FlowNode { node_id = node_id, node_result = undetLat
                       , node_inputs = inputs
                       , node_update = updater
                       }
    return $! node_id

-- | Compute the taggedness result of applying a constructor to the given arguments
--   *and* applying the strict field invariant. Marking all strict fields as tagged.
mkOutConLattice :: DataCon -> EnterInfo -> [EnterLattice] -> EnterLattice
mkOutConLattice con outer fields
    | null fields   = EnterLattice outer $ FieldsNone
    | conCount == 1 = EnterLattice outer $ FieldsProd out_fields
    | conCount > 1  = EnterLattice outer $ FieldsSum (Just con) out_fields
    | otherwise = panic "mkOutConLattice"
  where
    out_fields = mapStrictConArgs con (`setEnterInfo` NeverEnter) fields
    conCount = length (tyConDataCons $ dataConTyCon con)



type NodeArray = IOArray Int FlowNode
type FlagArray = IOUArray Int Bool

-- Dep-sorting nodes is good for performance. It means we analyze inputs nodes before
-- the nodes that depend on them. Which cuts down on needed iterations drastically.
depSortNodes :: UniqFM NodeId FlowNode -> [FlowNode]
depSortNodes in_nodes = reversePayload [] . topologicalSortG $ graphFromEdgedVerticesUniq vertices
  where
    vertices = foldUFM mkVertex [] in_nodes :: [Node NodeId FlowNode]

    mkVertex :: FlowNode -> [Node NodeId FlowNode] -> [Node NodeId FlowNode]
    mkVertex n xs = (DigraphNode n (node_id n) (node_inputs n)) : xs

    reversePayload :: [FlowNode] -> [Node NodeId FlowNode] -> [FlowNode]
    reversePayload acc []       =  acc
    reversePayload acc (x:xs)   =
        let !x' = node_payload x
        in reversePayload (x':acc) xs


solveConstraints :: HasDebugCallStack => DynFlags -> AM ()
solveConstraints dflags = do
        todos <- fs_uqNodeMap <$> get
        -- pprTraceM "sortSize" (ppr $ sizeUFM todos)
        let !dep_sorted_nodes = {-# SCC "nodeSorting" #-}
                                (depSortNodes todos) :: [FlowNode]
        iterate dep_sorted_nodes 5

        uqList <- map snd . nonDetUFMToList . fs_uqNodeMap <$> get
        doneList <- map snd . nonDetUFMToList . fs_doneNodes <$> get
        let resultNodes =  (uqList ++ doneList)

        seq (unsafePerformIO $ GHC.Utils.Error.dumpIfSet_dyn dflags
                Opt_D_dump_stg_tag_nodes "STG Infered tags" FormatText
                (dumpNodes resultNodes)) (return ())
        return ()
  where
    iterate :: [FlowNode] -> Int -> AM ()
    iterate xs n = do
        pprTraceM "Pass:" $ (ppr (length xs)) <+> text "nodes remaining."
        !change <- liftIO $ newIORef False
        !xs' <- runUpdates change False xs
        progress <- liftIO $ readIORef change

        if (not progress)
            then return ()
            --max iterations
            else if (n > 5)
                then -- pprTraceM "Warning:" (text "Aborting at" <+> ppr n <+> text "iterations") >>
                     return ()
                else iterate xs' (n+1)

    runUpdates :: IORef Bool -> Bool -> [FlowNode] -> AM [FlowNode]
    runUpdates !change_ref some_change [] = do
        -- Sadly it's hard to make using the stack to both return
        -- the list of nodes, as well as the Bool efficient.
        -- So I use this horrible hack of writing an IORef.
        liftIO $ writeIORef change_ref some_change
        return []
    runUpdates change_ref some_change (node:nodes) = do
        -- We keep track if anything changed, if not we can terminate the
        -- analysis early.
        !node_changed <- runNodeUpdate node
        let !some_change' = some_change || node_changed

        node_done <- isMarkedDone (node_id node)
        if node_done
            then runUpdates change_ref some_change' nodes
            else do
                pure (node:) <*> runUpdates change_ref some_change' nodes

runNodeUpdate :: FlowNode -> AM Bool
runNodeUpdate node = do
    let old_result = node_result node
    result <- node_update node
    let !node' = node { node_result = result }
    done <- GHC.Utils.Monad.allM isMarkedDone (node_inputs node)
    when done (markDone node')
    if (result == old_result)
        -- Nothing to do this round
        then return False
        else do
            return True


-- Constant mappings
addConstantNodes :: AM ()
addConstantNodes = do
    markDone litNode
    markDone addrNode
    markDone $ mkConstNode undetNodeId undetLat (text "undet")
    markDone $ mkConstNode unknownNodeId maybeLat (text "bot")
    markDone $ neverEnterNode
    markDone $ maybeEnterNode
    markDone $ alwaysEnterNode


mkConstNode :: NodeId -> EnterLattice -> SDoc -> FlowNode
mkConstNode id !val _desc =
    setNodeDesc _desc $ FlowNode
    { node_id = id
    , node_inputs = []
    , node_result = val
    , node_update = (return $! val)
    }

-- Some nodes we can reuse.
litNodeId, undetNodeId, unknownNodeId, neverNodeId, maybeNodeId,
    alwaysNodeId, addrNodeId, nullaryConNodeId :: NodeId
litNodeId       = NodeId $ mkUnique 'c' 2
undetNodeId     = NodeId $ mkUnique 'c' 3 -- Always returns undetLat
unknownNodeId   = NodeId $ mkUnique 'c' 4
neverNodeId     = NodeId $ mkUnique 'c' 5
maybeNodeId     = NodeId $ mkUnique 'c' 6
alwaysNodeId    = NodeId $ mkUnique 'c' 7
addrNodeId      = NodeId $ mkUnique 'c' 8
nullaryConNodeId = NodeId $ mkUnique 'c' 9

alwaysEnterNode, maybeEnterNode, neverEnterNode, litNode, addrNode, nullaryConNode :: FlowNode
alwaysEnterNode = mkConstNode alwaysNodeId  (flatLattice AlwaysEnter) (text "always")
maybeEnterNode  = mkConstNode maybeNodeId   (flatLattice MaybeEnter) (text "maybe")
neverEnterNode  = mkConstNode neverNodeId   (flatLattice NeverEnter) (text "never")
litNode         = mkConstNode litNodeId     (nullaryLattice NeverEnter) (text "lit")
addrNode        = mkConstNode addrNodeId    (nullaryLattice NeverEnter) (text "c_str")
nullaryConNode  = mkConstNode nullaryConNodeId (nullaryLattice NeverEnter) (text "nullCon")

{-
Note [Imported Ids]
~~~~~~~~~~~~~~~~~~~
# Assigning data flow nodes to imported ids.

We want to keep our Ids a simple newtype around Unique.
This is "easy" for things brought into scope by the AST we work with.
We simply put a mapping from the Id to the NodeId into SynContext.
We can then map ids to their data flow nodes based on the SynContext
we are in.

However imported Id's can show up in any place in the AST and we want to
avoid traversing the whole AST twice just to gather them up beforehand.
We solve this by creating a Node and NodeId for each imported
id when we come across the id the first time.

The next time we come across the same id getIdNodeId will check
fs_idNodeMap, find the node we created earlier and return the
same node.

!! Note that the Unique of an ID, and it's corresponding NodeId !!
!! are not correlated.                                          !!

# Taggedness of imported ids

This is currently determined fully in addImportedNode since
the result of tag inference is not exported in interface files.

The rules are simply:
    * Field info is always Unknown

    Enterinfo is:
    * NeverEnter for functions with known Arity
    * NeverEnter for nullarry constructors
    * NeverEnter for ids with Absent divergence. (absentError expressions)
    * AlwaysEnter for Thunks - Technically the RTS might evaluate them so *always* is a lie here.
    * MaybeEnter otherwise.

Note [Shadowing and NodeIds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Shadowing makes things more complex.

While constructing the data flow graph we have to be able to relate
every variable v to a data flow node describing v's value.
However the same variable can have different values in different because
of shadowing. For this reason we keep around a context which allows us to
map variables to nodes. Whenever a new variable comes into scope this context
is extended.

We then use getIdNodeId to get the node associated with an specific Id in
an specific context. It also takes care of imported Ids (See Note [Imported Ids]).

When we want to get the nodeId for a particular id which *must*
already present in a context we use getKnownIdNodeId.

-}

-- See Note [Shadowing and NodeIds]
getIdNodeId :: HasDebugCallStack => ContextStack -> Id -> AM NodeId
getIdNodeId ctxt id
    | Just node <- idMappedInCtxt id ctxt
    = return $! node
    | otherwise = do
        s <- get
        return $! fromMaybe (pprPanic "Unmapped id" (ppr id)) $
            lookupUFM (fs_idNodeMap s) id

-- See Note [Shadowing and NodeIds]
getKnownIdNodeId :: HasDebugCallStack => ContextStack -> Id -> NodeId
getKnownIdNodeId ctxt id
    | Just node <- idMappedInCtxt id ctxt
    = node
    | otherwise = pprPanic "Local id not mapped:" (ppr id)


-- | This adds a node containing information about an imported id.
--   Logic mimics somewhat what we do in StgCmmClosure.hs:mkLFImported
--   See also Note [Imported Ids]
addImportedNode :: Module -> Id -> AM ()
addImportedNode this_mod id
    -- Local id, it has to be mapped to an id via SynContext
    | nameIsLocalOrFrom this_mod (idName id) = return ()
    | otherwise = do
        -- Check if it is already cached.
        s <- get
        let idNodes = fs_idNodeMap s
        -- If not make up a new node.
        when (not $ elemUFM id idNodes) $ do
            new_node_id <- mkUniqueId
            let node
                    -- Functions tagged with arity are never entered, only applied.
                    | idFunRepArity id > 0
                    = set_if_desc neverEnterNode new_node_id (text "ext_func" <-> ppr id)

                    -- Known Nullarry constructors are also never entered
                    -- but for them it's important to preserve the information
                    -- of no fields.
                    | Just con <- (isDataConWorkId_maybe id)
                    , isNullaryRepDataCon con
                    = set_if_desc nullaryConNode new_node_id (text "ext_nullCon" <-> ppr id)

                    -- Imported binding of absentError
                    | (_, Absent) <- splitStrictSig (idStrictness id)
                    = set_if_desc neverEnterNode new_node_id (text "ext_absent_error" <-> ppr id)

                    | Just lf_info <- idLFInfo_maybe id
                    =   case lf_info of
                            -- Function, applied not entered.
                            LFReEntrant {}
                                -> set_if_desc neverEnterNode new_node_id (text "ext_lf_func" <-> ppr id)
                            -- Thunks need to be entered.
                            LFThunk {}
                                -> set_if_desc alwaysEnterNode new_node_id (text "ext_lf_thunk" <-> ppr id)
                            -- Constructors, already tagged.
                            LFCon {}
                                -- If we ever bind the fields we can infer the tags
                                -- based on the fields strictness. So a flat lattice
                                -- is fine.
                                -> set_if_desc neverEnterNode new_node_id (text "ext_lf_con" <-> ppr id)
                            LFUnknown {}
                                -> set_if_desc maybeEnterNode new_node_id (text "ext_lf_unknown" <-> ppr id)
                            LFUnlifted {}
                                -> set_if_desc neverEnterNode new_node_id (text "ext_lf_unlifted" <-> ppr id)
                            -- Shouldn't be possible. I don't think we can export letNoEscapes
                            LFLetNoEscape {}
                                -> set_if_desc maybeEnterNode new_node_id (text "ext_lf_lne" <-> ppr id)

                    -- General case, a potentially unevaluated imported id.
                    | not isFun
                    = set_if_desc maybeEnterNode new_node_id (text "ext_unknown_enter" <-> ppr id)

                    -- May or may not be entered.
                    | otherwise
                    = set_if_desc maybeEnterNode new_node_id (text "ext_unknown" <-> ppr id)
            put $!
                s { fs_idNodeMap = addToUFM (fs_idNodeMap s) id new_node_id
                , fs_doneNodes = addToUFM (fs_doneNodes s) new_node_id node }
  where
    isFun = isFunTy (unwrapType $ idType id)
    -- | When we don't use descriptions we can avoid creating
    --  a new node for e.g. each literal string. So we set the
    --  id only if descriptions are enabled.
    set_if_desc :: FlowNode -> NodeId -> SDoc -> FlowNode
    set_if_desc node node_id desc
        | hasNodeDesc = setNodeDesc desc $ node { node_id = node_id}
        | otherwise = node

-- | Returns the nodeId for a given imported Id.
importedFuncNode_Maybe :: Module -> Id -> AM (Maybe NodeId)
importedFuncNode_Maybe this_mod var_id
    -- Not an imported function
    | nameIsLocalOrFrom this_mod (idName var_id)
    = return Nothing
    | otherwise = do
        s <- get
        case lookupUFM (fs_idNodeMap s) var_id of
            Just node_id -> return $! Just node_id
            Nothing -> pprPanic "Imported id not mapped" (ppr var_id)

-- Get or make a nodeId for the given Id based on context.
mkCtxtEntry :: ContextStack -> Id -> AM (Id,NodeId)
mkCtxtEntry ctxt v
    | Just nodeId <- idMappedInCtxt v ctxt
    = return $! (v,nodeId)
    | otherwise
    = do
        !node_id <- mkUniqueId
        return $! (v, node_id)

{-# NOINLINE nodesTopBinds #-}
-- Note: We could expose the computed information about top level bindings
-- via interface files (or otherwise). But currently it's unused even though we return it.
nodesTopBinds :: Module -> [StgTopBinding] -> AM ([InferStgTopBinding], (VarEnv NodeId))
nodesTopBinds this_mod binds = do
    -- We preallocate node ids for the case where we must reference an node by id
    -- before we traversed the defining binding. (e.g. recursive groups)

    -- TODO: bindersOfTopBinds allocates an intermediate list, but we really
    -- shouldn't need to.
    let bind_ids = bindersOfTopBinds binds :: [Id]
    mappings <- foldM insertIdMapping mempty bind_ids :: AM (VarEnv NodeId)
    let topCtxt = CTopLevel mappings
    binds' <- mapM (nodesTop this_mod topCtxt) binds
    return (binds', mappings)
  where
    insertIdMapping :: VarEnv NodeId -> Id -> AM (VarEnv NodeId)
    insertIdMapping env v
        | idType v `eqType` addrPrimTy
        = return $! extendVarEnv env v addrNodeId
        | otherwise
        = extendVarEnv env v <$!> mkUniqueId

nodesTop :: Module -> SynContext -> StgTopBinding -> AM InferStgTopBinding
nodesTop _this_mod _ctxt (StgTopStringLit v str) = return (StgTopStringLit v str)
    -- String literals (and unlifted ids in general) are never entered.
    -- There is also no nested information so we can represent them all
    -- with a single preallocted data flow node. The ids are mapped to this
    -- node in `nodesTopBinds`

nodesTop this_mod ctxt (StgTopLifted bind)  = do
    bind' <- fst <$> nodesBind this_mod [ctxt] TopLevel NotLNE bind :: AM InferStgBinding
    return $! (StgTopLifted bind')

-- nodesBind creates the nodeIds for the bound rhs, the actual nodes are created in
-- nodeRhs. Returns the context including the let.
nodesBind :: Module -> ContextStack -> TopLevelFlag -> IsLNE -> StgBinding -> AM (InferStgBinding, ContextStack)
nodesBind this_mod ctxt bot lne (StgNonRec v rhs) = do
    boundId <- uncurry unitVarEnv <$> mkCtxtEntry ctxt v
    let ctxt' = ((CLet boundId lne) `extendCtxt` ctxt)
    rhs' <- (nodeRhs this_mod ctxt' bot v rhs)
    return $! (StgNonRec v rhs', (CLetBody boundId lne) `extendCtxt` ctxt)
nodesBind this_mod ctxt bot lne (StgRec binds) = do
    let ids = map fst binds
    boundIds <- mkVarEnv <$> mapM (mkCtxtEntry ctxt) ids :: AM (VarEnv NodeId)
    let ctxt' = (CLetRec boundIds lne) `extendCtxt` ctxt
    rhss' <- mapM (uncurry (nodeRhs this_mod ctxt' bot )) binds
    return $! (StgRec $ zip ids rhss', (CLetRecBody boundIds lne) `extendCtxt` ctxt)


{-
Note [RhsCon data flow]
~~~~~~~~~~~~~~~~~~~~~~~
Describes rules for lets like this:

    let x = Con args@[a1 .. an]

The data flow visually looks something like this.

+-----+      +------+
| con |      | args |
+--+--+      +--+---+
   |         |
   v         v
   +---------+
   | rhsNode |
   +---------+

The behaviour here is very similar to the one for
nodeConApp with a few alterations to account for the fact
that the result will be associated with a binding. The major
difference being that the rhsNode and the binding will be represented
by a single dataflow node.

The EnterLattices of the arguments are taken as is and are put
into the FieldInfo of the rhsNode.
The only exception is that we set strict fields to
NeverEnter because of the strict field invariant.
See also Note [The strict field invariant]

Doing this is implemented in mkOutConLattice.

The enterinfo is determined by a number of rules, required to uphold [The strict field invariant].
The main drivers are:
1) By default we infer tagged (NeverEnter) for all constructors allocated via a StgRhsCon because any
   regular constructor allocation results in a tagged pointer to the Constructor.
2) Top level constructor applications might turn into thunks if we need force any of their
   arguments to uphold the strict field invariant.
3) Recursive groups where strict arguments are defined in the group are currently
   considered MaybeEnter.

The enterInfo of the result is determined by checking these conditions
in order:

a.1) If the binding is not defined at the top level
   and is a non recursive binding:
->  NeverEnter, we can just wrap the constructor application in a Case.

a.2) If the binding is not defined at the top level
   and is in a recursive binding group, but all strict args
   are defined outside of the recursive group:
->  NeverEnter, we can just wrap the constructor application in a Case.

b) If there are no strict fields
->  NeverEnter, see 1)

c) If all strict field arguments are tagged (NeverEnter)
-> NeverEnter, see 1)

d) If any strict field arguments are UndetEnterInfo
-> UndetEnterInfo, if we don't know if a strict argument is already tagged
                   then we don't know if we need to wrap this application in
                   a case.

e) Otherwise
-> MaybeEnter

    Examples:
    ~~~~~~~~

Condition a.1) Unevaluated values in strict fields.

        data StrictLazy a b = SL !a b
        foo =
            ...
            let a1 = undefined
            let a2 = ...
            ...
            let x = SL a1 a2
            ...

    enterInfo(x) = NeverEnter <NeverEnter, enterInfo(a2)>

    Since we deal with a local let binding we will (after the analysis has run)
    push the allocation of the constructor past the evaluation of the arguments
    like this:

        foo =
            ...
            let a1 = undefined
            let a2 = True
            ...
            case a1 of a1'
                DEFAULT ->
                    let x = SL a1' a2
                    ...

    This is essentially what the Worker for the constructor would do
    as well. We can always do this for *any* local non-recursive let.

    Assinging NeverEnter to the first field will seem odd at first. But
    it makes sense once we consider that enterInfo(x) represents information
    about `x` *after* allocation of the constructor. Should arguments of
    strict fields be bottom then the constructor will not be allocated at
    all so there is no conflict.

    For this reason we can *always* set the information for strict fields
    to NeverEnter.

    Should there be multiple strict fields we simple generate more than
    one wrapping case expression.

Condition a.1) Tagged (NeverEnter) arguments to strict fields

    Rule a.1 also applies when we don't need a wrapping case expression.
    As is the case if the argument is already tagged. For example in this
    code:

        data StrictLazy a b = SL !a b
        foo =
            ...
            let a1 = True -- Nullary constructors are always unlifted/tagged.
            let x = SL a1 a2
            ...

    Since we know a1 is tagged we don't need to insert a case and
    we get the following enterInfo:

    enterInfo(x) == NeverEnter <NeverEnter, enterInfo(a2)>

    This also applies if x would be bound at the top level.


Condition a.2) wrapping local recursive groups:

        data StrictLazy a b = SL !a b
        foo =
            ...
            let a1 = undefined
            ...
            letrec {
                x = SL a1 y;
                y = Just x
            ...

    This can be wrapped just like the example for Rule a.1):

        foo =
            ...
            let a1 = undefined
            ...
            case a1 of a1'
                DEFAULT ->
                    letrec {
                        x = SL a1' y;
                        y = Just x
                    ...

    and results in enterInfo(x) = NeverEnter <NeverEnter, enterInfo(y)>

Condition b) Lazy constructors are always marked as NeverEnter.

        baz =
            ...
            let foo = Con1 a1 a2 a3
            ...

        bar = Con2 a1' a2'

    Both foo and bar get NeverEnter as enter info. The field info is
    exactly the same as the arguments. eg.

    enterInfo(foo) = NeverEnter<enterInfo(a1), enterInfo(a2), enterInfo(a3)>

    This works for both top level and local constructor bindings. This is because
    when we allocate a regular constructor the result is a tagged pointer
    (to the constructor). Since all fields are lazy we also are not in danger of
    having to wrap the constructor in a case (which would turn it into a Thunk).

Condition c) Strict constructors with tagged arguments.

        foo =
            let a1 = True
            let a2 = False
            ...
            let x = StrictPair a1 a2
            ...

    We can trivially infer that a1/a2 will be tagged (as they are nullary constructors).
    With this information available we can infer that there is no danger of the rhs
    turning into a thunk and as such we can infer enterInfo(x) = NeverEnter <NeverEnter, NeverEnter>

Condition d) Strict field undetermined

    Assuming we have infered so far that:

        enterInfo(a1) = UndetEnterInfo
        enterInfo(a2) = UndetEnterInfo

    and analyse this snippet:

        foo = StrictPair a1 a2

    We don't know if the arguments are tagged. As consequence we
    don't know if Case wrapping is required.

    Should we require Case wrapping, the binding will turn into a thunk.
    Should we not require wrapping, the binding will be tagged (NeverEnter).

    But since we can't determine this from the current state we the infered result will
    be:
        enterInfo(foo) = UndetEnterInfo <NeverEnter, NeverEnter>

    Keep in mind that the enterInfo for the fields of `foo` is NeverEnter since strict
    fields always get a enterInfo of NeverEnter.

Condition e) Default: Fall back to MaybeEnter.

    If none of the other conditions matched *then* we fall back to MaybeEnter.

    For us to reach e) there must be strict fields and
    they must be applied to a binder with enterInfo of AlwaysEnter/MaybeEnter/NoValue.

    If there is a value of MaybeEnter/AlwaysEnter for one of the strict arguments
    we need to evaluate this argument before allocation. In order to do this we will turn
    this RhsCon into a RhsClosure. Turning the rhs into a thunk.

    For example we have inferred:

        enterInfo(thunk) = AlwaysEnter

    and are looking at this code:

        foo = StrictJust thunk

    We will infer enterInfo(foo) = MaybeEnter <NeverEnter> and rewrite the binding to

        foo = case thunk of x -> StrictJust x

    Which turns foo into a thunk.

    Note that this case also handles the situations where we apply bottoming bindings
    to strict constructors. For example if enterInfo(thunk) is NoValue this
    represents storing the result of a computation which will not return into a
    strict field. We can safely treat this the same as the AlwaysEnter/MaybeEnter case.

    For example we might have:

        loop x = loop x
        thunk = loop ()

        foo = StrictJust thunk

    Then we infer enterInfo(foo) = MaybeEnter <NeverEnter> and rewrite the AST
    to

        loop x = loop x
        thunk = loop ()

        foo = case thunk of x -> StrictJust x

    In practice hitting this condition is quite rare. But it can make code slightly worse
    as some constructor applications at the top level turn into thunks.

-}

-- | Dealing with let bound rhss.
--  We pass in the id to which the RHS is bound. This allows us to check
--  if the RHS is part of a recursive group.
nodeRhs :: HasDebugCallStack => Module -> ContextStack -> TopLevelFlag
        -> Id -> StgRhs
        -> AM (InferStgRhs)
nodeRhs this_mod ctxt topFlag binding (StgRhsCon _ ccs con args)
  | null args = do
        -- pprTraceM "RhsConNullary" (ppr con <+> ppr node_id <+> ppr ctxt)
        let node = mkConstNode node_id (EnterLattice NeverEnter FieldsNone)
                                       (ppr binding <-> text "rhsConNullary")
        markDone $ node
        return $! (StgRhsCon (node_id,RhsCon) ccs con args)
  | otherwise = do
        mapM_ (addImportedNode this_mod ) [v | StgVarArg v <- args]
        node_inputs <- mapM (getConArgNodeId ctxt) args :: AM [NodeId]
        -- pprTraceM "RhsCon" (ppr con <+> ppr node_id <+> ppr args <+> ppr node_inputs <+> ppr ctxt)
        let node =  setNodeDesc (ppr binding <-> text "rhsCon") $
                    FlowNode
                        { node_id = node_id
                        , node_inputs = node_inputs
                        --, node_done   = False
                        , node_result = undetLat
                        , node_update = node_update node_id node_inputs
                        }
        addNode notDone node

        return $! (StgRhsCon (node_id,remainsConRhs) ccs con args)
  where
    node_id = getKnownIdNodeId ctxt binding
    !remainsConRhs
        | isTopLevel topFlag            = MaybeClosure
        -- a.1)
        | (CLet _ _ : _) <- ctxt        = RhsCon
        -- a.2)
        | (CLetRec binds _ : _) <- ctxt
        , not (binding `elemUFM` binds) = RhsCon
        -- strict argument defined in recursive group
        | otherwise                     = MaybeClosure

    node_update this_id node_inputs = do
        fieldResults <- mapM (lookupNodeResult) node_inputs
        let strictResults = getStrictConArgs con fieldResults
        let strictOuter = map enterInfo strictResults :: [EnterInfo]
        -- pprTraceM "RhsCon" (ppr con <+> ppr this_id <+> ppr fieldResults)
        -- See Note [RhsCon data flow]
        let outerTag
                -- a) If it's never turned into a closure it's always tagged.
                | remainsConRhs == RhsCon =
                    NeverEnter

                -- b) nothing to force/only lazy fields
                | not $ any isMarkedStrict $ dataConRepStrictness con
                =   NeverEnter

                -- c) If all of the strict inputs are tagged so is the output.
                | all (==NeverEnter) strictOuter
                = NeverEnter

                -- d) Taggedness depends on the taggedness of the arguments.
                | any (== UndetEnterInfo) strictOuter
                = UndetEnterInfo

                -- e)
                | otherwise
                = MaybeEnter


        -- Strict fields need to marked as neverEnter here, even if their inputs are not.
        -- This is because at the time we scrutinise the result of this rhs they will have
        -- been tagged.
        let result = mkOutConLattice con outerTag fieldResults
        let cappedResult = widenToNestingLevel nestingLimit result
        updateNodeResult this_id cappedResult
        return $! cappedResult




{-

TODO: As future work we could try to analyze which arguments a function is called with and use this to
      enhance the results of the analysis. But for now we don't do so.

TODO: Partial applications

* Currently we don't really try to retain field information of partial applications.
* But it might be worth doing so. If we then eventually fully apply the thunk we
  might bind the fields of the result and use the field information at that point.
  But it's not a big win so I haven't spent the time.


Note [RhsClosure data flow]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

    +---------+
    | rhsNode |  = <body>
    +---------+

This is rather simple:

We take enterInfo(body) and set the outermost enterInfo as follows:

1) NeverEnter iff <body> represents an absentError value. See [Taggedness of absentError]
2) AlwaysEnter iff there are no arguments. (It's a thunk).
3) NeverEnter otherwise. As functions aren't entered just applied.

We currently do not consider arguments at all, this would require a
guarantee that there are no call sites outside of this module.
Something currently not tracked by GHC.

nodeRhs really *only* modifies the outer enterInfo. Some examples:

Rule 1: Absent error:

    x = absentError foo
    => enterInfo(x) = NeverEnter

    ---

    bar = absentError foo
    x = bar
    => enterInfo(x) = absentError foo

Rule 2: AlwaysEnter:

    x = body@(case someThunk of y -> y)
    => enterInfo(x) = AlwaysEnter < enterInfo_ofFields(body)>

    That is we take whatever the result of analyzing body is, but we replace the
    outermost enterInfo with AlwaysEnter. This is the case for all thunks
    which are not absentError no matter how exactly the body looks like.

Rule 3: NeverEnter:

    f :: Int -> Int -> (Int,Int)
    f x y = body@(case x + y of bndr -> (bndr, bndr))
    => enterInfo(f) = NeverEnter < NeverEnter, NeverEnter >
                        ^ function    ^ bndr        ^ bndr

    The outermust enterInfo represents the (evaluated) function. We *omit* the enterInfo
    for the outermost enterInfo of the *value*. Because the result of a function is always
    evaluated so there is no need.

    For another example:

    f :: Int -> Int -> Maybe Int
    f x y = body@(Nothing)
    => enterInfo(f) = NeverEnter
                        ^ function, no fields so no more information than that.

-}

nodeRhs this_mod ctxt _topFlag binding (StgRhsClosure _ext _ccs _flag args body) = do
    (body', body_id) <- nodeExpr this_mod ctxt' body
    let node = setNodeDesc node_desc $
               FlowNode { node_id = node_id
                        , node_inputs = [body_id]
                        -- ^ We might infer things about nested fields once evaluated.
                        -- , node_done   = False
                        , node_result = EnterLattice enterInfo FieldsUndet
                        , node_update = node_update node_id body_id
                        }
    addNode notDone node
    return $! (StgRhsClosure _ext _ccs _flag args body')

  where
    node_id = getKnownIdNodeId ctxt binding
    node_desc
        | null args = text "rhsThunk:" <> (ppr binding)
        | otherwise = text "rhsFunc:" <> (ppr binding)
    -- We know nothing about the arguments.
    varMap = mkVarEnv (zip args (replicate arity unknownNodeId))
    ctxt' = (CClosureBody varMap `extendCtxt` ctxt)
    arity = length args
    enterInfo
        | isAbsentExpr body = NeverEnter
        | null args = AlwaysEnter
        | otherwise = NeverEnter      -- Thunks with arity > 0
                                    -- are only entered when applied.
    node_update this_id body_id = do
        -- SIMPLE: We ignore results from within the body.
        -- bodyInfo <- lookupNodeResult body_id
        let bodyInfo = undetLat
        let result = setEnterInfo bodyInfo enterInfo
        let cappedResult = widenToNestingLevel nestingLimit result
        if hasFinalFields cappedResult
            then do
                node <- getNode this_id
                markDone $ node { node_result = cappedResult }
            else updateNodeResult this_id cappedResult
        return $! cappedResult

-- Constraints for possible STG expressions. Most are delegated to helper functions.
nodeExpr :: Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeExpr this_mod ctxt (e@StgCase {})          = nodeCase this_mod ctxt e
nodeExpr this_mod ctxt (e@StgLet {})           = nodeLet this_mod ctxt e
nodeExpr this_mod ctxt (e@StgLetNoEscape {})   = nodeLetNoEscape this_mod ctxt e
nodeExpr this_mod ctxt (StgTick t e)           = do
    (e',nodeId) <- nodeExpr this_mod ctxt e
    return $! (StgTick t e', nodeId)
nodeExpr this_mod ctxt e@(StgConApp {})        = nodeConApp this_mod ctxt e
nodeExpr this_mod ctxt e@(StgApp {})           = nodeApp this_mod ctxt e
-- Do the boring ones right here
nodeExpr _ _ctxt  (StgLit lit)              = return $! (StgLit lit, litNodeId)
-- Not currently analysed, mostly deal with unlifted values anyway.
nodeExpr _ _ctxt  (StgOpApp op args res_ty) = return $! (StgOpApp op args res_ty, unknownNodeId)
nodeExpr _ _ctxt  (StgLam {})               = error "Invariant violated: No lambdas in STG representation."

{-
Note [Case Data Flow Nodes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case expressions result in a few control flow nodes and constraints.

Information between the different nodes for a case expression
is best shown by example:

    case e1 of bndr
    {   C1 f1 f2 -> alt1;
        C2       -> alt2; }

This result in data flow nodes and data flow between them
as shown below.

                              Always NoEnter
+-----------+                   +--------+
| Scrutinee +---- fieldInfo --->+  bndr  |
+-----+-----+                   +--------+
      |
      +
  fieldInfo
      +     bind field info
      |      to alt bndrs
      v          |
      |    +-----------+         +--------+
      |    |           |         |        |
      +--->+  f1  f2   +-------->+  alt1  +--------+
      |    |           |         |        |        |
      |    +-----------+         +--------+        v            +-----------+
      |                              |        +----+----+       |           |
      v                    result of alt rhs  | Combine +------>+ Case Node |
      |                              |        +----+----+       |           |
      |                          +--------+        ^            +-----------+
      |                          |        |        |
      +--------->------->------->+  alt2  +--------+
                                 |        |
                                 +--------+

Enumerating the case constraints:

1) Case Binder:
   enterInfo(bndr) = setOuter(NeverEnter, enterInfo(scrutinee))
   Binders are like the scrutinee but evaluated, that is outermost enterInfo = NeverEnter.

2) Field binders:
   enterInfo(field_n)
     | isStrictField = setOuter(NeverEnter, field_n(enterInfo((scrutinee)))
     | otherwise = field_n(enterInfo(scrutinee))
   Bound fields get their info from the scrutinees fieldInfo, except for strict fields whos
   outermost enterInfo get's set to NeverEnter.

3) Case expression:
    enterInfo(caseExpr) = lub(enterInfo(alt1), enterInfo(alt2), ... ,enterInfo(altn))

How branches are combined is explained in the Note [The lattice element combinators]
and Note [Combining Branches].

Note that the enterInfo of case alternatives can depend on case binders/field binders
if they reference them. But they are determined just as any other expression otherwise.



Examples:

    data StrictLazy a b = SL !a b

    ... case e of e'
            StrictLazy strict lazy -> lazy :: Int
            Nothing -> alt2 :: Int

    Let's assume enterInfo(x) = MaybeEnter if we apply the constraints we get:

1) Case Binder:
    enterInfo(e') = setOuter(NeverEnter, enterInfo(e))
    => setOuter(NeverEnter, MayberEnter)
    => NeverEnter

2) Field binders:

    For the lazy field:

    enterInfo(lazy) = field_n(enterInfo(scrutinee))
    => field_2(MaybeEnter)
    => UndetEnterInfo  -- See Note [Lattice for tag analysis]

    For the strict field:

    enterInfo(strict) = setOuter(NeverEnter, field_n(enterInfo(scrutinee)))
    => setOuter(NeverEnter, field_1(MaybeEnter))
    => setOuter(NeverEnter, MaybeEnter)
    => NeverEnter -- Because of the strict field invariant.


3)  Case expression:

    let's assume enterInfo(lazy) = MaybeEnter, enterInfo(alt2) = NeverEnter

    enterInfo(caseExpression) = lub(alt1, alt2)
    => lub(lazy, alt2)
    => lub(MaybeEnter, NeverEnter)
    => lub(MaybeEnter)

-}

-- See Note [Case Data Flow Node]
nodeCase :: Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeCase this_mod ctxt (StgCase scrut bndr alt_type alts) = do
    (scrut',scrutNodeId) <- nodeExpr this_mod (CCaseScrut `extendCtxt` ctxt) scrut
    bndrNodeId <- nodeCaseBndr scrutNodeId bndr
    let ctxt' = CCaseBndr (unitVarEnv bndr bndrNodeId) `extendCtxt` ctxt
    (alts', altNodeIds) <- unzip <$> mapM (nodeAlt this_mod ctxt' scrutNodeId) alts
    !caseNodeId <- mkJoinNode altNodeIds
    -- pprTraceM "Scrut, alts, rhss" $ ppr (scrut, scrutNodeId, altNodeIds, altsId)
    return $! (StgCase scrut' bndr alt_type alts' , caseNodeId)
nodeCase _ _ _ = panic "Impossible: nodeCase"


-- Take the result of the scrutinee and mark it as tagged.
nodeCaseBndr :: NodeId -> Id -> AM NodeId
nodeCaseBndr scrutNodeId _bndr = do
    !bndrNodeId <- mkUniqueId
    addNode notDone $ setNodeDesc (text "caseBndr" <-> parens (ppr scrutNodeId) <-> ppr _bndr) $
                      FlowNode
                        { node_id = bndrNodeId
                        , node_inputs = [scrutNodeId] --, node_done = False
                        , node_result = undetLat
                        , node_update = updater bndrNodeId
                        }
    return bndrNodeId
      where
        -- updater bndrNodeId = do
        --     scrutResult <- lookupNodeResult scrutNodeId
        --     let result = setEnterInfo scrutResult NeverEnter
        --     if hasFinalFields result
        --         then do
        --             node <- getNode bndrNodeId
        --             markDone $ node { node_result = result }
        --         else
        --             updateNodeResult bndrNodeId result
        --     return $! result
        -- SIMPLE: Just mark it tagged.
        updater bndrNodeId = do
            let result = neverEnterLat
            node <- getNode bndrNodeId
            markDone $ node { node_result = result }
            return $! result

nodeAlt :: HasDebugCallStack => Module -> ContextStack -> NodeId -> StgAlt -> AM (InferStgAlt, NodeId)
nodeAlt this_mod ctxt scrutNodeId (altCon, bndrs, rhs)
  | otherwise = do
    bndrMappings <- mkVarEnv <$> zipWithM mkAltBndrNode [0..] bndrs
    let ctxt' = (CAlt bndrMappings) `extendCtxt` ctxt
    (!rhs', !rhs_id) <- nodeExpr this_mod ctxt' rhs
    return $! ((altCon, bndrs, rhs'), rhs_id)

    where
        strictBnds :: [Id]
        strictBnds
          | DataAlt con <- altCon
          = getStrictConArgs con bndrs
          | otherwise = []

        -- Result for ONE of the bindings bound by the alt.
        -- Eg for an StgAlt of (Just, [foo], expr) we call mkAltBndrNode 0 foo
        mkAltBndrNode :: Int -> Id -> AM (Id,NodeId)
        mkAltBndrNode n bndr
          | isUnliftedType bndrTy
          , not (isUnboxedTupleType bndrTy)
          , not (isUnboxedSumType bndrTy)
          = do
                !node_id <- mkUniqueId
                addNode isDone litNode { node_id = node_id }
                return $! (bndr,node_id)
          | otherwise = do
                node_id <- mkUniqueId --New ID since we might shadow existing binds
                let updater = do
                        -- Fancy code:
                        -- so just hard code bndr_res
                        -- scrut_res <- lookupNodeResult scrutNodeId :: AM EnterLattice
                        -- let bndr_res = (indexField scrut_res n) :: EnterLattice
                        -- let is_strict_field = elem bndr strictBnds
                        -- let result
                        --         | is_strict_field
                        --         -- Tag things coming out of strict binds
                        --         = setEnterInfo bndr_res NeverEnter
                        --         | otherwise = bndr_res

                        -- pprTraceM "Updating altBndr:" (ppr (node_id, result) $$
                        --         text "Input:" <+> ppr scrutNodeId $$
                        --         text "scrut_res" <+> ppr scrut_res $$
                        --         text "bndr_res" <+> ppr bndr_res )
                        -- let finalFields = hasFinalFields result
                        -- if (is_strict_field && finalFields) || (finalFields && enterInfo result == MaybeEnter)
                        --     then do
                        --         node <- getNode node_id
                        --         markDone $ node {  node_result = result }
                        --     else
                        --         updateNodeResult node_id result
                        -- return $! result

                        -- Simple code: Just set tag for strict fields,
                        -- set non-tagged otherwise.
                        let is_strict_field = elem bndr strictBnds
                        let result
                                | is_strict_field = neverEnterLat
                                | otherwise = undetLat

                        node <- getNode node_id
                        markDone $ node {  node_result = result }
                        return $! result


                addNode notDone $ setNodeDesc (text "altBndr" <-> ppr altCon <-> ppr bndr) $
                    FlowNode
                        { node_id = node_id
                        , node_result = undetLat
                        -- , node_done = False
                        , node_inputs = [scrutNodeId]
                        , node_update = updater
                        }
                return $! (bndr,node_id)
            where
                bndrTy = idType bndr

-- Note [Let bindings and their context]

-- If we analyse a binding of the form:

--     let f x = e in body

-- then we analyze `e` in the context of CLet[Rec]
-- and `body` in the context of CLet[Rec]Body.

-- In each case the context carries the *same* mapping
-- of binding ids to node ids, however we use different
-- constructors in order to be able to differentiate between tail
-- call branches and regular references to an id.
-- See Note [Recursive Functions] for the details.

{-
Note [Let/ Data Flow]
~~~~~~~~~~~~~~~~~~~~~
This is rather simple. For a construct like:

    expr@(let x = rhs in body)

We bind the result of the rhs to the variable (x) which binds it.
The result of the whole expression is equivalent to the result of
the `body` expression.

The variable can be referenced from the body or the rhs itself
for let recs.

That is for a let expression of the form

    letExpr@(let var = rhs in body)

We have only the constraints that:

1) enterInfo(var) = enterInfo(rhs)
2) enterInfo(letExpr) = enterInfo(body)

Implementation wise the body/rhs can reference the data flow node
for the bound variable in the SynContext.

-}


nodeLet :: Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeLet this_mod ctxt (StgLet ext bind expr) = do
    (bind',ctxt') <- nodesBind this_mod ctxt NotTopLevel NotLNE bind
    (expr',node) <- nodeExpr this_mod ctxt' expr
    return $! (StgLet ext bind' expr', node)
nodeLet _ _ _ = panic "Impossible"

nodeLetNoEscape :: Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeLetNoEscape this_mod ctxt (StgLetNoEscape ext bind expr) = do
    (bind',ctxt') <- nodesBind this_mod ctxt NotTopLevel LNE bind
    (expr',node) <- nodeExpr this_mod ctxt' expr
    return $! (StgLetNoEscape ext bind' expr', node)
nodeLetNoEscape _ _ _ = panic "Impossible"

{-
Note [ConApp Data Flow]
~~~~~~~~~~~~~~~~~~~~~~~
Information from the constructor (strict fields)
and arguments is used to determine the result.

The information from both the constructor and the given arguments "flow"
into the final node to determine the result.

+-----+       +------+
| con |       | args |
+--+--+       +--+---+
   |             |
   v             v
   +-------------+
   | ConApp node |
   +-------------+

The enterInfo from this node is never used directly as it only appears
in expression contexts. (Rhs of case alternative, closures or lets).
In cases like (StgRhsClosure ... expr), expr == StgConApp
we determine the enterInfo of the rhs based on the fact that it's a RhsClosure.
In other contexts it's enterInfo will never be used by another node.

So if we have an ConApp expression of the form: app@(Con arg1 arg2 ... argn) we
solve the constraint:

    enterInfo(app) == MaybeEnter < fieldInfo(arg1), fieldInfo(arg2), ... , fieldInfo(argn) >

    where fieldInfo(arg_n) is defined as:

    fieldInfo(field_n)
        | isStrictField(field_n) = setOuter(NeverEnter, enterInfo(arg_n))
        | otherwise = enterInfo(arg_n)


We set strict field to NeverEnter because of the strict field invariant.
See also Note [The strict field invariant].

Doing this is implemented in mkOutConLattice.

Example:

    Consider this snippet and already derived information:

    data StrictLazy a b = SL !a b

        ...
        let thunk_x = ... :: Bool
        let thunk_y = ... :: Bool

        ... app@(SL thunk_x thunk_y)

        enterInfo(thunk_x) = MaybeEnter
        enterInfo(thunk_y) = MaybeEnter


    Here we get enterInfo(app) = MaybeEnter   < NeverEnter,   MaybeEnter>
                                  ^from ConApp  ^from strict field  ^from enterInfo(thunk_y)
                                                 invariant.
-}
nodeConApp :: HasDebugCallStack => Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeConApp this_mod ctxt (StgConApp _ext con args tys) = do
    node_id <- mkUniqueId
    mapM_ (addImportedNode this_mod) [v | StgVarArg v <- args]
    inputs <- mapM (getConArgNodeId ctxt) args :: AM [NodeId]
    let updater = do
            --SIMPLE
            -- fieldResults <- mapM lookupNodeResult inputs :: AM [EnterLattice]
            -- let result = mkOutConLattice con MaybeEnter fieldResults
            -- pprTraceM "UpdateConApp:" $ ppr (node_id,result) <+> text "inputs:" <> ppr inputs
            let result = neverEnterLat
            updateNodeResult node_id result
            return $! result

    addNode notDone $ setNodeDesc (text "conApp") $ FlowNode
        { node_id = node_id
        , node_result = undetLat
        , node_inputs = inputs
        , node_update = updater
        }

    return $! (StgConApp node_id con args tys, node_id)
nodeConApp _ _ _ = panic "Impossible"

{-
Note [App Data Flow]
~~~~~~~~~~~~~~~~~~~~
This is one of the more involved data flow constructs.
The actual flow if information is rather simple:

    `StgApp f arg`

Induces this data flow

+---+     +------+
| f |     | args |
+-+-+     +--+---+
  |          |
  v          v
  +----------+
  | app node |
  +----------+

However there are a lot of rules which go into how the "app node"
actually uses the information give. We check these conditions in order:

1) If f is imported:
->  We compute it's enterInfo as described by Note [Imported Ids]

2) If f is an absent expression:
->  We treat it as NeverEnter.

3) If f is a simple recursive tail call:
->  We mark the result as such: NoValue x RecFields
    See [Recursive Functions] for details.

4) If f is part of mutual recursive binds.
or is a unsaturated function call:
->  We throw up our hands and determine we know nothing.

5) If f is a unsaturated function call:
-> we also give up and infer no information.

6) If f is a saturated function:
->  We determine setOuter(MaybeEnter, enterInfo(f))

7) If f is not a function, and has no args:
->  We reuse the information of f

8)
In any other case:
->  We throw up our hands and determine we know nothing.

Examples:

1) Imported ids:

    e@(head xs)
    => enterInfo(e) = importedInfo(head)
    => NeverEnter

    Where importedInfo is implemented by `addImportedNode`

2) Absent error:

    e@(absentError foo)
    => enterInfo(e) = NeverEnter

4) Self-recursive calls:

    letrec {
        f x = ...
        g x = ...
        h x = ... expr@(g foo) ...
        }

    Here g is part of the recursive group so rule 7 triggers and given the context we derive:
        enterInfo(g foo) = MaybeEnter

5) Unsaturated functions:

    e@(f x), arity(f) > 1:

    enterInfo(f x) = MaybeEnter

    We don't retain field information of f. Since there is likely
    little gain for too much complexity. But we could revisit this.

6) Saturated function call:

    Given:

    f x = case x of _ -> (True,False)
    enterInfo(f) = NeverEnter < NeverEnter, NeverEnter >

    If we have an expression `e@(f x)` we derive:

    enterInfo(e)
    => setOuter(MaybeEnter, enterInfo(f))
    => setOuter(MaybeEnter, NeverEnter < NeverEnter, NeverEnter >)
    => MaybeEnter < NeverEnter, NeverEnter >

7) Variable expressions:

    For any variable expressions (e.g. expr@v) we derive enterInfo(expr) = enterInfo(v).

8) Other cases: We derive no information, that is for any other application we derive
    enterInfo(expr) = MaybeEnter

-}

nodeApp :: HasDebugCallStack => Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
-- SIMPLE
nodeApp _this_mod ctxt expr@(StgApp _ f args) = do
    node_id <- mkUniqueId
    let updater = do
            !result <- mkResult
            node <- getNode node_id
            markDone $ node { node_result = result }
            return $! result

    addNode notDone $ setNodeDesc (text "app" <-> ppr f <> ppr args) $
        FlowNode
            { node_id = node_id
            , node_result = undetLat
            , node_inputs = inputs
            , node_update = updater
            }

    return $! (StgApp node_id f args, node_id)
  where
    inputs = []

    -- See Note [App Data Flow]
    mkResult :: AM EnterLattice
    mkResult
        | isAbsentExpr expr = return $! nullaryLattice NeverEnter
        | otherwise         = return maybeLat

-- FANCY
-- nodeApp this_mod ctxt expr@(StgApp _ f args) = do
--     mapM_ (addImportedNode this_mod) (f:[v | StgVarArg v <- args])
--     maybeImportedFunc <- importedFuncNode_Maybe this_mod f
--     case () of
--         _
--             | Just node_id <- maybeImportedFunc
--             ->  return $! (StgApp node_id f args, node_id)
--             | otherwise -> do
--                 node_id <- mkUniqueId
--                 let updater = do
--                         !result <- mkResult

--                         -- pprTraceM "Updating " (ppr node_id)
--                         -- Try to peek into the function being applied
--                         -- node <- getNode node_id
--                         -- !input_nodes <- mapM getNode inputs
--                         -- pprTraceM "AppFields" (ppr (f, result) <+> ppr node $$
--                         --     text "inputs:" <+> ppr inputs $$
--                         --     ppr input_nodes
--                         --     )
--                         if (null inputs || isFinalValue result )
--                             -- We have collected the final result
--                             then do
--                                 -- pprTraceM "Limiting nesting for " (ppr node_id)
--                                 node <- getNode node_id
--                                 markDone $ node { node_result = result }
--                                 return $! result
--                             else do
--                                 updateNodeResult node_id result
--                                 return $! result

--                 addNode notDone $ setNodeDesc (text "app" <-> ppr f <> ppr args) $
--                     FlowNode
--                         { node_id = node_id, node_result = undetLat
--                         , node_inputs = inputs
--                         , node_update = updater
--                         }

--                 return $! (StgApp node_id f args, node_id)
--   where
--     inputs
--         | isAbsentExpr expr = []
--         | isFun && (not isSat) = []
--         | recTail = []
--         | isFun && isSat = [f_node_id]
--         | otherwise = [f_node_id]

--     -- See Note [App Data Flow]
--     mkResult :: AM EnterLattice
--     mkResult
--         | isAbsent =
--             -- pprTrace "Absent:" (ppr f) $
--             return $! nullaryLattice NeverEnter

--         | isFun && (not isSat) = return $! maybeLat

--         -- App in a direct self-recursive tail call context, returns nothing
--         | recTail = return $! nullaryLattice NoValue

--         | OtherRecursion <- recursionKind
--         =   lookupNodeResult f_node_id

--         | NoMutRecursion <- recursionKind =
--             -- pprTrace "simpleRec" (ppr f) $
--             lookupNodeResult f_node_id

--         | isFun && isSat = (`setEnterInfo` MaybeEnter) <$!> lookupNodeResult f_node_id


--         {- TODO: If we build a pap, but keep track of the field values we should
--             be able to use these if it's fully applied later in the body. eg:

--             case f x of pap ->
--                 let res = pap y in (resulting in tagged fields)
--                 if cond then Just <taggedThing> else res

--             But we currently don't do so.
--         -}
--         | not isFun
--         , null args
--         = lookupNodeResult f_node_id

--         | otherwise
--         = return $! maybeLat

--     recTail = recursionKind == NoMutRecursion && isRecTail f ctxt
--     isFun = isFunTy (unwrapType $ idType f)
--     arity = idFunRepArity f
--     isSat = arity > 0 && (length args == arity)
--     isAbsent = isAbsentExpr expr

--     -- We check if f is imported using importedFuncNode_Maybe so this
--     -- is guarantedd to be not imported when demanded.
--     f_node_id = getKnownIdNodeId ctxt f

--     recursionKind = getRecursionKind ctxt

--     getRecursionKind [] = NoRecursion
--     getRecursionKind ((CLetRec ids _) : _) | f `elemVarEnv` ids =
--                 if sizeUFM ids == 1 then NoMutRecursion else OtherRecursion
--     getRecursionKind (_ : todo) = getRecursionKind todo
nodeApp _ _ _ = panic "Impossible"

-- These are inserted by the WW transformation and we treat them semantically as tagged.
-- This avoids us seqing them when we shouldn't.
-- See [Taggedness of absentError]
isAbsentExpr :: GenStgExpr p -> Bool
isAbsentExpr (StgTick _t e) = isAbsentExpr e
isAbsentExpr (StgApp _ f _)
  | idUnique f == absentErrorIdKey = True
  -- I'm not convinced that this via strictness is required for module-internal functions.
  -- But it's hard to proof otherwise so we just accept this overhead.
  | (_, Absent) <- splitStrictSig (idStrictness f)
  = True
isAbsentExpr _ = False

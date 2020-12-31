--
-- Copyright (c) 2019 Andreas Klebinger
--

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

{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file -ddump-stg -ddump-cmm -ddump-asm -ddump-stg-final #-}
{-# OPTIONS_GHC -dsuppress-coercions -dno-suppress-type-signatures -dno-suppress-module-prefixes #-}
-- {-# OPTIONS_GHC -fprof-auto #-}
{-# OPTIONS_GHC -ticky -ticky-allocd -ticky-dyn-thunk #-}
{-# OPTIONS_GHC -dsuppress-ticks #-}

-- Node descriptions are not always present.
-- #if defined(DEBUG)
-- {-# OPTIONS_GHC -Wno-missing-fields #-}
-- #endif

module GHC.Stg.InferTags
    (findTags, EnterLattice)
where

#include "HsVersions.h"

#if defined(DEBUG)
#define WITH_NODE_DESC
#endif

import GHC.Prelude

import GHC.Types.Id
import GHC.Unit.Types (Module)
import GHC.Stg.Syntax as StgSyn hiding (AlwaysEnter)
import GHC.Stg.Utils

import GHC.Types.Unique.FM

import GHC.Driver.Session (DynFlags)


import GHC.Stg.InferTags.Types
import GHC.Stg.InferTags.Analysis
import GHC.Stg.InferTags.Rewrite

{-

Note [Debugging Taggedness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In practice bugs happen. So I added a few ways to make issues with this code easier to debug.

* There is a flag to dump the result of this analysis (ddump-stg-tag-nodes).
    I found this immensely helpful during developement.
* There is a flag to emit runtime checks to confirm the results of taggedness (dtag-inference-checks).
    If we case on a value we expect to be tagged it adds a runtime check for the tag. If there is no tag
    your program will terminate immediately which makes it a lot easier to find the root cause.
    This is done in GHC.StgToCmm.Expr.cgIdApp.
* Defining WITH_NODE_DESC extends the data flow graph with a more detailed description of the nodes,
    making it easier understand when dumping all the nodes of the graph with ddump-stg-tag-nodes.

    This is disabled by default, enabled by -DDEBUG, but can also be enabled manually by defining
    WITH_NODE_DESC. Since this has a significant performance impact it is disabled by default.

    When WITH_NODE_DESC is defined the FlowNode type will have an additional field _node_desc which
    should help map dataflow nodes to constructs in the Stg AST.



Note [Tag Inferrence - The basic idea]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This code has two goals:

a) Ensure constructors with strict fields are only
    allocated with tagged pointers in the strict fields.
b) Infer if a given case scrutinizing an id (case x of ...) is guaranteed to
    refer to a tagged pointer. See also Note [Optimizing function closures]

You might think that, because the wrapper of a constructor evaluates the
field before allocating the Constructor the strict fields would always be tagged.

However #16831 shows one example where this is not the case. The basic issue
is that the simplifier can and will discard a case like
    case x of x'
        _ -> rhs
if x is a binding with a unfolding of `OtherCon []`. Rightfully so since this
indicates that a value is *evaluated*. But while all *tagged* values are
evaluated not all evaluated vales are tagged. So we sometimes have to enter evaluated
values in order to get a properly tagged pointer. See also #15155, #14677 and slightly
related #17004.

Id's refered to by a tagged pointer do not have to be entered
when used in an case expression which has massive performance
benefits. See for example the results presented in the
"Faster Laziness Using Dynamic Pointer Tagging" paper.
If we can *infer* that a pointer is tagged without a runtime check
we can improve performance further. We can remove a conditional
jump in the generated code which means we have to execute fewer code
for one, but also free up prediction resources in the CPU for other
branches.
This is especially true for the traversals of strict data structures
where repeated checks for the presence can carry a high cost.

This Module contains the code for the actual inference and upholding
the strict field invariant. See Note [The strict field invariant]
Once computed the infered tag information is stored in the extension
point of StgApp. This is then used in StgCmmExpr to determine if we
need to enter an expression.

This is essentially a form of 0CFA analysis. The correspondence is as follows:

* Labeling expressions:

    Is done by creating a data flow node for the expression. Nodes are labeled
    by their id's. While labeling we also set up the initial dataflow state which
    is implicit encoded in the `node_result` field for each dataflow node.

    The data flow graph is created initially by nodesTopBinds.

    It's nodes are represented by the FlowNode type. Which incoming edges to
    each node being recorded in the nodes themselves.

* Inference rules:
    Inference rules are encoded in the `node_update` field of nodes.
    These functions when executed update the dataflow state based on the existing
    dataflow state. The functions themselves are created in the various `node*`
    functions eg. `nodeLiteral`

    The simpler ones just return a constant result (and hence are only run once).
    More complicated ones might reference current values of other nodes to
    determine their result and can be iterated often.

    All node* functions have explicit documentation describing both how
    data flows between their inputs and outputs, as well as how new
    constraints are generated. But it's fairly informal.

*  Solving of the constraints:
    Constraints are solved by iterating node_update functions until we reach
    a fixpoint or an iteration limit.

All inference rules have been crafted such that intermediate results are safe to use.
This means even if we fail to reach a fixpoint we can use the results gathered up to
that point safely.

The kind of information we track has it's own Note [Lattice for tag analysis].

Once the dataflow analysis has run we extract relevant information from the constraints in
the rewrite* (e.g. rewriteRhs) functions. This:
+ Updates the extension points where appropriate.
+ Inserts seq where required to uphold the strict field invariant.
    See Note [The strict field invariant].

I found that commonly 0CFA is represented as having distinct maps
for variables and labels. Implementation wise we instead
assign all occurences refering to a local variable the same label.
Which is done by keeping a mapping of variables to labels. This has
the advantage that it works even in the presence of shadowing.
This is beneficial for running the analysis. But does impose some
overhead during creation of the data flow graph (but overall improves perf).
Imported ids are simply referenced by their unique instead.

-}


{-# NOINLINE findTags #-}
findTags :: DynFlags -> Module -> [StgTopBinding] -> ([TgStgTopBinding], [(Id,EnterLattice)])
-- findTags this_mod us binds = passTopBinds binds
findTags dflags this_mod binds =
    let state = FlowState {
            fs_idNodeMap = mempty,
            fs_uqNodeMap = emptyUFM,
            fs_doneNodes = emptyUFM }
    -- Run the analysis
        analysis :: AM ([TgStgTopBinding], [(Id, EnterLattice)])
        analysis = do
            addConstantNodes
            (binds',_mapping) <- {-# SCC "mkFlowNodes" #-}
                                nodesTopBinds this_mod binds
            {-# SCC "solveConstraints" #-} (solveConstraints dflags)
            -- exports <- exportTaggedness mapping
            exports <- return mempty -- Should probably remove this
            !finalBinds <- {-# SCC "rewriteAST" #-}
                           rewriteTopBinds binds'
            return (finalBinds, exports)
        (!binds', exports) = evalAM state $ analysis
    in  (seqTopBinds binds') `seq`
            -- pprTrace "foundBinds" (ppr this_mod)
                (binds',exports)



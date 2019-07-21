--
-- Copyright (c) 2019 Andreas Klebinger
--

{-# LANGUAGE CPP, TypeFamilies, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs, TupleSections,DataKinds #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}


-- Vector stuff
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}


-- {-# LANGUAGE Strict #-}
-- {-# OPTIONS_GHC -O #-}

module StgInferTags (findTags) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes
import DataCon
import Id
import qualified StgSyn
import StgUtil
import Outputable
import CoreSyn (AltCon(..))
import Module

import VarSet
-- import UniqMap

import TyCon (tyConDataCons)
import Type -- (tyConAppTyCon, isUnliftedType, Type)
import UniqSupply
import RepType
import StgSyn hiding (AlwaysEnter)
import StgUtil

import Name
import PrelNames

import Control.Monad

import Unique
import UniqFM
import UniqSet (nonDetEltsUniqSet)
import Util
import Data.Ord (comparing)

import State
import Maybes
import Control.Applicative hiding (empty)

import GHC.Generics
import Control.DeepSeq -- hiding (deepseq)
import GHC.Stack
import Data.Bifunctor

import Unsafe.Coerce
import ErrUtils
import System.IO.Unsafe
import Outputable
import DynFlags
import Digraph

import VarEnv

import GHC.Exts (reallyUnsafePtrEquality#, isTrue#)

-- Debugging/Profiling things

import System.CPUTime
-- import System.IO.Unsafe

-- Grow them trees:

-- #define WITH_NODE_DESC

-- Avoid deep comparisons with reallyUnsafePtrEquality#
maybeEq :: a -> a -> Bool
maybeEq x1 x2 = isTrue# (reallyUnsafePtrEquality# x1 x2)


type instance BinderP       'InferTags = Id
type instance XRhsClosure   'InferTags = NoExtFieldSilent
type instance XRhsCon       'InferTags = NodeId
type instance XLet          'InferTags = NoExtFieldSilent
type instance XLetNoEscape  'InferTags = NoExtFieldSilent
type instance XStgApp       'InferTags = NodeId
type instance XStgConApp    'InferTags = NodeId


type InferStgTopBinding = GenStgTopBinding 'InferTags
type InferStgBinding    = GenStgBinding    'InferTags
type InferStgExpr       = GenStgExpr       'InferTags
type InferStgRhs        = GenStgRhs        'InferTags
type InferStgAlt        = GenStgAlt        'InferTags

data RecursionKind
    = SimpleRecursion
    -- ^ Simple direction recursion of the (syntactic) form
    --   let f x = ... if cond then e' else f x'

    | OtherRecursion
    -- ^ All other kinds
    | NoRecursion
    deriving Eq

-- deepseq _ x = x

-- Note [Lattice instance for tag analysis]

-- We use a semi lattice for the tag analysis, since all results are
-- monotonic towards top for each step of the analysis we never need
-- to use glb.

-- We might terminate analysis of an expression early. For example
-- to avoid trying to compute infinite results or when it is clear
-- no more information can be gathered.

-- When we do so we use `fixBot` to lift all remaining bot values to
-- the top of the lattice.

-- This is sadly required. Consider code of this form:

-- rhs = case scrut of
--     p1 -> Just 1 : tag = tagged <tagged>
--     p2 -> Just e2: tag = tagged <undet>

-- We determine the tag of the whole RHS by computing the lub
-- of both alternatives, in this case `lub (tagged <tagged>) (tagged <undet>) = tagged <tagged>`.

-- This is incorrect! e2 might be an expression which will not get tagged.
-- So whenever we stop analysing an expression we will take all remaining
-- undetermined values and lift them to the top of the latice.

-- The tag information of the rhs therefore becomes:

-- `lub (tagged <tagged>) (tagged <unknown>) = tagged <unknown>` which is correct.




class Lattice a where
    bot :: a
    top :: a


-- | Enterinfo for a binding IF IT USED AS AN UNAPPLIED ATOM.
--   In particular for
--     case (food<NoEnter> ingredients) of ...
--   we WILL need to evaluate food either way.
-- However if an id is determined to be NeverEnter we can say
-- that we can put it directly in strict fields without violating
-- the tagging invariant as well as casing on it as data without entering
-- eg the code:
-- case determineBurger<NoEnter> of
--     CheeseBurger -> e1
--     Regular -> e2
-- does not require us to emite code for entering determineBurger to branch on it's value.
data EnterInfo
    = UndetEnterInfo    -- ^ Not yet determined, happens for rhsCon if we don't
                        --   know if the strict fields are already tagged.
    | RecEnter          -- ^ Direct tail recursion
    | AlwaysEnter        -- ^ WILL need to be entered
    | MaybeEnter        -- ^ Could be either
    | NeverEnter        -- ^ Does NOT need to be entered.
    deriving (Eq,Ord,Show,Enum,Generic,NFData)

instance Outputable EnterInfo where
    ppr UndetEnterInfo  = char '?'
    ppr RecEnter        = text "rec"
    ppr AlwaysEnter     = text "ent"
    ppr MaybeEnter      = char 'm'
    ppr NeverEnter      = char 't'

{- |
              MaybeEnter
             /    |    \
      AlwaysEnter |  NeverEnter
             \    |    /
              RecEnter
                  |
            UndetEnterInfo

Where NeverEnter means something is tagged,
while AlwaysEnter means something isn't tagged.

-}
instance Lattice EnterInfo where
    bot = UndetEnterInfo
    top = MaybeEnter

data SumInfo
    = SumInfo !DataCon [EnterLattice] -- ^ A constructor application
    deriving (Generic)

instance Eq SumInfo where
    (==) (SumInfo c1 fs1) (SumInfo c2 fs2) =
        c1 == c2 && fs1 == fs2

instance NFData SumInfo where
    rnf (SumInfo _ fields) = rnf fields

instance Outputable SumInfo where
    ppr (SumInfo con fields) = char '<' <> ppr con <> char '>' <> ppr fields

data ProdInfo
    = FieldProdInfo [EnterLattice]
    deriving (Eq,Generic,NFData)

instance Outputable ProdInfo where
    ppr (FieldProdInfo fields) = text "p" <+> ppr fields


{- |

Lattice of roughly this shape:

           LatUnknown
           |        |
          / \       |
    LatProd LatSum  |
          |    |    |
           \   |   /
           LatUndet


f x = Just $! Just x : alwaysEnter < tagged < top > >


LatUnknown represents things over which we can't know anything except their enter behaviour.
LatUndet represents cases where we haven't been able to compute field information yet.

Prod/Sum tell us something about the values returned.

LatUndet/Unknown allows us to differentiate between lack of
information about returned values and "uncomputeable" field information.



    Note [Comparing Sums and Products]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    At a glance it makes sense that we would ever compare sum and product results.
    However consider this case:

    case v of
        True -> case x of prod -> Left prod
        False -> case y of sum -> Right sum

    Then we will infer taggedness of ![!], being a tagged
    result with the first field being tagged.

    However the first field will be a prod type in one and
    a sum type in the other case. But this does not concern
    us as taggedness is value-level property so their types
    don't have to match.

    We could go even further still and compare the fields of
    `prod` and `sum` against each other. But while correct the
    payoff is small and it's easy to get wrong so for now we
    widen the fields of any product and sum type comparison
    to the top of the latice.


    Note [Representing taggedness of recursive types]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    None of this is implemented yet, but here are some thoughts
    and an idea that on the surface seems feasible:

    TODO


-}

data EnterLattice
    -- | At most we can say something about the tag of the value itself.
    --   The fields are not yet known.
    --   Semantically: EnterInfo x bot(fields)
    = LatUnknown !EnterInfo

    -- | Direct tail recursion
    | LatRec !EnterInfo

    -- | We know something about the value itself, and we can find out more
    -- about it's return values as well.
    | LatUndet !EnterInfo

    -- Don't unpack Sum/Prod infos, as EnterInfo is modified somewhat often.
    | LatProd !EnterInfo !ProdInfo
    | LatSum !EnterInfo !SumInfo
    deriving (Generic,NFData)

instance Eq EnterLattice where
    (==) lat1 lat2
        | maybeEq lat1 lat2             = True
    (==) (LatUndet o1)   (LatUndet o2)  = o1 == o2
    (==) (LatUndet _ )   (_)            = False
    (==) (LatRec o1)     (LatRec o2)    = o1 == o2
    (==) (LatRec _ )     (_)            = False
    (==) (LatProd o1 p1) (LatProd o2 p2)= o1 == o2 && p1 == p2
    (==) (LatProd _  _ ) (_) = False
    (==) (LatSum o1 s1)  (LatSum o2 s2) = o1 == o2 && s1 == s2
    (==) (LatSum _  _ )  (_) = False
    (==) (LatUnknown o1) (LatUnknown o2) = o1 == o2
    (==) (LatUnknown _ ) (_) = False




-- | Get the (outer) EnterInfo value
getOuter :: EnterLattice -> EnterInfo
getOuter (LatUndet x) = x
getOuter (LatRec x) = x
getOuter (LatUnknown x) = x
getOuter (LatProd x _) = x
getOuter (LatSum x  _) = x

instance Outputable EnterLattice where
    ppr (LatUnknown outer) = ppr outer <+> text "top"
    ppr (LatRec outer)     = ppr outer <+> text "rec"
    ppr (LatUndet   outer) = ppr outer <+> text "undet"
    ppr (LatProd outer inner) =
        ppr outer <+> (ppr inner)
    ppr (LatSum outer inner) =
        ppr outer <+> (ppr inner)

instance Lattice EnterLattice where
    bot = LatUndet UndetEnterInfo
    top = LatUnknown MaybeEnter

combineEnterBranches :: EnterInfo -> EnterInfo -> EnterInfo
combineEnterBranches RecEnter _             = MaybeEnter
combineEnterBranches _ RecEnter             = MaybeEnter
combineEnterBranches MaybeEnter _           = MaybeEnter
combineEnterBranches _ MaybeEnter           = MaybeEnter
combineEnterBranches UndetEnterInfo _       = UndetEnterInfo
combineEnterBranches _ UndetEnterInfo       = UndetEnterInfo
combineEnterBranches NeverEnter AlwaysEnter = MaybeEnter
combineEnterBranches AlwaysEnter NeverEnter = MaybeEnter
combineEnterBranches x y
    | x == y = x

combineProdInfo :: ProdInfo -> ProdInfo -> ProdInfo
combineProdInfo (FieldProdInfo fs1) (FieldProdInfo fs2)
    = FieldProdInfo $ zipWithEqual "ProdInfo:combine" combineLatticeBranches fs1 fs2

combineSumInfo :: SumInfo -> SumInfo -> Maybe SumInfo
combineSumInfo (SumInfo c1 fs1) (SumInfo c2 fs2)
    | c1 /= c2  = Nothing
    | otherwise = Just $! SumInfo c1 $
                  zipWithEqual "SumInfo:combine" combineLatticeBranches fs1 fs2

combineLatticeBranches :: EnterLattice -> EnterLattice -> EnterLattice
combineLatticeBranches x1 x2 | x1 == x2 = x1
combineLatticeBranches (LatRec o1) x = x `setOuterInfo` (combineEnterBranches o1 $ getOuter x)
combineLatticeBranches x (LatRec o2) = x `setOuterInfo` (combineEnterBranches (getOuter x) o2)
-- Top stays top
combineLatticeBranches (LatUnknown o1) (LatUnknown o2) = LatUnknown (combineEnterBranches o1 o2)
combineLatticeBranches (LatUnknown o1) y = LatUnknown (combineEnterBranches o1 (getOuter y))
combineLatticeBranches x (LatUnknown o2) = LatUnknown (combineEnterBranches (getOuter x) o2)
-- Bot stays bot (unless compared against top)
combineLatticeBranches (LatUndet o1) (LatUndet o2) = LatUndet (combineEnterBranches o1 o2)
combineLatticeBranches (LatUndet o1) y = LatUndet (combineEnterBranches o1 (getOuter y))
combineLatticeBranches x (LatUndet o2) = LatUndet (combineEnterBranches (getOuter x) o2)
-- We currently do NOT combine results of different types
combineLatticeBranches (LatProd o1 _) (LatSum o2 _) = LatUnknown $ combineEnterBranches o1 o2
combineLatticeBranches (LatSum o1 _) (LatProd o2 _) = LatUnknown $ combineEnterBranches o1 o2

combineLatticeBranches (LatSum o1 s1) (LatSum o2 s2) =
    let outer = (combineEnterBranches o1 o2)
        fields = (combineSumInfo s1 s2)
    in maybe (LatUnknown outer) (LatSum outer) fields
combineLatticeBranches (LatProd o1 p1) (LatProd o2 p2) =
    LatProd (combineEnterBranches o1 o2) (combineProdInfo p1 p2)

-- Lattice when we know, and can only know, the outer layer.
flatLattice :: EnterInfo -> EnterLattice
flatLattice x = LatUnknown x

setOuterInfo :: HasCallStack => EnterLattice -> EnterInfo -> EnterLattice
setOuterInfo lat info
    | getOuter lat == info
    = lat
    | otherwise =
        case lat of
            LatUndet _       -> LatUndet info
            LatRec _         -> LatRec info
            LatUnknown _     -> LatUnknown info
            LatProd _ fields -> LatProd info fields
            LatSum  _ fields -> LatSum info fields

-- Lookup field info, defaulting towards bot
-- Zero indexed
indexField :: EnterLattice -> Int -> EnterLattice
indexField (LatUndet _) _ = bot
-- This can only happen if we case on a non-terminating function.
indexField (LatRec _) _ = bot
indexField (LatProd _ (FieldProdInfo fields)) n =
    case drop n fields of
        [] -> bot
        (x:_xs) -> x
indexField (LatSum _ sum) n
    | SumInfo _con fields <- sum
    = case drop n fields of
        -- We treat [] equal to [bot, bot, bot, ...]
        [] -> bot
        (x:_xs) -> x
    | otherwise = bot
-- Field information not available
indexField LatUnknown {} _ = top

hasOuterTag :: EnterLattice -> Bool
hasOuterTag lat = getOuter lat == NeverEnter

hasTopFields :: EnterLattice -> Bool
hasTopFields (LatUnknown    {}) = True
hasTopFields (LatProd _ (FieldProdInfo fields)) = all isTopValue fields
hasTopFields (LatSum  _ (SumInfo _ fields)) = all isTopValue fields

hasTopFields (LatRec        {}) = False
hasTopFields (LatProd       {}) = False
hasTopFields (LatSum        {}) = False
hasTopFields (LatUndet      {}) = False

isTopValue :: EnterLattice -> Bool
isTopValue lat = getOuter lat == MaybeEnter && hasTopFields lat

nestingLevelOver :: EnterLattice -> Int -> Bool
nestingLevelOver _ 0 = True
nestingLevelOver (LatProd _ (FieldProdInfo fields)) n
    = any (`nestingLevelOver` (n-1)) fields
nestingLevelOver (LatSum _ (SumInfo _ fields)) n
    = any (`nestingLevelOver` (n-1)) fields
nestingLevelOver _ _ = False

capAtLevel :: Int -> EnterLattice -> EnterLattice
capAtLevel 0 _ = top
capAtLevel n (LatProd e (FieldProdInfo fields)) =
    (LatProd e (FieldProdInfo $ map (capAtLevel (n-1)) fields))
capAtLevel n (LatSum e (SumInfo c fields)) =
    (LatSum e (SumInfo c $ map (capAtLevel (n-1)) fields))

capAtLevel _ l@(LatUnknown {}) = l
capAtLevel _ l@(LatRec {}) = l
capAtLevel _ l@(LatUndet {}) = l
capAtLevel _ l@(LatUndet {}) = l
capAtLevel _ l = l

{-
    -- Note [Constraints/Rules for tag/enter information]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Assumptions made:
        * StgApp is always fully applied
        * Now shadowing - currently not guaranted will fix later.

    We use a pseudo syntax allowing both haskell functions and rule info.
    In general:

    * These rules are meant for reference and understanding and reference.
      So please apply common sense when reading them and if they don't match the implementation
      please update them accordingly.
      If you feel inclined to make these more formal then please do so!

    * info[name] = e
        => assign for the node name the value of evaluating e to it's info field.

    * We play loose with expressions but in general they are haskell pseudocode.

    * For each assignment pattern of "<foo>[node]"" we use "foo node" in expressions
      to mean the value foo of node that was assigned in the last iteration.

    * We use 3 main elements of information: info, tag, fields
      * tag says something about the entering behaviour of the node itself
        when cased on.
      * fields says something about the entering behaviour of fields of the node
        when they are bound to case binders.
      * info, for convenience is the tuple of (tag x fields).

    * In the premises of the rules:
        * Generally @ gives AST nodes names.
        * Arguments to ADT constructors are often ommited if not relevant.

    * In some cases syntax is a bit ... ad hoc, but hopefully should be understandable
      from the context.

    * In general rules are not exclusive, so more than one rule might match some
      AST element/node. If in doubt if a rule is exclusive check the implementation.
      (sorry).


    Binds, top level and otherwise are implemented by nodeBind
    ========================================================
    [NonRec x rhs]
        => info[x] = info rhs

    Rec [(x1,rhs1), (x2,rhs2), ... ]]
        => info[x1] = info rhs1
           info[x2] = info rhs2
           ...

    Rhss are implemented by nodeRhs
    ========================================================

    Allocating constructors
    --------------------------------------------------------

    -- This is the general case for constructors without
    -- strict fields. They will always be tagged.

    rhs@[RhsCon con args], noneMarkedStrict args
        => info[rhs] = (NeverEnter, map info args)

    -- The special cases for constructors with strict fields.
    -- This works fine, but it requires us to reliable detect
    -- non-tagged cases in the arguments, or we might infer
    -- SP !x !x; .. SP <undet> <tagged> as tagged since we
    -- use lub to determine the outer tag based on the inner tag.

    -- This means we have to take great care to assign unknown
    -- bindings MaybeEnter (top of the lattice).

    -- We also mark the strict fields as neverEnter in the result node.

    -- Local non-recursive binds are allways tagged. The reason is simply,
    -- even when we need to tag arguments we can always wrap the whole let
    -- in a case expression. This however isn't true for top level bindings!
    -- So their tag depends also on the tags of their strict field arguments.

    rhs@[RhsCon con args], sargs@(strictArgs args), not isTopLevel, isNonRec
        => info[rhs] = (NeverEnter, map (noEnterSargs . info) args)

    rhs@[RhsCon con args], sargs@(strictArgs args)
        => info[rhs] = (lub Tagged sargs, map (noEnterSargs . info) args)



    Functions/Closures
    --------------------------------------------------------

    -- Closures always need to be entered. Their nested enterInfo
    -- is determined by the closure body. However we CAN mark them
    -- as no-enter if they take arguments. Because partial applications
    -- won't be entered.

    rhs@[RhsClosure args body], null args
        => info[rhs] = (AlwaysEnter, fieldsOf(info body))

    rhs@[RhsClosure args body], not (null args)
        => info[rhs] = (NeverEnter, fieldsOf(info body))


    Expressions:
    ========================================================

    -- The built in ones are almost all unboxed
    -- and user imported ones don't expose any info
    -- so this is always just top.

    TODO: The two exceptions are seq and dataToTag#
          (SeqOp/DataToTagOp respectivly)
    [StgOpApp]
        => top


    -- Proper STG doesn't contain lambdas.
    [StgLam]
        => panic

    -- TODO: Seq
    For any bind which is
        * non-top level
        * non-rec
        * binds a RhsCon
    the arguments to the strict fields can
    be assumed to be strict in the body. Reason being that we will
    wrap the let in a case and substitute all occurences of the
    untagged value with the strict value.

    -- Let's just flow along
    [StgLet bnd body]
        => info body

    -- Let's just flow along
    [StgLetNoEscape bnd body]
        => info body

    -- Literal expressions never require entering.
    [StgLit]
        => info[StgLit] = (NeverEnter, top)

    Function application
    --------------------------------------------------------

    -- AppAbsent
    -- The result does not need to be entered if it's an application of
    -- absentError
    app@[StgApp f _], f == absentError
        => info[app] = (NeverEnter, top)

    -- AppRec:
    -- In the recursive case, the app gives us no new information,
    -- but does not constrain us either.
    app@[StgApp f _], hasEnv[letrec f = @rhs(... app ...)]
        => info[app] = info[rhs]

    AppDefault
    -- We just pass it along.
    app@[StgApp f []] || app@[StgApp f args], length args == arity
        => info[app] = info f

    conApp@[StgConApp con args]
        => info[conApp] = (AlwaysEnter, map info args)
        + tagging of strict fields in the result node.

    -- This constraint is currently disabled.
    conApp@[StgConApp con [arg1,arg2,argi,... argn], hasCtxt[letrec argi = ...]
        => info[argi] = (enterInfo argi, top)

    Subparts of case expressions
    --------------------------------------------------------

    -- Cases are one of the harder parts.
    -- The lower bound of the alts determine the expressions tag
    case@[StgCase scrut bndr alts]
        => info[case] = lub alts

    -- The case binder is never entered
    [StgCase scrut bndr alts]
        => info [bndr] = (NeverEnter, fields scrut)

    -- Alternative results are determined from their rhs
    alt@[StgAlt con bnds expr]
        => info[alt] = info expr

    -- Strict fields are tagged and as such need not be entered.
    alt@[StgAlt con [b1,b2,..bn] expr], strictField bi, hasEnv[case scrut of ... alt, ...]
            => info[bi] = (NeverEnter, fields scrut !! i)

------------------------------------------------------

Implementation considerations:
We want to create a data flow graph for all of the above.

* We only care about results of let-bound ids
* This means we can map all outputs we care about via a map over Id
* We do however have other graph nodes
* We can map these via Uniques so we can update them.

Each flow node carries:
* Input dependencies
* It's id
* It's result (Or should it be put in a seperate map?)

We generate the flow graph by traversing over the Stg code (once)
and building up the nodes.

Then we calculate the fixpoint.

In the last step we transfer back the information gained from the analysis.

For now generate one node per rule.
We could common up some of these though for performance.

Note [Taggedness of let bound constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default a let bound StgRhsCon WILL result in a tagged binding.
However there are some exceptions:

* Imported non-nullary constructors.

    We don't store the tag in the Interface so can't recreate it - not tagged.

* Top level RhsCon with strict untagged arguments.

    In order these will only contain tagged references we have to turn them into
    functions who evaluate the possibly untagged arguments.

Note [Taggedness of absentError]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

WW under certain circumstances will determine that a strict

* Let bound absentErrors.
    These are closures without args so no tag.
    However we mark them as tagged as they have been proofen unused
    by WW and as such

-}

-- | Nodes identified by their id have the result mapped back the STG
--   all other nodes get an unique and are only there for the analysis.
--   We also map certain ids to uniqe based id's if they might be shadowed.
newtype NodeId = NodeId { nid_unique :: Unique } -- ^ Other nodes
    deriving (Eq, Generic)

instance Ord NodeId where
    compare = comparing (getKey . nid_unique)

instance Outputable NodeId where
    ppr (NodeId  i) = ppr i

instance NFData NodeId where
    rnf x = seq x ()

instance Uniquable NodeId where
    getUnique = nid_unique

instance Uniquable FlowNode where
    getUnique = getUnique . node_id


data FlowState
    = FlowState
    { fs_mod :: !Module
    , fs_iteration :: !Int -- ^ Current iteration
    , fs_us :: !UniqSupply
    , fs_idNodeMap :: !(UniqFM FlowNode) -- ^ Map of imported id nodes (indexed by `Id`).
    , fs_uqNodeMap :: !(UniqFM FlowNode) -- ^ Transient results, index by `NodeId`
    , fs_doneNodes :: !(UniqFM FlowNode) -- ^ We can be sure these will no longer change, index by `NodeId`
    }

type AM = State FlowState

instance MonadUnique AM where
    getUniqueSupplyM = do
        s <- get
        let (us1,us2) = splitUniqSupply $ fs_us s
        put $! s {fs_us = us1}
        return us2
    getUniqueM = do
        s <- get
        let (!u,!us) = takeUniqFromSupply $! fs_us s
        put $! s {fs_us = us}
        return $! u

-- TODOT: addNode == updateNode
-- | Add new (unfinished) node.
addNode :: FlowNode -> AM ()
addNode node = do
    s <- get
    if node_done node
        then put $ s { fs_doneNodes = addToUFM (fs_doneNodes s) node node
                     , fs_uqNodeMap = delFromUFM (fs_uqNodeMap s) node }
        else put $ s { fs_uqNodeMap = addToUFM (fs_uqNodeMap s) node node }

updateNode :: FlowNode -> AM ()
updateNode node = do
    s <- get
    let !id = node_id node
    let done = node_done node
    -- pprTraceM "Updating node" (ppr id)
    put $   if done
                then s  { fs_doneNodes = addToUFM (fs_doneNodes s) id node
                        , fs_uqNodeMap =
                                -- pprTrace "DeletingNode" (ppr id) $
                                delFromUFM (fs_uqNodeMap s) id }
                else s  { fs_uqNodeMap = addToUFM (fs_uqNodeMap s) id node }

-- | Move the node from the updateable to the finished set
markDone :: FlowNode -> AM ()
markDone node = do
    updateNode (node { node_done = True })

-- | Pessimistic check, defaulting to False when it's not clear.
isMarkedDone :: HasCallStack => NodeId -> AM Bool
isMarkedDone id = do
    s <- get
    return $ elemUFM id (fs_doneNodes s)

updateNodeResult :: NodeId -> EnterLattice -> AM ()
updateNodeResult id result = do
    node <- (getNode id)
    updateNode $ node {node_result = result}


getNode :: HasCallStack => NodeId -> AM FlowNode
getNode node_id = do
    s <- get
    return $ fromMaybe
                   (pprPanic "Node not found" (ppr node_id))
                   (lookupUFM (fs_doneNodes s) node_id <|> lookupUFM (fs_uqNodeMap s) node_id)


-- TODO: Can we make sure we never try to query non-existing nodes?
lookupNodeResult :: HasCallStack => NodeId -> AM EnterLattice
lookupNodeResult node_id = do
    s <- get
    let node = (lookupUFM (fs_uqNodeMap s) node_id <|>
                lookupUFM (fs_doneNodes s) node_id)
    case node of
        Nothing -> pprTraceM ("loopupNodeResult: Nothing\n" ++ prettyCallStack callStack) (ppr node_id) >>
                   return bot
        Just n  -> return $ node_result n

{-
    Note [Field information of function ids]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    tagged< -- (,)
        tagged<tagged<top>>, -- Just (I# lit)
        unknown -- a
    >

However currently we do not keep nested field information inside constructors if the
values stored is a function, so this does not work.

TODO: This could be done, at the expense of compile time. Figure out of it's worth and do it if useful.

-}

-- | If we use a *function* as an unapplied argument to a constructor we throw
-- away nested information and make do with NeverEnter Top for now.
-- See Note [Field information of function ids]
getConArgNodeId :: HasCallStack => [SynContext] -> StgArg -> AM NodeId
getConArgNodeId _    (StgLitArg _ ) = return litNodeId
getConArgNodeId ctxt (StgVarArg v )
    | isFunTy (unwrapType $ idType v)
    = return neverNodeId
    | otherwise
    = mkIdNodeId ctxt v

data FlowNode
    = FlowNode
    { node_id :: {-# UNPACK  #-} !NodeId -- ^ Node id
    , node_inputs :: [NodeId]  -- ^ Input dependencies
    , node_done :: {-# UNPACK #-} !Bool -- ^ Do no longer update this node
    , node_result :: !(EnterLattice) -- ^ Cached result
    , node_update :: (AM EnterLattice) -- ^ Calculates a new value for the node
                                       -- AND updates the environment with it.
#if defined(WITH_NODE_DESC)
    , _node_desc :: SDoc -- ^ Debugging purposes
#endif
    }

set_desc :: FlowNode -> SDoc -> FlowNode
#if defined(WITH_NODE_DESC)
set_desc n desc = n { _node_desc = desc}
#else
set_desc n _ = n
#endif

node_desc :: FlowNode -> SDoc
#if defined(WITH_NODE_DESC)
node_desc n = _node_desc n
#else
node_desc _n = empty
#endif

instance NFData FlowNode where
    rnf (   FlowNode
                { node_id = _
                , node_inputs = node_inputs
                , node_done = _
                , node_result = node_result
                , node_update = _
#if defined(WITH_NODE_DESC)
                , _node_desc = _
#endif
                })  = deepseq (node_inputs,node_result) ()


instance Outputable FlowNode where
    ppr node =
        hang
            (text "node_" <> pprId node <-> pprDone node <-> (node_desc node) )
            2
            ( (ppr $ node_inputs node) <> parens (ppr $ node_result node) )
      where
        pprId node =
            case node_id node of
                NodeId uq -> ppr uq
        pprDone node =
            if node_done node then text "done" else empty

data IsLNE = LNE | NotLNE deriving (Eq)

pprLne :: IsLNE -> SDoc
pprLne NotLNE = empty
pprLne LNE = text "-LNE"

data SynContext
    = CTopLevel     (VarEnv NodeId) -- ^ Maps top level binds to ids
    | CLetRec       (VarEnv NodeId) !IsLNE
    | CLetRecBody   (VarEnv NodeId) !IsLNE
    | CLet          (VarEnv NodeId)  !IsLNE
    | CLetBody      (VarEnv NodeId)  !IsLNE
    | CClosureBody  (VarEnv NodeId) -- ^ Args of a closure mapped to nodes in the body
    | CCaseScrut                  -- ^ Inside a case scrutinee
    | CCaseBndr     (VarEnv NodeId)  -- ^ Case binder mapping
    | CAlt          (VarEnv NodeId) -- ^ Alt binder mappings
    | CNone                       -- ^ No Context given
    deriving Eq

getCtxtIdMap :: SynContext -> Maybe (VarEnv NodeId)
getCtxtIdMap (CClosureBody m) = Just m
getCtxtIdMap (CCaseBndr m) = Just $ m
getCtxtIdMap (CCaseScrut) = Nothing
getCtxtIdMap (CAlt m) = Just m
getCtxtIdMap (CLetRec m _) = Just m
getCtxtIdMap (CLetRecBody m _) = Just m
getCtxtIdMap (CLet m _) = Just m
getCtxtIdMap (CLetBody m _) = Just m
getCtxtIdMap (CTopLevel m) = Just m
getCtxtIdMap (CNone) = Nothing

-- | isSingleMapOf v env == fromList [(v',_)] && v == v'
isSingleMapOf :: Id -> VarEnv NodeId -> Bool
isSingleMapOf v env =
    sizeUFM env == 1 && elemVarEnv v env

instance Outputable SynContext where
    ppr CNone = text "CNone"
    ppr (CTopLevel map) = text "CTop" <> ppr map
    ppr (CAlt map) = text "CAlt" <> ppr map
    ppr (CCaseBndr map) = text "CCaseBndr" <> ppr map
    ppr (CClosureBody map) = text "CClosure" <> ppr map
    ppr (CLetRec     ids lne) = text "CLetRec"     <> pprLne lne <> ppr ids
    ppr (CLetRecBody ids lne) = text "CLetRecBody" <> pprLne lne <> ppr ids
    ppr (CLet id lne)         = text "CLet"        <> pprLne lne <> ppr id
    ppr (CLetBody id lne)     = text "CLetBody"    <> pprLne lne <> ppr id

idMappedInCtxt :: Id -> [SynContext] -> Maybe NodeId
idMappedInCtxt id ctxt
    = go ctxt
  where
    go (ctxt:_)
        | Just argMap <- getCtxtIdMap ctxt
        , Just node <- lookupVarEnv argMap id
        = Just node
    go (_:todo) = go todo
    go [] = Nothing

-- | Lub like operator between all input node
mkJoinNode :: [SynContext] -> [NodeId] -> AM NodeId
mkJoinNode ctxt [] = return topNodeId
mkJoinNode ctxt [node] = return node
mkJoinNode ctxt inputs = do
    node_id <- mkUniqueId
    let updater = do
            input_results <- mapM lookupNodeResult inputs
            let result = foldl1' combineLatticeBranches input_results
            if isTopValue result
                then do
                    node <- getNode node_id
                    markDone $ node { node_result = result }
                else updateNodeResult node_id result
            return result

    addNode $ FlowNode { node_id = node_id, node_result = bot
                       , node_inputs = inputs, node_done = False
                       , node_update = updater
#if defined(WITH_NODE_DESC)
                       , _node_desc = text "branches"
#endif
                       }
    return node_id

-- Gives the lattice for evaluating con with arguments of the given taggedness.
-- | Take a lattice argument per constructor argument to simplify things.
mkOutConLattice :: DataCon -> EnterInfo -> [EnterLattice] -> EnterLattice
mkOutConLattice con outer fields
    | conCount == 1 = LatProd outer (FieldProdInfo out_fields)
    | conCount > 1  = LatSum outer (SumInfo con out_fields)
    | otherwise = panic "mkOutConLattice"
  where
    out_fields = mapStrictConArgs con (`setOuterInfo` NeverEnter) fields
    conCount = length (tyConDataCons $ dataConTyCon con)

{-# NOINLINE findTags #-}
findTags :: Module -> UniqSupply -> [StgTopBinding] -> ([TgStgTopBinding])
findTags this_mod us binds =
    -- pprTrace "findTags" (ppr this_mod) $
    let state = FlowState {
            fs_mod = this_mod,
            fs_iteration = 0,
            fs_us = us,
            fs_idNodeMap = mempty,
            fs_uqNodeMap = emptyUFM,
            fs_doneNodes = emptyUFM }
    -- Run the analysis
        (!binds') = (flip evalState) state $ do
            addConstantNodes
            binds' <- nodesTopBinds binds
            _nodes <- solveConstraints
            finalBinds <- rewriteTopBinds binds'
            return $ finalBinds
    in  (seqTopBinds binds') `seq`
        pprTrace "foundBinds" (ppr this_mod) binds'


-- passTopBinds :: [StgTopBinding] -> [TgStgTopBinding]
-- passTopBinds binds = map (passTop) binds

-- passTop :: StgTopBinding -> TgStgTopBinding
-- passTop (StgTopStringLit v s)   = StgTopStringLit v s
-- passTop (StgTopLifted bind)     = StgTopLifted (passBinds bind)

-- passBinds :: StgBinding -> TgStgBinding
-- passBinds (StgNonRec v rhs) = StgNonRec v (passRhs rhs)
-- passBinds (StgRec pairs)    = StgRec $ map (second passRhs) pairs

-- -- For top level lets we have to turn lets into closures.
-- passRhs :: StgRhs -> TgStgRhs
-- passRhs (StgRhsCon node_id ccs con args) = (StgRhsCon noExtFieldSilent ccs con args)
-- passRhs (StgRhsClosure ext ccs flag args body) =
--     StgRhsClosure ext ccs flag args $ passExpr body

-- passExpr :: StgExpr -> TgStgExpr
-- passExpr (StgCase scrut bndr ty alts) = StgCase (passExpr scrut) bndr ty (map passAlt alts)
-- passExpr (StgLet _ binds body)          = StgLet noExtFieldSilent (passBinds binds) (passExpr body)
-- passExpr (StgLetNoEscape _ binds body)  = StgLetNoEscape noExtFieldSilent (passBinds binds) (passExpr body)
-- passExpr (StgTick t e)     = StgTick t $ passExpr e
-- passExpr (StgConApp _ con args tys)     = StgConApp noExtFieldSilent con args tys

-- passExpr (StgApp _ f args)              =  StgApp MayEnter f args
-- passExpr (StgLit lit)                   = (StgLit lit)
-- passExpr (StgOpApp op args res_ty)      = (StgOpApp op args res_ty)
-- passExpr (StgLam {}) = error "Invariant violated: No lambdas in STG representation."

-- passAlt :: StgAlt -> TgStgAlt
-- passAlt (altCon, bndrs, rhs) = (altCon, bndrs, passExpr rhs)

-- Constant mappings
addConstantNodes :: AM ()
addConstantNodes = do
    addNode litNode
    markDone $ mkConstNode botNodeId bot
    addNode $ mkConstNode topNodeId top
    addNode $ mkConstNode neverNodeId (flatLattice NeverEnter)
    addNode $ mkConstNode maybeNodeId (flatLattice MaybeEnter)
    addNode $ mkConstNode alwaysNodeId (flatLattice AlwaysEnter)


mkConstNode :: NodeId -> EnterLattice -> FlowNode
mkConstNode id val =
    FlowNode
    { node_id = id
    , node_inputs = []
    , node_done = True
    , node_result = val
    , node_update = (return $ val)
#if defined(WITH_NODE_DESC)
    , _node_desc = (text "const")
#endif

    }

-- We don't realy do anything with literals, but for a uniform approach we
-- map them to (NeverEnter x Bot)
litNodeId, botNodeId, topNodeId, neverNodeId, maybeNodeId, alwaysNodeId :: NodeId
litNodeId       = NodeId $ mkUnique 'c' 2
botNodeId       = NodeId $ mkUnique 'c' 3 -- Always returns bot
topNodeId       = NodeId $ mkUnique 'c' 4
neverNodeId     = NodeId $ mkUnique 'c' 5
maybeNodeId     = NodeId $ mkUnique 'c' 6
alwaysNodeId    = NodeId $ mkUnique 'c' 7


litNode :: FlowNode
litNode =
    (mkConstNode litNodeId (flatLattice NeverEnter))
#if defined(WITH_NODE_DESC)
        { _node_desc = text "lit" }
#endif

{-
    Note [Shadowing and NodeIds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Shadowing makes things slightly more complex.

Let bindings are guaranteed to be unique as otherwise
this would result in linker errors, so we assign them NodeIds
based on their actual Id (Var).

For constructs potentially introducing a shadowing id like
case binders, or the binders of case alternatives we create
a new NodeId when traversing the AST. When we want to get the
nodeId for a particular we use mkIdNodeId.

This function checks if we assigned the id to a non-id based nodeId
and otherwise constructs a NodeId based on the actual Id (Var).

-}

-- Based on the ID look up the nodeid.
-- Checking if the ID has a mapping to a nodeId in the
-- context first.
mkIdNodeId :: HasCallStack => [SynContext] -> Id -> AM NodeId
mkIdNodeId ctxt id
    | Just node <- idMappedInCtxt id ctxt
    = return node
    | otherwise = do
        s <- get
        return $ fromMaybe (pprPanic "Unmapped id" (ppr id)) $
            node_id <$> lookupUFM (fs_idNodeMap s) id

-- | Same as mkIdNodeId but does NOT handle imported ids.
mkLocalIdNodeId :: HasCallStack => [SynContext] -> Id -> NodeId
mkLocalIdNodeId ctxt id
    | Just node <- idMappedInCtxt id ctxt
    = node
    | otherwise = pprPanic "Local id not mapped:" (ppr id)


mkUniqueId :: AM NodeId
mkUniqueId = NodeId <$> getUniqueM

{-
    Note [Imported Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We want to keep our Ids a simple newtype around Unique.
This is "easy" for things brought into scope by AST nodes,
we put a mapping from the Id to the NodeId into SynContext
which we can use to loop up the actual ids.

This is required in any way in order to avoid shadowing.

However imported Id's can show up in any place without context.
We solve this by creating a Node and NodeId for each imported
id when we come across the id the first time.

The next time we come across the same id mkIdNodeId will check
fs_idNodeMap, find the node we created earlier and return the
same node.

TODO: Currently we store the id-based nodes twice, once in idNodeMap
and once in uniqNodeMap. idNodeMap should only store the nodeId instead.

TODO: Once we can export taggedness in the Interface file we
will want to


-}


-- | This adds nodes with information we can figure out about imported ids into the env.
--   Mimics somewhat what we do in StgCmmClosure.hs:mkLFImported
--   It's helpful to think about this adding the semantics as if the imported ID
--   was defined as an top level binding.
addImportedNode :: Id -> AM ()
addImportedNode id = do
    node_id <- mkUniqueId
    s <- get
    let idNodes = fs_idNodeMap s
    let node = case lookupUFM idNodes id of
                    Just _ -> Nothing
                    Nothing
                        -- Local id, it has to be mapped to an id via SynContext
                        | nameIsLocalOrFrom (fs_mod s) (idName id)
                        -> Nothing -- TODO: Check this before running lookupUFM

                        -- Functions tagged with arity are never entered as atoms.
                        | idFunRepArity id > 0
                        -> Just $! (mkConstNode node_id (flatLattice NeverEnter))
                                    `set_desc` (text "ext_func" <-> ppr id)

                        -- Known Nullarry constructors are also never entered
                        | Just con <- (isDataConWorkId_maybe id)
                        , isNullaryRepDataCon con
                        -> Just $! (mkConstNode node_id (flatLattice NeverEnter))
                                    `set_desc` (text "ext_nullCon" <-> ppr id)

                        -- General case, a potentially unevaluated imported id.
                        | not isFun
                        ->  -- pprTrace "addImported_gen" (ppr id) $
                            Just $! (mkConstNode node_id (flatLattice MaybeEnter))
                                    `set_desc` (text "ext_unknown_enter" <-> ppr id)

                        -- May or may not be entered.
                        | otherwise
                        -> Just $! (mkConstNode node_id (flatLattice MaybeEnter))
                                    `set_desc` (text "ext_unknown" <-> ppr id)
    case node of
        Nothing -> return ()
        Just node -> do
            -- pprTraceM "Mapped imported id:" (ppr id <+> ppr node)
            put $
                s { fs_idNodeMap = addToUFM (fs_idNodeMap s) id node
                , fs_doneNodes = addToUFM (fs_doneNodes s) node node }
  where
    isFun = isFunTy (unwrapType $ idType id)

-- | Returns the nodeId for a given imported Id.
importedFuncNode :: Module -> Id -> AM (Maybe NodeId)
importedFuncNode this_mod var_id
    -- Not an imported function
    | nameIsLocalOrFrom this_mod (idName var_id)
    = return Nothing
    | otherwise = do
        s <- get
        case node_id <$> lookupUFM (fs_idNodeMap s) var_id of
            Just node_id -> return $ Just node_id
            Nothing -> pprPanic "Imported id not mapped" (ppr var_id)

-- TODO: Should we put stg bindings in dependency order?

{-# NOINLINE nodesTopBinds #-}
nodesTopBinds :: [StgTopBinding] -> AM [InferStgTopBinding]
nodesTopBinds binds = do
    let top_level_binds = (bindersOfTopBinds binds) :: IdSet
    -- TODO: Should is a map instead of a lookup list.
    mappings <- mkVarEnv <$> mapM (mkCtxtEntry [CNone]) (nonDetEltsUniqSet top_level_binds) ::  AM (VarEnv NodeId)
    -- TODOT: Better solution than stub nodes?
    -- mapM (\(v,u) -> mkStubNode u (ppr (v,u))) mappings
    let topCtxt = CTopLevel mappings
    mapM (nodesTop topCtxt) binds

mkCtxtEntry :: [SynContext] -> Id -> AM (Id,NodeId)
mkCtxtEntry ctxt v
    | Just nodeId <- idMappedInCtxt v ctxt
    = return (v,nodeId)
    | otherwise
    = do
        node_id <- mkUniqueId
        return (v, node_id)

nodesTop :: SynContext -> StgTopBinding -> AM InferStgTopBinding
-- Always "tagged"
nodesTop ctxt (StgTopStringLit v str) = do
    let node_id = mkLocalIdNodeId [ctxt] v
    let node = mkConstNode node_id (flatLattice NeverEnter)
                    `set_desc` text "c_str"
    markDone node
    return $ (StgTopStringLit v str)
nodesTop ctxt (StgTopLifted bind)  = do
    bind' <- fst <$> nodesBind [ctxt] TopLevel NotLNE bind :: AM InferStgBinding
    return $ (StgTopLifted bind')

-- nodesBind creates the nodeIds for the bound rhs, the actual nodes are created in
-- nodeRhs. Returns the context including the let
nodesBind :: [SynContext] -> TopLevelFlag -> IsLNE -> StgBinding -> AM (InferStgBinding, [SynContext])
nodesBind ctxt top lne (StgNonRec v rhs) = do
    boundId <- uncurry unitVarEnv <$> mkCtxtEntry ctxt v
    let ctxt' = ((CLet boundId lne):ctxt)
    (rhs',_) <- (nodeRhs ctxt' top v rhs)
    return (StgNonRec v rhs', (CLetBody boundId lne):ctxt)
nodesBind ctxt top lne (StgRec binds) = do
    let ids = map fst binds
    boundIds <- mkVarEnv <$> mapM (mkCtxtEntry ctxt) ids :: AM (VarEnv NodeId)
    let ctxt' = (CLetRec boundIds lne) : ctxt
    (rhss', _) <- unzip <$> mapM (uncurry (nodeRhs ctxt' top )) binds
    return $ (StgRec $ zip ids rhss', CLetRecBody boundIds lne: ctxt)

-- TODO: If we have a recursive let, but non of the recursive ids are in strict fields
--       we and should can still tag the resulting let.

{-
    data Foo a = Foo Foo !a
    ...
    let x = Foo y bla
        y = Foo x blub
    in expr
    ...
    Should result in x and y being tagged with a wrapper like this:

    case bla of bla' ->
    case blub of blub' ->
        let x = Foo y bla'
            y = Foo x blub'
        in expr

-}

-- | When dealing with a let bound rhs passing the id in allows us the shortcut the
--  the rule for the rhs tag to flow to the id
nodeRhs :: HasCallStack => [SynContext] -> TopLevelFlag -> Id -> StgRhs -> AM (InferStgRhs, NodeId)
nodeRhs ctxt topFlag binding (StgRhsCon _ _ccs con args)
  | null args = do
        -- pprTraceM "RhsConNullary" (ppr con <+> ppr node_id <+> ppr ctxt)
        let node = mkConstNode node_id (flatLattice NeverEnter)
        markDone $ node `set_desc` text "rhsConNullary"
        return (StgRhsCon node_id _ccs con args, node_id)
  | otherwise = do

        mapM_ addImportedNode [v | StgVarArg v <- args]
        node_inputs <- mapM (getConArgNodeId ctxt) args :: AM [NodeId]
        -- pprTraceM "RhsCon" (ppr con <+> ppr node_id <+> ppr args <+> ppr node_inputs <+> ppr ctxt)
        let node =  FlowNode
                        { node_id = node_id
                        , node_inputs = node_inputs
                        , node_done   = False
                        , node_result = bot
                        , node_update = node_update node_id node_inputs
#if defined(WITH_NODE_DESC)
                        , _node_desc = (text "rhsCon")
#endif
                        }
        addNode node
        return (StgRhsCon node_id _ccs con args, node_id)
  where
    node_id = mkLocalIdNodeId ctxt binding
    node_update this_id node_inputs = do
        fieldResults <- mapM (lookupNodeResult) node_inputs
        let strictResults = getStrictConArgs con fieldResults
        let strictOuter = map getOuter strictResults :: [EnterInfo]
        -- pprTraceM "RhsCon" (ppr con <+> ppr this_id <+> ppr fieldResults)
        -- Rule 2
        let outerTag
                -- Non-toplevel bindings are wrapped with a case expr.
                -- This means we can always tag the resulting let,
                -- although it might no longer be static.
                -- However we do not do this for recursive binds
                -- for now. TODO: Could be done also for recursive binds in the common case.
                | not (isTopLevel topFlag)
                , (CLet vmap _ : _) <- ctxt
                , elemVarEnv binding vmap
                =   -- pprTrace "Avoided tag by wrapping" (ppr binding)
                    NeverEnter
                -- All lazy fields
                | not $ any isMarkedStrict $ dataConRepStrictness con
                =   NeverEnter

                -- If any of the inputs are undetermined so is the output,
                | any (== UndetEnterInfo) strictOuter
                = UndetEnterInfo

                | all (==NeverEnter) strictOuter
                = NeverEnter

                | otherwise
                = MaybeEnter

        -- Strict fields need to marked as neverEnter here, even if they are not analysed as such
        -- This is because when we READ the result of this rhs they will have been tagged.
        let result = mkOutConLattice con outerTag fieldResults
        let cappedResult = capAtLevel 10 result
        updateNodeResult this_id cappedResult
        return $ cappedResult




{- TODO:
    Is it worth to instantiate local thunks with their actual arguments
    or an approximation (lub) of them?

    This would require a notion of "internal" ideas beyond the concept of top level bindings.

TODO: Partial applications

* RhsCon is never partially applied
* Partially applied RhsClosures will have arity info exposed.
* This means we can assign the field info EVEN for partial results,
  we just have to make sure to only use field info for fully applied
  results.


-}
nodeRhs ctxt _topFlag binding (StgRhsClosure _ext _ccs _flag args body) = do
    mapM_ addImportedNode args
    (body', body_id) <- nodeExpr ctxt' body
    let node = FlowNode { node_id = node_id
                        , node_inputs = [body_id]
                        -- ^ We might infer things about nested fields once evaluated.
                        , node_done   = False
                        , node_result = LatUndet enterInfo
                        , node_update = node_update node_id body_id
#if defined(WITH_NODE_DESC)
                        , _node_desc = node_desc
#endif
                        }
    addNode node
    return (StgRhsClosure _ext _ccs _flag args body', node_id)

  where
    node_id = mkLocalIdNodeId ctxt binding
#if defined(WITH_NODE_DESC)
    node_desc
        | null args = text "rhsThunk:" <> (ppr binding)
        | otherwise = text "rhsFunc:" <> (ppr binding)
#endif
    varMap = mkVarEnv (zip args (replicate arity topNodeId))
    ctxt' = (CClosureBody varMap :ctxt)
    arity = length args
    enterInfo
        | isAbsentExpr body = NeverEnter
        | null args = AlwaysEnter
        | otherwise = MaybeEnter      -- Thunks with arity > 0
                                    -- are only entered when applied.
    node_update this_id body_id = do
        bodyInfo <- lookupNodeResult body_id
        let result = setOuterInfo bodyInfo enterInfo
        let cappedResult = capAtLevel 10 result
        if hasTopFields cappedResult
            then do
                node <- getNode this_id
                markDone $ node { node_result = cappedResult }
            else updateNodeResult this_id cappedResult
        return cappedResult

nodeExpr :: [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeExpr ctxt (e@StgCase {})          = nodeCase ctxt e
nodeExpr ctxt (e@StgLet {})           = nodeLet ctxt e
nodeExpr ctxt (e@StgLetNoEscape {})   = nodeLetNoEscape ctxt e
nodeExpr ctxt (StgTick t e)           = do
    (e',nodeId) <- nodeExpr ctxt e
    return (StgTick t e', nodeId)
nodeExpr ctxt e@(StgConApp {})        = nodeConApp ctxt e
nodeExpr ctxt e@(StgApp {})           = nodeApp ctxt e
-- Do the boring ones right here
nodeExpr _ctxt  (StgLit lit)            = return $ (StgLit lit, litNodeId)
nodeExpr _ctxt  (StgOpApp op args res_ty) = return $ (StgOpApp op args res_ty, topNodeId)
nodeExpr _ctxt  (StgLam {})             = error "Invariant violated: No lambdas in STG representation."

nodeCase :: [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeCase ctxt (StgCase scrut bndr alt_type alts) = do
    (scrut',scrutNodeId) <- nodeExpr (CCaseScrut:ctxt) scrut
    bndrNodeId <- nodeCaseBndr scrutNodeId bndr
    let ctxt' = CCaseBndr (unitVarEnv bndr bndrNodeId) : ctxt
    (alts', altNodeIds) <- unzip <$> mapM (nodeAlt ctxt' scrutNodeId) alts
    caseNodeId <- mkJoinNode ctxt' altNodeIds
    -- pprTraceM "Scrut, alts, rhss" $ ppr (scrut, scrutNodeId, altNodeIds, altsId)
    return (StgCase scrut' bndr alt_type alts' , caseNodeId)
nodeCase _ _ = panic "Impossible: nodeCase"


-- Take the result of the scrutinee and mark it as tagged.
nodeCaseBndr :: NodeId -> Id -> AM NodeId
nodeCaseBndr scrutNodeId bndr = do
    bndrNodeId <- mkUniqueId
    addNode $ FlowNode  { node_id = bndrNodeId
                        , node_inputs = [scrutNodeId], node_done = False
                        , node_result = bot, node_update = updater bndrNodeId
#if defined(WITH_NODE_DESC)
                        , _node_desc = text "caseBndr" <-> parens (ppr scrutNodeId) <-> ppr bndr
#endif
                        }
    return bndrNodeId
      where
        updater bndrNodeId = do
            scrutResult <- lookupNodeResult scrutNodeId
            let result = setOuterInfo scrutResult NeverEnter
            if hasTopFields result
                then do
                    node <- getNode bndrNodeId
                    markDone $ node { node_result = result }
                else
                    updateNodeResult bndrNodeId result
            return result


-- TODO: Shadowing is possible here for the alt bndrs
nodeAlt :: HasCallStack => [SynContext] -> NodeId -> StgAlt -> AM (InferStgAlt, NodeId)
nodeAlt ctxt scrutNodeId (altCon, bndrs, rhs)
  | otherwise = do
    bndrMappings <- mkVarEnv <$> zipWithM mkAltBndrNode [0..] bndrs
    let ctxt' = (CAlt bndrMappings):ctxt
    (rhs', rhs_id) <- nodeExpr ctxt' rhs
    return ((altCon, bndrs, rhs'), rhs_id)

    where
        strictBnds
          | DataAlt con <- altCon
          = getStrictConArgs con bndrs
          | otherwise = []

        -- Result for ONE of the bindings bound by the alt.
        -- Eg for (Just, foo, expr) we call mkAltBndrNode 0 foo
        mkAltBndrNode :: Int -> Id -> AM (Id,NodeId)
        mkAltBndrNode n bndr
          | isUnliftedType bndrTy
          , not (isUnboxedTupleType bndrTy)
          , not (isUnboxedSumType bndrTy)
          = do
                node_id <- mkUniqueId
                addNode litNode { node_id = node_id }
                return (bndr,node_id)
          | otherwise = do
                node_id <- mkUniqueId --Shadows existing binds
                let updater = do
                        scrut_res <- lookupNodeResult scrutNodeId :: AM EnterLattice
                        let bndr_res = (indexField scrut_res n)
                        let is_strict_field = elem bndr strictBnds
                        let result
                                | is_strict_field
                                -- Tag things coming out of strict binds
                                = setOuterInfo bndr_res NeverEnter
                                | otherwise = bndr_res
                        -- pprTraceM "Updating altBndr:" (ppr (node_id, result) $$
                        --         text "Input:" <+> ppr scrutNodeId $$
                        --         text "scrut_res" <+> ppr scrut_res $$
                        --         text "bndr_res" <+> ppr bndr_res )
                        let topFields = hasTopFields result
                        if (is_strict_field && topFields) || (topFields && getOuter result == MaybeEnter)
                            then do
                                node <- getNode node_id
                                markDone $ node { node_result = result }
                            else
                                updateNodeResult node_id result
                        return $! result
                addNode FlowNode
                    { node_id = node_id
                    , node_result = bot
                    , node_done = False
                    , node_inputs = [scrutNodeId]
                    , node_update = updater
#if defined(WITH_NODE_DESC)
                    , _node_desc = text "altBndr" <-> ppr altCon <-> ppr bndr
#endif
                    }
                return (bndr,node_id)
            where
                bndrTy = idType bndr

(<->) :: SDoc -> SDoc -> SDoc
(<->) a b = a <> char '_' <> b

-- Note [Let bindings and their context]

-- If we analyse a binding of the form:

--     let f x = e in body

-- then we analyze `e` in the context of CLet[Rec]
-- and `body` in the context of CLet[Rec]Body.

-- In each case the context carries the *same* mapping
-- of binding ids to node ids, however we use different
-- constructors in order to be able to differentiate between tail
-- call branches and regular references to an id.


nodeLet :: [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeLet ctxt (StgLet ext bind expr) = do
    (bind',ctxt') <- nodesBind ctxt NotTopLevel NotLNE bind
    (expr',node) <- nodeExpr ctxt' expr
    return $ (StgLet ext bind' expr', node)
nodeLet _ _ = panic "Impossible"

nodeLetNoEscape :: [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeLetNoEscape ctxt (StgLetNoEscape ext bind expr) = do
    (bind',ctxt') <- nodesBind ctxt NotTopLevel LNE bind
    (expr',node) <- nodeExpr ctxt' expr
    return $ (StgLetNoEscape ext bind' expr', node)
nodeLetNoEscape _ _ = panic "Impossible"

nodeConApp :: HasCallStack => [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeConApp ctxt (StgConApp _ext con args tys) = do
    node_id <- mkUniqueId
    mapM_ addImportedNode [v | StgVarArg v <- args]
    inputs <- mapM (getConArgNodeId ctxt) args :: AM [NodeId]
    let updater = do
            fieldResults <- mapM lookupNodeResult inputs :: AM [EnterLattice]
            let result = mkOutConLattice con top fieldResults
            -- pprTraceM "UpdateConApp:" $ ppr (node_id,result) <+> text "inputs:" <> ppr inputs
            updateNodeResult node_id result
            return result

    addNode FlowNode
        { node_id = node_id
        , node_result = bot
        , node_inputs = inputs
        , node_done = False
        , node_update = updater
#if defined(WITH_NODE_DESC)
        , _node_desc = text "conApp"
#endif
        }

    return (StgConApp node_id con args tys, node_id)
nodeConApp _ _ = panic "Impossible"

{-
    * A recursive call won't produce any new information.
    * Neither will imported functions

    Note [RecEnter Context]

Consider this function:

    f x
        | x < 0 = Just Nothing
        | otherwise = f $ x-1

After initializing all nodes to bot the `otherwise` branch
would naivly be analyzed as bottom. This means the body of
f x would evaluate to bot, and we will never be able to
reasonably analyze the result of f.

We deal with this by looking for branches in a functions body
which are tail calls to the function itself.

In any such case we assign the branch a special value representing
this kind of recursion. When combining branches this value behaves
different to bot, as Rec `combine` x will always result in x.





    -- TODO:    Mutual recursion

-}
nodeApp :: HasCallStack => [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeApp ctxt expr@(StgApp _ f args) = do
    mapM_ addImportedNode (f:[v | StgVarArg v <- args])
    s <- get
    let this_mod = fs_mod s
    maybeImportedFunc <- importedFuncNode this_mod f

    case () of
        _
            | Just node_id <- maybeImportedFunc
            ->  return (StgApp node_id f args, node_id)
            | otherwise -> do
                node_id <- mkUniqueId
                let updater = do
                        -- pprTraceM "Updating " (ppr node_id)
                        -- Try to peek into the function being applied
                        !result <- mkResult
                        -- node <- getNode node_id
                        !input_nodes <- mapM getNode inputs
                        -- pprTraceM "AppFields" (ppr (f, result) <+> ppr node $$
                        --     text "inputs:" <+> ppr inputs $$
                        --     ppr input_nodes
                        --     )
                        if (null inputs || isTopValue result )
                            -- We have collected the final result
                            then do
                                -- pprTraceM "Limiting nesting for " (ppr node_id)
                                node <- getNode node_id
                                markDone $ node { node_result = result }
                                return $ result
                            else do
                                updateNodeResult node_id result
                                return result

                addNode $ FlowNode
                    { node_id = node_id, node_result = bot
                    , node_inputs = inputs, node_done = False
                    , node_update = updater
#if defined(WITH_NODE_DESC)
                    , _node_desc = text "app" <-> ppr f <> ppr args
#endif
                    }

                return (StgApp node_id f args, node_id)
  where
    -- Determine if f in this position is a recursive tail call
    -- and as such safe to set to RecEnter
    -- See Note [RecEnter Context]
    isRecTail :: [SynContext] -> Bool
    isRecTail (CTopLevel _ : _) = False
    isRecTail (CLetRec bnds _: _)
        | isSingleMapOf f bnds
        = True
    isRecTail (CLetRec _ LNE    : ctxt) = isRecTail ctxt
    isRecTail (CLetRec _ NotLNE : _   ) = False
    isRecTail (CLetRecBody bnds _ :ctxt)
        | f `elemVarEnv` bnds = False
        | otherwise = isRecTail ctxt
    isRecTail (CLet _ LNE    : ctxt) = isRecTail ctxt
    isRecTail (CLet _ NotLNE : _) = False
    isRecTail (CLetBody bnds _ : ctxt)
        | elemVarEnv f bnds = False
        | otherwise = isRecTail ctxt
    isRecTail (CClosureBody args : ctxt)
        | f `elemVarEnv` args = False
        | otherwise = isRecTail ctxt
    isRecTail (CCaseScrut : _) = False
    isRecTail (CCaseBndr bnd : ctxt )
        | elemVarEnv f bnd
        = False
        | otherwise
        = isRecTail ctxt
    isRecTail (CAlt bnds : ctxt)
        | f `elemVarEnv` bnds = False
        | otherwise = isRecTail ctxt
    isRecTail (CNone : _) = panic "impossible"
    isRecTail x = pprPanic "Incomplete" $ ppr x

    inputs
        | isAbsentExpr expr = []
        | OtherRecursion <- recursionKind = []
        | isFun && (not isSat) = []
        | recTail = []
        | isFun && isSat = [f_node_id]
        | otherwise = [f_node_id]

    mkResult :: AM EnterLattice
    mkResult
        | isAbsent = pprTrace "Absent:" (ppr f) $ return $ flatLattice NeverEnter

        -- I'm fairly certain we can do better than this on mutual recursion.
        -- But it also seems to change hardly anything on GHC
        | OtherRecursion <- recursionKind
        =   -- pprTrace "mutRec" (ppr f_node_id) $
            return top

        | isFun && (not isSat) = return top

        -- App in a direct self-recursive tail call context, returns nothing
        | recTail = return $ LatRec RecEnter

        | SimpleRecursion <- recursionKind =
            -- pprTrace "simpleRec" (ppr f) $
            lookupNodeResult f_node_id

        | isFun && isSat = (`setOuterInfo` MaybeEnter) <$> lookupNodeResult f_node_id


        -- TODO: If we build a pap, but keep track of the field values we should
        -- be able to use these if it's fully applied later in the body.
        {- eg:
            case f x of pap ->
            let res = pap y (resulting in tagged fields)
            if cond then Just <taggedThing> else res
        -}
        | not isFun
        --, ASSERT null args -- Otherwise data is applied to arguments.
        , null args
        = lookupNodeResult f_node_id

        | otherwise
        =   -- pprTrace "Unsat?" (ppr (f,args)) $
            return top

    recTail = recursionKind == SimpleRecursion && isRecTail ctxt
    isFun = isFunTy (unwrapType $ idType f)
    arity = idFunRepArity f
    isSat = arity > 0 && (length args == arity)
    isAbsent = isAbsentExpr expr

    -- We check if f is imported using importedFuncNode so this
    -- is guarantedd to be not imported when demanded.
    f_node_id = mkLocalIdNodeId ctxt f

    recursionKind = getRecursionKind ctxt

    getRecursionKind [] = NoRecursion
    getRecursionKind ((CLetRec ids _) : _) | f `elemVarEnv` ids =
                if sizeUFM ids == 1 then SimpleRecursion else OtherRecursion
    getRecursionKind (_ : todo) = getRecursionKind todo
nodeApp _ _ = panic "Impossible"

sccNodes :: [FlowNode] -> [FlowNode]
-- sccNodes in_nodes = in_nodes
sccNodes in_nodes = reverse . map node_payload . topologicalSortG $ graphFromEdgedVerticesUniq vertices
  where
    components = stronglyConnCompFromEdgedVerticesUniqR vertices
    vertices = map mkVertex in_nodes :: [Node NodeId FlowNode]
    mkVertex :: FlowNode -> Node NodeId FlowNode
    mkVertex n = DigraphNode (n) (node_id n) (node_inputs n)

solveConstraints :: HasCallStack => AM ()
solveConstraints = do
        -- uqCount <- sizeUFM . fs_uqNodeMap <$> get
        -- doneCount <- sizeUFM . fs_doneNodes <$> get

        -- idList <- map snd . nonDetUFMToList . fs_idNodeMap <$> get
        -- uqList <- map snd . nonDetUFMToList . fs_uqNodeMap <$> get
        -- doneList <- map snd . nonDetUFMToList . fs_doneNodes <$> get
        -- -- mapM_ (pprTraceM "node:" . ppr) (idList ++ uqList ++ doneList)
        -- pprTraceM "Initial: (uqList, doneList)" (ppr (uqCount, doneCount))
        pprTraceM "IterateStart" empty
        iterate 1
        -- iterate (-11)

        -- uqCount <- sizeUFM . fs_uqNodeMap <$> get
        -- doneCount <- sizeUFM . fs_doneNodes <$> get
        -- pprTraceM "ListLengthsFinal" $ ppr (uqCount, doneCount)

        -- remainingNodes <- nonDetEltsUFM . fs_uqNodeMap <$> get
        -- mapM_ (pprTraceM "UnfinishedNodes:" . ppr) $ remainingNodes


        idList <- map snd . nonDetUFMToList . fs_idNodeMap <$> get
        uqList <- map snd . nonDetUFMToList . fs_uqNodeMap <$> get
        doneList <- map snd . nonDetUFMToList . fs_doneNodes <$> get
        let resultNodes =  (uqList ++ doneList)
        seq (unsafePerformIO $ dumpIfSet_dyn unsafeGlobalDynFlags
                Opt_D_dump_stg_tag_nodes "STG Infered tags"
                (vcat $ map ppr resultNodes)) (return ())
        -- mapM_ (pprTraceM "node:" . ppr) resultNodes
        return ()
  where
    iterate :: Int -> AM ()
    iterate n = do
        -- pprTraceM "iterate - pass " (ppr n)
        uqNodes <- fs_uqNodeMap <$> get
        -- return $! seqEltsUFM rnf uqNodes
        uqCount <- sizeUFM . fs_uqNodeMap <$> get
        pprTraceM "IterateUndone:" $ ppr (sizeUFM uqNodes)

        progress <- or <$> (mapM update (sccNodes . nonDetEltsUFM $ uqNodes)) :: AM Bool
        if (not progress)
            then return ()
            --max iterations
            else if (n > 5)
                then -- pprTraceM "Warning:" (text "Aborting at" <+> ppr n <+> text "iterations") >>
                     return ()
                else iterate (n+1)

    update :: FlowNode -> AM Bool
    update node = do
        let old_result = node_result node
        result <- node_update node
        done <- and <$> (mapM isMarkedDone (node_inputs node))
        let node' = node { node_result = result }
        when (done || result `nestingLevelOver` 10) (markDone node')
        if (result == old_result)
            -- Nothing to do this round
            then return False
            else do
                return True


{-
------------------------------------------------------------
    Add cases around strict fields where required.
------------------------------------------------------------
-}

rewriteTopBinds :: [InferStgTopBinding] -> AM [TgStgTopBinding]
rewriteTopBinds binds = mapM (rewriteTop) binds

rewriteTop :: InferStgTopBinding -> AM TgStgTopBinding
rewriteTop (StgTopStringLit v s) = return (StgTopStringLit v s)
rewriteTop      (StgTopLifted bind)  = do
    (StgTopLifted . fst) <$> (rewriteBinds TopLevel bind)

rewriteBinds :: TopLevelFlag -> InferStgBinding -> AM (TgStgBinding, TgStgExpr -> TgStgExpr)
rewriteBinds topFlag (StgNonRec v rhs)
    | TopLevel    <- topFlag = do
        bind <- (StgNonRec v <$> rewriteRhsInplace v rhs)
        return (bind, id)
    | NotTopLevel <- topFlag = do
        (rhs, wrapper) <-  rewriteRhs v rhs
        return (StgNonRec v rhs, wrapper)
rewriteBinds topFlag (StgRec binds)
    | TopLevel    <- topFlag = do
        bind <- mkRec <$> mapM (uncurry rewriteRhsInplace) binds
        return (bind, id)
    | NotTopLevel <- topFlag = do
        rhss <- mapM (uncurry rewriteRhsInplace) binds :: AM ([TgStgRhs])
        return (mkRec rhss, id)
  where
    mkRec :: [TgStgRhs] -> TgStgBinding
    mkRec rhss = StgRec (zip (map fst binds) rhss)
    -- rhss' <- mapM (uncurry rewriteRhsInplace) binds
    -- return $ StgRec (zip (map fst binds) rhss')

-- For top level lets we have to turn lets into closures.
rewriteRhsInplace :: HasCallStack => Id -> InferStgRhs -> AM TgStgRhs
rewriteRhsInplace _binding (StgRhsCon node_id ccs con args) = do
    node <- getNode node_id
    fieldInfos <- mapM lookupNodeResult (node_inputs node)
    let strictIndices = getStrictConArgs con (zip [0..] fieldInfos) :: [(Int,EnterLattice)]
    let needsEval = map fst . filter (not . hasOuterTag . snd) $ strictIndices :: [Int]
    -- TODO: selectIndices is not a performant solution, fix that.
    let evalArgs = [v | StgVarArg v <- selectIndices needsEval args] :: [Id]

    if (null evalArgs)
        then return (StgRhsCon noExtFieldSilent ccs con args)
        else do
            tagInfo <- lookupNodeResult node_id
            -- pprTraceM "Creating closure for " $ ppr _binding <+> ppr (node_id, tagInfo)
            conExpr <- mkSeqs evalArgs con args (panic "mkSeqs should not need to provide types")
            return $ (StgRhsClosure noExtFieldSilent ccs ReEntrant [] conExpr)

rewriteRhsInplace _binding (StgRhsClosure ext ccs flag args body) = do
    -- pprTraceM "rewriteRhsClosure" $ ppr binding <+> ppr tagInfo
    StgRhsClosure ext ccs flag args <$> rewriteExpr False body

-- | When dealing with a let bound rhs passing the id in allows us the shortcut the
--  the rule for the rhs tag to flow to the id
rewriteRhs :: Id -> InferStgRhs -> AM (TgStgRhs, TgStgExpr -> TgStgExpr)
rewriteRhs binding (StgRhsCon node_id ccs con args) = do
    node <- getNode node_id
    fieldInfos <- mapM lookupNodeResult (node_inputs node)
    -- tagInfo <- lookupNodeResult node_id
    -- pprTraceM "rewriteRhsCon" $ ppr binding <+> ppr tagInfo
    -- pprTraceM "rewriteConApp" $ ppr con <+> vcat [
    --     text "args" <+> ppr args,
    --     text "tagInfo" <+> ppr tagInfo,
    --     text "fieldInfos" <+> ppr fieldInfos
    --     -- text "strictIndices" <+> ppr strictIndices,
    --     -- text "needsEval" <+> ppr needsEval,
    --     -- text "evalArgs" <+> ppr evalArgs
    --     ]

    -- TODO: use zip3
    let strictIndices = getStrictConArgs con (zip [0..] fieldInfos) :: [(Int,EnterLattice)]
    let needsEval = map fst . filter (not . hasOuterTag . snd) $ strictIndices :: [Int]
    -- TODO: selectIndices is not a performant solution, fix that.
    let evalArgs = [v | StgVarArg v <- selectIndices needsEval args] :: [Id]

    if (null evalArgs)
        then return (StgRhsCon noExtFieldSilent ccs con args, id)
        else do
            -- tagInfo <- lookupNodeResult node_id
            -- pprTraceM "Creating seqs (wrapped) for " $ ppr binding <+> ppr (node_id, tagInfo)

            evaldArgs <- mapM mkLocalArgId evalArgs -- Create case binders
            let varMap = zip evalArgs evaldArgs -- Match them up with original ids
            let updateArg (StgLitArg lit) = (StgLitArg lit)
                updateArg (StgVarArg v)
                    | Just v' <- lookup v varMap
                    = StgVarArg v'
                    | otherwise = StgVarArg v
            let evaldConArgs = map updateArg args
            return ((StgRhsCon noExtFieldSilent ccs con evaldConArgs), \expr -> foldr (\(v, vEvald) e -> mkSeq v vEvald e) expr varMap)
rewriteRhs _binding (StgRhsClosure ext ccs flag args body) = do
    pure (,) <*>
        (StgRhsClosure ext ccs flag args <$> rewriteExpr False body) <*>
        pure id

type IsScrut = Bool

rewriteExpr :: IsScrut -> InferStgExpr -> AM TgStgExpr
rewriteExpr _ (e@StgCase {})          = rewriteCase e
rewriteExpr _ (e@StgLet {})           = rewriteLet e
rewriteExpr _ (e@StgLetNoEscape {})   = rewriteLetNoEscape e
rewriteExpr isScrut (StgTick t e)     = StgTick t <$!> rewriteExpr isScrut e
rewriteExpr _ e@(StgConApp {})        = rewriteConApp e

rewriteExpr isScrut e@(StgApp {})     = rewriteApp isScrut e
rewriteExpr _ (StgLit lit)           = return (StgLit lit)
rewriteExpr _ (StgOpApp op args res_ty) = return $ (StgOpApp op args res_ty)
rewriteExpr _ (StgLam {}) = error "Invariant violated: No lambdas in STG representation."

rewriteCase :: InferStgExpr -> AM TgStgExpr
rewriteCase (StgCase scrut bndr alt_type alts) =
    pure StgCase <*>
        rewriteExpr True scrut <*>
        pure bndr <*>
        pure alt_type <*>
        mapM rewriteAlt alts

rewriteCase _ = panic "Impossible: nodeCase"

rewriteAlt :: InferStgAlt -> AM TgStgAlt
rewriteAlt (altCon, bndrs, rhs) = do
    !rhs' <- rewriteExpr False rhs
    return (altCon, bndrs, rhs')

rewriteLet :: InferStgExpr -> AM TgStgExpr
rewriteLet (StgLet xt bind expr) = do
    (!bind', !wrapper) <- rewriteBinds NotTopLevel bind
    !expr' <- rewriteExpr False expr
    return $! wrapper (StgLet xt bind' expr')
rewriteLet _ = panic "Impossible"

rewriteLetNoEscape :: InferStgExpr -> AM TgStgExpr
rewriteLetNoEscape (StgLetNoEscape xt bind expr) = do
    (bind', wrapper) <- rewriteBinds NotTopLevel bind
    expr' <- rewriteExpr False expr
    return $ wrapper (StgLetNoEscape xt bind' expr')
rewriteLetNoEscape _ = panic "Impossible"

rewriteConApp :: InferStgExpr -> AM TgStgExpr
rewriteConApp (StgConApp nodeId con args tys) = do
    node <- getNode nodeId
    -- We look at the INPUT because the output of this node will always have tagged
    -- strict fields in the end.
    fieldInfos <- mapM lookupNodeResult (node_inputs node)
    let strictIndices = getStrictConArgs con (zip3 [(0 :: Int) ..] fieldInfos args) :: [(Int,EnterLattice, StgArg)]
    let needsEval = map fstOf3 . filter (not . hasOuterTag . sndOf3) $ strictIndices :: [Int]
    let evalArgs = [v | StgVarArg v <- selectIndices needsEval args] :: [Id]
    if (not $ null evalArgs)
        then do
            -- pprTraceM "Creating conAppSeqs for " $ ppr nodeId <+> parens ( ppr evalArgs ) <+> parens ( ppr fieldInfos )
            mkSeqs evalArgs con args tys
        else return (StgConApp noExtFieldSilent con args tys)

rewriteConApp _ = panic "Impossible"

rewriteApp :: IsScrut -> InferStgExpr -> AM TgStgExpr
rewriteApp True (StgApp nodeId f args)
    | null args = do
    tagInfo <- lookupNodeResult nodeId
    let !enter = (enterInfo $ getOuter tagInfo)
    return $ StgApp enter f args
  where
    enterInfo AlwaysEnter       = -- pprTrace "alwaysEnter" (ppr f)
                                --   StgSyn.AlwaysEnter
                                -- Reenters evaluated closures too often
                                  StgSyn.MayEnter
    enterInfo NeverEnter        = StgSyn.NoEnter
    enterInfo MaybeEnter        = StgSyn.MayEnter
    enterInfo RecEnter          = StgSyn.MayEnter
    enterInfo UndetEnterInfo    = StgSyn.MayEnter

rewriteApp _ (StgApp _ f args) = return $ StgApp MayEnter f args -- TODO? Also apply here?
rewriteApp _ _ = panic "Impossible"


----------------------------------------------
-- Utilities for rewriting ConRhs to ConClosure

-- We should really replace ALL references to the evaluatee with the evaluted binding.
-- Not just in the constructor args.

mkSeq :: Id -> Id -> TgStgExpr -> TgStgExpr
mkSeq id bndr expr =
    -- pprTrace "mkSeq" (ppr (id,bndr)) $
    let altTy = mkStgAltType bndr [(DEFAULT, [], panic "Not used")]
    in
    StgCase (StgApp MayEnter id []) bndr altTy [(DEFAULT, [], expr)]

-- Create a ConApp which is guaranteed to evaluate the given ids.
mkSeqs :: [Id] -> DataCon -> [StgArg] -> [Type] -> AM TgStgExpr
mkSeqs untaggedIds con args tys = do
    argMap <- mapM (\arg -> (arg,) <$> mkLocalArgId arg ) untaggedIds :: AM [(InId, OutId)]
    -- mapM_ (pprTraceM "Forcing strict args:" . ppr) argMap
    let taggedArgs
            = map   (\v -> case v of
                        StgVarArg v' -> StgVarArg $ fromMaybe v' $ lookup v' argMap
                        lit -> lit)
                    args

    let conBody = StgConApp noExtFieldSilent con taggedArgs tys
    let body = foldr (\(v,bndr) expr -> mkSeq v bndr expr) conBody argMap
    return body

mkLocalArgId :: Id -> AM Id
mkLocalArgId id = do
    u <- getUniqueM
    return $ setIdUnique (localiseId id) u

-- These are inserted by the WW transformation and we treat them semantically as tagged.
-- This avoids us seqing them again.
isAbsentExpr :: GenStgExpr p -> Bool
isAbsentExpr (StgTick _t e) = isAbsentExpr e
isAbsentExpr (StgApp _ f _)
  | idUnique f == absentErrorIdKey = True
isAbsentExpr _ = False
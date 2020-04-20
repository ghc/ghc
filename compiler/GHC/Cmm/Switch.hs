{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GHC.Cmm.Switch (
     SwitchTargets,
     mkSwitchTargets,
     switchTargetsCases, switchTargetsDefault, switchTargetsRange, switchTargetsSigned,
     mapSwitchTargets, switchTargetsToTable, switchTargetsFallThrough,
     switchTargetsToList, eqSwitchTargetWith,

     SwitchPlan(..),
     targetSupportsSwitch,
     createSwitchPlan,
  ) where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Driver.Session
import GHC.Cmm.Dataflow.Label (Label)

import Data.Maybe
import Data.List (groupBy)
import Data.Function (on)
import qualified Data.Map as M

-- Note [Cmm Switches, the general plan]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Compiling a high-level switch statement, as it comes out of a STG case
-- expression, for example, allows for a surprising amount of design decisions.
-- Therefore, we cleanly separated this from the Stg → Cmm transformation, as
-- well as from the actual code generation.
--
-- The overall plan is:
--  * The Stg → Cmm transformation creates a single `SwitchTargets` in
--    emitSwitch and emitCmmLitSwitch in GHC.StgToCmm.Utils.
--    At this stage, they are unsuitable for code generation.
--  * A dedicated Cmm transformation (GHC.Cmm.Switch.Implement) replaces these
--    switch statements with code that is suitable for code generation, i.e.
--    a nice balanced tree of decisions with dense jump tables in the leafs.
--    The actual planning of this tree is performed in pure code in createSwitchPlan
--    in this module. See Note [createSwitchPlan].
--  * The actual code generation will not do any further processing and
--    implement each CmmSwitch with a jump tables.
--
-- When compiling to LLVM or C, GHC.Cmm.Switch.Implement leaves the switch
-- statements alone, as we can turn a SwitchTargets value into a nice
-- switch-statement in LLVM resp. C, and leave the rest to the compiler.
--
-- See Note [GHC.Cmm.Switch vs. GHC.Cmm.Switch.Implement] why the two module are
-- separated.

-----------------------------------------------------------------------------
-- Note [Magic Constants in GHC.Cmm.Switch]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- There are a lot of heuristics here that depend on magic values where it is
-- hard to determine the "best" value (for whatever that means). These are the
-- magic values:

-- | Number of consecutive default values allowed in a jump table. If there are
-- more of them, the jump tables are split.
--
-- Currently 7, as it costs 7 words of additional code when a jump table is
-- split (at least on x64, determined experimentally).
maxJumpTableHole :: Integer
maxJumpTableHole = 7

-- | Minimum size of a jump table. If the number is smaller, the switch is
-- implemented using conditionals.
-- Currently 5, because an if-then-else tree of 4 values is nice and compact.
minJumpTableSize :: Int
minJumpTableSize = 5

-- | Minimum non-zero offset for a jump table. See Note [Jump Table Offset].
minJumpTableOffset :: Integer
minJumpTableOffset = 2


-----------------------------------------------------------------------------
-- Switch Targets

-- Note [SwitchTargets]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- The branches of a switch are stored in a SwitchTargets, which consists of an
-- (optional) default jump target, and a map from values to jump targets.
--
-- If the default jump target is absent, the behaviour of the switch outside the
-- values of the map is undefined.
--
-- We use an Integer for the keys the map so that it can be used in switches on
-- unsigned as well as signed integers.
--
-- The map may be empty (we prune out-of-range branches here, so it could be us
-- emptying it).
--
-- Before code generation, the table needs to be brought into a form where all
-- entries are non-negative, so that it can be compiled into a jump table.
-- See switchTargetsToTable.


-- | A value of type SwitchTargets contains the alternatives for a 'CmmSwitch'
-- value, and knows whether the value is signed, the possible range, an
-- optional default value and a map from values to jump labels.
data SwitchTargets =
    SwitchTargets
        Bool                       -- Signed values
        (Integer, Integer)         -- Range
        (Maybe Label)              -- Default value
        (M.Map Integer Label)      -- The branches
    deriving (Show, Eq)

-- | The smart constructor mkSwitchTargets normalises the map a bit:
--  * No entries outside the range
--  * No entries equal to the default
--  * No default if all elements have explicit values
mkSwitchTargets :: Bool -> (Integer, Integer) -> Maybe Label -> M.Map Integer Label -> SwitchTargets
mkSwitchTargets signed range@(lo,hi) mbdef ids
    = SwitchTargets signed range mbdef' ids'
  where
    ids' = dropDefault $ restrict ids
    mbdef' | defaultNeeded = mbdef
           | otherwise     = Nothing

    -- Drop entries outside the range, if there is a range
    restrict = restrictMap (lo,hi)

    -- Drop entries that equal the default, if there is a default
    dropDefault | Just l <- mbdef = M.filter (/= l)
                | otherwise       = id

    -- Check if the default is still needed
    defaultNeeded = fromIntegral (M.size ids') /= hi-lo+1


-- | Changes all labels mentioned in the SwitchTargets value
mapSwitchTargets :: (Label -> Label) -> SwitchTargets -> SwitchTargets
mapSwitchTargets f (SwitchTargets signed range mbdef branches)
    = SwitchTargets signed range (fmap f mbdef) (fmap f branches)

-- | Returns the list of non-default branches of the SwitchTargets value
switchTargetsCases :: SwitchTargets -> [(Integer, Label)]
switchTargetsCases (SwitchTargets _ _ _ branches) = M.toList branches

-- | Return the default label of the SwitchTargets value
switchTargetsDefault :: SwitchTargets -> Maybe Label
switchTargetsDefault (SwitchTargets _ _ mbdef _) = mbdef

-- | Return the range of the SwitchTargets value
switchTargetsRange :: SwitchTargets -> (Integer, Integer)
switchTargetsRange (SwitchTargets _ range _ _) = range

-- | Return whether this is used for a signed value
switchTargetsSigned :: SwitchTargets -> Bool
switchTargetsSigned (SwitchTargets signed _ _ _) = signed

-- | switchTargetsToTable creates a dense jump table, usable for code generation.
--
-- Also returns an offset to add to the value; the list is 0-based on the
-- result of that addition.
--
-- The conversion from Integer to Int is a bit of a wart, as the actual
-- scrutinee might be an unsigned word, but it just works, due to wrap-around
-- arithmetic (as verified by the CmmSwitchTest test case).
switchTargetsToTable :: SwitchTargets -> (Int, [Maybe Label])
switchTargetsToTable (SwitchTargets _ (lo,hi) mbdef branches)
    = (fromIntegral (-start), [ labelFor i | i <- [start..hi] ])
  where
    labelFor i = case M.lookup i branches of Just l -> Just l
                                             Nothing -> mbdef
    start | lo >= 0 && lo < minJumpTableOffset  = 0  -- See Note [Jump Table Offset]
          | otherwise                           = lo

-- Note [Jump Table Offset]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Usually, the code for a jump table starting at x will first subtract x from
-- the value, to avoid a large amount of empty entries. But if x is very small,
-- the extra entries are no worse than the subtraction in terms of code size, and
-- not having to do the subtraction is quicker.
--
-- I.e. instead of
--     _u20N:
--             leaq -1(%r14),%rax
--             jmp *_n20R(,%rax,8)
--     _n20R:
--             .quad   _c20p
--             .quad   _c20q
-- do
--     _u20N:
--             jmp *_n20Q(,%r14,8)
--
--     _n20Q:
--             .quad   0
--             .quad   _c20p
--             .quad   _c20q
--             .quad   _c20r

-- | The list of all labels occurring in the SwitchTargets value.
switchTargetsToList :: SwitchTargets -> [Label]
switchTargetsToList (SwitchTargets _ _ mbdef branches)
    = maybeToList mbdef ++ M.elems branches

-- | Groups cases with equal targets, suitable for pretty-printing to a
-- c-like switch statement with fall-through semantics.
switchTargetsFallThrough :: SwitchTargets -> ([([Integer], Label)], Maybe Label)
switchTargetsFallThrough (SwitchTargets _ _ mbdef branches) = (groups, mbdef)
  where
    groups = map (\xs -> (map fst xs, snd (head xs))) $
             groupBy ((==) `on` snd) $
             M.toList branches

-- | Custom equality helper, needed for "GHC.Cmm.CommonBlockElim"
eqSwitchTargetWith :: (Label -> Label -> Bool) -> SwitchTargets -> SwitchTargets -> Bool
eqSwitchTargetWith eq (SwitchTargets signed1 range1 mbdef1 ids1) (SwitchTargets signed2 range2 mbdef2 ids2) =
    signed1 == signed2 && range1 == range2 && goMB mbdef1 mbdef2 && goList (M.toList ids1) (M.toList ids2)
  where
    goMB Nothing Nothing = True
    goMB (Just l1) (Just l2) = l1 `eq` l2
    goMB _ _ = False
    goList [] [] = True
    goList ((i1,l1):ls1) ((i2,l2):ls2) = i1 == i2 && l1 `eq` l2 && goList ls1 ls2
    goList _ _ = False

-----------------------------------------------------------------------------
-- Code generation for Switches


-- | A SwitchPlan abstractly describes how a Switch statement ought to be
-- implemented. See Note [createSwitchPlan]
data SwitchPlan
    = Unconditionally Label
    | IfEqual Integer Label SwitchPlan
    | IfLT Bool Integer SwitchPlan SwitchPlan
    | JumpTable SwitchTargets
  deriving Show
--
-- Note [createSwitchPlan]
-- ~~~~~~~~~~~~~~~~~~~~~~~
--
-- A SwitchPlan describes how a Switch statement is to be broken down into
-- smaller pieces suitable for code generation.
--
-- createSwitchPlan creates such a switch plan, in these steps:
--  1. It splits the switch statement at segments of non-default values that
--     are too large. See splitAtHoles and Note [Magic Constants in GHC.Cmm.Switch]
--  2. Too small jump tables should be avoided, so we break up smaller pieces
--     in breakTooSmall.
--  3. We fill in the segments between those pieces with a jump to the default
--     label (if there is one), returning a SeparatedList in mkFlatSwitchPlan
--  4. We find and replace two less-than branches by a single equal-to-test in
--     findSingleValues
--  5. The thus collected pieces are assembled to a balanced binary tree.

{-
  Note [Two alts + default]
  ~~~~~~~~~~~~~~~~~~~~~~~~~

Discussion and a bit more info at #14644

When dealing with a switch of the form:
switch(e) {
  case 1: goto l1;
  case 3000: goto l2;
  default: goto ldef;
}

If we treat it as a sparse jump table we would generate:

if (e > 3000) //Check if value is outside of the jump table.
    goto ldef;
else {
    if (e < 3000) { //Compare to upper value
        if(e != 1) //Compare to remaining value
            goto ldef;
          else
            goto l2;
    }
    else
        goto l1;
}

Instead we special case this to :

if (e==1) goto l1;
else if (e==3000) goto l2;
else goto l3;

This means we have:
* Less comparisons for: 1,<3000
* Unchanged for 3000
* One more for >3000

This improves code in a few ways:
* One comparison less means smaller code which helps with cache.
* It exchanges a taken jump for two jumps no taken in the >range case.
  Jumps not taken are cheaper (See Agner guides) making this about as fast.
* For all other cases the first range check is removed making it faster.

The end result is that the change is not measurably slower for the case
>3000 and faster for the other cases.

This makes running this kind of match in an inner loop cheaper by 10-20%
depending on the data.
In nofib this improves wheel-sieve1 by 4-9% depending on problem
size.

We could also add a second conditional jump after the comparison to
keep the range check like this:
    cmp 3000, rArgument
    jg <default>
    je <branch 2>
While this is fairly cheap it made no big difference for the >3000 case
and slowed down all other cases making it not worthwhile.
-}


-- | Does the target support switch out of the box? Then leave this to the
-- target!
targetSupportsSwitch :: HscTarget -> Bool
targetSupportsSwitch HscC = True
targetSupportsSwitch HscLlvm = True
targetSupportsSwitch _ = False

-- | This function creates a SwitchPlan from a SwitchTargets value, breaking it
-- down into smaller pieces suitable for code generation.
createSwitchPlan :: SwitchTargets -> SwitchPlan
-- Lets do the common case of a singleton map quickly and efficiently (#10677)
createSwitchPlan (SwitchTargets _signed _range (Just defLabel) m)
    | [(x, l)] <- M.toList m
    = IfEqual x l (Unconditionally defLabel)
-- And another common case, matching "booleans"
createSwitchPlan (SwitchTargets _signed (lo,hi) Nothing m)
    | [(x1, l1), (_x2,l2)] <- M.toAscList m
    --Checking If |range| = 2 is enough if we have two unique literals
    , hi - lo == 1
    = IfEqual x1 l1 (Unconditionally l2)
-- See Note [Two alts + default]
createSwitchPlan (SwitchTargets _signed _range (Just defLabel) m)
    | [(x1, l1), (x2,l2)] <- M.toAscList m
    = IfEqual x1 l1 (IfEqual x2 l2 (Unconditionally defLabel))
createSwitchPlan (SwitchTargets signed range mbdef m) =
    -- pprTrace "createSwitchPlan" (text (show ids) $$ text (show (range,m)) $$ text (show pieces) $$ text (show flatPlan) $$ text (show plan)) $
    plan
  where
    pieces = concatMap breakTooSmall $ splitAtHoles maxJumpTableHole m
    flatPlan = findSingleValues $ mkFlatSwitchPlan signed mbdef range pieces
    plan = buildTree signed $ flatPlan


---
--- Step 1: Splitting at large holes
---
splitAtHoles :: Integer -> M.Map Integer a -> [M.Map Integer a]
splitAtHoles _        m | M.null m = []
splitAtHoles holeSize m = map (\range -> restrictMap range m) nonHoles
  where
    holes = filter (\(l,h) -> h - l > holeSize) $ zip (M.keys m) (tail (M.keys m))
    nonHoles = reassocTuples lo holes hi

    (lo,_) = M.findMin m
    (hi,_) = M.findMax m

---
--- Step 2: Avoid small jump tables
---
-- We do not want jump tables below a certain size. This breaks them up
-- (into singleton maps, for now).
breakTooSmall :: M.Map Integer a -> [M.Map Integer a]
breakTooSmall m
  | M.size m > minJumpTableSize = [m]
  | otherwise                   = [M.singleton k v | (k,v) <- M.toList m]

---
---  Step 3: Fill in the blanks
---

-- | A FlatSwitchPlan is a list of SwitchPlans, with an integer inbetween every
-- two entries, dividing the range.
-- So if we have (abusing list syntax) [plan1,n,plan2], then we use plan1 if
-- the expression is < n, and plan2 otherwise.

type FlatSwitchPlan = SeparatedList Integer SwitchPlan

mkFlatSwitchPlan :: Bool -> Maybe Label -> (Integer, Integer) -> [M.Map Integer Label] -> FlatSwitchPlan

-- If we have no default (i.e. undefined where there is no entry), we can
-- branch at the minimum of each map
mkFlatSwitchPlan _ Nothing _ [] = pprPanic "mkFlatSwitchPlan with nothing left to do" empty
mkFlatSwitchPlan signed  Nothing _ (m:ms)
  = (mkLeafPlan signed Nothing m , [ (fst (M.findMin m'), mkLeafPlan signed Nothing m') | m' <- ms ])

-- If we have a default, we have to interleave segments that jump
-- to the default between the maps
mkFlatSwitchPlan signed (Just l) r ms = let ((_,p1):ps) = go r ms in (p1, ps)
  where
    go (lo,hi) []
        | lo > hi = []
        | otherwise = [(lo, Unconditionally l)]
    go (lo,hi) (m:ms)
        | lo < min
        = (lo, Unconditionally l) : go (min,hi) (m:ms)
        | lo == min
        = (lo, mkLeafPlan signed (Just l) m) : go (max+1,hi) ms
        | otherwise
        = pprPanic "mkFlatSwitchPlan" (integer lo <+> integer min)
      where
        min = fst (M.findMin m)
        max = fst (M.findMax m)


mkLeafPlan :: Bool -> Maybe Label -> M.Map Integer Label -> SwitchPlan
mkLeafPlan signed mbdef m
    | [(_,l)] <- M.toList m -- singleton map
    = Unconditionally l
    | otherwise
    = JumpTable $ mkSwitchTargets signed (min,max) mbdef m
  where
    min = fst (M.findMin m)
    max = fst (M.findMax m)

---
---  Step 4: Reduce the number of branches using ==
---

-- A sequence of three unconditional jumps, with the outer two pointing to the
-- same value and the bounds off by exactly one can be improved
findSingleValues :: FlatSwitchPlan -> FlatSwitchPlan
findSingleValues (Unconditionally l, (i, Unconditionally l2) : (i', Unconditionally l3) : xs)
  | l == l3 && i + 1 == i'
  = findSingleValues (IfEqual i l2 (Unconditionally l), xs)
findSingleValues (p, (i,p'):xs)
  = (p,i) `consSL` findSingleValues (p', xs)
findSingleValues (p, [])
  = (p, [])

---
---  Step 5: Actually build the tree
---

-- Build a balanced tree from a separated list
buildTree :: Bool -> FlatSwitchPlan -> SwitchPlan
buildTree _ (p,[]) = p
buildTree signed sl = IfLT signed m (buildTree signed sl1) (buildTree signed sl2)
  where
    (sl1, m, sl2) = divideSL sl



--
-- Utility data type: Non-empty lists with extra markers in between each
-- element:
--

type SeparatedList b a = (a, [(b,a)])

consSL :: (a, b) -> SeparatedList b a -> SeparatedList b a
consSL (a, b) (a', xs) = (a, (b,a'):xs)

divideSL :: SeparatedList b a -> (SeparatedList b a, b, SeparatedList b a)
divideSL (_,[]) = error "divideSL: Singleton SeparatedList"
divideSL (p,xs) = ((p, xs1), m, (p', xs2))
  where
    (xs1, (m,p'):xs2) = splitAt (length xs `div` 2) xs

--
-- Other Utilities
--

restrictMap :: (Integer,Integer) -> M.Map Integer b -> M.Map Integer b
restrictMap (lo,hi) m = mid
  where (_,   mid_hi) = M.split (lo-1) m
        (mid, _) =      M.split (hi+1) mid_hi

-- for example: reassocTuples a [(b,c),(d,e)] f == [(a,b),(c,d),(e,f)]
reassocTuples :: a -> [(a,a)] -> a -> [(a,a)]
reassocTuples initial [] last
    = [(initial,last)]
reassocTuples initial ((a,b):tuples) last
    = (initial,a) : reassocTuples b tuples last

-- Note [GHC.Cmm.Switch vs. GHC.Cmm.Switch.Implement]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- I (Joachim) separated the two somewhat closely related modules
--
--  - GHC.Cmm.Switch, which provides the CmmSwitchTargets type and contains the strategy
--    for implementing a Cmm switch (createSwitchPlan), and
--  - GHC.Cmm.Switch.Implement, which contains the actual Cmm graph modification,
--
-- for these reasons:
--
--  * GHC.Cmm.Switch is very low in the dependency tree, i.e. does not depend on any
--    GHC specific modules at all (with the exception of Output and
--    GHC.Cmm.Dataflow (Literal)).
--  * GHC.Cmm.Switch.Implement is the Cmm transformation and hence very high in
--    the dependency tree.
--  * GHC.Cmm.Switch provides the CmmSwitchTargets data type, which is abstract, but
--    used in GHC.Cmm.Node.
--  * Because GHC.Cmm.Switch is low in the dependency tree, the separation allows
--    for more parallelism when building GHC.
--  * The interaction between the modules is very explicit and easy to
--    understand, due to the small and simple interface.

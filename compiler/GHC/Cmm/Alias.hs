{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHC.Cmm.Alias
    ( AbsMem(..)
    , bothHeaps, heapsConflict, bothMems
    , memConflicts --, exprMem, loadAddr, storeAddr

    , exprMem, loadAddr, storeAddr

    , cmmHpAlias, node_exit_hps, HpSet(..), regAliasesHp
    , sizeHpSet

    )
where

import GHC.Prelude as Prelude

import GHC.Platform
import GHC.Cmm
import GHC.Cmm.Ppr.Expr () -- For Outputable instances
import GHC.Cmm.Ppr () -- For Outputable instances
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Label

import GHC.Utils.Outputable

import Data.Set as Set
import qualified Data.Semigroup
import GHC.Cmm.Utils (regUsedIn)
-- import GHC.Utils.Trace (pprTrace)

-----------------------------------------------------------------------------
-- Abstracting over memory access
-----------------------------------------------------------------------------

-- An abstraction of memory read or written.
data AbsMem
  = NoMem            -- ^ no memory accessed
  | AnyMem           -- ^ arbitrary memory
  | HeapMem !HeapType-- ^ heap memory
  | StackMem         -- ^ definitely stack memory
  | SpMem            -- ^ <size>[Sp+n] see Note [SpMem Aliasing]
       {-# UNPACK #-} !Int
       {-# UNPACK #-} !Int
  deriving Show

instance Outputable AbsMem where
  ppr x = parens (text . show $ x)

-- See Note [Heap Kinds]
data HeapType = OldHeap | NewHeap | AnyHeap deriving (Show,Eq)

bothHeaps :: HeapType -> HeapType -> HeapType
bothHeaps h1 h2 | h1 == h2 = h1
bothHeaps _  _  = AnyHeap

heapsConflict :: HeapType -> HeapType -> Bool
heapsConflict AnyHeap  _       = True
heapsConflict _        AnyHeap = True
heapsConflict OldHeap  OldHeap = True
heapsConflict NewHeap  NewHeap = False
heapsConflict OldHeap  NewHeap = False
heapsConflict NewHeap  OldHeap = False

{- Note [Heap Kinds]
~~~~~~~~~~~~~~~~~~~~
Our goal is to allow sinking into assignments to Hp.
That is for a sequence like:

  c1 = [R1 + 8]
  c2 = [R1 + 16]
  [Hp-16] = c1
  [Hp-8] = c2

We want to inline the assignments to get:

  [Hp-16] = [R1 + 8]
  [Hp-8] = [R1 + 16]

To achieve this we split heap memory references into three kinds.
OldHeap, NewHeap, AnyHeap.

AnyHeap is the conservative estimate of a reference where a write/read
might conflight with any other write/read.

OldHeap represents reads from memory where objects existing on entry to
the current function are located.

NewHeap represents the area of memory into which we allocate new objects.
Since we only create *new* objects there it won't conflict with reading
from already existing objects. And while we write to various Hp-relative
memory locations by constructions none of these do conflict.

* Reading from regular heap memory is defined to be OldHeap.
* Writing to regular heap memory is defined to be AnyHeap.
* Writing via HpReg is defined to be NewHeap. A write like this
  always allocates a *new* object (by design) so it won't affect
  reads from existing objects.
* An expression depending on New+Old heap is treated as AnyHeap
* Reading via HpReg (or an alias to it) is treated as AnyMem.

New/OldHeap don't conflict. All other kinds of reference combinations do conflict.

This means we can sink reads from `OldHeap` past writes to `NewHeap` (Hp)
giving use better code as we can remove all the intermediate variables which
sometimes used to get spilled to the C stack.

This depends on Hp never being used to write to "old" heap. This
isn't something our code generation ever does, so that is fine.

Note [CmmCalls and Hp Aliasing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since we assume all foreign calls clopper heap/stack (see Note [Foreign calls clobber heap])
we can relax the Hp aliasing slightly. In particular we will never sink memory accessing
expressions across calls so using Hp and Hp-aliasing variables as arguments/targets for function
calls is allowed.

This is important if we have code like this:

    // Allocate some closure
    I64[Hp - 32] = x1_s5bz_info;
    ...
    hp_ptr::P64 = Hp - 32;
    I64[Hp - n] = foo;
    ...
    if <cond> goto c6cS;
    ...
c6cS:
    // Evaluate the thunk
    call (I64[hp_ptr])(hp_ptr) returns to c6cQ, args: 8, res: 8, upd: 8;

Is this code problematic for sink? Not really. While hp_ptr aliases to the
same area of memory as Hp it's only used inside a call. And we currently
never sink reads/writes across calls anyway.
The end result being that using hp-aliasing variables as arguments/targets
for function calls is fine.

-----

The other issue with calls are their results. Naturally a call might return a newly
allocated heap object as result. But since we don't sink across calls we can assume
any write to Hp after a call will write to different memory than where the call allocated
the object. So even if technically the result can point to the nursery we will treat it
as OldHeap after the call.

--------------------------------------------------------------------------------

Note [SpMem Aliasing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Having SpMem is important because it lets us float loads from Sp
past stores to Sp as long as they don't overlap, and this helps to
unravel some long sequences of
   x1 = [Sp + 8]
   x2 = [Sp + 16]
   ...
   [Sp + 8]  = xi
   [Sp + 16] = xj

Note that SpMem is invalidated if Sp is changed, but the definition
of 'conflicts' above handles that.

ToDo: this won't currently fix the following commonly occurring code:
   x1 = [R1 + 8]
   x2 = [R1 + 16]
   ..
   [Hp - 8] = x1
   [Hp - 16] = x2
   ..

because [R1 + 8] and [Hp - 8] are both HeapMem.  We know that
assignments to [Hp + n] do not conflict with any other heap memory,
but this is tricky to nail down.  What if we had

  x = Hp + n
  [x] = ...

 the store to [x] should be "new heap", not "old heap".
 Furthermore, you could imagine that if we started inlining
 functions in Cmm then there might well be reads of heap memory
 that was written in the same basic block.  To take advantage of
 non-aliasing of heap memory we will have to be more clever.

 NB: We now solved the last point. See Note [Heap Kinds].

Note [Foreign calls clobber heap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is tempting to say that foreign calls clobber only
non-heap/stack memory, but unfortunately we break this invariant in
the RTS.  For example, in stg_catch_retry_frame we call
stmCommitNestedTransaction() which modifies the contents of the
TRec it is passed (this actually caused incorrect code to be
generated).

Since the invariant is true for the majority of foreign calls,
perhaps we ought to have a special annotation for calls that can
modify heap/stack memory.  For now we just use the conservative
definition here.

Some CallishMachOp imply a memory barrier e.g. AtomicRMW and
therefore we should never float any memory operations across one of
these calls.

`suspendThread` releases the capability used by the thread, hence we mustn't
float accesses to heap, stack or virtual global registers stored in the
capability (e.g. with unregisterised build, see #19237).

-}

bothMems :: AbsMem -> AbsMem -> AbsMem
bothMems NoMem    x         = x
bothMems x        NoMem     = x
bothMems (HeapMem h1) (HeapMem h2) = HeapMem $! bothHeaps h1 h2
bothMems StackMem StackMem     = StackMem
bothMems (SpMem o1 w1) (SpMem o2 w2)
  | o1 == o2  = SpMem o1 (max w1 w2)
  | otherwise = StackMem
bothMems SpMem{}  StackMem  = StackMem
bothMems StackMem SpMem{}   = StackMem
bothMems _         _        = AnyMem

memConflicts :: AbsMem -> AbsMem -> Bool
memConflicts NoMem      _          = False
memConflicts _          NoMem      = False
memConflicts HeapMem{}  StackMem   = False
memConflicts StackMem   HeapMem{}  = False
memConflicts SpMem{}    HeapMem{}  = False
memConflicts HeapMem{}  SpMem{}    = False
memConflicts (HeapMem h1) (HeapMem h2) = heapsConflict h1 h2
memConflicts (SpMem o1 w1) (SpMem o2 w2)
  | o1 < o2   = o1 + w1 > o2
  | otherwise = o2 + w2 > o1
memConflicts _         _         = True

-----------------------------------------------------------------------------
-- Abstracting over memory access - considering which registers might alias to Hp
--
-- Currently these will panic when trying to load values via Hp or Hp aliased
-- expressions. If we ever allow use of Hp for memory reads then we need to return
-- AnyHeap instead.
-----------------------------------------------------------------------------

exprMem :: Platform -> Maybe HpSet -> CmmExpr -> AbsMem
exprMem platform hps (CmmLoad addr w _a) = bothMems   (loadAddr platform hps addr (typeWidth w))
                                                        (exprMem platform hps addr)
exprMem platform hps (CmmMachOp _ es) = let args = fmap (exprMem platform hps) es
                                          in Prelude.foldr (\l r -> l `seq` r `seq` bothMems l r) NoMem args
exprMem _        _   (CmmStackSlot {}) = AnyMem
exprMem _        _   _                = NoMem

-- We treat reading from Hp different than loading from Hp, hence the load/store distinction.
-- See Note [Heap Kinds]
loadAddr, storeAddr :: Platform -> Maybe HpSet -> CmmExpr -> Width -> AbsMem
loadAddr p hps = refAddrHp p hps False
storeAddr p hps = refAddrHp p hps True

refAddrHp :: Platform -> Maybe HpSet -> Bool -> CmmExpr -> Width -> AbsMem
refAddrHp platform hps is_store e w = -- pprTrace "refAddrHp" (ppr e) $
  case e of
   CmmReg r       -> regAddrHp platform hps is_store r 0 w
   CmmRegOff r i  -> regAddrHp platform hps is_store r i w
   _other | regUsedIn platform spReg e -> StackMem
          | foldRegsUsed platform (\b r -> b || r `maybe_regAliasesHp` hps) False e -> trace_hp_mem (text "refAddrHp") (AnyMem)
          | otherwise                  -> -- pprTrace "refAddrAny" (ppr e)
                                          AnyMem

regAddrHp :: Platform -> Maybe HpSet -> Bool -> CmmReg -> Int -> Width -> AbsMem
regAddrHp _ _hps _store   (CmmGlobal Sp) i w = SpMem i (widthInBytes w)
regAddrHp _ _hps is_store (CmmGlobal Hp) _ _
    | is_store  = HeapMem NewHeap
    | otherwise = trace_hp_mem (text "HpStore") (HeapMem AnyHeap)
regAddrHp _ _hps _store   (CmmGlobal CurrentTSO) _ _ = HeapMem (AnyHeap) -- important for PrimOps
regAddrHp platform hps is_store r _ _
    | isGcPtrType (cmmRegType platform r)
    = if is_store
          then (HeapMem AnyHeap)
          else if r `maybe_regAliasesHp` hps
              then trace_hp_mem (text "Aliased HpRead") (HeapMem AnyHeap)
              else (HeapMem OldHeap) -- yay! GCPtr pays for itself
regAddrHp _ _hps _store _ _ _ = AnyMem

trace_hp_mem :: SDoc -> a -> a
trace_hp_mem _err x =
    -- pprTrace "trace_hp_mem" err $
    x

-----------------------------------------------------------------------------
-- Calculating what variables transitively depend on the value of Hp on block entry.
-----------------------------------------------------------------------------

-- | The variables aliased to HP on entry to a block
data HpSet = HpSet { localSet :: !LocalRegSet, globalSet :: !GlobalRegSet }

instance Outputable HpSet where
    ppr (HpSet local global) = parens (text "HpSet" <+> text "local:" <+> ppr (regSetToList local) <+> ppr (regSetToList global))

instance Semigroup HpSet where
    (<>) = plusHpSet

instance Monoid HpSet where
    mempty = emptyHpSet

sizeHpSet :: HpSet -> Int
sizeHpSet (HpSet l g) = sizeRegSet l + sizeRegSet g

plusHpSet :: HpSet -> HpSet -> HpSet
plusHpSet (HpSet l1 g1) (HpSet l2 g2) = HpSet (plusRegSet l1 l2) (plusRegSet g1 g2) :: HpSet

regAliasesHp :: CmmReg -> HpSet -> Bool
regAliasesHp reg hp_set = go reg hp_set
    where go (CmmLocal r)   (HpSet l_set _g_set) = elemRegSet r l_set
          go (CmmGlobal r)  (HpSet _l_set g_set) = elemRegSet r g_set

-- | If we have no information about aliasing we must assume everything can alias to Hp.
maybe_regAliasesHp :: CmmReg -> Maybe HpSet -> Bool
maybe_regAliasesHp _reg Nothing    = True
maybe_regAliasesHp reg  (Just hps) = regAliasesHp reg hps


emptyHpSet :: HpSet
emptyHpSet = HpSet mempty mempty

-- | The dataflow lattice
hpLattice :: DataflowLattice (HpSet)
hpLattice = DataflowLattice emptyHpSet add
  where
    add (OldFact old@(HpSet lold gold)) (NewFact (HpSet lnew gnew)) =
        let !changed = (Set.size l_join + Set.size g_join > Set.size lold + Set.size gold)
            join@(HpSet l_join g_join) = HpSet (Set.union lold lnew) (Set.union gold gnew)
        in if changed then Changed join
                      else NotChanged old

-- Given a set of registers aliasing to Hp compute the set of registers
-- aliasing Hp after this node.
node_exit_hps
    ::  ( OutputableP Platform (CmmNode e x)
        )
    => Platform -> (CmmNode e x) -> HpSet -> HpSet
node_exit_hps platform node hp_set@(HpSet lset gset) =
    let !result_aliases_hp =
            case node of
                -- See Note [CmmCalls and Hp Aliasing]
                CmmCall{}        -> False
                CmmForeignCall{} -> False
                -- Default (conservative) case. If the statement uses Hp assume it's result aliases Hp.
                _default -> ( foldRegsUsed platform (\b r -> b || r == hpReg || aliasesHp r) False node)
                where
                    aliasesHp r = r `regAliasesHp` hp_set

        {-# INLINE update #-}
        update :: forall r. (Ord r,Outputable r) => RegSet r -> r -> RegSet r
        update s r = if result_aliases_hp
            then -- pprTrace "Adding hp" (text "r:" <> ppr r <+> text "node:" <> pdoc platform node) $
                 extendRegSet s r
            else deleteFromRegSet s r

        g_hps = foldRegsDefd platform (\s reg -> update s reg) gset node :: GlobalRegSet
        l_hps = foldRegsDefd platform (\s reg -> update s reg) lset node :: LocalRegSet

        in (HpSet l_hps g_hps)

-- | Compute hp aliasing registers at exit
xferHp :: Platform -> TransferFun HpSet
xferHp p = blockTransferFwd p hpLattice node_exit_hps

-- | Compute a map from blocks a set of registers that alias to Hp on *entry* to that block.
cmmHpAlias :: Platform -> CmmGraph -> LabelMap HpSet
cmmHpAlias platform graph =
    analyzeCmmFwd hpLattice (xferHp platform) graph mapEmpty

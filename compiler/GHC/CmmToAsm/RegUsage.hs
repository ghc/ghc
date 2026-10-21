{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}

-- | Registers read and written by an instruction, and a small DSL for the
-- native code generators to describe them.
--
-- See Note [Register usage DSL].
module GHC.CmmToAsm.RegUsage
   ( RegUsage(..)
   , noUsage
     -- * Describing the register usage of an instruction
   , Usage
   , (<.>)
   , runUsage
   , readReg
   , writeReg
   , modifyReg
   , readRegs
   , writeRegs
   , foldMapUsage
   , withPlatform
   , interesting
   )
where

import GHC.Prelude
import GHC.Exts (oneShot, isTrue#, reallyUnsafePtrEquality)
import Data.Semigroup (Semigroup(..))

import GHC.CmmToAsm.Format
import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Regs (freeReg)

-- | Holds a list of source and destination registers used by a
--      particular instruction.
--
--   Machine registers that are pre-allocated to stgRegs are filtered
--      out, because they are uninteresting from a register allocation
--      standpoint.  (We wouldn't want them to end up on the free list!)
--
--   As far as we are concerned, the fixed registers simply don't exist
--      (for allocation purposes, anyway).
--
data RegUsage = RU
  { reads  :: ![RegWithFormat]
  , writes :: ![RegWithFormat]
  } deriving Show

-- | No regs read or written to.
noUsage :: RegUsage
noUsage  = RU [] []

{- Note [Register usage DSL]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The liveness passes call 'regUsageOfInstr' once per instruction. The lists
it builds must be cheap. Each backend describes an instruction as a Monoid
expression and applies 'runUsage' once:

    regUsageOfInstr platform instr = runUsage platform $ case instr of
      ADD fmt src dst -> readOp fmt src <.> modifyOp fmt dst
      ...

readOp, writeOp and modifyOp are backend helpers over operands. They use the
primitives of this module:

  * 'readReg', 'writeReg' and 'modifyReg' for one register.
  * 'readRegs' and 'writeRegs' for lists that come from the code generator
    (call parameters, clobbered registers).
  * 'withPlatform' when a helper needs the platform (word size, format of a
    register).

In runUsage (a <.> b), the registers that a reads come before the registers
that b reads. The same rule applies to the written registers. The two lists
follow the order of the expression. runUsage keeps only the 'interesting'
registers. The order is important: the linear allocator assigns registers
to the virtual registers of an instruction in list order.

A backend must obey these rules. They keep regUsageOfInstr small, and they
make it allocate only the final lists. The reasons are in
Note [Register usage DSL implementation].

1. Write the modified operand as the rightmost item of the expression:

       readOp fmt src <.> modifyOp fmt dst

   This rule is only an optimisation. A modified register in a different
   position gives the same lists, but it costs one more cons cell.

2. Keep the case of regUsageOfInstr non-recursive. Put a recursive case
   (for example the x86 LOCK prefix) in a separate equation.

3. Make the helpers over operands (readOp, writeOp, modifyOp) top-level
   functions with a NOINLINE pragma. If GHC inlines them, it copies the
   dispatch over the operand constructors into each alternative. Then
   regUsageOfInstr becomes several times larger (300 times for AArch64).
   This rule also applies to the backend function that computes the format
   of a register.

4. Compute platform-dependent values inside 'withPlatform', not before. The
   Usage receives the platform, so the helpers do not need a closure. Force
   such a value with a bang when several items use it (for example the
   format of an address), or GHC allocates a thunk for it.
-}

{- Note [Register usage DSL implementation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [Register usage DSL] describes the interface. This note describes the
implementation and gives the reasons for the rules of the interface.

The types
---------
A Usage is a function that receives the platform and an accumulator, and
that returns a new accumulator. The accumulator is the RegUsage built so
far:

    newtype Usage = Usage (Platform -> RegUsage -> RegUsage)

    data RegUsage = RU { reads :: ![RegWithFormat], writes :: ![RegWithFormat] }

(The real constructor is Usage', and the pattern synonym Usage adds the
one-shot annotations.) runUsage applies the function to 'noUsage', the
RegUsage with two empty lists, and returns the result:

    runUsage platform (Usage f) = f platform noUsage

(<.>) is function composition: in (a <.> b), the function of b runs first
and the function of a receives its result. Thus the items of an expression
are applied from right to left. mempty is the identity function.

A primitive adds one register to the front of one list. For example,
readReg builds a RegWithFormat and puts it at the front of the read list:

    readReg fmt r = Usage $ \platform !acc ->
      if interesting platform r
        then case acc of
          RU rs ws -> let !x = RegWithFormat r fmt
                      in RU (x : rs) ws
        else acc

The order of the lists follows from the right-to-left application: the
rightmost item is added first, so it ends at the back of the lists.

Sharing of the modified registers
---------------------------------
A modified register goes to the front of both lists. This usually costs two
cons cells. When the two lists are the same list, one cons cell can serve
as both lists. modifyReg detects this case with a pointer comparison:

    modifyReg fmt r = Usage $ \platform !acc ->
      if interesting platform r
        then case acc of
          RU rs ws
            | isTrue# (reallyUnsafePtrEquality rs ws)
            -> let !x = RegWithFormat r fmt
                   !l = x : rs
               in RU l l
            | otherwise
            -> let !x = RegWithFormat r fmt
               in RU (x : rs) (x : ws)
        else acc

The pointer comparison is safe. Two lists with the same pointer are equal,
so one new cell for both of them gives the correct result. A false negative
only costs one more cons cell. All cells are built with a strict let, so
the lists are never thunks.

The two lists of noUsage are the same static closure for []. Thus the
rightmost modified register of an expression shares its cell, and so do
the modified registers that follow it directly. A readReg or a writeReg
makes the two lists different. Thus

    readOp fmt src <.> modifyReg fmt dst

allocates one RegWithFormat and one cons cell per register, plus the RU.
This is the reason for rule 1 of the interface.

Allocation of the Usage values
------------------------------
The combinators and the primitives have an INLINE pragma. RegUsage is a
type with one constructor and strict fields. After simplification, each
alternative of regUsageOfInstr allocates only the final lists and the
final RU: no Usage function, no closure and no intermediate RU. The
lambdas are one-shot, see Note [The one-shot state monad trick] in
GHC.Utils.Monad. This requires that the initial accumulator is a known
constant in each alternative. This is the reason for rule 2.

The backend helpers over operands have a NOINLINE pragma (rule 3). A
Usage receives the platform, so these helpers do not need a closure
(rule 4). The same applies to 'interesting' and to the backend function
that computes the format of a register from its class. A call to such a
function is strict in the accumulator, and it returns a strict product.
Thus worker/wrapper passes the lists unboxed, and no intermediate RU is
allocated. A strict let builds each cell, so each element is a constructor
and not a thunk.

A value that a 'withPlatform' callback computes for several items, such as
the format of an address, must be forced with a bang (rule 4). Without the
bang, GHC allocates a thunk for the value on each call, because each item
uses it only on some paths. With the bang, the callback computes the value
once, inside the accumulator lambda. Do not force such a value before the
accumulator lambda (for example in a helper that returns a Usage): the
helper would lose its arity, and GHC would allocate closures and
accumulators.
-}

-- | Registers read and written by an instruction, as a 'Monoid'. Combine
-- usages with '(<.>)'.
--
-- See Note [Register usage DSL].
newtype Usage = Usage' (Platform -> RegUsage -> RegUsage)

-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern Usage :: (Platform -> RegUsage -> RegUsage) -> Usage
pattern Usage f <- Usage' f
  where
    Usage f = Usage' (oneShot $ \platform -> oneShot $ \acc -> f platform acc)
{-# COMPLETE Usage #-}

instance Semigroup Usage where
   Usage f <> Usage g = Usage (\platform acc -> f platform (g platform acc))
   {-# INLINE (<>) #-}

instance Monoid Usage where
   mempty = Usage (\_ !acc -> acc)
   {-# INLINE mempty #-}

-- | Combine two usages: the registers of the left one come first.
--
-- A dedicated operator, because 'GHC.Prelude' hides the Semigroup '(<>)'
-- ("GHC.Utils.Outputable" exports its own).
(<.>) :: Usage -> Usage -> Usage
(<.>) = (<>)
infixr 6 <.>
{-# INLINE (<.>) #-}

-- | Build the register usage described by a 'Usage'.
--
-- The accumulator is the 'RegUsage' built so far. A 'Usage' is applied
-- right to left, so the accumulator holds the usage of everything to the
-- right of the current item. See Note [Register usage DSL implementation].
runUsage :: Platform -> Usage -> RegUsage
runUsage platform (Usage f) = f platform noUsage
{-# INLINE runUsage #-}

-- | A register read at the given format.
readReg :: Format -> Reg -> Usage
readReg fmt r = Usage $ \platform !acc ->
   if interesting platform r
      then case acc of
         RU rs ws -> let !x = RegWithFormat r fmt
                     in RU (x : rs) ws
      else acc
{-# INLINE readReg #-}

-- | A register written at the given format.
writeReg :: Format -> Reg -> Usage
writeReg fmt r = Usage $ \platform !acc ->
   if interesting platform r
      then case acc of
         RU rs ws -> let !x = RegWithFormat r fmt
                     in RU rs (x : ws)
      else acc
{-# INLINE writeReg #-}

-- | A register read and written at the given format. When the two lists
-- are the same list (for example when this is the last item of the
-- expression), the new cell is shared by both lists.
-- See Note [Register usage DSL implementation].
modifyReg :: Format -> Reg -> Usage
modifyReg fmt r = Usage $ \platform !acc ->
   if interesting platform r
      then case acc of
         RU rs ws
           | isTrue# (reallyUnsafePtrEquality rs ws)
           -> let !x = RegWithFormat r fmt
                  !l = x : rs
              in RU l l
           | otherwise
           -> let !x = RegWithFormat r fmt
              in RU (x : rs) (x : ws)
      else acc
{-# INLINE modifyReg #-}

-- | Registers read, given as a list that has not been filtered yet (for
-- example call parameters, or the registers live at a jump).
readRegs :: [RegWithFormat] -> Usage
readRegs regs = Usage $ \platform !acc ->
   case acc of
      RU rs ws -> RU (keepRegs platform regs rs) ws
{-# INLINE readRegs #-}

-- | Registers written, given as a list that has not been filtered yet (for
-- example the registers clobbered by a call).
writeRegs :: [RegWithFormat] -> Usage
writeRegs regs = Usage $ \platform !acc ->
   case acc of
      RU rs ws -> RU rs (keepRegs platform regs ws)
{-# INLINE writeRegs #-}

-- | The usage of each element of a list, in list order.
foldMapUsage :: (a -> Usage) -> [a] -> Usage
foldMapUsage f xs = Usage $ \platform acc0 -> go platform xs acc0
   where
      -- an explicit strict worker, so that the accumulator is passed unboxed
      go _ []        !acc = acc
      go p (x : xs') !acc = case f x of Usage g -> g p (go p xs' acc)
{-# INLINE foldMapUsage #-}

-- | A usage that depends on the platform (for example on the word size).
withPlatform :: (Platform -> Usage) -> Usage
withPlatform f = Usage $ \platform !acc -> case f platform of Usage g -> g platform acc
{-# INLINE withPlatform #-}

-- | Prepend the interesting registers of a list to a tail. Spine-strict, so
-- that no thunk is allocated.
keepRegs :: Platform -> [RegWithFormat] -> [RegWithFormat] -> [RegWithFormat]
keepRegs platform regs tl = foldr keep tl regs
   where
      keep rf rest
         | interesting platform (regWithFormat_reg rf) = rest `seq` (rf : rest)
         | otherwise                                   = rest
{-# INLINE keepRegs #-}

-- | Is this register interesting for the register allocator?
--
-- Virtual registers are. Real registers are, unless they are pinned (STG
-- registers, the stack pointer, ...).
interesting :: Platform -> Reg -> Bool
interesting _        (RegVirtual _)              = True
interesting platform (RegReal (RealRegSingle i)) = freeReg platform i
{-# NOINLINE interesting #-}
-- NOINLINE: inlined at every register of every instruction, the platform
-- dispatch of 'freeReg' makes regUsageOfInstr several times larger.

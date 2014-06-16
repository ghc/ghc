{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
--
-- Monad for Stg to C-- code generation
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmMonad (
        FCode,        -- type

        initC, runC, thenC, thenFC, listCs,
        returnFC, fixC,
        newUnique, newUniqSupply,

        newLabelC, emitLabel,

        emit, emitDecl, emitProc,
        emitProcWithConvention, emitProcWithStackFrame,
        emitOutOfLine, emitAssign, emitStore, emitComment,

        getCmm, aGraphToGraph,
        getCodeR, getCode, getHeapUsage,

        mkCmmIfThenElse, mkCmmIfThen, mkCmmIfGoto,
        mkCall, mkCmmCall,

        forkClosureBody, forkLneBody, forkAlts, codeOnly,

        ConTagZ,

        Sequel(..), ReturnKind(..),
        withSequel, getSequel,

        setTickyCtrLabel, getTickyCtrLabel,

        withUpdFrameOff, getUpdFrameOff, initUpdFrameOff,

        HeapUsage(..), VirtualHpOffset,        initHpUsage,
        getHpUsage,  setHpUsage, heapHWM,
        setVirtHp, getVirtHp, setRealHp,

        getModuleName,

        -- ideally we wouldn't export these, but some other modules access internal state
        getState, setState, getSelfLoop, withSelfLoop, getInfoDown, getDynFlags, getThisPackage,

        -- more localised access to monad state
        CgIdInfo(..),
        getBinds, setBinds,

        -- out of general friendliness, we also export ...
        CgInfoDownwards(..), CgState(..)        -- non-abstract
    ) where

#include "HsVersions.h"

import Cmm
import StgCmmClosure
import DynFlags
import Hoopl
import Maybes
import MkGraph
import BlockId
import CLabel
import SMRep
import Module
import Id
import VarEnv
import OrdList
import Unique
import UniqSupply
import FastString
import Outputable

import qualified Control.Applicative as A
import Control.Monad
import Data.List
import Prelude hiding( sequence, succ )

infixr 9 `thenC`        -- Right-associative!
infixr 9 `thenFC`


--------------------------------------------------------
-- The FCode monad and its types
--
-- FCode is the monad plumbed through the Stg->Cmm code generator, and
-- the Cmm parser.  It contains the following things:
--
--  - A writer monad, collecting:
--    - code for the current function, in the form of a CmmAGraph.
--      The function "emit" appends more code to this.
--    - the top-level CmmDecls accumulated so far
--
--  - A state monad with:
--    - the local bindings in scope
--    - the current heap usage
--    - a UniqSupply
--
--  - A reader monad, for CgInfoDownwards, containing
--    - DynFlags,
--    - the current Module
--    - the update-frame offset
--    - the ticky counter label
--    - the Sequel (the continuation to return to)
--    - the self-recursive tail call information

--------------------------------------------------------

newtype FCode a = FCode (CgInfoDownwards -> CgState -> (# a, CgState #))

instance Functor FCode where
  fmap f (FCode g) = FCode $ \i s -> case g i s of (# a, s' #) -> (# f a, s' #)

instance A.Applicative FCode where
      pure = return
      (<*>) = ap

instance Monad FCode where
        (>>=) = thenFC
        return = returnFC

{-# INLINE thenC #-}
{-# INLINE thenFC #-}
{-# INLINE returnFC #-}

initC :: IO CgState
initC  = do { uniqs <- mkSplitUniqSupply 'c'
            ; return (initCgState uniqs) }

runC :: DynFlags -> Module -> CgState -> FCode a -> (a,CgState)
runC dflags mod st fcode = doFCode fcode (initCgInfoDown dflags mod) st

returnFC :: a -> FCode a
returnFC val = FCode (\_info_down state -> (# val, state #))

thenC :: FCode () -> FCode a -> FCode a
thenC (FCode m) (FCode k) =
        FCode $ \info_down state -> case m info_down state of
                                     (# _,new_state #) -> k info_down new_state

listCs :: [FCode ()] -> FCode ()
listCs [] = return ()
listCs (fc:fcs) = do
        fc
        listCs fcs

thenFC  :: FCode a -> (a -> FCode c) -> FCode c
thenFC (FCode m) k = FCode $
        \info_down state ->
            case m info_down state of
              (# m_result, new_state #) ->
                 case k m_result of
                   FCode kcode -> kcode info_down new_state

fixC :: (a -> FCode a) -> FCode a
fixC fcode = FCode (
        \info_down state ->
                let
                        (v,s) = doFCode (fcode v) info_down state
                in
                        (# v, s #)
        )

--------------------------------------------------------
--        The code generator environment
--------------------------------------------------------

-- This monadery has some information that it only passes
-- *downwards*, as well as some ``state'' which is modified
-- as we go along.

data CgInfoDownwards        -- information only passed *downwards* by the monad
  = MkCgInfoDown {
        cgd_dflags    :: DynFlags,
        cgd_mod       :: Module,            -- Module being compiled
        cgd_updfr_off :: UpdFrameOffset,    -- Size of current update frame
        cgd_ticky     :: CLabel,            -- Current destination for ticky counts
        cgd_sequel    :: Sequel,            -- What to do at end of basic block
        cgd_self_loop :: Maybe SelfLoopInfo -- Which tail calls can be compiled
                                            -- as local jumps? See Note
                                            -- [Self-recursive tail calls] in
                                            -- StgCmmExpr
  }

type CgBindings = IdEnv CgIdInfo

data CgIdInfo
  = CgIdInfo
        { cg_id :: Id   -- Id that this is the info for
                        -- Can differ from the Id at occurrence sites by
                        -- virtue of being externalised, for splittable C
                        -- See Note [Externalise when splitting]
        , cg_lf  :: LambdaFormInfo
        , cg_loc :: CgLoc                     -- CmmExpr for the *tagged* value
        }

-- Note [Externalise when splitting]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If we're splitting the object with -fsplit-objs, we need to
-- externalise *all* the top-level names, and then make sure we only
-- use the externalised one in any C label we use which refers to this
-- name.

instance Outputable CgIdInfo where
  ppr (CgIdInfo { cg_id = id, cg_loc = loc })
    = ppr id <+> ptext (sLit "-->") <+> ppr loc

-- Sequel tells what to do with the result of this expression
data Sequel
  = Return Bool         -- Return result(s) to continuation found on the stack.
                        -- True <=> the continuation is update code (???)

  | AssignTo
        [LocalReg]      -- Put result(s) in these regs and fall through
                        -- NB: no void arguments here
                        --
        Bool            -- Should we adjust the heap pointer back to
                        -- recover space that's unused on this path?
                        -- We need to do this only if the expression
                        -- may allocate (e.g. it's a foreign call or
                        -- allocating primOp)

-- See Note [sharing continuations] below
data ReturnKind
  = AssignedDirectly
  | ReturnedTo BlockId ByteOff

-- Note [sharing continuations]
--
-- ReturnKind says how the expression being compiled returned its
-- results: either by assigning directly to the registers specified
-- by the Sequel, or by returning to a continuation that does the
-- assignments.  The point of this is we might be able to re-use the
-- continuation in a subsequent heap-check.  Consider:
--
--    case f x of z
--      True  -> <True code>
--      False -> <False code>
--
-- Naively we would generate
--
--    R2 = x   -- argument to f
--    Sp[young(L1)] = L1
--    call f returns to L1
--  L1:
--    z = R1
--    if (z & 1) then Ltrue else Lfalse
--  Ltrue:
--    Hp = Hp + 24
--    if (Hp > HpLim) then L4 else L7
--  L4:
--    HpAlloc = 24
--    goto L5
--  L5:
--    R1 = z
--    Sp[young(L6)] = L6
--    call stg_gc_unpt_r1 returns to L6
--  L6:
--    z = R1
--    goto L1
--  L7:
--    <True code>
--  Lfalse:
--    <False code>
--
-- We want the gc call in L4 to return to L1, and discard L6.  Note
-- that not only can we share L1 and L6, but the assignment of the
-- return address in L4 is unnecessary because the return address for
-- L1 is already on the stack.  We used to catch the sharing of L1 and
-- L6 in the common-block-eliminator, but not the unnecessary return
-- address assignment.
--
-- Since this case is so common I decided to make it more explicit and
-- robust by programming the sharing directly, rather than relying on
-- the common-block elimiantor to catch it.  This makes
-- common-block-elimianteion an optional optimisation, and furthermore
-- generates less code in the first place that we have to subsequently
-- clean up.
--
-- There are some rarer cases of common blocks that we don't catch
-- this way, but that's ok.  Common-block-elimation is still available
-- to catch them when optimisation is enabled.  Some examples are:
--
--   - when both the True and False branches do a heap check, we
--     can share the heap-check failure code L4a and maybe L4
--
--   - in a case-of-case, there might be multiple continuations that
--     we can common up.
--
-- It is always safe to use AssignedDirectly.  Expressions that jump
-- to the continuation from multiple places (e.g. case expressions)
-- fall back to AssignedDirectly.
--


initCgInfoDown :: DynFlags -> Module -> CgInfoDownwards
initCgInfoDown dflags mod
  = MkCgInfoDown { cgd_dflags    = dflags
                 , cgd_mod       = mod
                 , cgd_updfr_off = initUpdFrameOff dflags
                 , cgd_ticky     = mkTopTickyCtrLabel
                 , cgd_sequel    = initSequel
                 , cgd_self_loop = Nothing }

initSequel :: Sequel
initSequel = Return False

initUpdFrameOff :: DynFlags -> UpdFrameOffset
initUpdFrameOff dflags = widthInBytes (wordWidth dflags) -- space for the RA


--------------------------------------------------------
--        The code generator state
--------------------------------------------------------

data CgState
  = MkCgState {
     cgs_stmts :: CmmAGraph,          -- Current procedure

     cgs_tops  :: OrdList CmmDecl,
        -- Other procedures and data blocks in this compilation unit
        -- Both are ordered only so that we can
        -- reduce forward references, when it's easy to do so

     cgs_binds :: CgBindings,

     cgs_hp_usg  :: HeapUsage,

     cgs_uniqs :: UniqSupply }

data HeapUsage   -- See Note [Virtual and real heap pointers]
  = HeapUsage {
        virtHp :: VirtualHpOffset,       -- Virtual offset of highest-allocated word
                                         --   Incremented whenever we allocate
        realHp :: VirtualHpOffset        -- realHp: Virtual offset of real heap ptr
                                         --   Used in instruction addressing modes
    }

type VirtualHpOffset = WordOff


{- Note [Virtual and real heap pointers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The code generator can allocate one or more objects contiguously, performing
one heap check to cover allocation of all the objects at once.  Let's call
this little chunk of heap space an "allocation chunk".  The code generator
will emit code to
  * Perform a heap-exhaustion check
  * Move the heap pointer to the end of the allocation chunk
  * Allocate multiple objects within the chunk

The code generator uses VirtualHpOffsets to address words within a
single allocation chunk; these start at one and increase positively.
The first word of the chunk has VirtualHpOffset=1, the second has
VirtualHpOffset=2, and so on.

 * The field realHp tracks (the VirtualHpOffset) where the real Hp
   register is pointing.  Typically it'll be pointing to the end of the
   allocation chunk.

 * The field virtHp gives the VirtualHpOffset of the highest-allocated
   word so far.  It starts at zero (meaning no word has been allocated),
   and increases whenever an object is allocated.

The difference between realHp and virtHp gives the offset from the
real Hp register of a particular word in the allocation chunk. This
is what getHpRelOffset does.  Since the returned offset is relative
to the real Hp register, it is valid only until you change the real
Hp register.  (Changing virtHp doesn't matter.)
-}


initCgState :: UniqSupply -> CgState
initCgState uniqs
  = MkCgState { cgs_stmts  = mkNop
              , cgs_tops   = nilOL
              , cgs_binds  = emptyVarEnv
              , cgs_hp_usg = initHpUsage
              , cgs_uniqs  = uniqs }

stateIncUsage :: CgState -> CgState -> CgState
-- stateIncUsage@ e1 e2 incorporates in e1
-- the heap high water mark found in e2.
stateIncUsage s1 s2@(MkCgState { cgs_hp_usg = hp_usg })
     = s1 { cgs_hp_usg  = cgs_hp_usg  s1 `maxHpHw`  virtHp hp_usg }
       `addCodeBlocksFrom` s2

addCodeBlocksFrom :: CgState -> CgState -> CgState
-- Add code blocks from the latter to the former
-- (The cgs_stmts will often be empty, but not always; see codeOnly)
s1 `addCodeBlocksFrom` s2
  = s1 { cgs_stmts = cgs_stmts s1 <*> cgs_stmts s2,
         cgs_tops  = cgs_tops  s1 `appOL` cgs_tops  s2 }


-- The heap high water mark is the larger of virtHp and hwHp.  The latter is
-- only records the high water marks of forked-off branches, so to find the
-- heap high water mark you have to take the max of virtHp and hwHp.  Remember,
-- virtHp never retreats!
--
-- Note Jan 04: ok, so why do we only look at the virtual Hp??

heapHWM :: HeapUsage -> VirtualHpOffset
heapHWM = virtHp

initHpUsage :: HeapUsage
initHpUsage = HeapUsage { virtHp = 0, realHp = 0 }

maxHpHw :: HeapUsage -> VirtualHpOffset -> HeapUsage
hp_usg `maxHpHw` hw = hp_usg { virtHp = virtHp hp_usg `max` hw }

--------------------------------------------------------
-- Operators for getting and setting the state and "info_down".
--------------------------------------------------------

getState :: FCode CgState
getState = FCode $ \_info_down state -> (# state, state #)

setState :: CgState -> FCode ()
setState state = FCode $ \_info_down _ -> (# (), state #)

getHpUsage :: FCode HeapUsage
getHpUsage = do
        state <- getState
        return $ cgs_hp_usg state

setHpUsage :: HeapUsage -> FCode ()
setHpUsage new_hp_usg = do
        state <- getState
        setState $ state {cgs_hp_usg = new_hp_usg}

setVirtHp :: VirtualHpOffset -> FCode ()
setVirtHp new_virtHp
  = do  { hp_usage <- getHpUsage
        ; setHpUsage (hp_usage {virtHp = new_virtHp}) }

getVirtHp :: FCode VirtualHpOffset
getVirtHp
  = do  { hp_usage <- getHpUsage
        ; return (virtHp hp_usage) }

setRealHp ::  VirtualHpOffset -> FCode ()
setRealHp new_realHp
  = do  { hp_usage <- getHpUsage
        ; setHpUsage (hp_usage {realHp = new_realHp}) }

getBinds :: FCode CgBindings
getBinds = do
        state <- getState
        return $ cgs_binds state

setBinds :: CgBindings -> FCode ()
setBinds new_binds = do
        state <- getState
        setState $ state {cgs_binds = new_binds}

withState :: FCode a -> CgState -> FCode (a,CgState)
withState (FCode fcode) newstate = FCode $ \info_down state ->
  case fcode info_down newstate of
    (# retval, state2 #) -> (# (retval,state2), state #)

newUniqSupply :: FCode UniqSupply
newUniqSupply = do
        state <- getState
        let (us1, us2) = splitUniqSupply (cgs_uniqs state)
        setState $ state { cgs_uniqs = us1 }
        return us2

newUnique :: FCode Unique
newUnique = do
        state <- getState
        let (u,us') = takeUniqFromSupply (cgs_uniqs state)
        setState $ state { cgs_uniqs = us' }
        return u

------------------
getInfoDown :: FCode CgInfoDownwards
getInfoDown = FCode $ \info_down state -> (# info_down,state #)

getSelfLoop :: FCode (Maybe SelfLoopInfo)
getSelfLoop = do
        info_down <- getInfoDown
        return $ cgd_self_loop info_down

withSelfLoop :: SelfLoopInfo -> FCode a -> FCode a
withSelfLoop self_loop code = do
        info_down <- getInfoDown
        withInfoDown code (info_down {cgd_self_loop = Just self_loop})

instance HasDynFlags FCode where
    getDynFlags = liftM cgd_dflags getInfoDown

getThisPackage :: FCode PackageId
getThisPackage = liftM thisPackage getDynFlags

withInfoDown :: FCode a -> CgInfoDownwards -> FCode a
withInfoDown (FCode fcode) info_down = FCode $ \_ state -> fcode info_down state

doFCode :: FCode a -> CgInfoDownwards -> CgState -> (a,CgState)
doFCode (FCode fcode) info_down state =
  case fcode info_down state of
    (# a, s #) -> ( a, s )

-- ----------------------------------------------------------------------------
-- Get the current module name

getModuleName :: FCode Module
getModuleName = do { info <- getInfoDown; return (cgd_mod info) }

-- ----------------------------------------------------------------------------
-- Get/set the end-of-block info

withSequel :: Sequel -> FCode a -> FCode a
withSequel sequel code
  = do  { info  <- getInfoDown
        ; withInfoDown code (info {cgd_sequel = sequel, cgd_self_loop = Nothing }) }

getSequel :: FCode Sequel
getSequel = do  { info <- getInfoDown
                ; return (cgd_sequel info) }

-- ----------------------------------------------------------------------------
-- Get/set the size of the update frame

-- We keep track of the size of the update frame so that we
-- can set the stack pointer to the proper address on return
-- (or tail call) from the closure.
-- There should be at most one update frame for each closure.
-- Note: I'm including the size of the original return address
-- in the size of the update frame -- hence the default case on `get'.

withUpdFrameOff :: UpdFrameOffset -> FCode a -> FCode a
withUpdFrameOff size code
  = do  { info  <- getInfoDown
        ; withInfoDown code (info {cgd_updfr_off = size }) }

getUpdFrameOff :: FCode UpdFrameOffset
getUpdFrameOff
  = do  { info  <- getInfoDown
        ; return $ cgd_updfr_off info }

-- ----------------------------------------------------------------------------
-- Get/set the current ticky counter label

getTickyCtrLabel :: FCode CLabel
getTickyCtrLabel = do
        info <- getInfoDown
        return (cgd_ticky info)

setTickyCtrLabel :: CLabel -> FCode a -> FCode a
setTickyCtrLabel ticky code = do
        info <- getInfoDown
        withInfoDown code (info {cgd_ticky = ticky})


--------------------------------------------------------
--                 Forking
--------------------------------------------------------

forkClosureBody :: FCode () -> FCode ()
-- forkClosureBody compiles body_code in environment where:
--   - sequel, update stack frame and self loop info are
--     set to fresh values
--   - state is set to a fresh value, except for local bindings
--     that are passed in unchanged. It's up to the enclosed code to
--     re-bind the free variables to a field of the closure.

forkClosureBody body_code
  = do  { dflags <- getDynFlags
        ; info   <- getInfoDown
        ; us     <- newUniqSupply
        ; state  <- getState
        ; let body_info_down = info { cgd_sequel    = initSequel
                                    , cgd_updfr_off = initUpdFrameOff dflags
                                    , cgd_self_loop = Nothing }
              fork_state_in = (initCgState us) { cgs_binds = cgs_binds state }
              ((),fork_state_out) = doFCode body_code body_info_down fork_state_in
        ; setState $ state `addCodeBlocksFrom` fork_state_out }

forkLneBody :: FCode a -> FCode a
-- 'forkLneBody' takes a body of let-no-escape binding and compiles
-- it in the *current* environment, returning the graph thus constructed.
--
-- The current environment is passed on completely unchanged to
-- the successor.  In particular, any heap usage from the enclosed
-- code is discarded; it should deal with its own heap consumption.
forkLneBody body_code
  = do  { info_down <- getInfoDown
        ; us        <- newUniqSupply
        ; state     <- getState
        ; let fork_state_in = (initCgState us) { cgs_binds = cgs_binds state }
              (result, fork_state_out) = doFCode body_code info_down fork_state_in
        ; setState $ state `addCodeBlocksFrom` fork_state_out
        ; return result }

codeOnly :: FCode () -> FCode ()
-- Emit any code from the inner thing into the outer thing
-- Do not affect anything else in the outer state
-- Used in almost-circular code to prevent false loop dependencies
codeOnly body_code
  = do  { info_down <- getInfoDown
        ; us        <- newUniqSupply
        ; state     <- getState
        ; let   fork_state_in = (initCgState us) { cgs_binds   = cgs_binds state
                                                 , cgs_hp_usg  = cgs_hp_usg state }
                ((), fork_state_out) = doFCode body_code info_down fork_state_in
        ; setState $ state `addCodeBlocksFrom` fork_state_out }

forkAlts :: [FCode a] -> FCode [a]
-- (forkAlts' bs d) takes fcodes 'bs' for the branches of a 'case', and
-- an fcode for the default case 'd', and compiles each in the current
-- environment.  The current environment is passed on unmodified, except
-- that the virtual Hp is moved on to the worst virtual Hp for the branches

forkAlts branch_fcodes
  = do  { info_down <- getInfoDown
        ; us <- newUniqSupply
        ; state <- getState
        ; let compile us branch
                = (us2, doFCode branch info_down branch_state)
                where
                  (us1,us2) = splitUniqSupply us
                  branch_state = (initCgState us1) {
                                        cgs_binds  = cgs_binds state
                                      , cgs_hp_usg = cgs_hp_usg state }
              (_us, results) = mapAccumL compile us branch_fcodes
              (branch_results, branch_out_states) = unzip results
        ; setState $ foldl stateIncUsage state branch_out_states
                -- NB foldl.  state is the *left* argument to stateIncUsage
        ; return branch_results }

-- collect the code emitted by an FCode computation
getCodeR :: FCode a -> FCode (a, CmmAGraph)
getCodeR fcode
  = do  { state1 <- getState
        ; (a, state2) <- withState fcode (state1 { cgs_stmts = mkNop })
        ; setState $ state2 { cgs_stmts = cgs_stmts state1  }
        ; return (a, cgs_stmts state2) }

getCode :: FCode a -> FCode CmmAGraph
getCode fcode = do { (_,stmts) <- getCodeR fcode; return stmts }

-- 'getHeapUsage' applies a function to the amount of heap that it uses.
-- It initialises the heap usage to zeros, and passes on an unchanged
-- heap usage.
--
-- It is usually a prelude to performing a GC check, so everything must
-- be in a tidy and consistent state.
--
-- Note the slightly subtle fixed point behaviour needed here

getHeapUsage :: (VirtualHpOffset -> FCode a) -> FCode a
getHeapUsage fcode
  = do  { info_down <- getInfoDown
        ; state <- getState
        ; let   fstate_in = state { cgs_hp_usg  = initHpUsage }
                (r, fstate_out) = doFCode (fcode hp_hw) info_down fstate_in
                hp_hw = heapHWM (cgs_hp_usg fstate_out)        -- Loop here!

        ; setState $ fstate_out { cgs_hp_usg = cgs_hp_usg state }
        ; return r }

-- ----------------------------------------------------------------------------
-- Combinators for emitting code

emitCgStmt :: CgStmt -> FCode ()
emitCgStmt stmt
  = do  { state <- getState
        ; setState $ state { cgs_stmts = cgs_stmts state `snocOL` stmt }
        }

emitLabel :: BlockId -> FCode ()
emitLabel id = emitCgStmt (CgLabel id)

emitComment :: FastString -> FCode ()
#if 0 /* def DEBUG */
emitComment s = emitCgStmt (CgStmt (CmmComment s))
#else
emitComment _ = return ()
#endif

emitAssign :: CmmReg  -> CmmExpr -> FCode ()
emitAssign l r = emitCgStmt (CgStmt (CmmAssign l r))

emitStore :: CmmExpr  -> CmmExpr -> FCode ()
emitStore l r = emitCgStmt (CgStmt (CmmStore l r))


newLabelC :: FCode BlockId
newLabelC = do { u <- newUnique
               ; return $ mkBlockId u }

emit :: CmmAGraph -> FCode ()
emit ag
  = do  { state <- getState
        ; setState $ state { cgs_stmts = cgs_stmts state <*> ag } }

emitDecl :: CmmDecl -> FCode ()
emitDecl decl
  = do  { state <- getState
        ; setState $ state { cgs_tops = cgs_tops state `snocOL` decl } }

emitOutOfLine :: BlockId -> CmmAGraph -> FCode ()
emitOutOfLine l stmts = emitCgStmt (CgFork l stmts)

emitProcWithStackFrame
   :: Convention                        -- entry convention
   -> Maybe CmmInfoTable                -- info table?
   -> CLabel                            -- label for the proc
   -> [CmmFormal]                       -- stack frame
   -> [CmmFormal]                       -- arguments
   -> CmmAGraph                         -- code
   -> Bool                              -- do stack layout?
   -> FCode ()

emitProcWithStackFrame _conv mb_info lbl _stk_args [] blocks False
  = do  { dflags <- getDynFlags
        ; emitProc_ mb_info lbl [] blocks (widthInBytes (wordWidth dflags)) False
        }
emitProcWithStackFrame conv mb_info lbl stk_args args blocks True -- do layout
  = do  { dflags <- getDynFlags
        ; let (offset, live, entry) = mkCallEntry dflags conv args stk_args
        ; emitProc_ mb_info lbl live (entry <*> blocks) offset True
        }
emitProcWithStackFrame _ _ _ _ _ _ _ = panic "emitProcWithStackFrame"

emitProcWithConvention :: Convention -> Maybe CmmInfoTable -> CLabel
                       -> [CmmFormal]
                       -> CmmAGraph
                       -> FCode ()
emitProcWithConvention conv mb_info lbl args blocks
  = emitProcWithStackFrame conv mb_info lbl [] args blocks True

emitProc :: Maybe CmmInfoTable -> CLabel -> [GlobalReg] -> CmmAGraph -> Int -> FCode ()
emitProc  mb_info lbl live blocks offset
 = emitProc_ mb_info lbl live blocks offset True

emitProc_ :: Maybe CmmInfoTable -> CLabel -> [GlobalReg] -> CmmAGraph -> Int -> Bool
          -> FCode ()
emitProc_ mb_info lbl live blocks offset do_layout
  = do  { dflags <- getDynFlags
        ; l <- newLabelC
        ; let
              blks = labelAGraph l blocks

              infos | Just info <- mb_info = mapSingleton (g_entry blks) info
                    | otherwise            = mapEmpty

              sinfo = StackInfo { arg_space = offset
                                , updfr_space = Just (initUpdFrameOff dflags)
                                , do_layout = do_layout }

              tinfo = TopInfo { info_tbls = infos
                              , stack_info=sinfo}

              proc_block = CmmProc tinfo lbl live blks

        ; state <- getState
        ; setState $ state { cgs_tops = cgs_tops state `snocOL` proc_block } }

getCmm :: FCode () -> FCode CmmGroup
-- Get all the CmmTops (there should be no stmts)
-- Return a single Cmm which may be split from other Cmms by
-- object splitting (at a later stage)
getCmm code
  = do  { state1 <- getState
        ; ((), state2) <- withState code (state1 { cgs_tops  = nilOL })
        ; setState $ state2 { cgs_tops = cgs_tops state1 }
        ; return (fromOL (cgs_tops state2)) }


mkCmmIfThenElse :: CmmExpr -> CmmAGraph -> CmmAGraph -> FCode CmmAGraph
mkCmmIfThenElse e tbranch fbranch = do
  endif <- newLabelC
  tid   <- newLabelC
  fid   <- newLabelC
  return $ mkCbranch e tid fid <*>
            mkLabel tid <*> tbranch <*> mkBranch endif <*>
            mkLabel fid <*> fbranch <*> mkLabel endif

mkCmmIfGoto :: CmmExpr -> BlockId -> FCode CmmAGraph
mkCmmIfGoto e tid = do
  endif <- newLabelC
  return $ mkCbranch e tid endif <*> mkLabel endif

mkCmmIfThen :: CmmExpr -> CmmAGraph -> FCode CmmAGraph
mkCmmIfThen e tbranch = do
  endif <- newLabelC
  tid   <- newLabelC
  return $ mkCbranch e tid endif <*>
         mkLabel tid <*> tbranch <*> mkLabel endif


mkCall :: CmmExpr -> (Convention, Convention) -> [CmmFormal] -> [CmmActual]
       -> UpdFrameOffset -> [CmmActual] -> FCode CmmAGraph
mkCall f (callConv, retConv) results actuals updfr_off extra_stack = do
  dflags <- getDynFlags
  k      <- newLabelC
  let area = Young k
      (off, _, copyin) = copyInOflow dflags retConv area results []
      copyout = mkCallReturnsTo dflags f callConv actuals k off updfr_off extra_stack
  return (copyout <*> mkLabel k <*> copyin)

mkCmmCall :: CmmExpr -> [CmmFormal] -> [CmmActual] -> UpdFrameOffset
          -> FCode CmmAGraph
mkCmmCall f results actuals updfr_off
   = mkCall f (NativeDirectCall, NativeReturn) results actuals updfr_off []


-- ----------------------------------------------------------------------------
-- turn CmmAGraph into CmmGraph, for making a new proc.

aGraphToGraph :: CmmAGraph -> FCode CmmGraph
aGraphToGraph stmts
  = do  { l <- newLabelC
        ; return (labelAGraph l stmts) }

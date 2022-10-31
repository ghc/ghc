{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
--
-- Monad for Stg to C-- code generation
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Monad (
        type FCode,
        type FCode',

        initC, initFCodeState, runC, fixC,
        newUnique,

        emitLabel,

        emit, emitDecl,
        emitProcWithConvention, emitProcWithStackFrame,
        emitOutOfLine, emitAssign, emitStore, emitStore',
        emitComment, emitTick, emitUnwind,

        newTemp,

        getCmm, aGraphToGraph, getPlatform, getProfile,
        getCodeR, getCode, getCodeScoped, getHeapUsage,
        getContext,

        mkCmmIfThenElse, mkCmmIfThen, mkCmmIfGoto,
        mkCmmIfThenElse', mkCmmIfThen', mkCmmIfGoto',

        mkCall, mkCmmCall,

        forkClosureBody, forkLneBody, forkAlts, forkAltPair, codeOnly,

        ConTagZ,

        Sequel(..), ReturnKind(..),
        withSequel, getSequel,

        setTickyCtrLabel, getTickyCtrLabel,
        tickScope, getTickScope,

        withUpdFrameOff, getUpdFrameOff,

        HeapUsage(..), VirtualHpOffset,        initHpUsage,
        getHpUsage,  setHpUsage, heapHWM,
        setVirtHp, getVirtHp, setRealHp,

        getModuleName,

        -- ideally we wouldn't export these, but some other modules access internal state
        getState, setState, getSelfLoop, withSelfLoop,
        getConfig, getStgToCmmConfig,

        -- more localised access to monad state
        CgIdInfo(..),
        getBinds, setBinds,
        -- out of general friendliness, we also export ...
        StgToCmmConfig(..), CgState(..), -- non-abstract

        ContainsSDocContext(..),
    ) where

import GHC.Prelude hiding( sequence, succ )

import GHC.Platform
import GHC.Platform.Profile
import GHC.Platform.Profile.Class
import GHC.Cmm.Builder.Config
import GHC.Cmm
import GHC.StgToCmm.Config
import GHC.StgToCmm.Closure
import GHC.StgToCmm.Sequel
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Graph as CmmGraph
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Runtime.Heap.Layout
import GHC.Unit
import GHC.Types.Id
import GHC.Types.Var.Env
import GHC.Data.OrdList
import GHC.Types.Basic( ConTagZ )
import GHC.Types.Unique
import GHC.Types.Unique.Supply
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Exts (oneShot)

import Control.Monad
import Data.List (mapAccumL)


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
--  - A reader monad, for StgToCmmConfig, containing
--    - the profile,
--    - the current Module
--    - the debug level
--    - a bunch of flags see StgToCmm.Config for full details

--  - A second reader monad with:
--    - the update-frame offset
--    - the ticky counter label
--    - the Sequel (the continuation to return to)
--    - the self-recursive tail call information
--    - The tick scope for new blocks and ticks
--

--------------------------------------------------------

newtype FCode' c a = FCode0 { doFCode :: c -> FCodeState -> CgState -> (a, CgState) }

type FCode = FCode' StgToCmmConfig

-- Not derived because of #18202.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
instance Functor (FCode' c) where
  fmap f (FCode m) =
    FCode $ \cfg fst state ->
      case m cfg fst state of
        (x, state') -> (f x, state')

-- This pattern synonym makes the simplifier monad eta-expand,
-- which as a very beneficial effect on compiler performance
-- See #18202.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
{-# COMPLETE FCode #-}
pattern FCode :: (c -> FCodeState -> CgState -> (a, CgState))
              -> FCode' c a
pattern FCode m <- FCode0 m
  where
    FCode m = FCode0 $ oneShot (\cfg -> oneShot
                                 (\fstate -> oneShot
                                   (\state -> m cfg fstate state)))

instance Applicative (FCode' c) where
    pure val = FCode (\_cfg _fstate state -> (val, state))
    {-# INLINE pure #-}
    (<*>) = ap

instance Monad (FCode' c) where
    FCode m >>= k = FCode $
        \cfg fstate state ->
            case m cfg fstate state of
              (m_result, new_state) ->
                 case k m_result of
                   FCode kcode -> kcode cfg fstate new_state
    {-# INLINE (>>=) #-}

instance MonadUnique (FCode' c) where
  getUniqueSupplyM = cgs_uniqs <$> getState
  getUniqueM = FCode $ \_ _ st ->
    let (u, us') = takeUniqFromSupply (cgs_uniqs st)
    in (u, st { cgs_uniqs = us' })

initC :: IO CgState
initC  = do { uniqs <- mkSplitUniqSupply 'c'
            ; return (initCgState uniqs) }

runC :: c -> FCodeState -> CgState -> FCode' c a -> (a, CgState)
runC cfg fst st fcode = doFCode fcode cfg fst st

fixC :: (a -> FCode' c a) -> FCode' c a
fixC fcode = FCode $
    \cfg fstate state ->
      let (v, s) = doFCode (fcode v) cfg fstate state
      in (v, s)

--------------------------------------------------------
--        The code generator environment
--------------------------------------------------------
type CgBindings = IdEnv CgIdInfo

data CgIdInfo
  = CgIdInfo
        { cg_id :: Id   -- Id that this is the info for
        , cg_lf  :: LambdaFormInfo
        , cg_loc :: CgLoc                     -- CmmExpr for the *tagged* value
        }

instance OutputableP Platform CgIdInfo where
  pdoc env (CgIdInfo { cg_id = id, cg_loc = loc })
    = ppr id <+> text "-->" <+> pdoc env loc

-- See Note [sharing continuations] below
data ReturnKind
  = AssignedDirectly
  | ReturnedTo BlockId ByteOff

-- Note [sharing continuations]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
-- the common-block eliminator to catch it.  This makes
-- common-block-elimination an optional optimisation, and furthermore
-- generates less code in the first place that we have to subsequently
-- clean up.
--
-- There are some rarer cases of common blocks that we don't catch
-- this way, but that's ok.  Common-block-elimination is still available
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
-- If you are wondering why you have to be careful forcing CgState then
-- the reason is the knot-tying in 'getHeapUsage'. This problem is tracked
-- in #19245

data FCodeState =
  MkFCodeState { fcs_upframeoffset :: UpdFrameOffset     -- ^ Size of current update frame UpdFrameOffset must be kept lazy or
                                                         -- else the RTS will deadlock _and_ also experience a severe
                                                         -- performance degradation
              , fcs_sequel        :: !Sequel             -- ^ What to do at end of basic block
              , fcs_selfloop      :: Maybe SelfLoopInfo  -- ^ Which tail calls can be compiled as local jumps?
                                                         --   See Note [Self-recursive tail calls] in GHC.StgToCmm.Expr
              , fcs_ticky         :: !CLabel             -- ^ Destination for ticky counts
              , fcs_tickscope     :: !CmmTickScope       -- ^ Tick scope for new blocks & ticks
              }

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
  = s1 { cgs_stmts = cgs_stmts s1 CmmGraph.<*> cgs_stmts s2,
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
-- Operators for getting and setting the state and "stgToCmmConfig".
--------------------------------------------------------

getState :: FCode' c CgState
getState = FCode $ \_cfg _fstate state -> (state, state)

setState :: CgState -> FCode' c ()
setState state = FCode $ \_cfg _fstate _ -> ((), state)

getHpUsage :: FCode' c HeapUsage
getHpUsage = do
        state <- getState
        return $ cgs_hp_usg state

setHpUsage :: HeapUsage -> FCode' c ()
setHpUsage new_hp_usg = do
        state <- getState
        setState $ state {cgs_hp_usg = new_hp_usg}

setVirtHp :: VirtualHpOffset -> FCode' c ()
setVirtHp new_virtHp
  = do  { hp_usage <- getHpUsage
        ; setHpUsage (hp_usage {virtHp = new_virtHp}) }

getVirtHp :: FCode' c VirtualHpOffset
getVirtHp
  = do  { hp_usage <- getHpUsage
        ; return (virtHp hp_usage) }

setRealHp ::  VirtualHpOffset -> FCode' c ()
setRealHp new_realHp
  = do  { hp_usage <- getHpUsage
        ; setHpUsage (hp_usage {realHp = new_realHp}) }

getBinds :: FCode' c CgBindings
getBinds = do
        state <- getState
        return $ cgs_binds state

setBinds :: CgBindings -> FCode' c ()
setBinds new_binds = do
        state <- getState
        setState $ state {cgs_binds = new_binds}

withCgState :: FCode' c a -> CgState -> FCode' c (a,CgState)
withCgState (FCode fcode) newstate = FCode $ \cfg fstate state ->
  case fcode cfg fstate newstate of
    (retval, state2) -> ((retval,state2), state)

newUniqSupply :: FCode' c UniqSupply
newUniqSupply = do
        state <- getState
        let (us1, us2) = splitUniqSupply (cgs_uniqs state)
        setState $ state { cgs_uniqs = us1 }
        return us2

newUnique :: FCode' c Unique
newUnique = do
        state <- getState
        let (u,us') = takeUniqFromSupply (cgs_uniqs state)
        setState $ state { cgs_uniqs = us' }
        return u

newTemp :: MonadUnique m => CmmType -> m LocalReg
newTemp rep = do { uniq <- getUniqueM
                 ; return (LocalReg uniq rep) }

------------------
initFCodeState :: Platform -> FCodeState
initFCodeState p =
  MkFCodeState { fcs_upframeoffset = platformWordSizeInBytes p
               , fcs_sequel        = Return
               , fcs_selfloop      = Nothing
               , fcs_ticky         = mkTopTickyCtrLabel
               , fcs_tickscope     = GlobalScope
               }

getFCodeState :: FCode' c FCodeState
getFCodeState = FCode $ \_ fstate state -> (fstate,state)

-- basically local for the reader monad
withFCodeState :: FCode' c a -> FCodeState -> FCode' c a
withFCodeState (FCode fcode) fst = FCode $ \cfg _ state -> fcode cfg fst state

getSelfLoop :: FCode' c (Maybe SelfLoopInfo)
getSelfLoop = fcs_selfloop <$> getFCodeState

withSelfLoop :: SelfLoopInfo -> FCode' c a -> FCode' c a
withSelfLoop self_loop code = do
        fstate <- getFCodeState
        withFCodeState code (fstate {fcs_selfloop = Just self_loop})

-- ----------------------------------------------------------------------------
-- Get/set the end-of-block info

withSequel :: Sequel -> FCode' c a -> FCode' c a
withSequel sequel code
  = do  { fstate <- getFCodeState
        ; withFCodeState code (fstate { fcs_sequel = sequel
                                      , fcs_selfloop = Nothing }) }

getSequel :: FCode' c Sequel
getSequel = fcs_sequel <$> getFCodeState

-- ----------------------------------------------------------------------------
-- Get/set the size of the update frame

-- We keep track of the size of the update frame so that we
-- can set the stack pointer to the proper address on return
-- (or tail call) from the closure.
-- There should be at most one update frame for each closure.
-- Note: I'm including the size of the original return address
-- in the size of the update frame -- hence the default case on `get'.

withUpdFrameOff :: UpdFrameOffset -> FCode' c a -> FCode' c a
withUpdFrameOff size code
  = do  { fstate <- getFCodeState
        ; withFCodeState code (fstate {fcs_upframeoffset = size }) }

getUpdFrameOff :: FCode' c UpdFrameOffset
getUpdFrameOff = fcs_upframeoffset <$> getFCodeState

-- ----------------------------------------------------------------------------
-- Get/set the current ticky counter label

getTickyCtrLabel :: FCode' c CLabel
getTickyCtrLabel = fcs_ticky <$> getFCodeState

setTickyCtrLabel :: CLabel -> FCode' c a -> FCode' c a
setTickyCtrLabel ticky code = do
        fstate <- getFCodeState
        withFCodeState code (fstate {fcs_ticky = ticky})

-- ----------------------------------------------------------------------------
-- Manage tick scopes

-- | The current tick scope. We will assign this to generated blocks.
getTickScope :: FCode' c CmmTickScope
getTickScope = fcs_tickscope <$> getFCodeState

-- | Places blocks generated by the given code into a fresh
-- (sub-)scope. This will make sure that Cmm annotations in our scope
-- will apply to the Cmm blocks generated therein - but not the other
-- way around.
tickScope :: ContainsEmitDebugInfo c => FCode' c a -> FCode' c a
tickScope code = do
        cfg <- getConfig
        fstate <- getFCodeState
        if not $ emitDebugInfo cfg then code else do
          u <- newUnique
          let scope' = SubScope u (fcs_tickscope fstate)
          withFCodeState code fstate{ fcs_tickscope = scope' }

-- ----------------------------------------------------------------------------
-- Config related helpers

getConfig :: FCode' c c
getConfig = FCode $ \cfg _ state -> (cfg,state)

getStgToCmmConfig :: FCode StgToCmmConfig
getStgToCmmConfig = getConfig

getProfile :: ContainsPlatformProfile c => FCode' c Profile
getProfile = platformProfile <$> getConfig

getPlatform :: ContainsPlatformProfile c => FCode' c Platform
getPlatform = profilePlatform <$> getProfile

getContext :: ContainsSDocContext c => FCode' c SDocContext
getContext = sdocContext <$> getConfig

-- ----------------------------------------------------------------------------
-- Get the current module name

getModuleName :: FCode Module
getModuleName = stgToCmmThisModule <$> getStgToCmmConfig


--------------------------------------------------------
--                 Forking
--------------------------------------------------------

forkClosureBody
  :: ContainsPlatformProfile c
  => FCode' c () -> FCode' c ()
-- forkClosureBody compiles body_code in environment where:
--   - sequel, update stack frame and self loop info are
--     set to fresh values
--   - state is set to a fresh value, except for local bindings
--     that are passed in unchanged. It's up to the enclosed code to
--     re-bind the free variables to a field of the closure.

forkClosureBody body_code
  = do  { platform <- getPlatform
        ; cfg      <- getConfig
        ; fstate   <- getFCodeState
        ; us       <- newUniqSupply
        ; state    <- getState
        ; let fcs = fstate { fcs_sequel        = Return
                           , fcs_upframeoffset = platformWordSizeInBytes platform
                           , fcs_selfloop      = Nothing
                           }
              fork_state_in = (initCgState us) { cgs_binds = cgs_binds state }
              ((),fork_state_out) = doFCode body_code cfg fcs fork_state_in
        ; setState $ state `addCodeBlocksFrom` fork_state_out }

forkLneBody :: FCode' c a -> FCode' c a
-- 'forkLneBody' takes a body of let-no-escape binding and compiles
-- it in the *current* environment, returning the graph thus constructed.
--
-- The current environment is passed on completely unchanged to
-- the successor.  In particular, any heap usage from the enclosed
-- code is discarded; it should deal with its own heap consumption.
forkLneBody body_code
  = do  { cfg   <- getConfig
        ; us    <- newUniqSupply
        ; state <- getState
        ; fstate <- getFCodeState
        ; let fork_state_in = (initCgState us) { cgs_binds = cgs_binds state }
              (result, fork_state_out) = doFCode body_code cfg fstate fork_state_in
        ; setState $ state `addCodeBlocksFrom` fork_state_out
        ; return result }

codeOnly :: FCode' c () -> FCode' c ()
-- Emit any code from the inner thing into the outer thing
-- Do not affect anything else in the outer state
-- Used in almost-circular code to prevent false loop dependencies
codeOnly body_code
  = do  { cfg   <- getConfig
        ; us    <- newUniqSupply
        ; state <- getState
        ; fstate <- getFCodeState
        ; let   fork_state_in = (initCgState us) { cgs_binds   = cgs_binds state
                                                 , cgs_hp_usg  = cgs_hp_usg state }
                ((), fork_state_out) = doFCode body_code cfg fstate fork_state_in
        ; setState $ state `addCodeBlocksFrom` fork_state_out }

forkAlts :: [FCode' c a] -> FCode' c [a]
-- (forkAlts' bs d) takes fcodes 'bs' for the branches of a 'case', and
-- an fcode for the default case 'd', and compiles each in the current
-- environment.  The current environment is passed on unmodified, except
-- that the virtual Hp is moved on to the worst virtual Hp for the branches

forkAlts branch_fcodes
  = do  { cfg   <- getConfig
        ; us    <- newUniqSupply
        ; state <- getState
        ; fstate <- getFCodeState
        ; let compile us branch
                = (us2, doFCode branch cfg fstate branch_state)
                where
                  (us1,us2) = splitUniqSupply us
                  branch_state = (initCgState us1) {
                                        cgs_binds  = cgs_binds state
                                      , cgs_hp_usg = cgs_hp_usg state }
              (_us, results) = mapAccumL compile us branch_fcodes
              (branch_results, branch_out_states) = unzip results
        ; setState $ foldl' stateIncUsage state branch_out_states
                -- NB foldl.  state is the *left* argument to stateIncUsage
        ; return branch_results }

forkAltPair :: FCode' c a -> FCode' c a -> FCode' c (a,a)
-- Most common use of 'forkAlts'; having this helper function avoids
-- accidental use of failible pattern-matches in @do@-notation
forkAltPair x y = do
  xy' <- forkAlts [x,y]
  case xy' of
    [x',y'] -> return (x',y')
    _ -> panic "forkAltPair"

-- collect the code emitted by an FCode computation
getCodeR :: FCode' c a -> FCode' c (a, CmmAGraph)
getCodeR fcode
  = do  { state1 <- getState
        ; (a, state2) <- withCgState fcode (state1 { cgs_stmts = mkNop })
        ; setState $ state2 { cgs_stmts = cgs_stmts state1  }
        ; return (a, cgs_stmts state2) }

getCode :: FCode' c a -> FCode' c CmmAGraph
getCode fcode = do { (_,stmts) <- getCodeR fcode; return stmts }

-- | Generate code into a fresh tick (sub-)scope and gather generated code
getCodeScoped :: ContainsEmitDebugInfo c => FCode' c a -> FCode' c (a, CmmAGraphScoped)
getCodeScoped fcode
  = do  { state1 <- getState
        ; ((a, tscope), state2) <-
            tickScope $
            flip withCgState state1 { cgs_stmts = mkNop } $
            do { a   <- fcode
               ; scp <- getTickScope
               ; return (a, scp) }
        ; setState $ state2 { cgs_stmts = cgs_stmts state1  }
        ; return (a, (cgs_stmts state2, tscope)) }


-- 'getHeapUsage' applies a function to the amount of heap that it uses.
-- It initialises the heap usage to zeros, and passes on an unchanged
-- heap usage.
--
-- It is usually a prelude to performing a GC check, so everything must
-- be in a tidy and consistent state.
--
-- Note the slightly subtle fixed point behaviour needed here

getHeapUsage :: (VirtualHpOffset -> FCode' c a) -> FCode' c a
getHeapUsage fcode
  = do  { cfg <- getConfig
        ; state <- getState
        ; fcstate <- getFCodeState
        ; let   fstate_in = state { cgs_hp_usg  = initHpUsage }
                (r, fstate_out) = doFCode (fcode hp_hw) cfg fcstate fstate_in
                hp_hw = heapHWM (cgs_hp_usg fstate_out)        -- Loop here!

        ; setState $ fstate_out { cgs_hp_usg = cgs_hp_usg state }
        ; return r }

-- ----------------------------------------------------------------------------
-- Combinators for emitting code

emitCgStmt :: CgStmt -> FCode' c ()
emitCgStmt stmt
  = do  { state <- getState
        ; setState $ state { cgs_stmts = cgs_stmts state `snocOL` stmt }
        }

emitLabel :: BlockId -> FCode' c ()
emitLabel id = do tscope <- getTickScope
                  emitCgStmt (CgLabel id tscope)

emitComment :: FastString -> FCode' c ()
emitComment s
  | debugIsOn = emitCgStmt (CgStmt (CmmComment s))
  | otherwise = return ()

emitTick :: CmmTickish -> FCode' c ()
emitTick = emitCgStmt . CgStmt . CmmTick

emitUnwind :: ContainsEmitDebugInfo c => [(GlobalReg, Maybe CmmExpr)] -> FCode' c ()
emitUnwind regs = do
  debug <- emitDebugInfo <$> getConfig
  when debug $
     emitCgStmt $ CgStmt $ CmmUnwind regs

emitAssign :: CmmReg  -> CmmExpr -> FCode' c ()
emitAssign l r = emitCgStmt (CgStmt (CmmAssign l r))

-- | Assumes natural alignment.
emitStore :: CmmExpr -> CmmExpr -> FCode' c ()
emitStore = emitStore' NaturallyAligned

emitStore' :: AlignmentSpec -> CmmExpr -> CmmExpr -> FCode' c ()
emitStore' alignment l r = emitCgStmt (CgStmt (CmmStore l r alignment))

emit :: CmmAGraph -> FCode' c ()
emit ag
  = do  { state <- getState
        ; setState $ state { cgs_stmts = cgs_stmts state CmmGraph.<*> ag } }

emitDecl :: CmmDecl -> FCode' c ()
emitDecl decl
  = do  { state <- getState
        ; setState $ state { cgs_tops = cgs_tops state `snocOL` decl } }

emitOutOfLine :: BlockId -> CmmAGraphScoped -> FCode' c ()
emitOutOfLine l (stmts, tscope) = emitCgStmt (CgFork l stmts tscope)

emitProcWithStackFrame
   :: ContainsPlatformProfile c
   => Convention                        -- entry convention
   -> Maybe CmmInfoTable                -- info table?
   -> CLabel                            -- label for the proc
   -> [CmmFormal]                       -- stack frame
   -> [CmmFormal]                       -- arguments
   -> CmmAGraphScoped                   -- code
   -> Bool                              -- do stack layout?
   -> FCode' c ()

emitProcWithStackFrame _conv mb_info lbl _stk_args [] blocks False
  = do  { platform <- getPlatform
        ; emitProc mb_info lbl [] blocks (widthInBytes (wordWidth platform)) False
        }
emitProcWithStackFrame conv mb_info lbl stk_args args (graph, tscope) True
        -- do layout
  = do  { profile <- getProfile
        ; let (offset, live, entry) = mkCallEntry profile conv args stk_args
              graph' = entry CmmGraph.<*> graph
        ; emitProc mb_info lbl live (graph', tscope) offset True
        }
emitProcWithStackFrame _ _ _ _ _ _ _ = panic "emitProcWithStackFrame"

emitProcWithConvention :: ContainsPlatformProfile c
                       => Convention -> Maybe CmmInfoTable -> CLabel
                       -> [CmmFormal]
                       -> CmmAGraphScoped
                       -> FCode' c ()
emitProcWithConvention conv mb_info lbl args blocks
  = emitProcWithStackFrame conv mb_info lbl [] args blocks True

emitProc :: Maybe CmmInfoTable -> CLabel -> [GlobalReg] -> CmmAGraphScoped
         -> Int -> Bool -> FCode' c ()
emitProc mb_info lbl live blocks offset do_layout
  = do  { l <- newBlockId
        ; let
              blks :: CmmGraph
              blks = labelAGraph l blocks

              infos | Just info <- mb_info = mapSingleton (g_entry blks) info
                    | otherwise            = mapEmpty

              sinfo = StackInfo { arg_space = offset
                                , do_layout = do_layout }

              tinfo = TopInfo { info_tbls = infos
                              , stack_info=sinfo}

              proc_block = CmmProc tinfo lbl live blks

        ; state <- getState
        ; setState $ state { cgs_tops = cgs_tops state `snocOL` proc_block } }

getCmm :: FCode' c a -> FCode' c (a, CmmGroup)
-- Get all the CmmTops (there should be no stmts)
-- Return a single Cmm which may be split from other Cmms by
-- object splitting (at a later stage)
getCmm code
  = do  { state1 <- getState
        ; (a, state2) <- withCgState code (state1 { cgs_tops  = nilOL })
        ; setState $ state2 { cgs_tops = cgs_tops state1 }
        ; return (a, fromOL (cgs_tops state2)) }


mkCmmIfThenElse :: CmmExpr -> CmmAGraph -> CmmAGraph -> FCode' c CmmAGraph
mkCmmIfThenElse e tbranch fbranch = mkCmmIfThenElse' e tbranch fbranch Nothing

mkCmmIfThenElse' :: CmmExpr -> CmmAGraph -> CmmAGraph
                 -> Maybe Bool -> FCode' c CmmAGraph
mkCmmIfThenElse' e tbranch fbranch likely = do
  tscp  <- getTickScope
  endif <- newBlockId
  tid   <- newBlockId
  fid   <- newBlockId

  let
    (test, then_, else_, likely') = case likely of
      Just False | Just e' <- maybeInvertCmmExpr e
        -- currently NCG doesn't know about likely
        -- annotations. We manually switch then and
        -- else branch so the likely false branch
        -- becomes a fallthrough.
        -> (e', fbranch, tbranch, Just True)
      _ -> (e, tbranch, fbranch, likely)

  return $ catAGraphs [ mkCbranch test tid fid likely'
                      , mkLabel tid tscp, then_, mkBranch endif
                      , mkLabel fid tscp, else_, mkLabel endif tscp ]

mkCmmIfGoto :: CmmExpr -> BlockId -> FCode' c CmmAGraph
mkCmmIfGoto e tid = mkCmmIfGoto' e tid Nothing

mkCmmIfGoto' :: CmmExpr -> BlockId -> Maybe Bool -> FCode' c CmmAGraph
mkCmmIfGoto' e tid l = do
  endif <- newBlockId
  tscp  <- getTickScope
  return $ catAGraphs [ mkCbranch e tid endif l, mkLabel endif tscp ]

mkCmmIfThen :: CmmExpr -> CmmAGraph -> FCode' c CmmAGraph
mkCmmIfThen e tbranch = mkCmmIfThen' e tbranch Nothing

mkCmmIfThen' :: CmmExpr -> CmmAGraph -> Maybe Bool -> FCode' c CmmAGraph
mkCmmIfThen' e tbranch l = do
  endif <- newBlockId
  tid   <- newBlockId
  tscp  <- getTickScope
  return $ catAGraphs [ mkCbranch e tid endif l
                      , mkLabel tid tscp, tbranch, mkLabel endif tscp ]

mkCall :: ContainsPlatformProfile c
       => CmmExpr -> (Convention, Convention) -> [CmmFormal] -> [CmmExpr]
       -> UpdFrameOffset -> [CmmExpr] -> FCode' c CmmAGraph
mkCall f (callConv, retConv) results actuals updfr_off extra_stack = do
  profile <- getProfile
  k       <- newBlockId
  tscp    <- getTickScope
  let area = Young k
      (off, _, copyin) = copyInOflow profile retConv area results []
      copyout = mkCallReturnsTo profile f callConv actuals k off updfr_off extra_stack
  return $ catAGraphs [copyout, mkLabel k tscp, copyin]

mkCmmCall :: ContainsPlatformProfile c
          => CmmExpr -> [CmmFormal] -> [CmmExpr] -> UpdFrameOffset
          -> FCode' c CmmAGraph
mkCmmCall f results actuals updfr_off
   = mkCall f (NativeDirectCall, NativeReturn) results actuals updfr_off []


-- ----------------------------------------------------------------------------
-- turn CmmAGraph into CmmGraph, for making a new proc.

aGraphToGraph :: CmmAGraphScoped -> FCode' c CmmGraph
aGraphToGraph stmts
  = do  { l <- newBlockId
        ; return (labelAGraph l stmts) }

-- ----------------------------------------------------------------------------

class ContainsEmitDebugInfo c where
  emitDebugInfo :: c -> Bool

instance ContainsEmitDebugInfo CmmBuilderConfig where
  emitDebugInfo = cmmBuilderEmitDebugInfo

instance ContainsEmitDebugInfo StgToCmmConfig where
  emitDebugInfo = stgToCmmEmitDebugInfo

class ContainsSDocContext c where
  sdocContext :: c -> SDocContext

instance ContainsSDocContext CmmBuilderConfig where
  sdocContext = cmmBuilderContext

instance ContainsSDocContext StgToCmmConfig where
  sdocContext = stgToCmmContext


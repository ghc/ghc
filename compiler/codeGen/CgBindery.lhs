%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgBindery]{Utility functions related to doing @CgBindings@}

\begin{code}

module CgBindery (
        CgBindings, CgIdInfo,
        StableLoc, VolatileLoc,

        cgIdInfoId, cgIdInfoArgRep, cgIdInfoLF,

        stableIdInfo, heapIdInfo,
        taggedStableIdInfo, taggedHeapIdInfo,
        letNoEscapeIdInfo, idInfoToAmode,

        addBindC, addBindsC,

        nukeVolatileBinds,
        nukeDeadBindings,
        getLiveStackSlots,
        getLiveStackBindings,

        bindArgsToStack,  rebindToStack,
        bindNewToNode, bindNewToUntagNode, bindNewToReg, bindArgsToRegs,
        bindNewToTemp,
        getArgAmode, getArgAmodes, 
        getCgIdInfo, 
        getCAddrModeIfVolatile, getVolatileRegs,
        maybeLetNoEscape, 
    ) where

import CgMonad
import CgHeapery
import CgStackery
import CgUtils
import CLabel
import ClosureInfo
import Constants

import OldCmm
import PprCmm           ( {- instance Outputable -} )
import SMRep
import Id
import DataCon
import VarEnv
import VarSet
import Literal
import Maybes
import Name
import StgSyn
import Unique
import UniqSet
import Outputable
import FastString

\end{code}

%************************************************************************
%*                                                                      *
\subsection[Bindery-datatypes]{Data types}
%*                                                                      *
%************************************************************************

@(CgBinding a b)@ is a type of finite maps from a to b.

The assumption used to be that @lookupCgBind@ must get exactly one
match. This is {\em completely wrong} in the case of compiling
letrecs (where knot-tying is used). An initial binding is fed in (and
never evaluated); eventually, a correct binding is put into the
environment. So there can be two bindings for a given name.

\begin{code}
type CgBindings = IdEnv CgIdInfo

data CgIdInfo
  = CgIdInfo    
        { cg_id :: Id   -- Id that this is the info for
                        -- Can differ from the Id at occurrence sites by 
                        -- virtue of being externalised, for splittable C
        , cg_rep :: CgRep
        , cg_vol :: VolatileLoc
        , cg_stb :: StableLoc
        , cg_lf  :: LambdaFormInfo 
        , cg_tag :: {-# UNPACK #-} !Int  -- tag to be added in idInfoToAmode
         }

mkCgIdInfo :: Id -> VolatileLoc -> StableLoc -> LambdaFormInfo -> CgIdInfo
mkCgIdInfo id vol stb lf
  = CgIdInfo { cg_id = id, cg_vol = vol, cg_stb = stb, 
               cg_lf = lf, cg_rep = idCgRep id, cg_tag = tag }
  where
    tag
      | Just con <- isDataConWorkId_maybe id,
          {- Is this an identifier for a static constructor closure? -}
        isNullaryRepDataCon con
          {- If yes, is this a nullary constructor?
             If yes, we assume that the constructor is evaluated and can
             be tagged.
           -}
      = tagForCon con

      | otherwise
      = funTagLFInfo lf

voidIdInfo :: Id -> CgIdInfo
voidIdInfo id = CgIdInfo { cg_id = id, cg_vol = NoVolatileLoc
                         , cg_stb = VoidLoc, cg_lf = mkLFArgument id
                         , cg_rep = VoidArg, cg_tag = 0 }
        -- Used just for VoidRep things

data VolatileLoc        -- These locations die across a call
  = NoVolatileLoc
  | RegLoc      CmmReg             -- In one of the registers (global or local)
  | VirHpLoc    VirtualHpOffset  -- Hp+offset (address of closure)
  | VirNodeLoc  ByteOff            -- Cts of offset indirect from Node
                                   -- ie *(Node+offset).
                                   -- NB. Byte offset, because we subtract R1's
                                   -- tag from the offset.

mkTaggedCgIdInfo :: Id -> VolatileLoc -> StableLoc -> LambdaFormInfo -> DataCon
                 -> CgIdInfo
mkTaggedCgIdInfo id vol stb lf con
  = CgIdInfo { cg_id = id, cg_vol = vol, cg_stb = stb, 
               cg_lf = lf, cg_rep = idCgRep id, cg_tag = tagForCon con }
\end{code}

@StableLoc@ encodes where an Id can be found, used by
the @CgBindings@ environment in @CgBindery@.

\begin{code}
data StableLoc
  = NoStableLoc

  | VirStkLoc   VirtualSpOffset         -- The thing is held in this
                                        -- stack slot

  | VirStkLNE   VirtualSpOffset         -- A let-no-escape thing; the
                                        -- value is this stack pointer
                                        -- (as opposed to the contents of the slot)

  | StableLoc   CmmExpr
  | VoidLoc     -- Used only for VoidRep variables.  They never need to
                -- be saved, so it makes sense to treat treat them as
                -- having a stable location

instance Outputable CgIdInfo where
  ppr (CgIdInfo id _ vol stb _ _)
    -- TODO, pretty pring the tag info
    = ppr id <+> ptext (sLit "-->") <+> vcat [ppr vol, ppr stb]

instance Outputable VolatileLoc where
  ppr NoVolatileLoc = empty
  ppr (RegLoc r)     = ptext (sLit "reg") <+> ppr r
  ppr (VirHpLoc v)   = ptext (sLit "vh")  <+> ppr v
  ppr (VirNodeLoc v) = ptext (sLit "vn")  <+> ppr v

instance Outputable StableLoc where
  ppr NoStableLoc   = empty
  ppr VoidLoc       = ptext (sLit "void")
  ppr (VirStkLoc v) = ptext (sLit "vs")    <+> ppr v
  ppr (VirStkLNE v) = ptext (sLit "lne")   <+> ppr v
  ppr (StableLoc a) = ptext (sLit "amode") <+> ppr a
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Bindery-idInfo]{Manipulating IdInfo}
%*                                                                      *
%************************************************************************

\begin{code}
stableIdInfo :: Id -> CmmExpr -> LambdaFormInfo -> CgIdInfo
stableIdInfo id amode   lf_info = mkCgIdInfo id NoVolatileLoc (StableLoc amode) lf_info

heapIdInfo :: Id -> VirtualHpOffset -> LambdaFormInfo -> CgIdInfo
heapIdInfo id offset    lf_info = mkCgIdInfo id (VirHpLoc offset) NoStableLoc lf_info

letNoEscapeIdInfo :: Id -> VirtualSpOffset -> LambdaFormInfo -> CgIdInfo
letNoEscapeIdInfo id sp lf_info = mkCgIdInfo id NoVolatileLoc (VirStkLNE sp) lf_info

stackIdInfo :: Id -> VirtualSpOffset -> LambdaFormInfo -> CgIdInfo
stackIdInfo id sp       lf_info = mkCgIdInfo id NoVolatileLoc (VirStkLoc sp) lf_info

nodeIdInfo :: Id -> Int -> LambdaFormInfo -> CgIdInfo
nodeIdInfo id offset    lf_info = mkCgIdInfo id (VirNodeLoc (wORD_SIZE*offset)) NoStableLoc lf_info

regIdInfo :: Id -> CmmReg -> LambdaFormInfo -> CgIdInfo
regIdInfo id reg        lf_info = mkCgIdInfo id (RegLoc reg) NoStableLoc lf_info

taggedStableIdInfo :: Id -> CmmExpr -> LambdaFormInfo -> DataCon -> CgIdInfo
taggedStableIdInfo id amode lf_info con
  = mkTaggedCgIdInfo id NoVolatileLoc (StableLoc amode) lf_info con

taggedHeapIdInfo :: Id -> VirtualHpOffset -> LambdaFormInfo -> DataCon
                 -> CgIdInfo
taggedHeapIdInfo id offset lf_info con
  = mkTaggedCgIdInfo id (VirHpLoc offset) NoStableLoc lf_info con

untagNodeIdInfo :: Id -> Int -> LambdaFormInfo -> Int -> CgIdInfo
untagNodeIdInfo id offset    lf_info tag
  = mkCgIdInfo id (VirNodeLoc (wORD_SIZE*offset - tag)) NoStableLoc lf_info


idInfoToAmode :: CgIdInfo -> FCode CmmExpr
idInfoToAmode info
  = case cg_vol info of {
      RegLoc reg        -> returnFC (CmmReg reg) ;
      VirNodeLoc nd_off -> returnFC (CmmLoad (cmmOffsetB (CmmReg nodeReg) nd_off)
                                             mach_rep) ;
      VirHpLoc hp_off   -> do { off <- getHpRelOffset hp_off
                              ; return $! maybeTag off };
      NoVolatileLoc -> 

    case cg_stb info of
      StableLoc amode  -> returnFC $! maybeTag amode
      VirStkLoc sp_off -> do { sp_rel <- getSpRelOffset sp_off
                             ; return (CmmLoad sp_rel mach_rep) }

      VirStkLNE sp_off -> getSpRelOffset sp_off

      VoidLoc -> return $ pprPanic "idInfoToAmode: void" (ppr (cg_id info))
                -- We return a 'bottom' amode, rather than panicing now
                -- In this way getArgAmode returns a pair of (VoidArg, bottom)
                -- and that's exactly what we want

      NoStableLoc -> pprPanic "idInfoToAmode: no loc" (ppr (cg_id info))
    }
  where
    mach_rep = argMachRep (cg_rep info)

    maybeTag amode  -- add the tag, if we have one
      | tag == 0   = amode
      | otherwise  = cmmOffsetB amode tag
      where tag = cg_tag info

cgIdInfoId :: CgIdInfo -> Id
cgIdInfoId = cg_id 

cgIdInfoLF :: CgIdInfo -> LambdaFormInfo
cgIdInfoLF = cg_lf

cgIdInfoArgRep :: CgIdInfo -> CgRep
cgIdInfoArgRep = cg_rep

maybeLetNoEscape :: CgIdInfo -> Maybe VirtualSpOffset
maybeLetNoEscape (CgIdInfo { cg_stb = VirStkLNE sp_off }) = Just sp_off
maybeLetNoEscape _                                        = Nothing
\end{code}

%************************************************************************
%*                                                                      *
\subsection[CgMonad-bindery]{Monad things for fiddling with @CgBindings@}
%*                                                                      *
%************************************************************************

There are three basic routines, for adding (@addBindC@), modifying
(@modifyBindC@) and looking up (@getCgIdInfo@) bindings.

A @Id@ is bound to a @(VolatileLoc, StableLoc)@ triple.
The name should not already be bound. (nice ASSERT, eh?)

\begin{code}
addBindC :: Id -> CgIdInfo -> Code
addBindC name stuff_to_bind = do
        binds <- getBinds
        setBinds $ extendVarEnv binds name stuff_to_bind

addBindsC :: [(Id, CgIdInfo)] -> Code
addBindsC new_bindings = do
        binds <- getBinds
        let new_binds = foldl (\ binds (name,info) -> extendVarEnv binds name info)
                              binds
                              new_bindings
        setBinds new_binds

modifyBindC :: Id -> (CgIdInfo -> CgIdInfo) -> Code
modifyBindC name mangle_fn = do
        binds <- getBinds
        setBinds $ modifyVarEnv mangle_fn binds name

getCgIdInfo :: Id -> FCode CgIdInfo
getCgIdInfo id
  = do  {       -- Try local bindings first
        ; local_binds  <- getBinds
        ; case lookupVarEnv local_binds id of {
            Just info -> return info ;
            Nothing   -> do

        {       -- Try top-level bindings
          static_binds <- getStaticBinds
        ; case lookupVarEnv static_binds id of {
            Just info -> return info ;
            Nothing   ->

                -- Should be imported; make up a CgIdInfo for it
        let 
            name = idName id
        in
        if isExternalName name then do
            let ext_lbl = CmmLit (CmmLabel (mkClosureLabel name $ idCafInfo id))
            return (stableIdInfo id ext_lbl (mkLFImported id))
        else
        if isVoidArg (idCgRep id) then
                -- Void things are never in the environment
            return (voidIdInfo id)
        else
        -- Bug  
        cgLookupPanic id
        }}}}
    
                        
cgLookupPanic :: Id -> FCode a
cgLookupPanic id
  = do  static_binds <- getStaticBinds
        local_binds <- getBinds
--      srt <- getSRTLabel
        pprPanic "cgLookupPanic (probably invalid Core; try -dcore-lint)"
                (vcat [ppr id,
                ptext (sLit "static binds for:"),
                vcat [ ppr (cg_id info) | info <- varEnvElts static_binds ],
                ptext (sLit "local binds for:"),
                vcat [ ppr (cg_id info) | info <- varEnvElts local_binds ]
--              ptext (sLit "SRT label") <+> pprCLabel srt
              ])
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Bindery-nuke-volatile]{Nuking volatile bindings}
%*                                                                      *
%************************************************************************

We sometimes want to nuke all the volatile bindings; we must be sure
we don't leave any (NoVolatile, NoStable) binds around...

\begin{code}
nukeVolatileBinds :: CgBindings -> CgBindings
nukeVolatileBinds binds
  = mkVarEnv (foldr keep_if_stable [] (varEnvElts binds))
  where
    keep_if_stable (CgIdInfo { cg_stb = NoStableLoc }) acc = acc
    keep_if_stable info acc
      = (cg_id info, info { cg_vol = NoVolatileLoc }) : acc
\end{code}


%************************************************************************
%*                                                                      *
\subsection[lookup-interface]{Interface functions to looking up bindings}
%*                                                                      *
%************************************************************************

\begin{code}
getCAddrModeIfVolatile :: Id -> FCode (Maybe CmmExpr)
getCAddrModeIfVolatile id
  = do  { info <- getCgIdInfo id
        ; case cg_stb info of
                NoStableLoc -> do -- Aha!  So it is volatile!
                        amode <- idInfoToAmode info
                        return $ Just amode
                _ -> return Nothing }
\end{code}

@getVolatileRegs@ gets a set of live variables, and returns a list of
all registers on which these variables depend. These are the regs
which must be saved and restored across any C calls. If a variable is
both in a volatile location (depending on a register) {\em and} a
stable one (notably, on the stack), we modify the current bindings to
forget the volatile one.

\begin{code}
getVolatileRegs :: StgLiveVars -> FCode [GlobalReg]
getVolatileRegs vars = do
  do    { stuff <- mapFCs snaffle_it (varSetElems vars)
        ; returnFC $ catMaybes stuff }
  where
    snaffle_it var = do
        { info <- getCgIdInfo var 
        ; let
                -- commoned-up code...
             consider_reg reg
                =       -- We assume that all regs can die across C calls
                        -- We leave it to the save-macros to decide which
                        -- regs *really* need to be saved.
                  case cg_stb info of
                        NoStableLoc     -> returnFC (Just reg) -- got one!
                        _ -> do
                                { -- has both volatile & stable locations;
                                  -- force it to rely on the stable location
                                  modifyBindC var nuke_vol_bind 
                                ; return Nothing }

        ; case cg_vol info of
            RegLoc (CmmGlobal reg) -> consider_reg reg
            VirNodeLoc _           -> consider_reg node
            _                      -> returnFC Nothing  -- Local registers
        }

    nuke_vol_bind info = info { cg_vol = NoVolatileLoc }

getArgAmode :: StgArg -> FCode (CgRep, CmmExpr)
getArgAmode (StgVarArg var) 
  = do  { info <- getCgIdInfo var
        ; amode <- idInfoToAmode info
        ; return (cgIdInfoArgRep info, amode ) }

getArgAmode (StgLitArg lit) 
  = do  { cmm_lit <- cgLit lit
        ; return (typeCgRep (literalType lit), CmmLit cmm_lit) }

getArgAmodes :: [StgArg] -> FCode [(CgRep, CmmExpr)]
getArgAmodes [] = returnFC []
getArgAmodes (atom:atoms)
  = do { amode  <- getArgAmode  atom 
       ; amodes <- getArgAmodes atoms
       ; return ( amode : amodes ) }
\end{code}

%************************************************************************
%*                                                                      *
\subsection[binding-and-rebinding-interface]{Interface functions for binding and re-binding names}
%*                                                                      *
%************************************************************************

\begin{code}
bindArgsToStack :: [(Id, VirtualSpOffset)] -> Code
bindArgsToStack args
  = mapCs bind args
  where
    bind(id, offset) = addBindC id (stackIdInfo id offset (mkLFArgument id))

bindArgsToRegs :: [(Id, GlobalReg)] -> Code
bindArgsToRegs args
  = mapCs bind args
  where
    bind (arg, reg) = bindNewToReg arg (CmmGlobal reg) (mkLFArgument arg)

bindNewToNode :: Id -> VirtualHpOffset -> LambdaFormInfo -> Code
bindNewToNode id offset lf_info
  = addBindC id (nodeIdInfo id offset lf_info)

bindNewToUntagNode :: Id -> VirtualHpOffset -> LambdaFormInfo -> Int -> Code
bindNewToUntagNode id offset lf_info tag
  = addBindC id (untagNodeIdInfo id offset lf_info tag)

-- Create a new temporary whose unique is that in the id,
-- bind the id to it, and return the addressing mode for the
-- temporary.
bindNewToTemp :: Id -> FCode LocalReg
bindNewToTemp id
  = do  addBindC id (regIdInfo id (CmmLocal temp_reg) lf_info)
        return temp_reg
  where
    uniq     = getUnique id
    temp_reg = LocalReg uniq (argMachRep (idCgRep id))
    lf_info  = mkLFArgument id  -- Always used of things we
                                -- know nothing about

bindNewToReg :: Id -> CmmReg -> LambdaFormInfo -> Code
bindNewToReg name reg lf_info
  = addBindC name info
  where
    info = mkCgIdInfo name (RegLoc reg) NoStableLoc lf_info

rebindToStack :: Id -> VirtualSpOffset -> Code
rebindToStack name offset
  = modifyBindC name replace_stable_fn
  where
    replace_stable_fn info = info { cg_stb = VirStkLoc offset }
\end{code}

%************************************************************************
%*                                                                      *
\subsection[CgMonad-deadslots]{Finding dead stack slots}
%*                                                                      *
%************************************************************************

nukeDeadBindings does the following:

      - Removes all bindings from the environment other than those
        for variables in the argument to nukeDeadBindings.
      - Collects any stack slots so freed, and returns them to the  stack free
        list.
      - Moves the virtual stack pointer to point to the topmost used
        stack locations.

You can have multi-word slots on the stack (where a Double# used to
be, for instance); if dead, such a slot will be reported as *several*
offsets (one per word).

Probably *naughty* to look inside monad...

\begin{code}
nukeDeadBindings :: StgLiveVars  -- All the *live* variables
                 -> Code
nukeDeadBindings live_vars = do
        binds <- getBinds
        let (dead_stk_slots, bs') =
                dead_slots live_vars 
                        [] []
                        [ (cg_id b, b) | b <- varEnvElts binds ]
        setBinds $ mkVarEnv bs'
        freeStackSlots dead_stk_slots
\end{code}

Several boring auxiliary functions to do the dirty work.

\begin{code}
dead_slots :: StgLiveVars
           -> [(Id,CgIdInfo)]
           -> [VirtualSpOffset]
           -> [(Id,CgIdInfo)]
           -> ([VirtualSpOffset], [(Id,CgIdInfo)])

-- dead_slots carries accumulating parameters for
--      filtered bindings, dead slots
dead_slots _ fbs ds []
  = (ds, reverse fbs) -- Finished; rm the dups, if any

dead_slots live_vars fbs ds ((v,i):bs)
  | v `elementOfUniqSet` live_vars
    = dead_slots live_vars ((v,i):fbs) ds bs
          -- Live, so don't record it in dead slots
          -- Instead keep it in the filtered bindings

  | otherwise
    = case cg_stb i of
        VirStkLoc offset
         | size > 0
         -> dead_slots live_vars fbs ([offset-size+1 .. offset] ++ ds) bs

        _ -> dead_slots live_vars fbs ds bs
  where
    size :: WordOff
    size = cgRepSizeW (cg_rep i)

getLiveStackSlots :: FCode [VirtualSpOffset]
-- Return the offsets of slots in stack containig live pointers
getLiveStackSlots 
  = do  { binds <- getBinds
        ; return [off | CgIdInfo { cg_stb = VirStkLoc off, 
                                   cg_rep = rep } <- varEnvElts binds, 
                        isFollowableArg rep] }

getLiveStackBindings :: FCode [(VirtualSpOffset, CgIdInfo)]
getLiveStackBindings
  = do { binds <- getBinds
       ; return [(off, bind) |
                 bind <- varEnvElts binds,
                 CgIdInfo { cg_stb = VirStkLoc off,
                            cg_rep = rep} <- [bind],
                 isFollowableArg rep] }
\end{code}


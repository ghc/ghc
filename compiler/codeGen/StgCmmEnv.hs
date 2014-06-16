-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: the binding environment
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------
module StgCmmEnv (
        CgIdInfo,

        litIdInfo, lneIdInfo, rhsIdInfo, mkRhsInit,
        idInfoToAmode,

        NonVoid(..), unsafe_stripNV, nonVoidIds,

        addBindC, addBindsC,

        bindArgsToRegs, bindToReg, rebindToReg,
        bindArgToReg, idToReg,
        getArgAmode, getNonVoidArgAmodes,
        getCgIdInfo,
        maybeLetNoEscape,
    ) where

#include "HsVersions.h"

import TyCon
import StgCmmMonad
import StgCmmUtils
import StgCmmClosure

import CLabel

import DynFlags
import MkGraph
import BlockId
import CmmExpr
import CmmUtils
import FastString
import Id
import VarEnv
import Control.Monad
import Name
import StgSyn
import Outputable

-------------------------------------
--        Non-void types
-------------------------------------
-- We frequently need the invariant that an Id or a an argument
-- is of a non-void type. This type is a witness to the invariant.

newtype NonVoid a = NonVoid a
  deriving (Eq, Show)

-- Use with care; if used inappropriately, it could break invariants.
unsafe_stripNV :: NonVoid a -> a
unsafe_stripNV (NonVoid a) = a

instance (Outputable a) => Outputable (NonVoid a) where
  ppr (NonVoid a) = ppr a

nonVoidIds :: [Id] -> [NonVoid Id]
nonVoidIds ids = [NonVoid id | id <- ids, not (isVoidRep (idPrimRep id))]

-------------------------------------
--        Manipulating CgIdInfo
-------------------------------------

mkCgIdInfo :: Id -> LambdaFormInfo -> CmmExpr -> CgIdInfo
mkCgIdInfo id lf expr
  = CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = CmmLoc expr }

litIdInfo :: DynFlags -> Id -> LambdaFormInfo -> CmmLit -> CgIdInfo
litIdInfo dflags id lf lit
  = CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = CmmLoc (addDynTag dflags (CmmLit lit) tag) }
  where
    tag = lfDynTag dflags lf

lneIdInfo :: DynFlags -> Id -> [NonVoid Id] -> CgIdInfo
lneIdInfo dflags id regs
  = CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = LneLoc blk_id (map (idToReg dflags) regs) }
  where
    lf     = mkLFLetNoEscape
    blk_id = mkBlockId (idUnique id)


rhsIdInfo :: Id -> LambdaFormInfo -> FCode (CgIdInfo, LocalReg)
rhsIdInfo id lf_info
  = do dflags <- getDynFlags
       reg <- newTemp (gcWord dflags)
       return (mkCgIdInfo id lf_info (CmmReg (CmmLocal reg)), reg)

mkRhsInit :: DynFlags -> LocalReg -> LambdaFormInfo -> CmmExpr -> CmmAGraph
mkRhsInit dflags reg lf_info expr
  = mkAssign (CmmLocal reg) (addDynTag dflags expr (lfDynTag dflags lf_info))

idInfoToAmode :: CgIdInfo -> CmmExpr
-- Returns a CmmExpr for the *tagged* pointer
idInfoToAmode (CgIdInfo { cg_loc = CmmLoc e }) = e
idInfoToAmode cg_info
  = pprPanic "idInfoToAmode" (ppr (cg_id cg_info))        -- LneLoc

addDynTag :: DynFlags -> CmmExpr -> DynTag -> CmmExpr
-- A tag adds a byte offset to the pointer
addDynTag dflags expr tag = cmmOffsetB dflags expr tag

maybeLetNoEscape :: CgIdInfo -> Maybe (BlockId, [LocalReg])
maybeLetNoEscape (CgIdInfo { cg_loc = LneLoc blk_id args}) = Just (blk_id, args)
maybeLetNoEscape _other                                      = Nothing



---------------------------------------------------------
--        The binding environment
--
-- There are three basic routines, for adding (addBindC),
-- modifying(modifyBindC) and looking up (getCgIdInfo) bindings.
---------------------------------------------------------

addBindC :: CgIdInfo -> FCode ()
addBindC stuff_to_bind = do
        binds <- getBinds
        setBinds $ extendVarEnv binds (cg_id stuff_to_bind) stuff_to_bind

addBindsC :: [CgIdInfo] -> FCode ()
addBindsC new_bindings = do
        binds <- getBinds
        let new_binds = foldl (\ binds info -> extendVarEnv binds (cg_id info) info)
                              binds
                              new_bindings
        setBinds new_binds

getCgIdInfo :: Id -> FCode CgIdInfo
getCgIdInfo id
  = do  { dflags <- getDynFlags
        ; local_binds <- getBinds -- Try local bindings first
        ; case lookupVarEnv local_binds id of {
            Just info -> return info ;
            Nothing   -> do {

                -- Should be imported; make up a CgIdInfo for it
          let name = idName id
        ; if isExternalName name then
              let ext_lbl = CmmLabel (mkClosureLabel name $ idCafInfo id)
              in return (litIdInfo dflags id (mkLFImported id) ext_lbl)
          else
              cgLookupPanic id -- Bug
        }}}

cgLookupPanic :: Id -> FCode a
cgLookupPanic id
  = do  local_binds <- getBinds
        pprPanic "StgCmmEnv: variable not found"
                (vcat [ppr id,
                ptext (sLit "local binds for:"),
                vcat [ ppr (cg_id info) | info <- varEnvElts local_binds ]
              ])


--------------------
getArgAmode :: NonVoid StgArg -> FCode CmmExpr
getArgAmode (NonVoid (StgVarArg var))  =
  do { info  <- getCgIdInfo var; return (idInfoToAmode info) }
getArgAmode (NonVoid (StgLitArg lit))  = liftM CmmLit $ cgLit lit

getNonVoidArgAmodes :: [StgArg] -> FCode [CmmExpr]
-- NB: Filters out void args,
--     so the result list may be shorter than the argument list
getNonVoidArgAmodes [] = return []
getNonVoidArgAmodes (arg:args)
  | isVoidRep (argPrimRep arg) = getNonVoidArgAmodes args
  | otherwise = do { amode  <- getArgAmode (NonVoid arg)
                    ; amodes <- getNonVoidArgAmodes args
                    ; return ( amode : amodes ) }

------------------------------------------------------------------------
--        Interface functions for binding and re-binding names
------------------------------------------------------------------------

bindToReg :: NonVoid Id -> LambdaFormInfo -> FCode LocalReg
-- Bind an Id to a fresh LocalReg
bindToReg nvid@(NonVoid id) lf_info
  = do dflags <- getDynFlags
       let reg = idToReg dflags nvid
       addBindC (mkCgIdInfo id lf_info (CmmReg (CmmLocal reg)))
       return reg

rebindToReg :: NonVoid Id -> FCode LocalReg
-- Like bindToReg, but the Id is already in scope, so
-- get its LF info from the envt
rebindToReg nvid@(NonVoid id)
  = do  { info <- getCgIdInfo id
        ; bindToReg nvid (cg_lf info) }

bindArgToReg :: NonVoid Id -> FCode LocalReg
bindArgToReg nvid@(NonVoid id) = bindToReg nvid (mkLFArgument id)

bindArgsToRegs :: [NonVoid Id] -> FCode [LocalReg]
bindArgsToRegs args = mapM bindArgToReg args

idToReg :: DynFlags -> NonVoid Id -> LocalReg
-- Make a register from an Id, typically a function argument,
-- free variable, or case binder
--
-- We re-use the Unique from the Id to make it easier to see what is going on
--
-- By now the Ids should be uniquely named; else one would worry
-- about accidental collision
idToReg dflags (NonVoid id)
             = LocalReg (idUnique id)
                        (case idPrimRep id of VoidRep -> pprPanic "idToReg" (ppr id)
                                              _ -> primRepCmmType dflags (idPrimRep id))



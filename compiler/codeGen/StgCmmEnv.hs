-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: the binding environment
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module StgCmmEnv (
	CgIdInfo,

	cgIdInfoId, cgIdInfoLF,

	litIdInfo, lneIdInfo, regIdInfo,
	idInfoToAmode,

        NonVoid(..), isVoidId, nonVoidIds,

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

import BlockId
import CmmExpr
import CmmUtils
import MkGraph (CmmAGraph, mkAssign)
import FastString
import Id
import VarEnv
import Control.Monad
import Name
import StgSyn
import Outputable

-------------------------------------
--	Non-void types
-------------------------------------
-- We frequently need the invariant that an Id or a an argument
-- is of a non-void type. This type is a witness to the invariant.

newtype NonVoid a = NonVoid a
  deriving (Eq, Show)

instance (Outputable a) => Outputable (NonVoid a) where
  ppr (NonVoid a) = ppr a

isVoidId :: Id -> Bool
isVoidId = isVoidRep . idPrimRep

nonVoidIds :: [Id] -> [NonVoid Id]
nonVoidIds ids = [NonVoid id | id <- ids, not (isVoidRep (idPrimRep id))]

-------------------------------------
--	Manipulating CgIdInfo
-------------------------------------

mkCgIdInfo :: Id -> LambdaFormInfo -> CmmExpr -> CgIdInfo
mkCgIdInfo id lf expr
  = CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = CmmLoc expr, 
	       cg_tag = lfDynTag lf }

litIdInfo :: Id -> LambdaFormInfo -> CmmLit -> CgIdInfo
litIdInfo id lf lit
  = CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = CmmLoc (addDynTag (CmmLit lit) tag) 
	     , cg_tag = tag }
  where
    tag = lfDynTag lf

lneIdInfo :: Id -> [LocalReg] -> CgIdInfo
lneIdInfo id regs 
  = CgIdInfo { cg_id = id, cg_lf = lf
             , cg_loc = LneLoc blk_id regs
	     , cg_tag = lfDynTag lf }
  where
    lf     = mkLFLetNoEscape
    blk_id = mkBlockId (idUnique id)

-- Because the register may be spilled to the stack in untagged form, we
-- modify the initialization code 'init' to immediately tag the
-- register, and store a plain register in the CgIdInfo.  We allocate
-- a new register in order to keep single-assignment and help out the
-- inliner. -- EZY
regIdInfo :: Id -> LambdaFormInfo -> CmmExpr -> FCode (CgIdInfo, CmmAGraph)
regIdInfo id lf_info expr
  = do { reg <- newTemp (cmmExprType expr)
       ; let init = mkAssign (CmmLocal reg)
                             (addDynTag expr (lfDynTag lf_info))
       ; return (mkCgIdInfo id lf_info (CmmReg (CmmLocal reg)), init) }

idInfoToAmode :: CgIdInfo -> CmmExpr
-- Returns a CmmExpr for the *tagged* pointer
idInfoToAmode (CgIdInfo { cg_loc = CmmLoc e }) = e
idInfoToAmode cg_info
  = pprPanic "idInfoToAmode" (ppr (cg_id cg_info))	-- LneLoc

addDynTag :: CmmExpr -> DynTag -> CmmExpr
-- A tag adds a byte offset to the pointer
addDynTag expr tag = cmmOffsetB expr tag

cgIdInfoId :: CgIdInfo -> Id
cgIdInfoId = cg_id 

cgIdInfoLF :: CgIdInfo -> LambdaFormInfo
cgIdInfoLF = cg_lf

maybeLetNoEscape :: CgIdInfo -> Maybe (BlockId, [LocalReg])
maybeLetNoEscape (CgIdInfo { cg_loc = LneLoc blk_id args}) = Just (blk_id, args)
maybeLetNoEscape _other   				   = Nothing



---------------------------------------------------------
--	The binding environment
-- 
-- There are three basic routines, for adding (addBindC), 
-- modifying(modifyBindC) and looking up (getCgIdInfo) bindings.
---------------------------------------------------------

addBindC :: Id -> CgIdInfo -> FCode ()
addBindC name stuff_to_bind = do
	binds <- getBinds
	setBinds $ extendVarEnv binds name stuff_to_bind

addBindsC :: [CgIdInfo] -> FCode ()
addBindsC new_bindings = do
	binds <- getBinds
	let new_binds = foldl (\ binds info -> extendVarEnv binds (cg_id info) info)
			      binds
			      new_bindings
	setBinds new_binds

getCgIdInfo :: Id -> FCode CgIdInfo
getCgIdInfo id
  = do	{ 	-- Try local bindings first
	; local_binds  <- getBinds
	; case lookupVarEnv local_binds id of {
	    Just info -> return info ;
	    Nothing   -> do

	{ 	-- Try top-level bindings
	  static_binds <- getStaticBinds
	; case lookupVarEnv static_binds id of {
	    Just info -> return info ;
	    Nothing   ->

		-- Should be imported; make up a CgIdInfo for it
	let 
	    name = idName id
	in
	if isExternalName name then do
	    let ext_lbl = CmmLabel (mkClosureLabel name $ idCafInfo id)
	    return (litIdInfo id (mkLFImported id) ext_lbl)
	else
	-- Bug	
	cgLookupPanic id
	}}}}
    
cgLookupPanic :: Id -> FCode a
cgLookupPanic id
  = do	static_binds <- getStaticBinds
	local_binds <- getBinds
	srt <- getSRTLabel
	pprPanic "StgCmmEnv: variable not found"
		(vcat [ppr id,
		ptext (sLit "static binds for:"),
		vcat [ ppr (cg_id info) | info <- varEnvElts static_binds ],
		ptext (sLit "local binds for:"),
		vcat [ ppr (cg_id info) | info <- varEnvElts local_binds ],
	        ptext (sLit "SRT label") <+> ppr srt
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
--	Interface functions for binding and re-binding names
------------------------------------------------------------------------

bindToReg :: NonVoid Id -> LambdaFormInfo -> FCode LocalReg
-- Bind an Id to a fresh LocalReg
bindToReg nvid@(NonVoid id) lf_info
  = do	{ let reg = idToReg nvid
	; addBindC id (mkCgIdInfo id lf_info (CmmReg (CmmLocal reg)))
	; return reg }

rebindToReg :: NonVoid Id -> FCode LocalReg
-- Like bindToReg, but the Id is already in scope, so 
-- get its LF info from the envt
rebindToReg nvid@(NonVoid id)
  = do	{ info <- getCgIdInfo id
	; bindToReg nvid (cgIdInfoLF info) }

bindArgToReg :: NonVoid Id -> FCode LocalReg
bindArgToReg nvid@(NonVoid id) = bindToReg nvid (mkLFArgument id)

bindArgsToRegs :: [NonVoid Id] -> FCode [LocalReg]
bindArgsToRegs args = mapM bindArgToReg args

idToReg :: NonVoid Id -> LocalReg
-- Make a register from an Id, typically a function argument,
-- free variable, or case binder
--
-- We re-use the Unique from the Id to make it easier to see what is going on
--
-- By now the Ids should be uniquely named; else one would worry
-- about accidental collision 
idToReg (NonVoid id) = LocalReg (idUnique id) 
                        (case idPrimRep id of VoidRep -> pprPanic "idToReg" (ppr id)
                                              _ -> primRepCmmType (idPrimRep id))



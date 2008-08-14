-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: the binding environment
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmEnv (
	CgIdInfo,

	cgIdInfoId, cgIdInfoLF,

	litIdInfo, lneIdInfo, regIdInfo,
	idInfoToAmode,

	addBindC, addBindsC,

	bindArgsToRegs, bindToReg, rebindToReg,
	bindArgToReg, idToReg,
	getArgAmode, getNonVoidArgAmodes, 
	getCgIdInfo, 
	maybeLetNoEscape, 
    ) where

#include "HsVersions.h"

import StgCmmMonad
import StgCmmUtils
import StgCmmClosure

import CLabel

import BlockId
import Cmm
import CmmUtils
import FastString
import PprCmm		( {- instance Outputable -} )
import Id
import VarEnv
import Maybes
import Name
import StgSyn
import Outputable



-------------------------------------
--	Manipulating CgIdInfo
-------------------------------------

mkCgIdInfo :: Id -> LambdaFormInfo -> CmmExpr -> CgIdInfo
mkCgIdInfo id lf expr
  = CgIdInfo { cg_id = id, cg_loc = CmmLoc expr, 
	       cg_lf = lf, cg_rep = idPrimRep id, 
	       cg_tag = lfDynTag lf }

lneIdInfo :: Id -> [LocalReg] -> CgIdInfo
lneIdInfo id regs 
  = CgIdInfo { cg_id = id, cg_loc = LneLoc blk_id regs,
	       cg_lf = lf, cg_rep = idPrimRep id, 
	       cg_tag = lfDynTag lf }
  where
    lf     = mkLFLetNoEscape
    blk_id = mkBlockId (idUnique id)

litIdInfo :: Id -> LambdaFormInfo -> CmmLit -> CgIdInfo
litIdInfo id lf_info lit = mkCgIdInfo id lf_info (CmmLit lit)

regIdInfo :: Id -> LambdaFormInfo -> LocalReg -> CgIdInfo
regIdInfo id lf_info reg = mkCgIdInfo id lf_info (CmmReg (CmmLocal reg))

idInfoToAmode :: CgIdInfo -> CmmExpr
-- Returns a CmmExpr for the *tagged* pointer
idInfoToAmode (CgIdInfo { cg_loc = CmmLoc e, cg_tag = tag })
  = addDynTag e tag
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

addBindsC :: [(Id, CgIdInfo)] -> FCode ()
addBindsC new_bindings = do
	binds <- getBinds
	let new_binds = foldl (\ binds (name,info) -> extendVarEnv binds name info)
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
	        ptext (sLit "SRT label") <+> pprCLabel srt
	      ])


--------------------
getArgAmode :: StgArg -> FCode CmmExpr
getArgAmode (StgVarArg var)  = do { info  <- getCgIdInfo var; return (idInfoToAmode info) }
getArgAmode (StgLitArg lit)  = return (CmmLit (mkSimpleLit lit))
getArgAmode (StgTypeArg _)   = panic "getArgAmode: type arg"

getNonVoidArgAmodes :: [StgArg] -> FCode [CmmExpr]
-- NB: Filters out void args, 
--     so the result list may be shorter than the argument list
getNonVoidArgAmodes [] = return []
getNonVoidArgAmodes (arg:args)
  | isVoidRep (argPrimRep arg) = getNonVoidArgAmodes args
  | otherwise = do { amode  <- getArgAmode  arg 
	 	   ; amodes <- getNonVoidArgAmodes args
	 	   ; return ( amode : amodes ) }


------------------------------------------------------------------------
--	Interface functions for binding and re-binding names
------------------------------------------------------------------------

bindToReg :: Id -> LambdaFormInfo -> FCode LocalReg
-- Bind an Id to a fresh LocalReg
bindToReg id lf_info
  = do	{ let reg = idToReg id
	; addBindC id (regIdInfo id lf_info reg)
	; return reg }

rebindToReg :: Id -> FCode LocalReg
-- Like bindToReg, but the Id is already in scope, so 
-- get its LF info from the envt
rebindToReg id 
  = do	{ info <- getCgIdInfo id
	; bindToReg id (cgIdInfoLF info) }

bindArgToReg :: Id -> FCode LocalReg
bindArgToReg id = bindToReg id (mkLFArgument id)

bindArgsToRegs :: [Id] -> FCode [LocalReg]
bindArgsToRegs args = mapM bindArgToReg args

idToReg :: Id -> LocalReg
-- Make a register from an Id, typically a function argument,
-- free variable, or case binder
--
-- We re-use the Unique from the Id to make it easier to see what is going on
--
-- By now the Ids should be uniquely named; else one would worry
-- about accidental collision 
idToReg id = LocalReg (idUnique id) 
		      (primRepCmmType (idPrimRep id))



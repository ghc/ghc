%
% (c) The GRASP/AQUA Project, Glasgow University, 1999
%
\section[ParseUtil]{Parser Utilities}

\begin{code}
module ParseUtil (
	  parseError	      -- String -> Pa
	, mkVanillaCon, mkRecCon,

	, mkRecConstrOrUpdate -- HsExp -> [HsFieldUpdate] -> P HsExp
	, groupBindings
	
	, CallConv(..)
	, mkImport            -- CallConv -> Safety 
			      -- -> (FAST_STRING, RdrName, RdrNameHsType)
			      -- -> SrcLoc 
			      -- -> P RdrNameHsDecl
	, mkExport            -- CallConv
			      -- -> (FAST_STRING, RdrName, RdrNameHsType)
			      -- -> SrcLoc 
			      -- -> P RdrNameHsDecl
	, mkExtName           -- RdrName -> CLabelString
			      
	, checkPrec 	      -- String -> P String
	, checkContext	      -- HsType -> P HsContext
	, checkInstType	      -- HsType -> P HsType
	, checkDataHeader     -- HsQualType -> P (HsContext,HsName,[HsName])
	, checkPattern	      -- HsExp -> P HsPat
	, checkPatterns	      -- SrcLoc -> [HsExp] -> P [HsPat]
	, checkDo	      -- [Stmt] -> P [Stmt]
	, checkValDef	      -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
	, checkValSig	      -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
 ) where

#include "HsVersions.h"

import List		( isSuffixOf )

import Lex
import HsSyn		-- Lots of it
import ForeignCall	( CCallConv, Safety, CCallTarget(..), CExportSpec(..),
			  DNCallSpec(..))
import SrcLoc
import RdrHsSyn		( RdrBinding(..),
			  RdrNameHsType, RdrNameBangType, RdrNameContext,
			  RdrNameHsTyVar, RdrNamePat, RdrNameHsExpr,
			  RdrNameGRHSs, RdrNameHsRecordBinds,
			  RdrNameMonoBinds, RdrNameConDetails, RdrNameHsDecl,
			  mkNPlusKPat
			)
import RdrName
import PrelNames	( unitTyCon_RDR )
import OccName  	( dataName, varName, tcClsName,
			  occNameSpace, setOccNameSpace, occNameUserString )
import CStrings		( CLabelString )
import FastString	( nullFastString )
import Outputable

-----------------------------------------------------------------------------
-- Misc utils

parseError :: String -> P a
parseError s = 
  getSrcLocP `thenP` \ loc ->
  failMsgP (hcat [ppr loc, text ": ", text s])


-----------------------------------------------------------------------------
-- mkVanillaCon

-- When parsing data declarations, we sometimes inadvertently parse
-- a constructor application as a type (eg. in data T a b = C a b `D` E a b)
-- This function splits up the type application, adds any pending
-- arguments, and converts the type constructor back into a data constructor.

mkVanillaCon :: RdrNameHsType -> [RdrNameBangType] -> P (RdrName, RdrNameConDetails)

mkVanillaCon ty tys
 = split ty tys
 where
   split (HsAppTy t u)  ts = split t (unbangedType u : ts)
   split (HsTyVar tc)   ts = tyConToDataCon tc	`thenP` \ data_con ->
			     returnP (data_con, VanillaCon ts)
   split _		 _ = parseError "Illegal data/newtype declaration"

mkRecCon :: RdrName -> [([RdrName],RdrNameBangType)] -> P (RdrName, RdrNameConDetails)
mkRecCon con fields
  = tyConToDataCon con	`thenP` \ data_con ->
    returnP (data_con, RecCon fields)

tyConToDataCon :: RdrName -> P RdrName
tyConToDataCon tc
  | occNameSpace tc_occ == tcClsName
  = returnP (setRdrNameOcc tc (setOccNameSpace tc_occ dataName))
  | otherwise
  = parseError (showSDoc (text "Not a constructor:" <+> quotes (ppr tc)))
  where 
    tc_occ   = rdrNameOcc tc


----------------------------------------------------------------------------
-- Various Syntactic Checks

checkInstType :: RdrNameHsType -> P RdrNameHsType
checkInstType t 
  = case t of
	HsForAllTy tvs ctxt ty ->
		checkDictTy ty [] `thenP` \ dict_ty ->
	      	returnP (HsForAllTy tvs ctxt dict_ty)

	ty ->   checkDictTy ty [] `thenP` \ dict_ty->
	      	returnP (HsForAllTy Nothing [] dict_ty)

checkContext :: RdrNameHsType -> P RdrNameContext
checkContext (HsTupleTy _ ts) 	-- (Eq a, Ord b) shows up as a tuple type
  = mapP (\t -> checkPred t []) ts `thenP` \ps ->
    returnP ps

checkContext (HsTyVar t)	-- Empty context shows up as a unit type ()
  | t == unitTyCon_RDR = returnP []

checkContext t 
  = checkPred t [] `thenP` \p ->
    returnP [p]

checkPred :: RdrNameHsType -> [RdrNameHsType] -> P (HsPred RdrName)
checkPred (HsTyVar t) args | not (isRdrTyVar t) 
  	= returnP (HsClassP t args)
checkPred (HsAppTy l r) args = checkPred l (r:args)
checkPred (HsPredTy (HsIParam n ty)) [] = returnP (HsIParam n ty)
checkPred _ _ = parseError "Illegal class assertion"

checkDictTy :: RdrNameHsType -> [RdrNameHsType] -> P RdrNameHsType
checkDictTy (HsTyVar t) args@(_:_) | not (isRdrTyVar t) 
  	= returnP (mkHsDictTy t args)
checkDictTy (HsAppTy l r) args = checkDictTy l (r:args)
checkDictTy _ _ = parseError "Malformed context in instance header"

-- Put more comments!
-- Checks that the lhs of a datatype declaration
-- is of the form Context => T a b ... z
checkDataHeader :: String	-- data/newtype/class
		-> RdrNameHsType 
		-> P (RdrNameContext, RdrName, [RdrNameHsTyVar])

checkDataHeader s (HsForAllTy Nothing cs t) =
   checkSimple s t []	     `thenP` \(c,ts) ->
   returnP (cs,c,map UserTyVar ts)
checkDataHeader s t =
   checkSimple s t []	     `thenP` \(c,ts) ->
   returnP ([],c,map UserTyVar ts)

-- Checks the type part of the lhs of 
-- a data/newtype/class declaration
checkSimple :: String -> RdrNameHsType -> [RdrName] -> P ((RdrName,[RdrName]))
checkSimple s (HsAppTy l (HsTyVar a)) xs | isRdrTyVar a 
   = checkSimple s l (a:xs)
checkSimple s (HsTyVar tycon) xs | not (isRdrTyVar tycon) = returnP (tycon,xs)

checkSimple s (HsOpTy (HsTyVar t1) tycon (HsTyVar t2)) [] 
  | not (isRdrTyVar tycon) && isRdrTyVar t1 && isRdrTyVar t2
  = returnP (tycon,[t1,t2])

checkSimple s t _ = parseError ("Malformed " ++ s ++ " declaration")

---------------------------------------------------------------------------
-- Checking statements in a do-expression
-- 	We parse   do { e1 ; e2 ; }
-- 	as [ExprStmt e1, ExprStmt e2]
-- checkDo (a) checks that the last thing is an ExprStmt
--	   (b) transforms it to a ResultStmt

checkDo []	         = parseError "Empty 'do' construct"
checkDo [ExprStmt e _ l] = returnP [ResultStmt e l]
checkDo [s] 	         = parseError "The last statement in a 'do' construct must be an expression"
checkDo (s:ss)	         = checkDo ss	`thenP` \ ss' ->
			   returnP (s:ss')

---------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: SrcLoc -> RdrNameHsExpr -> P RdrNamePat
checkPattern loc e = setSrcLocP loc (checkPat e [])

checkPatterns :: SrcLoc -> [RdrNameHsExpr] -> P [RdrNamePat]
checkPatterns loc es = mapP (checkPattern loc) es

checkPat :: RdrNameHsExpr -> [RdrNamePat] -> P RdrNamePat
checkPat (HsVar c) args | isRdrDataCon c = returnP (ConPatIn c args)
checkPat (HsApp f x) args = 
	checkPat x [] `thenP` \x ->
	checkPat f (x:args)
checkPat e [] = case e of
	EWildPat	   -> returnP WildPatIn
	HsVar x		   -> returnP (VarPatIn x)
	HsLit l 	   -> returnP (LitPatIn l)
	HsOverLit l	   -> returnP (NPatIn l)
	ELazyPat e	   -> checkPat e [] `thenP` (returnP . LazyPatIn)
	EAsPat n e	   -> checkPat e [] `thenP` (returnP . AsPatIn n)
        ExprWithTySig e t  -> checkPat e [] `thenP` \e ->
			      -- Pattern signatures are parsed as sigtypes,
			      -- but they aren't explicit forall points.  Hence
			      -- we have to remove the implicit forall here.
			      let t' = case t of 
					  HsForAllTy Nothing [] ty -> ty
					  other -> other
			      in
			      returnP (SigPatIn e t')

	OpApp (HsVar n) (HsVar plus) _ (HsOverLit lit@(HsIntegral _ _)) 
		  	   | plus == plus_RDR
			   -> returnP (mkNPlusKPat n lit)
			   where
			      plus_RDR = mkUnqual varName SLIT("+")	-- Hack

	OpApp l op fix r   -> checkPat l [] `thenP` \l ->
			      checkPat r [] `thenP` \r ->
			      case op of
			   	 HsVar c -> returnP (ConOpPatIn l c fix r)
			   	 _ -> patFail

	HsPar e		   -> checkPat e [] `thenP` (returnP . ParPatIn)
	ExplicitList _ es  -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (ListPatIn ps)
	ExplicitPArr _ es  -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (PArrPatIn ps)

	ExplicitTuple es b -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (TuplePatIn ps b)

	RecordCon c fs     -> mapP checkPatField fs `thenP` \fs ->
			      returnP (RecPatIn c fs)
-- Generics 
	HsType ty          -> returnP (TypePatIn ty) 
	_ -> patFail

checkPat _ _ = patFail

checkPatField :: (RdrName, RdrNameHsExpr, Bool) 
	-> P (RdrName, RdrNamePat, Bool)
checkPatField (n,e,b) =
	checkPat e [] `thenP` \p ->
	returnP (n,p,b)

patFail = parseError "Parse error in pattern"


---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef 
	:: RdrNameHsExpr
	-> Maybe RdrNameHsType
	-> RdrNameGRHSs
	-> SrcLoc
	-> P RdrBinding

checkValDef lhs opt_sig grhss loc
 = case isFunLhs lhs [] of
	   Just (f,inf,es) -> 
		checkPatterns loc es `thenP` \ps ->
		returnP (RdrValBinding (FunMonoBind f inf [Match ps opt_sig grhss] loc))

           Nothing ->
		checkPattern loc lhs `thenP` \lhs ->
		returnP (RdrValBinding (PatMonoBind lhs grhss loc))

checkValSig
	:: RdrNameHsExpr
	-> RdrNameHsType
	-> SrcLoc
	-> P RdrBinding
checkValSig (HsVar v) ty loc = returnP (RdrSig (Sig v ty loc))
checkValSig other     ty loc = parseError "Type signature given for an expression"


-- A variable binding is parsed as an RdrNameFunMonoBind.
-- See comments with HsBinds.MonoBinds

isFunLhs :: RdrNameHsExpr -> [RdrNameHsExpr] -> Maybe (RdrName, Bool, [RdrNameHsExpr])
isFunLhs (OpApp l (HsVar op) fix r) es  | not (isRdrDataCon op)
			  	= Just (op, True, (l:r:es))
					| otherwise
				= case isFunLhs l es of
				    Just (op', True, j : k : es') ->
				      Just (op', True, j : OpApp k (HsVar op) fix r : es')
				    _ -> Nothing
isFunLhs (HsVar f) es | not (isRdrDataCon f)
			 	= Just (f,False,es)
isFunLhs (HsApp f e) es 	= isFunLhs f (e:es)
isFunLhs (HsPar e)   es@(_:_) 	= isFunLhs e es
isFunLhs _ _ 			= Nothing

---------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P ()
checkPrec i | 0 <= i && i <= 9 = returnP ()
	    | otherwise        = parseError "Precedence out of range"

mkRecConstrOrUpdate 
	:: RdrNameHsExpr 
	-> RdrNameHsRecordBinds
	-> P RdrNameHsExpr

mkRecConstrOrUpdate (HsVar c) fs | isRdrDataCon c
  = returnP (RecordCon c fs)
mkRecConstrOrUpdate exp fs@(_:_) 
  = returnP (RecordUpd exp fs)
mkRecConstrOrUpdate _ _
  = parseError "Empty record update"

-----------------------------------------------------------------------------
-- utilities for foreign declarations

-- supported calling conventions
--
data CallConv = CCall  CCallConv	-- ccall or stdcall
	      | DNCall			-- .NET

-- construct a foreign import declaration
--
mkImport :: CallConv 
	 -> Safety 
	 -> (FAST_STRING, RdrName, RdrNameHsType) 
	 -> SrcLoc 
	 -> P RdrNameHsDecl
mkImport (CCall  cconv) safety (entity, v, ty) loc =
  parseCImport entity cconv safety v			 `thenP` \importSpec ->
  returnP $ ForD (ForeignImport v ty importSpec                     False loc)
mkImport (DNCall      ) _      (entity, v, ty) loc =
  returnP $ ForD (ForeignImport v ty (DNImport (DNCallSpec entity)) False loc)

-- parse the entity string of a foreign import declaration for the `ccall' or
-- `stdcall' calling convention'
--
parseCImport :: FAST_STRING 
	     -> CCallConv 
	     -> Safety 
	     -> RdrName 
	     -> P ForeignImport
parseCImport entity cconv safety v
  -- FIXME: we should allow white space around `dynamic' and `wrapper' -=chak
  | entity == SLIT ("dynamic") = 
    returnP $ CImport cconv safety _NIL_ _NIL_ (CFunction DynamicTarget)
  | entity == SLIT ("wrapper") =
    returnP $ CImport cconv safety _NIL_ _NIL_ CWrapper
  | otherwise		       = parse0 (_UNPK_ entity)
    where
      -- using the static keyword?
      parse0 (' ':                    rest) = parse0 rest
      parse0 ('s':'t':'a':'t':'i':'c':rest) = parse1 rest
      parse0                          rest  = parse1 rest
      -- check for header file name
      parse1     ""               = parse4 ""    _NIL_        False _NIL_
      parse1     (' ':rest)       = parse1 rest
      parse1 str@('&':_   )       = parse2 str   _NIL_
      parse1 str@('[':_   )       = parse3 str   _NIL_        False
      parse1 str
	| ".h" `isSuffixOf` first = parse2 rest  (_PK_ first)
        | otherwise               = parse4 str   _NIL_        False _NIL_
        where
	  (first, rest) = break (\c -> c == ' ' || c == '&' || c == '[') str
      -- check for address operator (indicating a label import)
      parse2     ""         header = parse4 ""   header False _NIL_
      parse2     (' ':rest) header = parse2 rest header
      parse2     ('&':rest) header = parse3 rest header True
      parse2 str@('[':_   ) header = parse3 str	 header False
      parse2 str	    header = parse4 str	 header False _NIL_
      -- check for library object name
      parse3 (' ':rest) header isLbl = parse3 rest header isLbl
      parse3 ('[':rest) header isLbl = 
        case break (== ']') rest of 
	  (lib, ']':rest)           -> parse4 rest header isLbl (_PK_ lib)
	  _			    -> parseError "Missing ']' in entity"
      parse3 str	header isLbl = parse4 str  header isLbl _NIL_
      -- check for name of C function
      parse4 ""         header isLbl lib = build (mkExtName v) header isLbl lib
      parse4 (' ':rest) header isLbl lib = parse4 rest         header isLbl lib
      parse4 str	header isLbl lib
        | all (== ' ') rest              = build (_PK_ first)  header isLbl lib
	| otherwise			 = parseError "Malformed entity string"
        where
	  (first, rest) = break (== ' ') str
      --
      build cid header False lib = returnP $
        CImport cconv safety header lib (CFunction (StaticTarget cid))
      build cid header True  lib = returnP $
        CImport cconv safety header lib (CLabel                  cid )

-- construct a foreign export declaration
--
mkExport :: CallConv
         -> (FAST_STRING, RdrName, RdrNameHsType) 
	 -> SrcLoc 
	 -> P RdrNameHsDecl
mkExport (CCall  cconv) (entity, v, ty) loc = returnP $ 
  ForD (ForeignExport v ty (CExport (CExportStatic entity' cconv)) False loc)
  where
    entity' | nullFastString entity = mkExtName v
	    | otherwise		    = entity
mkExport DNCall (entity, v, ty) loc =
  parseError "Foreign export is not yet supported for .NET"

-- Supplying the ext_name in a foreign decl is optional; if it
-- isn't there, the Haskell name is assumed. Note that no transformation
-- of the Haskell name is then performed, so if you foreign export (++),
-- it's external name will be "++". Too bad; it's important because we don't
-- want z-encoding (e.g. names with z's in them shouldn't be doubled)
-- (This is why we use occNameUserString.)
--
mkExtName :: RdrName -> CLabelString
mkExtName rdrNm = _PK_ (occNameUserString (rdrNameOcc rdrNm))

-----------------------------------------------------------------------------
-- group function bindings into equation groups

-- we assume the bindings are coming in reverse order, so we take the srcloc
-- from the *last* binding in the group as the srcloc for the whole group.

groupBindings :: [RdrBinding] -> RdrBinding
groupBindings binds = group Nothing binds
  where group :: Maybe RdrNameMonoBinds -> [RdrBinding] -> RdrBinding
	group (Just bind) [] = RdrValBinding bind
	group Nothing [] = RdrNullBind

		-- don't group together FunMonoBinds if they have
		-- no arguments.  This is necessary now that variable bindings
		-- with no arguments are now treated as FunMonoBinds rather
		-- than pattern bindings (tests/rename/should_fail/rnfail002).
	group (Just (FunMonoBind f inf1 mtchs ignore_srcloc))
		    (RdrValBinding (FunMonoBind f' _ 
					[mtch@(Match (_:_) _ _)] loc)
			: binds)
	    | f == f' = group (Just (FunMonoBind f inf1 (mtch:mtchs) loc)) binds

	group (Just so_far) binds
	    = RdrValBinding so_far `RdrAndBindings` group Nothing binds
	group Nothing (bind:binds)
	    = case bind of
		RdrValBinding b@(FunMonoBind _ _ _ _) -> group (Just b) binds
		other -> bind `RdrAndBindings` group Nothing binds
\end{code}

%
\section{Generate COM+ extended assembler}

\begin{code}
module IlxGen( ilxGen ) where

#include "HsVersions.h"

import Char	( ord, chr )
import StgSyn
import Id	( idType, idName, isDeadBinder, idArityInfo )
import IdInfo   ( arityLowerBound )
import Var	( Var, Id, TyVar, isId, isTyVar, tyVarKind, tyVarName )
import VarEnv
import TyCon	( TyCon,  tyConPrimRep, isUnboxedTupleTyCon, tyConDataCons, 
		  newTyConRep, tyConTyVars, isDataTyCon, isAlgTyCon, tyConArity
		)
import Class	( Class )
import Type	( liftedTypeKind, openTypeKind, unliftedTypeKind,
		  isUnLiftedType, isTyVarTy, mkTyVarTy, predRepTy,
		  splitForAllTys, splitFunTys, applyTy, applyTys
		)
import TypeRep	( Type(..) )
import DataCon	( isUnboxedTupleCon, dataConTyCon, dataConRepType, dataConRepArgTys )
import Literal	( Literal(..) )
import PrelNames	-- Lots of keys
import PrimOp		( PrimOp(..), CCallTarget(..),CCall(..) )
import TysWiredIn	( mkTupleTy, tupleCon )
import PrimRep		( PrimRep(..) )
import Name		( nameModule, nameOccName, isGlobalName, isLocalName, isDllName, NamedThing(getName) )
import Subst   		( substTy, mkTyVarSubst )

import Module		( Module, PackageName, ModuleName, moduleName, 
                          modulePackage, preludePackage,
			  isPrelModule, isHomeModule, isVanillaModule,
                          pprModuleName, mkHomeModule, mkModuleName
			)

import UniqFM
import BasicTypes	( Boxity(..) )
import CStrings		( pprCLabelString )
import Outputable
import Char		( ord )
import List		( partition, elem, insertBy,any  )
import UniqSet

import TysPrim  ( byteArrayPrimTyCon, mutableByteArrayPrimTyCon )

-- opt_SimplDoEtaReduction is used to help with assembly naming conventions for different
-- versions of compiled Haskell code.  We add a ".O" to all assembly and module 
-- names when this is set (because that's clue that -O was set).  
-- One day this will be configured by the command line.
import CmdLineOpts	( opt_Static, opt_InPackage, opt_SimplDoEtaReduction )

\end{code}



%************************************************************************
%*									*
\subsection{Main driver}
%*									*
%************************************************************************

\begin{code}
ilxGen :: Module -> [TyCon] -> [(StgBinding,[Id])] -> SDoc
	-- The TyCons should include those arising from classes
ilxGen mod tycons binds_w_srts
  =  vcat [vcat (map (ilxImportPackage topenv) (uniqSetToList import_packages)),
            vcat (map (ilxImportModule topenv) (uniqSetToList import_modules)),
            vcat (map (ilxImportTyCon topenv) (uniqSetToList import_tycons)),
            vcat (map (ilxTyCon topenv) data_tycons),
            vcat (map (ilxBindClosures topenv) binds),
	    ilxTopBind mod topenv toppairs
	 ]
    where
      binds = map fst binds_w_srts
      (import_packages,import_modules,import_tycons) = importsBinds binds (importsPrelude emptyImpInfo)
      toppairs = ilxPairs binds
      topenv = extendIlxEnvWithTops (emptyIlxEnv False mod) mod toppairs
 	-- Generate info from class decls as well
      data_tycons = filter isDataTyCon tycons
\end{code}

%************************************************************************
%*									*
\subsection{Find Imports}
%*									*
%************************************************************************

\begin{code}

importsBinds :: [StgBinding] -> ImportsInfo-> ImportsInfo
importsBinds binds = foldR importsBind binds

importsNone :: ImportsInfo -> ImportsInfo
importsNone sofar = sofar

importsBind :: StgBinding -> ImportsInfo -> ImportsInfo
importsBind (StgNonRec _ b rhs) = importsRhs rhs.importsVar b
importsBind (StgRec _ pairs) = foldR (\(b,rhs) -> importsRhs rhs . importsVar b) pairs

importsRhs (StgRhsCon _ con args) = importsDataCon con . importsStgArgs args
importsRhs (StgRhsClosure _ _ _ upd args body) = importsExpr body. importsVars args

importsExpr :: StgExpr -> ImportsInfo -> ImportsInfo
importsExpr (StgLit l) = importsNone
importsExpr (StgApp f args) = importsVar f.importsStgArgs args
importsExpr (StgConApp con args) = importsDataCon con.importsStgArgs args
importsExpr (StgPrimApp op args res_ty) = importsType res_ty. importsStgArgs args
importsExpr (StgSCC cc expr) = importsExpr expr
importsExpr (StgCase scrut _ _ bndr srt alts)
  = importsExpr scrut. imports_alts alts. importsVar bndr
   where
    imports_alts (StgAlgAlts _ alts deflt) 	-- The Maybe TyCon part is dealt with 
						-- by the case-binder's type
      = foldR imports_alg_alt alts .  imports_deflt deflt
       where
        imports_alg_alt (con, bndrs, _, rhs)
	  = importsExpr rhs . importsDataCon con. importsVars bndrs

    imports_alts (StgPrimAlts _ alts deflt)
      = foldR imports_prim_alt alts . imports_deflt deflt
       where
        imports_prim_alt (lit, rhs) = importsExpr rhs
    imports_deflt StgNoDefault = importsNone
    imports_deflt (StgBindDefault rhs) = importsExpr rhs


importsExpr (StgLetNoEscape _ _ bind body) = importsExpr (StgLet bind body)
importsExpr (StgLet bind body)
  = importsBind bind .  importsExpr body

importsApp v args = importsVar v.  importsStgArgs args
importsStgArgs args = foldR importsStgArg args

importsStgArg :: StgArg -> ImportsInfo -> ImportsInfo
importsStgArg (StgTypeArg ty) = importsType ty
importsStgArg (StgVarArg v) = importsVar v
importsStgArg _ = importsNone

importsVars vs = foldR importsVar vs
importsVar v = importsName (idName v). importsType (idType v)

importsName n
   | isLocalName n = importsNone
   | thisModule == nameModule n  = importsNone
   | isHomeModule (nameModule n) =  addModuleImpInfo (moduleName (nameModule n))
   | isVanillaModule (nameModule n) =  addPackageImpInfo preludePackage
   | otherwise = addPackageImpInfo (modulePackage (nameModule n))


importsModule m
   | thisModule   == m = importsNone
   | isHomeModule m =  trace "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n" (addModuleImpInfo (moduleName m))
   | isVanillaModule m =  addPackageImpInfo preludePackage
   | otherwise       = addPackageImpInfo (modulePackage m)

importsType :: Type -> ImportsInfo -> ImportsInfo
importsType ty = importsType2 (deepIlxRepType ty)

importsType2 :: Type -> ImportsInfo -> ImportsInfo
importsType2 (AppTy f x) =  importsType2 f .  importsType2 x
importsType2 (TyVarTy _) = importsNone
importsType2 (TyConApp tc args) =importsTyCon tc . importsTypeArgs2 args
importsType2 (FunTy arg res) =  importsType arg .  importsType2 res
importsType2 (ForAllTy tv body_ty) =  importsType2 body_ty
importsType2 (NoteTy _ ty) = importsType2 ty
importsTypeArgs2 tys = foldR importsType2 tys

importsDataCon dcon = importsTyCon (dataConTyCon dcon)

importsMaybeTyCon Nothing   = importsNone
importsMaybeTyCon (Just tc) = importsName (getName tc)

importsTyCon tc | (not (isDataTyCon tc) || 
                   isLocalName (getName tc) || 
                   thisModule == nameModule (getName tc)) = importsNone
importsTyCon tc | otherwise = importsName (getName tc) . addTyConImpInfo tc

importsPrelude | preludePackage == opt_InPackage = addModuleImpInfo (mkModuleName "PrelGHC")
	       | otherwise			 = addPackageImpInfo preludePackage

type ImportsInfo = (UniqSet PackageName, UniqSet ModuleName, UniqSet TyCon) 
   -- (Packages, Modules, Datatypes)

emptyImpInfo :: ImportsInfo
emptyImpInfo = (emptyUniqSet, emptyUniqSet, emptyUniqSet)
addPackageImpInfo p (w,x,y) = (addOneToUniqSet w p, x, y)
addModuleImpInfo m (w,x,y) = (w, addOneToUniqSet x m, y)
addTyConImpInfo tc (w,x,y) = (w, x, addOneToUniqSet y tc)

ilxImportTyCon :: IlxEnv -> TyCon -> SDoc
ilxImportTyCon env tycon | isDataTyCon tycon = pprIlxTyConDef True env tycon
ilxImportTyCon env tycon | otherwise =  empty

ilxImportPackage :: IlxEnv -> PackageName -> SDoc
ilxImportPackage env p = text ".assembly extern ilx" <+> singleQuotes (ppr p <> hscOptionQual) <+> text "{ }"

ilxImportModule :: IlxEnv -> ModuleName -> SDoc
ilxImportModule env m = text ".module extern ilx" <+> singleQuotes (ppr m  <> hscOptionQual)


\end{code}

%************************************************************************
%*									*
\subsection{Type declarations}
%*									*
%************************************************************************

\begin{code}


ilxTyCon :: IlxEnv -> TyCon -> SDoc
ilxTyCon env tycon =  pprIlxTyConDef False env tycon

-- filter to get only dataTyCons?
pprIlxTyConDef importing env tycon = 
	vcat [empty $$ line,
	      text ".classunion" <+> (if importing then text "extern" else empty) <+> text "thunk" 
                  <+> ((nameReference env (getName tycon)) <> (ppr tycon))   <+> tyvars_text <+> alts_text]
   where
     tyvars = tyConTyVars tycon
     (ilx_tvs, non_ilx_tvs) = categorizeTyVars tyvars
     alts_env = extendIlxEnvWithFormalTyVars env ilx_tvs 
     tyvars_text = pprTyVarBinders alts_env ilx_tvs 
     alts = vcat (map (pprIlxDataCon alts_env) (tyConDataCons tycon))
     alts_text = nest 2 (braces alts)

pprIlxDataCon env dcon =
        text ".alternative" <+> pprId dcon <+> 
        parens (pprSepWithCommas (pprIlxTypeL env) (map deepIlxRepType (filter (not. isVoidIlxRepType) (dataConRepArgTys dcon))))
\end{code}


%************************************************************************
%*									*
\subsection{Getting the .closures and literals out}			*
%************************************************************************

\begin{code}

ilxBindClosures :: IlxEnv -> StgBinding -> SDoc
ilxBindClosures env (StgNonRec _ b rhs) = ilxRhsClosures env (b,rhs)
ilxBindClosures env (StgRec _ pairs)  
  = vcat (map (ilxRhsClosures new_env) pairs)
  where
     new_env = extendIlxEnvWithBinds env pairs

---------------
ilxRhsClosures env (bndr, StgRhsCon _ _ _)
  = empty

ilxRhsClosures env (bndr, StgRhsClosure _ _ fvs upd args rhs)
  = vcat [ilxExprClosures next_env rhs,

	 empty $$ line,
	 kind_text <+> squotes cloname <+>  free_vs_text,
	 nest 2 (braces (
	    nest 2 (vcat [empty,
                          vcat [text ".apply" <+> closure_sig_text,
                                body_text
                          ],
                          empty
                    ])
                ))
    ]
  where
    kind_of_thing = case upd of
			  Updatable -> ASSERT( null args ) ".thunk"
			  other	    -> ".closure"
    kind_text = text kind_of_thing 
		
    cloname = ilxEnvQualifyByModule env (ppr bndr)
    next_env = ilxPlaceStgRhsClosure env bndr 
    (free_vs_text,env_with_fvs) = pprFreeBinders next_env fvs


    closure_sig_text =     
      vcat [ text "()",
             (case args of 
               []    -> empty
               other -> args_text),
             text "-->" <+>  rty_text]

    (args_text,env_with_args) = pprArgBinders env_with_fvs args

        -- Find the type returned, from the no. of args and the type of "bndr"
    rty_text = 
      case retType env_with_fvs (idIlxRepType bndr) args of
       Just (env,ty) -> pprIlxTypeR env ty 
       Nothing -> trace "WARNING!  IlxGen.trace could not find return type - see generated ILX for context where this occurs." (text "// Could not find return type:" <+> pprIlxTypeR env_with_fvs (idIlxRepType bndr)<+> text ", non representation: " <+> pprIlxTypeR env_with_fvs (idType bndr))

    -- strip off leading ForAll and Fun type constructions
    -- up to the given number of arguments, extending the environment as
    -- we go.  
    retType env ty [] = Just (env, ty)
    retType env (ForAllTy tv ty) (arg:args) = retType (extendIlxEnvWithTyArgs env [tv]) ty args
    retType env (FunTy l r) (arg:args) = retType env r args
    retType _ _ _  = Nothing

	-- Code for the local variables
    locals = ilxExprLocals env_with_args rhs

    env_with_locals = extendIlxEnvWithLocals env_with_args locals

	-- Code for the body of the main apply method
    body_code = vcat [empty,
                      pprIlxLocals env_with_args locals,
		      ilxExpr (IlxEEnv env_with_locals (mkUniqSet (filter (not.isTyVar) args))) rhs Return,
                      empty
	        ]

    body_text = nest 2 (braces (text ".maxstack 100" <+> nest 2 body_code))


pprIlxLocals env [] = empty
pprIlxLocals env vs 
   = text ".locals" <+> parens (pprSepWithCommas (pprIlxLocal env) (filter nonVoidLocal vs))
  where
    nonVoidLocal (LocalId v,_) = not (isVoidIlxRepId v)
    nonVoidLocal _ = True

pprIlxLocal env (LocalId v,_) = pprIlxTypeL env (idIlxRepType v) <+> pprId v
pprIlxLocal env (LocalSDoc (ty,doc,pin),_) = pprIlxTypeL env (deepIlxRepType ty) <+> (if pin then text "pinned" else empty) <+> doc


pprFreeBinders env fvs 
    = (ilx_tvs_text <+> vs_text, env2)
    where   
       (free_ilx_tvs, free_non_ilx_tvs,free_vs) = categorizeVars fvs
       real_free_vs = filter (not . isVoidIlxRepId) free_vs
        -- ignore the higher order type parameters for the moment
       env1 = extendIlxEnvWithFreeTyVars env free_ilx_tvs 
       ilx_tvs_text = pprTyVarBinders env1 free_ilx_tvs
       vs_text = parens (pprSepWithCommas ppr_id real_free_vs)
       ppr_id v = pprIlxTypeL env1 (idIlxRepType v) <+> pprId v 
       env2 = extendIlxEnvWithFreeVars env1 real_free_vs 

pprIdBinder env v = parens (pprIlxTypeL env (idIlxRepType v) <+> pprId v)

	-- Declarations for the arguments of the main apply method
pprArgBinders env [] = (empty,env)
pprArgBinders env (arg:args)
    = (arg_text <+> rest_text, res_env)
   where 
     (arg_text,env') = pprArgBinder env arg
     (rest_text,res_env) = pprArgBinders env' args 

-- We could probably omit some void argument binders, but
-- don't...
pprArgBinder env arg 
  | isVoidIlxRepId arg = (text "(unit)", extendIlxEnvWithArgs env [arg])
  | otherwise 
      = if isTyVar arg then 
         let env' = extendIlxEnvWithTyArgs env [arg] in 
         (pprTyVarBinder env' arg, env')
      else (pprIdBinder env arg,extendIlxEnvWithArgs env [arg])

--------------
-- Compute local variables used by generated method.
-- The names of some generated locals are recorded as SDocs.

data LocalSpec = LocalId Id | LocalSDoc (Type, SDoc, Bool)  -- flag is for pinning

ilxExprLocals :: IlxEnv -> StgExpr -> [(LocalSpec,Maybe (IlxEnv,StgRhs))]
ilxExprLocals env (StgLet bind body) 		  = ilxBindLocals env bind ++ ilxExprLocals env body
ilxExprLocals env (StgLetNoEscape _ _ bind body)  = ilxBindLocals env bind ++ ilxExprLocals env body  -- TO DO????
ilxExprLocals env (StgCase scrut _ _ bndr _ alts) 
     = ilxExprLocals (ilxPlaceStgCaseScrut env) scrut ++ 
       (if isDeadBinder bndr then [] else [(LocalId bndr,Nothing)]) ++ 
       ilxAltsLocals env alts
ilxExprLocals env (StgPrimApp (CCallOp (CCall (StaticTarget c) casm gc cconv)) args ret_ty) 
     = concat (ilxMapPlaceArgs 0 ilxCCallArgLocals env args)
ilxExprLocals _ _  = []

-- Generate locals to use for pinning arguments as we cross the boundary
-- to C.
ilxCCallArgLocals env arg@(StgVarArg v) | pinCCallArg v = 
   [(LocalSDoc (idType v, ilxEnvQualifyByExact env (ppr v) <> text "pin", True), Nothing)]
ilxCCallArgLocals _ _ | otherwise = []

ilxBindLocals env (StgNonRec _ b rhs) = [(LocalId b,Just (env, rhs))]
ilxBindLocals env (StgRec _ pairs)    = map (\(x,y) -> (LocalId x,Just (env, y))) pairs

ilxAltsLocals env (StgAlgAlts  _ alts deflt) = ilxDefltLocals env deflt ++ concat (ilxMapPlaceAlts ilxAlgAltLocals env alts)
ilxAltsLocals env (StgPrimAlts _ alts deflt) = ilxDefltLocals env deflt ++ concat (ilxMapPlaceAlts ilxPrimAltLocals env alts)

ilxAlgAltLocals env (con, bndrs, _, rhs) = map (\x -> (LocalId x,Nothing)) (filter (\v -> isId v && not (isDeadBinder v)) bndrs) ++ ilxExprLocals env rhs
ilxPrimAltLocals env (lit, rhs)          = ilxExprLocals env rhs

ilxDefltLocals env StgNoDefault 	= []
ilxDefltLocals env (StgBindDefault rhs) = ilxExprLocals (ilxPlaceStgBindDefault env) rhs

--------------
ilxExprClosures :: IlxEnv -> StgExpr -> SDoc
ilxExprClosures env (StgApp _ args)
  = vcat (ilxMapPlaceArgs 0 (ilxArgClosures) env args)  -- get strings
ilxExprClosures env (StgConApp _ args)
  = vcat (ilxMapPlaceArgs 0 (ilxArgClosures) env args) -- get strings
ilxExprClosures env (StgPrimApp _ args _)
  = vcat (ilxMapPlaceArgs 0 (ilxArgClosures) env args) -- get strings
ilxExprClosures env (StgLet bind body)
  = ilxBindClosures env bind $$ ilxExprClosures (extendIlxEnvWithBinds env (ilxPairs1 bind)) body
ilxExprClosures env (StgLetNoEscape _ _ bind body)  -- TO DO????
  = ilxBindClosures env bind $$ ilxExprClosures (extendIlxEnvWithBinds env (ilxPairs1 bind)) body
ilxExprClosures env (StgCase scrut _ _ _ _ alts)
  = ilxExprClosures (ilxPlaceStgCaseScrut env) scrut $$ ilxAltsClosures env alts 
ilxExprClosures env (StgLit lit) 
  = ilxGenLit env lit 
ilxExprClosures env other 
  = empty

ilxAltsClosures env (StgAlgAlts _ alts deflt)
  = vcat [ilxExprClosures (ilxPlaceAlt env i) rhs | (i,(_, _, _, rhs))  <- [1..] `zip` alts]
    $$ 
    ilxDefltClosures env deflt

ilxAltsClosures env (StgPrimAlts _ alts deflt)
  = vcat [ilxExprClosures (ilxPlaceAlt env i) rhs | (i,(_, rhs)) <- [1..] `zip` alts]
    $$ 
    vcat [ ilxGenLit (ilxPlacePrimAltLit env i) lit | (i,(lit,_)) <- [1..] `zip` alts]
    $$ 
    ilxDefltClosures  env deflt

ilxDefltClosures env (StgBindDefault rhs) = ilxExprClosures (ilxPlaceStgBindDefault env) rhs
ilxDefltClosures env StgNoDefault	  = empty

ilxArgClosures env (StgLitArg lit) = ilxGenLit env lit 
ilxArgClosures _ _ = empty



ilxGenLit env (MachStr fs) 
  = vcat [text ".field static assembly char "  <+> squotes nm <+> text "at" <+> nm <> text "L",
          text ".data" <+> nm <> text "L" <+> text "= char *("  <> pprFSInILStyle fs  <> text ")"
         ]
 where
   nm = ilxEnvQualifyByExact env (text "string")

ilxGenLit  _ _ = empty

\end{code}


%************************************************************************
%*									*
\subsection{Generating code}
%*									*
%************************************************************************


\begin{code}

-- Environment when generating expressions
data IlxEEnv = IlxEEnv IlxEnv (UniqSet Id)

data Sequel = Return | Jump IlxLabel

ilxSequel Return     = text "ret"
ilxSequel (Jump lbl) = text "br" <+> pprIlxLabel lbl

isReturn Return = True
isReturn (Jump _) = False


ilxExpr :: IlxEEnv -> StgExpr 
	-> Sequel 	-- What to do at the end
	-> SDoc

ilxExpr eenv@(IlxEEnv env _) (StgApp fun args) sequel
  = ilxFunApp env fun args (isReturn sequel) $$ ilxSequel sequel

-- ilxExpr eenv (StgLit lit) sequel
ilxExpr eenv@(IlxEEnv env _) (StgLit lit) sequel
  = pushLit env lit $$ ilxSequel sequel

-- ilxExpr eenv (StgConApp data_con args) sequel
ilxExpr eenv@(IlxEEnv env _) (StgConApp data_con args) sequel
  = text " /* ilxExpr:StgConApp */ " <+>  ilxConApp env data_con args $$ ilxSequel sequel

-- ilxExpr eenv (StgPrimApp primop args _) sequel
ilxExpr eenv@(IlxEEnv env _) (StgPrimApp primop args ret_ty) sequel
  = ilxPrimApp env primop args ret_ty $$ ilxSequel sequel

--BEGIN TEMPORARY
-- The following are versions of a peephole optimizations for "let t = \[] t2[fvs] in t"
-- I think would be subsumed by a general treatmenet of let-no-rec bindings??
ilxExpr eenv@(IlxEEnv env _) (StgLet (StgNonRec _ bndr (StgRhsClosure _ _ fvs upd [] rhs)) (StgApp fun [])) sequel 
              | (bndr == fun && null (ilxExprLocals env rhs)) -- TO DO???
  = ilxExpr eenv rhs sequel
ilxExpr eenv@(IlxEEnv env _) (StgLetNoEscape _ _ (StgNonRec _ bndr (StgRhsClosure _ _ fvs upd [] rhs)) (StgApp fun [])) sequel 
              | (bndr == fun && null (ilxExprLocals env rhs)) -- TO DO???
  = ilxExpr eenv rhs sequel
--END TEMPORARY

ilxExpr eenv (StgLet bind body) sequel
  = ilxBind eenv bind $$ ilxExpr eenv body sequel


ilxExpr eenv (StgLetNoEscape _ _ bind body) sequel -- TO DO???
  = ilxBind eenv bind $$ ilxExpr eenv body sequel

-- StgCase: Special case 1 to avoid spurious branch.
ilxExpr eenv@(IlxEEnv env live) (StgCase (StgApp fun args) live_in_case live_in_alts bndr _ alts) sequel
  = vcat [ilxWipe env (uniqSetToList (live `minusUniqSet` live_in_case)),
	  ilxFunApp (ilxPlaceStgCaseScrut env) fun args False,
          --ilxWipe env (uniqSetToList (live_in_case `minusUniqSet` live_in_alts)),
	  --ilxAlts (IlxEEnv env live_in_alts) bndr alts sequel
	  ilxAlts (IlxEEnv env live_in_case) bndr alts sequel
    ]

-- StgCase: Special case 2 to avoid spurious branch.
ilxExpr eenv@(IlxEEnv env live) (StgCase (StgPrimApp primop args ret_ty) live_in_case live_in_alts bndr _ alts) sequel
  = vcat [ilxWipe env (uniqSetToList (live `minusUniqSet` live_in_case)),
	  ilxPrimApp (ilxPlaceStgCaseScrut env) primop args ret_ty,
          --ilxWipe env (uniqSetToList (live_in_case `minusUniqSet` live_in_alts)),
	  --ilxAlts (IlxEEnv env live_in_alts) bndr alts sequel
	  ilxAlts (IlxEEnv env live_in_case) bndr alts sequel
    ]

-- StgCase: Normal case.
ilxExpr eenv@(IlxEEnv env live) (StgCase scrut live_in_case live_in_alts bndr _ alts) sequel
  = vcat [ilxWipe env (uniqSetToList (live `minusUniqSet` live_in_case)),
	  ilxExpr (IlxEEnv (ilxPlaceStgCaseScrut env) live_in_case) scrut (Jump join_lbl),
	  ilxLabel join_lbl,
          --ilxWipe env (uniqSetToList (live_in_case `minusUniqSet` live_in_alts)),
	  --ilxAlts (IlxEEnv env live_in_alts) bndr alts sequel
	  ilxAlts (IlxEEnv env live_in_case) bndr alts sequel
    ]
  where
    join_lbl = mkJoinLabel bndr

-- Wipe out locals and arguments that are no longer in use, to
-- prevent space leaks. If the VM is implemented 100% correctly then
-- this should probably not be needed, as the live variable analysis
-- in the JIT would tell the GC that these locals and arguments are
-- no longer live.  However I'm putting it in here so we can
-- check out if it helps.
--
-- Also, in any case this doesn't capture everything we need.  e.g.
-- when making a call:
--     case f x of ...
-- where x is not used in the alternatives, then the variable x
-- is no longer live from the point it is transferred to the call
-- onwards.  We should expunge "live_in_case - live_in_alts" right
-- before making the call, not after returning from the call....
--
-- Strictly speaking we also don't need to do this for primitive
-- values such as integers and addresses, i.e. things not
-- mapped down to GC'able objects.
ilxWipe env ids 
   = vcat (map (ilxWipeOne env) (filter (not.isVoidIlxRepId) ids))

ilxWipeOne env id
   = case lookupIlxVarEnv env id of
	  Just Local  -> text "ldloca " <+> pprId id <+> text "initobj" <+> (pprIlxTypeL env (idIlxRepType id))
	  Just Arg   -> text "deadarg " <+> pprId id <+> text "," <+> (pprIlxTypeL env (idIlxRepType id))
	  Just (CloVar _)  -> ilxComment (text "not yet wiping closure variable" <+> pprId id )
	  _ -> ilxComment (text "cannot wipe non-local/non-argument" <+> pprId id )
  where 
      

----------------------

ilxAlts :: IlxEEnv -> Id -> StgCaseAlts -> Sequel -> SDoc
ilxAlts eenv@(IlxEEnv env live) bndr alts sequel
	-- At the join label, the result is on top
	-- of the stack
  = vcat [store_in_bndr,
	  do_case_analysis alts
    ]
  where
    scrut_rep_ty = deepIlxRepType (idType bndr)

    store_in_bndr | isDeadBinder bndr = empty
                  | isVoidIlxRepId bndr 
                        = ilxComment (text "ignoring store of zero-rep value to be analyzed")
		  | otherwise	      = text "dup" $$ (text "stloc" <+> pprId bndr)

    do_case_analysis (StgAlgAlts _ []    deflt)
	= do_deflt deflt

    do_case_analysis (StgAlgAlts _ args deflt) 
        = do_alg_alts ([1..] `zip` args) deflt

    do_case_analysis (StgPrimAlts _ alts deflt)
	= do_prim_alts ([1..] `zip` alts) $$ do_deflt deflt

    do_alg_alts [(i, alt@(data_con,bndrs,used_flags, rhs))] StgNoDefault | isUnboxedTupleCon data_con
      -- Collapse the analysis of unboxed tuples where 
      -- some or all elements are zero-sized
      --
      -- TO DO: add bndrs to set of live variables
          = case bndrs' of
                  [h] -> bind_collapse bndrs used_flags <+> do_rhs_no_pop alt_env rhs
                  _ -> bind_components alt_env dcon' bndrs 0 used_flags <+> do_rhs alt_env rhs
           where 
            bndrs' = filter (not. isVoidIlxRepId) bndrs
            -- Replacement unboxed tuple type constructor, used if any of the
            -- arguments have zero-size and more than one remains.
            dcon'  = tupleCon Unboxed (length bndrs')

            alt_env = IlxEEnv (ilxPlaceAlt env i) live
            --alt_env = IlxEEnv (ilxPlaceAlt env i) 

            bind_collapse [] _ = panic "bind_collapse: unary element not found"
            bind_collapse (h:t) (is_used:used_flags) 
                | isVoidIlxRepId h = ilxComment (text "zero-rep binding eliminated") <+> (bind_collapse t used_flags)
	        | not is_used = ilxComment (text "not used") <+> text "pop"
                | otherwise = text "stloc" <+> pprId h


    do_alg_alts [(i, alt@(data_con,bndrs,used_flags, rhs))] StgNoDefault 
            = vcat [text "castdata" <+> sep [pprIlxTypeR env scrut_rep_ty <> comma,
		  			     ilxConRef env data_con],
 		do_alg_alt (IlxEEnv (ilxPlaceAlt env i) live) alt
	      ]

    do_alg_alts alts deflt
	= vcat [text "datacase" <+> sep [pprIlxTypeR env scrut_rep_ty,text ",",
					 pprSepWithCommas pp_case labels_w_alts],
		do_deflt deflt,
		vcat (map do_labelled_alg_alt labels_w_alts)
	  ]
	where
	  pp_case (i, (lbl, (data_con, _, _, _))) = parens (ilxConRef env data_con <> comma <> pprIlxLabel lbl)
	  labels_w_alts = [(i,(mkAltLabel bndr i, alt)) | (i, alt) <- alts]

    do_prim_alts [] = empty
    do_prim_alts ((i, (lit,alt)) : alts) 
	= vcat [text "dup", pushLit (ilxPlacePrimAltLit env i) lit, text "bne.un" <+> pprIlxLabel lbl, 
		do_rhs (IlxEEnv (ilxPlaceAlt env i) live) alt, 
		ilxLabel lbl, do_prim_alts alts]
	where
	  lbl = mkAltLabel bndr i

    do_labelled_alg_alt (i,(lbl, alt)) 
        = ilxLabel lbl $$ do_alg_alt (IlxEEnv (ilxPlaceAlt env i) live) alt

    do_alg_alt alt_eenv (data_con, bndrs, used_flags, rhs) 
      = vcat [bind_components alt_eenv data_con bndrs 0 used_flags,
	      do_rhs alt_eenv rhs
	     ]

    bind_components alt_eenv data_con [] n _ = empty
    bind_components alt_eenv data_con (h:t) n (is_used:used_flags) 
       | isVoidIlxRepId h 
             -- don't increase the count in this case
             = ilxComment (text "zero-rep binding eliminated") 
               <+> bind_components alt_eenv data_con t n used_flags
       | otherwise 
             = bind_component alt_eenv data_con h is_used n 
               <+> bind_components alt_eenv data_con t (n + 1) used_flags

    bind_component alt_eenv@(IlxEEnv alt_env _) data_con bndr is_used reduced_fld_no 
	| not is_used 
            = ilxComment (text "not used")
        | isVoidIlxRepId bndr 
            = ilxComment (text "ignoring bind of zero-rep variable")
	| otherwise   = vcat [text "dup",
			      ld_data alt_env data_con reduced_fld_no bndr,
			      text "stloc" <+> pprId bndr]

    do_deflt (StgBindDefault rhs) = do_rhs (IlxEEnv (ilxPlaceStgBindDefault env) live) rhs
    do_deflt StgNoDefault 	  = empty

    do_rhs alt_eenv rhs  
        | isVoidIlxRepId bndr = do_rhs_no_pop alt_eenv rhs     -- void on the stack, nothing to pop
        | otherwise = text "pop" $$ do_rhs_no_pop alt_eenv rhs  -- drop the value

    do_rhs_no_pop alt_env rhs = ilxExpr alt_env rhs sequel

    ld_data alt_env data_con reduced_fld_no bndr
      | isUnboxedTupleCon data_con
      = text "ldfld" <+> sep [text "!" <> integer reduced_fld_no,
			      pprIlxTypeR alt_env scrut_rep_ty <> text "::fld" <> integer reduced_fld_no]
      | otherwise 
      = text "lddata" <+> sep [pprIlxTypeR alt_env scrut_rep_ty <> comma, 
		               ilxConRef env data_con <> comma,
			       integer reduced_fld_no]


-------------------------

ilxBestTermArity = 3
ilxBestTypeArity = 7


-- Constants of unlifted types are represented as
-- applications to no arguments.
ilxFunApp env fun [] _ | isUnLiftedType (idType fun)
  = pushId env fun

ilxFunApp env fun args tail_call 
  =	-- For example:
        --	ldloc f		function of type forall a. a->a
	--	ldloc x		arg of type Int
	--	.tail callfunc <Int32> (!0) --> !0
	--
    vcat [pushId env fun,ilxFunAppArgs env 0 (idIlxRepType fun) args tail_call known_clo]
  where
    known_clo =
      case lookupIlxBindEnv env fun of
	  Just (place, StgRhsClosure  _ _ _ Updatable _ _) ->  Nothing 
	  Just (place, StgRhsClosure  _ _ fvs _ args _)  -> Just (place,fun,args,fvs)
	  _ ->  trace (show fun ++ " --> " ++ show (arityLowerBound (idArityInfo fun)))  Nothing 

-- Push as many arguments as ILX allows us to in one go.
-- Recurse until we're done.
ilxFunAppArgs env num_sofar funty args tail_call known_clo
 =   vcat [vcat (ilxMapPlaceArgs num_sofar pushArgWithVoids env now_args),
	   call_instr <+> text "()" <+> now_args_text
                     <+> text "-->" 
                     <+> (pprIlxTypeR env_after_now_tyvs later_ty),
           later
          ]
  where
    now_args_text = 
      case now_arg_tys of
        [] -> empty
        _ -> hsep (map (pprIlxArgInfo env_after_now_tyvs) now_arg_tys)

    (now_args,now_arg_tys,env_after_now_tyvs,later_args,later_ty) = 
	case args of
          (StgTypeArg v:rest) -> get_type_args ilxBestTypeArity args env funty
          _ -> get_term_args ilxBestTermArity args env funty

     -- Only apply up to maxArity real (non-type) arguments
     -- at a time.  ILX should, in principle, allow us to apply
     -- arbitrary numbers, but you will get more succinct 
     -- (and perhaps more efficient) IL code
     -- if you apply in clumps according to its maxArity setting.
     -- This is because it has to unwind the stack and store it away
     -- in local variables to do the partial applications.
     --
     -- Similarly, ILX only allows one type application at a time, at
     -- least until we implement unwinding the stack for this case.
     --
     -- NB: In the future we may have to be more careful 
     -- all the way through 
     -- this file to bind type variables as we move through
     -- type abstractions and "forall" types.  This would apply
     -- especially if the type variables were ever bound by expressions
     -- involving the type variables.  

    -- This part strips off at most "max" term applications or one type application
    get_type_args 0 args env funty = ([],[],env,args,funty)
    get_type_args max args env (NoteTy _ ty) = 
          trace "IlxGen Internal Error: non representation type passed to get_args" (get_type_args max args env ty)
    get_type_args max ((arg@(StgTypeArg v)):rest) env (ForAllTy tv rem_funty) 
        = if isIlxTyVar tv then 
            let env2 = extendIlxEnvWithFormalTyVars env [tv] in 
            let rest_ty = deepIlxRepType (substTy (mkTyVarSubst [tv] [v]) rem_funty) in 
            let (now,now_tys,env3,later,later_ty) = get_type_args (max - 1) rest env rest_ty in 
            let arg_ty = mkTyVarTy tv in 
            (arg:now,(arg,arg_ty):now_tys,env2, later, later_ty)
          else 
             get_type_args max rest env rem_funty  -- ? subst??
    get_type_args _ (StgTypeArg _:_) _ _ = trace "IlxGen Internal Error: get_type_args could not get ForAllTy for corresponding arg" ([],[],env,[],funty)
    get_type_args _ args env funty = ([],[],env,args,funty)

    -- We could probably skip some  void-rep arguments.  Instead we
    -- emit "ldvoid" for these and let ILXASM optimize them away.
    -- get_term_args max (h@(StgVarArg v):t) env (FunTy dom ran) | isVoidIlxRepId v   
        --  = get_term_args max t env ran
    get_term_args max args env (NoteTy _ ty) =
        -- Skip NoteTy types 
          trace "IlxGen Internal Error: non representation type passed to get_term_args" (get_term_args max args env ty)
    get_term_args 0 args env funty = ([],[],env,args,funty)
        -- Stop if no more
    get_term_args _ (args@(StgTypeArg _:_)) env funty = ([],[],env,args,funty)
    get_term_args max (h:t) env (FunTy dom ran) = 
          let (now,now_tys,env2,later,later_ty) = get_term_args (max - 1) t env ran in 
          (h:now, (h,dom):now_tys,env2,later,later_ty)
    get_term_args max (h:t) env funty = trace "IlxGen Internal Error: get_term_args could not get FunTy or ForAllTy for corresponding arg" ([],[],env,[],funty)
    get_term_args max args env funty = ([],[],env,args,funty)

    -- Are there any remaining arguments?
    done  = case later_args of
          [] -> True
          _ -> False

    -- If so, generate the subsequent calls.
    later = if done then text "// done"  
            else ilxFunAppArgs env (num_sofar + length now_args) later_ty later_args tail_call Nothing

    -- Work out whether to issue a direct call a known closure (callclo) or
    -- an indirect call (callfunc).  Basically, see if the identifier has
    -- been let-bound, and then check we are applying exactly the right 
    -- number of arguments.  Also check that it's not a thunk (actually, this
    -- is done up above).
    -- 
    -- The nasty "all" check makes sure that 
    -- the set of type variables in scope at the callsite is a superset 
    -- of the set of type variables needed for the direct call.  This is
    -- is needed because not all of the type variables captured by a 
    -- let-bound binding will get propogated down to the callsite, and 
    -- the ILX system of polymorphism demands that the free type variables
    -- get reapplied when we issue the direct "callclo".  The
    -- type variables are in reality also "bound up" in the closure that is
    -- passed as the first argument, so when we do an indirect call
    -- to that closure we're fine, which is why we don't need them in 
    -- the "callfunc" case.
    basic_call_instr =
      case known_clo of
        Just (known_env,fun,needed,fvs) | (length needed == length args) && 
                                          all (\x -> elemIlxTyVarEnv x env) free_ilx_tvs -> 
           vcat [text "callclo class",
                 nameReference env (idName fun) <+> squotes (ilxEnvQualifyByModule env (ppr fun)),
                 pprTypeArgs pprIlxTypeR env (map mkTyVarTy free_ilx_tvs)]
           <> text ","
          where 
           (free_ilx_tvs, free_non_ilx_tvs,free_vs) = categorizeVars fvs
        otherwise -> text "callfunc"
    call_instr =
           if (tail_call && done) then text "tail." <+> basic_call_instr
	   else basic_call_instr


--------------------------
-- Print the arg info at the call site
-- For type args we are, at the moment, required to
-- give both the actual and the formal (bound).  The formal
-- bound is always System.Object at the moment (bounds are
-- not properly implemented in ILXASM in any case, and nor do
-- we plan on making use og them) For
-- non-type args the actuals are on the stack, and we just give the
-- formal type.
pprIlxArgInfo env (StgTypeArg  arg,ty) =  
    angleBrackets (pprIlxTypeR env (deepIlxRepType arg) <+> ilxComment (text "actual for tyvar")) <+> text "<class [mscorlib] System.Object>" 
pprIlxArgInfo env (_,ty) =  
    parens (pprIlxTypeL env ty)


----------------------------
-- Code for a binding
ilxBind :: IlxEEnv -> StgBinding -> SDoc
ilxBind eenv@(IlxEEnv env _) bind = 
    vcat [vcat (map (ilxRhs env rec) pairs), 
          vcat (map (ilxFixupRec env rec) pairs)]
       where 
         rec = ilxRecIds1 bind
         pairs = ilxPairs1 bind


----------------------------
-- Allocate a closure or constructor.  Fix up recursive definitions.
ilxRhs :: IlxEnv -> [Id] -> (Id, StgRhs) -> SDoc

ilxRhs env rec (bndr, _) | isVoidIlxRepId bndr  
  = empty

ilxRhs env rec (bndr, StgRhsCon _ con args)
  = vcat [text " /* ilxRhs:StgRhsCon */ " <+> ilxConApp env con args,
	   text "stloc" <+> pprId bndr
          ]

ilxRhs env rec (bndr, StgRhsClosure _ _ fvs upd args rhs)
  = 	-- Assume .closure v<any A>(int64,!A) { 
	--		.apply <any B> (int32) (B) { ... }
	--	   }
	-- Then
        --    let v = \B (x:int32) (y:B). ... 
        -- becomes:
        --    newclo v<int32>(int64,!0)
	--    stloc v
    vcat [vcat (map pushFv free_vs),
          (if null free_non_ilx_tvs then empty else (ilxComment (text "ignored some higher order type arguments in application - code will be non-verifiable"))),
	  text "newclo" <+> clotext,
	  text "stloc" <+> pprId bndr
    ]
  where
    pushFv id = if elem id rec then text "ldnull" else pushId env id
    (free_ilx_tvs, free_non_ilx_tvs,free_vs) = categorizeVars fvs
    clotext = pprIlxBoxedTyConApp env (ilxEnvQualifyByModule env (ppr bndr)) (map mkTyVarTy free_ilx_tvs)

ilxFixupRec env rec (bndr, _) | isVoidIlxRepId bndr = ilxComment (text "no recursive fixup for void-rep-id")

ilxFixupRec env rec (bndr, StgRhsCon _ con args)
  = text "// no recursive fixup"

ilxFixupRec env rec (bndr, StgRhsClosure _ _ fvs upd args rhs)
     = vcat [vcat (map fixFv rec)]
  where
    fixFv recid = if elem recid fvs then 
                    vcat [pushId env bndr,
                          pushId env recid,
                          text "stclofld" <+> clotext <> text "," <+> pprId recid] 
                else text "//no fixup needed for" <+> pprId recid
    (free_ilx_tvs, free_non_ilx_tvs,free_vs) = categorizeVars fvs
    clotext = pprIlxBoxedTyConApp env (ilxEnvQualifyByModule env (ppr bndr)) (map mkTyVarTy free_ilx_tvs)



---------------------------------------------
-- Code for a top-level binding in a module
ilxPairs binds = concat (map ilxPairs1 binds)

ilxPairs1 (StgNonRec _ bndr rhs) = [(bndr,rhs)]
ilxPairs1 (StgRec _ pairs)       = pairs

ilxRecIds1 (StgNonRec _ bndr rhs) = []
ilxRecIds1 (StgRec _ pairs)       = map fst pairs

---------------------------------------------
-- Code for a top-level binding in a module
-- TODO: fix up recursions amongst CAF's
-- e.g. 
--    x = S x
-- for infinity...
-- 
-- For the moment I've put in a completely spurious "reverse"...
--
-- Consider: make fixing up of CAF's part of ILX?  i.e.
-- put static, constant, allocated datastructures into ILX. 

stableSortBy :: (a -> a -> Ordering) -> [a] -> [a]
stableSortBy f (h:t) = insertBy f h (stableSortBy f t)
stableSortBy f [] = []

usedBy :: (Id,StgRhs) -> (Id,StgRhs) -> Ordering
usedBy (m,_) (_,StgRhsCon _ data_con args) | any (isArg m) args = LT
usedBy (m,_) (n,_) | m == n = EQ
usedBy (m,_) (_,_) = GT

isArg m  (StgVarArg n) = (n == m)
isArg m _ = False


ilxTopBind :: Module -> IlxEnv -> [(Id,StgRhs)] -> SDoc
--ilxTopBind mod env (StgNonRec _ bndr rhs) = 
--ilxTopRhs env (bndr,rhs)
ilxTopBind mod env pairs       = 
   vcat [text ".class" <+> pprId mod,
         nest 2 (braces (nest 2 (vcat [empty,cctor, flds, empty])))]
     where
       cctor = vcat [text ".method static rtspecialname specialname void .cctor()",
                     nest 2 (braces 
                      (nest 2 (vcat [text ".maxstack 100",
			             text "ldstr \"LOG: initializing module" <+> pprId mod <+> text "\" call void ['mscorlib']System.Console::WriteLine(class [mscorlib]System.String)",
                                     vcat (map (ilxTopRhs mod env) (stableSortBy usedBy pairs)), 
			             text "ldstr \"LOG: initialized module" <+> pprId mod <+> text "\" call void ['mscorlib']System.Console::WriteLine(class [mscorlib]System.String)",
                                     text "ret",
                                     empty])))]
       flds =   vcat (map (ilxTopRhsStorage mod env) pairs)

--ilxTopRhs mod env (bndr, _) | isVoidIlxRepId bndr 
--  = empty

ilxTopRhs mod env (bndr, StgRhsClosure _ _ fvs upd args rhs)
  = vcat [vcat (map (pushId env) free_vs),
         (if null free_non_ilx_tvs then empty else (ilxComment (text "ignored some higher order type arguments in application - code will be non verifiable...."))),
	  text "newclo" <+> pprIlxBoxedTyConApp env (ilxEnvQualifyByModule env (ppr bndr)) (map mkTyVarTy free_ilx_tvs),
	  text "stsfld"  <+> pprFieldRef env (mod,bndTy,bndr)
    ]
  where
    (free_ilx_tvs, free_non_ilx_tvs,free_vs) = categorizeVars fvs
    bndTy = idIlxRepType bndr

ilxTopRhs mod env (bndr, StgRhsCon _ data_con args)
  = vcat [ text " /* ilxTopRhs: StgRhsCon */ " <+> ilxConApp env data_con args, 
	   text "stsfld" <+> pprFieldRef env (mod,bndTy,bndr)
    ]
  where
    bndTy = idIlxRepType bndr

pprFieldRef env (mod,ty,id) 
  =  pprIlxTypeL env ty <+> moduleReference env mod <+> pprId mod <> text "::" <> pprId id

ilxTopRhsStorage mod env (bndr, StgRhsClosure _ _ _ _ _ _) 
  =   text ".field public static " <+> pprIlxTypeL env bndTy <+> pprId bndr
  where
    bndTy = idIlxRepType bndr
ilxTopRhsStorage mod env (bndr, StgRhsCon _ _ _) 
  =   text ".field public static " <+> pprIlxTypeL env bndTy <+> pprId bndr
  where
    bndTy = idIlxRepType bndr

--------------------------------------
-- Push an argument
pushArgWithVoids =  pushArg_aux True
pushArg = pushArg_aux False

pushArg_aux voids env (StgTypeArg ty) = empty
pushArg_aux voids env (StgVarArg var) = pushId_aux voids env var
pushArg_aux voids env (StgLitArg lit) = pushLit env lit


mapi f l = mapi_aux f l 0

mapi_aux f [] n = []
mapi_aux f (h:t) n = f n h : mapi_aux f t (n+1)

--------------------------------------
-- Push an Id
pushId = pushId_aux False

pushId_aux :: Bool -> IlxEnv -> Id -> SDoc
pushId_aux voids _ id | isVoidIlxRepId id =
   if voids then  text "ldunit" else ilxComment (text "pushId: void rep skipped")
pushId_aux _ env var 
  = case lookupIlxVarEnv env var of
	  Just Arg    -> text "ldarg"    <+> pprId var
	  Just (CloVar n) -> text "ldenv" <+> int n
	  Just Local  -> text "ldloc"    <+> pprId var
	  Just (Top m)  -> 
             vcat [ilxComment (text "pushId (Top) " <+> pprId m), 
                   text "ldsfld" <+> pprIlxTypeL env (idIlxRepType var)
                      <+> moduleReference env m <+> pprId (moduleName m) <> text "::" <> pprId var]

	  Nothing ->  
             vcat [ilxComment (text "pushId (import) " <+> pprIlxTopVar env var), 
                   text "ldsfld" <+> pprIlxTypeL env (idIlxRepType var) 
                    <+> pprIlxTopVar env var]

--------------------------------------
-- Push a literal
pushLit env (MachChar c)   = text "ldc.i4" <+> int c
pushLit env (MachStr s)    = text "ldsflda char "  <+> ilxEnvQualifyByExact env (text "string") -- pprFSInILStyle s 
pushLit env (MachInt i)    = text "ldc.i4" <+> integer i
pushLit env (MachInt64 i)  = text "ldc.i8" <+> integer i
pushLit env (MachWord w)   = text "ldc.i4" <+> integer w <+> text "conv.u4"
pushLit env (MachWord64 w) = text "ldc.i8" <+> integer w <+> text "conv.u8"
pushLit env (MachFloat f)  = text "ldc.r4" <+> rational f
pushLit env (MachDouble f) = text "ldc.r8" <+> rational f
pushLit env (MachLitLit _ _) = trace "WARNING: Cannot compile MachLitLit to ILX in IlxGen.lhs" (text "// MachLitLit!!!  Not valid in ILX!!")
pushLit env (MachAddr w) = text "ldc.i4 conv.i " <+> integer w


pprIlxTopVar env v
  | isGlobalName n = (nameReference env n) <> pprId (nameModule n) <> text "::" <> squotes (ppr (nameModule n) <> text "_" <> ppr (nameOccName n))
  | otherwise	   = pprId (nameOccName n)
  where
    n = idName v

\end{code}


%************************************************************************
%*									*
\subsection{Printing types}
%*									*
%************************************************************************


\begin{code}

isVoidIlxRepType (NoteTy   _ ty) = isVoidIlxRepType ty
isVoidIlxRepType (TyConApp tc _) | (tyConPrimRep tc == VoidRep) = True
isVoidIlxRepType (TyConApp tc tys) 
  = case newTyConRep tc of
	 Just rep_ty -> isVoidIlxRepType (applyTys rep_ty tys)
	 Nothing     -> 
            isUnboxedTupleTyCon tc && 
            null (filter (not. isVoidIlxRepType) tys)
isVoidIlxRepType _ = False

isVoidIlxRepId id = isVoidIlxRepType (idType id)



-- Get rid of all NoteTy and NewTy artifacts
deepIlxRepType :: Type -> Type
deepIlxRepType (FunTy l r)
  = FunTy (deepIlxRepType l) (deepIlxRepType r)

deepIlxRepType (TyConApp tc tys) 
  = case newTyConRep tc of
	 Just rep_ty -> ASSERT( length tys == tyConArity tc )
		-- The assert should hold because deepIlxRepType should
		-- only be applied to *types* (of kind *)
			deepIlxRepType (applyTys rep_ty tys)
	 Nothing     -> 
           -- collapse UnboxedTupleTyCon down when it contains VoidRep types.
            if isUnboxedTupleTyCon tc then 
               let tys' = map deepIlxRepType (filter (not. isVoidIlxRepType) tys) in 
               case tys' of
                  [h] -> h
                  _ -> mkTupleTy Unboxed (length tys') tys'
            else 
              TyConApp tc (map deepIlxRepType tys)
deepIlxRepType (AppTy f x)  = AppTy (deepIlxRepType f) (deepIlxRepType x)
deepIlxRepType (ForAllTy b ty) = ForAllTy b (deepIlxRepType ty)
deepIlxRepType (NoteTy   _ ty) = deepIlxRepType ty
deepIlxRepType (PredTy p)      = deepIlxRepType (predRepTy p)
deepIlxRepType ty@(TyVarTy tv) = ty

idIlxRepType id = deepIlxRepType (idType id)

--------------------------
-- Some primitive type constructors are not thunkable.
-- Everything else needs to be marked thunkable.
pprIlxTypeL :: IlxEnv -> Type -> SDoc

pprIlxTypeL env ty | isUnLiftedType ty ||  isVoidIlxRepType ty = pprIlxTypeR env ty
pprIlxTypeL env ty = text "thunk" <> angleBrackets (pprIlxTypeR env ty)

--------------------------
-- Print non-thunkable version of type.
--

pprIlxTypeR :: IlxEnv -> Type -> SDoc
pprIlxTypeR env ty | isVoidIlxRepType ty = text "unit"
pprIlxTypeR env ty@(AppTy f _) | isTyVarTy f    = ilxComment (text "type app:" <+> pprId ty) <+> (text "class [mscorlib]System.Object")
pprIlxTypeR env ty@(AppTy f x)     = trace "pprIlxTypeR: should I be beta reducing types?!" (ilxComment (text "pprIlxTypeR: should I be beta reducing types...") <+> pprIlxTypeR env (applyTy f x))
pprIlxTypeR env (TyVarTy tv)       = pprIlxTyVar env tv

-- The following is a special rule for types constructed out of 
-- higher kinds, e.g. Monad f or Functor f.  
--
-- The code below is not as general as it should be, but as I
-- have no idea if this approach will even work, I'm going to
-- just try it out on some simple cases arising from the prelude.
pprIlxTypeR env ty@(TyConApp tc (h:t)) | isAlgTyCon tc && null (tyConTyVars tc)
   = ilxComment (text "what the fuck? 2") <+> (pprIlxTypeR env (TyConApp tc t))
pprIlxTypeR env ty@(TyConApp tc (h:t)) | isAlgTyCon tc && not (isIlxTyVar (hd (tyConTyVars tc)))
   = pprIlxTypeR env (TyConApp tc t)
pprIlxTypeR env (TyConApp tc args) = pprIlxTyConApp env tc args

  -- nb. the only legitimate place for VoidIlxRepTypes to occur in normalized IlxRepTypes 
  -- is on the left of an arrow
  --  We could probably eliminate all but a final occurrence of these.
pprIlxTypeR env (FunTy arg res)
    = pprIlxFunTy (pprIlxTypeL env arg) (pprIlxTypeR env res)

pprIlxTypeR env ty@(ForAllTy tv body_ty) | isIlxTyVar tv
  = parens (text "forall" <+> pprTyVarBinders env' [tv] <+> nest 2 (pprIlxTypeR env' body_ty))
    where
       env' = extendIlxEnvWithFormalTyVars env [tv]

pprIlxTypeR env ty@(ForAllTy tv body_ty) | otherwise
  = ilxComment (text "higher order type var " <+> pprId tv) <+>
    pprIlxFunTy (text "class [mscorlib]System.Object") (pprIlxTypeR env body_ty)

pprIlxTypeR env (NoteTy _ ty)       
   = trace "WARNING! non-representation type given to pprIlxTypeR: see generated ILX for context where this occurs"
     (vcat [text "/* WARNING! non-representation type given to pprIlxTypeR! */",
           pprIlxTypeR env ty ])

pprIlxFunTy dom ran = parens (hsep [text "func",parens dom,text "-->", ran])

pprIlxTyConApp env tc args =
   case lookupUFM tyPrimConTable (getUnique tc) of
	Just f  -> f env args
        Nothing -> 
            (if isUnboxedTupleTyCon tc then pprIlxUnBoxedTyConApp else pprIlxBoxedTyConApp)
              env ((nameReference env (getName tc)) <> (ppr tc)) args

pprIlxUnBoxedTyConApp env tcdoc args = text "value class" <+> tcdoc <> pprTypeArgs pprIlxTypeL env args
pprIlxBoxedTyConApp env tcdoc args = text "class" <+> tcdoc <> pprTypeArgs pprIlxTypeR env args

-- Returns e.g: <Int32, Bool>
pprTypeArgs f env tys = pprTypeArgs_aux f env (filter (not . isVoidIlxRepType) tys)

pprTypeArgs_aux f env []  = empty
pprTypeArgs_aux f env tys = angleBrackets (pprSepWithCommas (f env) tys)


pprTyVarBinders :: IlxEnv -> [TyVar] -> SDoc
-- Returns e.g: <class [mscorlib]System.Object> <class [mscorlib]System.Object>
-- plus a new environment with the type variables added.
pprTyVarBinders env [] = empty
pprTyVarBinders env tvs = angleBrackets (pprSepWithCommas (pprTyVarBinder_aux env) tvs)

pprTyVarBinder :: IlxEnv -> TyVar -> SDoc
pprTyVarBinder env tv = 
    if isIlxTyVar tv then 
       angleBrackets (pprTyVarBinder_aux env tv)
    else
       ilxComment (text "higher order tyvar" <+> pprId tv <+> 
                         text ":" <+> pprIlxTypeR env (tyVarKind tv)) <+>
             ilxComment (text "omitted")
             -- parens (text "class [mscorlib]System.Object" <+> pprId tv)


pprTyVarBinder_aux env tv = 
   ilxComment (text "tyvar" <+> pprId tv <+> text ":" <+> 
                        pprIlxTypeR env (tyVarKind tv)) <+>
             (text "class [mscorlib]System.Object")

-- Only a subset of Haskell types can be generalized using the type quantification
-- of ILX
isIlxForAllKind h = 
        ( h == liftedTypeKind) ||
        ( h == unliftedTypeKind) ||
        ( h == openTypeKind)

isIlxTyVar v = isTyVar v && isIlxForAllKind (tyVarKind v)

categorizeVars fvs = (ilx_tvs, non_ilx_tvs, vs)
         where
           (tvs, vs) = partition isTyVar fvs
           (ilx_tvs, non_ilx_tvs) = categorizeTyVars tvs

categorizeTyVars tyvs = partition isIlxTyVar tyvs

pprValArgTys ppr_ty env tys = parens (pprSepWithCommas (ppr_ty env) tys)

pprId id = squotes (ppr id)
squotes s = text "'" <> s <> text "'"

\end{code}			

%************************************************************************
%*									*
\subsection{IlxEnv}	
%*									*
%************************************************************************

\begin{code}
type IlxTyEnv = [TyVar]
emptyIlxTyEnv = []

-- Nb. There is currently no distinction between the kinds of type variables.
-- We may need to add this to print out correct numbers, esp. for
-- "forall" types
extendIlxTyEnvWithFreeTyVars env tyvars = env ++ mkIlxTyEnv tyvars -- bound by .closure x<...> in a closure declared with type parameters
extendIlxTyEnvWithFormalTyVars env tyvars = env ++ mkIlxTyEnv tyvars -- bound by "forall <...>" in a type
extendIlxTyEnvWithTyArgs env tyvars = env ++ mkIlxTyEnv tyvars -- bound by "<...>" in a closure implementing a universal type

formalIlxTyEnv tyvars = mkIlxTyEnv tyvars
mkIlxTyEnv tyvars = [ v | v <- tyvars, isIlxTyVar v ]

pprIlxTyVarInIlxTyEnv :: IlxTyEnv -> TyVar -> SDoc
pprIlxTyVarInIlxTyEnv tv_env tv
  = go 0 tv_env
  where
    go n [] 		    
      = pprTrace "pprIlxTyVar" (pprId tv <+> text "tv_env = { "
           <+> pprSepWithCommas (\x -> pprId x <+> text ":" 
           <+> pprIlxTypeR (IlxEnv (thisModule, tv_env, emptyVarEnv, emptyVarEnv, (empty,empty),False)) (tyVarKind x)) tv_env <+> text "}") 
        (char '!' <> pprId tv) 
    go n (x:xs)
      = {- pprTrace "go" (ppr (tyVarName tv) <+> ppr (tyVarName x)) -}
        (if tyVarName x== tyVarName tv then  char '!' <> int n <+> ilxComment (char '!' <> pprId tv) 
         else go (n+1) xs)


data HowBound = Top Module 	-- Bound in a modules
	      | Arg	-- Arguments to the enclosing closure
	      | CloVar Int -- A free variable of the enclosing closure
                           -- The int is the index of the field in the 
                           -- environment
	      | Local	-- Local let binding

-- The SDoc prints a unique name for the syntactic block we're currently processing,
-- e.g. Foo_bar_baz when inside closure baz inside closure bar inside module Foo.
data IlxEnv = IlxEnv (Module, IlxTyEnv, IdEnv HowBound,IdEnv (IlxEnv, StgRhs), Place,Bool)
type Place = (SDoc,SDoc)
thisModule = mkHomeModule (mkModuleName "")

pprIlxTyVar (IlxEnv (_, tv_env, _, _,_,_)) tv = pprIlxTyVarInIlxTyEnv tv_env tv 

emptyIlxEnv :: Bool -> Module -> IlxEnv
emptyIlxEnv trace mod = IlxEnv (mod, emptyIlxTyEnv, emptyVarEnv, emptyVarEnv, (ppr mod,empty),trace)

nextPlace place sdoc = place <> sdoc
usePlace  place sdoc = place <> sdoc

ilxEnvSetPlace (IlxEnv (m, tv_env, id_env,  bind_env, (mod,exact),tr)) sdoc 
   = IlxEnv (m, tv_env, id_env,  bind_env, (mod, sdoc),tr)
ilxEnvNextPlace (IlxEnv (m, tv_env, id_env,  bind_env, (mod,exact),tr)) sdoc 
   = IlxEnv (m, tv_env, id_env,  bind_env, (mod, nextPlace exact sdoc),tr)
ilxEnvQualifyByModule (IlxEnv (_, _, _, _,(mod,_),_)) sdoc = usePlace mod sdoc
ilxEnvQualifyByExact (IlxEnv (_, _, _, _,(mod,exact),_)) sdoc = usePlace mod sdoc <> usePlace exact sdoc

ilxPlaceStgBindDefault env = ilxEnvNextPlace env (text "D")
ilxPlaceStgRhsClosure env bndr = ilxEnvSetPlace env (ppr bndr) -- binders are already unique
ilxPlaceStgCaseScrut env = ilxEnvNextPlace env (text "S")

ilxPlaceAlt :: IlxEnv -> Int -> IlxEnv
ilxPlaceAlt env i = ilxEnvNextPlace env (text "a" <> int i)
ilxPlacePrimAltLit env i = ilxEnvNextPlace env (text "P" <> int i)
ilxMapPlaceArgs start f env args = [ f (ilxEnvNextPlace env (text "A" <> int i)) a | (i,a) <- [start..] `zip` args ]
ilxMapPlaceAlts f env alts = [ f (ilxPlaceAlt env i) alt | (i,alt) <- [1..] `zip` alts ]

extendIlxEnvWithFreeTyVars (IlxEnv (mod, tv_env, id_env,  bind_env, place,tr)) tyvars 
  = IlxEnv (mod, extendIlxTyEnvWithFreeTyVars tv_env tyvars,id_env,  bind_env, place,tr)

extendIlxEnvWithFormalTyVars (IlxEnv (mod, tv_env, id_env,  bind_env, place,tr)) tyvars 
  = IlxEnv (mod, extendIlxTyEnvWithFormalTyVars tv_env tyvars,id_env,  bind_env, place,tr)

extendIlxEnvWithTyArgs (IlxEnv (mod, tv_env, id_env,  bind_env, place,tr)) tyvars 
  = IlxEnv (mod, extendIlxTyEnvWithTyArgs tv_env tyvars,id_env,  bind_env, place,tr)

extendIlxEnvWithArgs :: IlxEnv -> [Var] -> IlxEnv
extendIlxEnvWithArgs (IlxEnv (mod, tv_env, id_env,  bind_env, place,tr)) args
  = IlxEnv (mod, extendIlxTyEnvWithTyArgs tv_env [tv      | tv <- args, isIlxTyVar tv],
            extendVarEnvList id_env [(v,Arg) | v  <- args, not (isIlxTyVar v)], 
	     bind_env, place,tr)

extendIlxEnvWithFreeVars (IlxEnv (mod, tv_env, id_env,  bind_env, place,tr)) args
  = IlxEnv (mod, 
            extendIlxTyEnvWithFreeTyVars tv_env [tv | tv <- args, isIlxTyVar tv],
            extendVarEnvList id_env (clovs 0 args), 
            bind_env, 
            place,tr)
   where
     clovs _ [] = []
     clovs n (x:xs) = if not (isIlxTyVar x) then (x,CloVar n):clovs (n+1) xs else clovs n xs

extendIlxEnvWithBinds env@(IlxEnv (mod, tv_env, id_env, bind_env, place,tr)) bnds
  = IlxEnv (mod, tv_env, id_env, 
            extendVarEnvList bind_env [(v,(env,rhs)) | (v,rhs) <- bnds], 
            place,tr)

extendIlxEnvWithLocals (IlxEnv (m, tv_env, id_env, bind_env, p,tr)) locals
  = IlxEnv (m, tv_env, 
            extendVarEnvList id_env [(v,Local) | (LocalId v,_) <- locals],
            extendVarEnvList bind_env [(v,(env,rhs)) | (LocalId v,Just (env,rhs)) <- locals], 
            p,tr)
extendIlxEnvWithTops env@(IlxEnv (m, tv_env, id_env, bind_env, place,tr)) mod binds
  = IlxEnv (m, tv_env, 
            extendVarEnvList id_env [(bndr,Top mod) | (bndr,rhs) <- binds], 
            extendVarEnvList bind_env [(bndr,(env, rhs)) | (bndr,rhs) <- binds], 
            place,tr)

formalIlxEnv (IlxEnv (m, tv_env, id_env, bind_env, place, tr)) tyvars 
  = IlxEnv (m, formalIlxTyEnv tyvars, id_env, bind_env, place, tr)

elemIlxTyVarEnv var env@(IlxEnv (_, tv_env, _,_,_,_)) = elem var tv_env 
elemIlxVarEnv var env@(IlxEnv (_, _, id_env,_,_,_)) = elemVarEnv var id_env 
lookupIlxVarEnv env@(IlxEnv (_, _, id_env,_,_,_)) var = lookupVarEnv id_env var
lookupIlxBindEnv env@(IlxEnv (_, _, _, bind_env,_,_)) var = lookupVarEnv bind_env var

\end{code}


\begin{code}
type IlxLabel = SDoc

pprIlxLabel lbl = lbl

mkJoinLabel :: Id -> IlxLabel
mkJoinLabel v = text "J_" <> ppr v

mkAltLabel  :: Id -> Int -> IlxLabel
mkAltLabel v n = text "A" <> int n <> ppr v

ilxLabel :: IlxLabel -> SDoc
ilxLabel lbl =  line $$ (pprIlxLabel lbl <> colon)
\end{code}


%************************************************************************
%*									*
\subsection{Local pretty helper functions}
%*									*
%************************************************************************

\begin{code}
pprSepWithCommas :: (a -> SDoc) -> [a] -> SDoc
pprSepWithCommas pp xs = sep (punctuate comma (map pp xs))
ilxComment pp   = text "/*" <+> pp <+> text "*/"
singleQuotes pp = char '\'' <> pp <> char '\''

line = text "// ----------------------------------"

hscOptionQual = if opt_SimplDoEtaReduction then text ".O" else text ".Onot"

nameReference (IlxEnv (thisMod, _, _, _, _, _)) n
  | isLocalName n = empty
  | thisMod == nameModule n  = text ""
  | isHomeModule (nameModule n)   = moduleNameReference (moduleName (nameModule n))
  | isVanillaModule (nameModule n) =  packageReference preludePackage
  | otherwise = packageReference (modulePackage (nameModule n))

packageReference p = brackets ((text "ilx") <+> singleQuotes (ppr p  <> hscOptionQual))
moduleNameReference m = brackets ((text ".module") <+> (text "ilx") <+> singleQuotes (pprModuleName m <> hscOptionQual))

moduleReference (IlxEnv (thisMod, _, _, _, _, _)) m
  | thisMod   == m = text ""
  | isHomeModule m = moduleNameReference (moduleName m)
  | isVanillaModule m =  packageReference preludePackage
  | otherwise  =  packageReference (modulePackage m)

------------------------------------------------
-- This code is copied from absCSyn/CString.lhs,
-- and modified to do the correct thing!  It's
-- still a mess though.  Also, still have to do the
-- right thing for embedded nulls.

pprFSInILStyle :: FAST_STRING -> SDoc
pprFSInILStyle fs = doubleQuotes (text (stringToC (_UNPK_ fs)))

stringToC   :: String -> String
-- Convert a string to the form required by C in a C literal string
-- Tthe hassle is what to do w/ strings like "ESC 0"...
stringToC ""  = ""
stringToC [c] = charToC c
stringToC (c:cs)
    -- if we have something "octifiable" in "c", we'd better "octify"
    -- the rest of the string, too.
  = if (c < ' ' || c > '~')
    then (charToC c) ++ (concat (map char_to_C cs))
    else (charToC c) ++ (stringToC cs)
  where
    char_to_C c | c == '\n' = "\\n"	-- use C escapes when we can
		| c == '\a' = "\\a"
		| c == '\b' = "\\b"	-- ToDo: chk some of these...
		| c == '\r' = "\\r"
		| c == '\t' = "\\t"
		| c == '\f' = "\\f"
		| c == '\v' = "\\v"
		| otherwise = '\\' : (trigraph (ord c))

charToC :: Char -> String
-- Convert a character to the form reqd in a C character literal
charToC c = if (c >= ' ' && c <= '~')	-- non-portable...
	    then case c of
		  '\'' -> "\\'"
		  '\\' -> "\\\\"
		  '"'  -> "\\\""
		  '\n' -> "\\n"
		  '\a' -> "\\a"
		  '\b' -> "\\b"
		  '\r' -> "\\r"
		  '\t' -> "\\t"
		  '\f' -> "\\f"
		  '\v' -> "\\v"
		  _    -> [c]
	    else '\\' : (trigraph (ord c))

trigraph :: Int -> String
trigraph n
  = [chr ((n `div` 100) `rem` 10 + ord '0'),
     chr ((n `div` 10) `rem` 10 + ord '0'),
     chr (n `rem` 10 + ord '0')]


\end{code}

%************************************************************************
%*									*
\subsection{PrimOps and Constructors}
%*									*
%************************************************************************

\begin{code}
----------------------------
-- Allocate a fresh constructor

ilxConApp env data_con args
  | isUnboxedTupleCon data_con
     = let tm_args' = filter (not. isVoidIlxRepType . stgArgType) tm_args in 
       case tm_args' of
        [h] -> 
          -- Collapse the construction of an unboxed tuple type where
          -- every element is zero-sized
            vcat (ilxMapPlaceArgs 0 pushArg env tm_args')
        _ -> 
          -- Minimize the construction of an unboxed tuple type, which
          -- may contain zero-sized elements.  Recompute all the 
          -- bits and pieces from the simpler case below for the new data
          -- type constructor....
           let data_con' = tupleCon Unboxed (length tm_args') in 
           let rep_ty_args' = filter (not . isVoidIlxRepType) rep_ty_args in 

           let tycon' = dataConTyCon data_con' in
           let (formal_tyvars', formal_tau_ty') = splitForAllTys (dataConRepType data_con') in 
           let (formal_arg_tys', _)     = splitFunTys formal_tau_ty' in
           let formal_env' 	     = formalIlxEnv env formal_tyvars' in 

           vcat [vcat (ilxMapPlaceArgs 0 pushArg env tm_args'),
	           sep [text "newobj void ",
  		        pprIlxTyConApp env tycon' rep_ty_args',
                        text "::.ctor",
                        pprValArgTys pprIlxTypeR formal_env' (map deepIlxRepType formal_arg_tys')
	           ]
             ]
 | otherwise
    -- Now all other constructions
     =	--  Assume C :: forall a. a -> T a -> T a
   	--	ldloc x		arg of type Int
	--	ldloc y		arg of type T Int
	-- 	newdata classunion T<Int32>, C(!0, T <!0>)
	--
        let tycon   = dataConTyCon data_con in 
        let (formal_tyvars, formal_tau_ty) = splitForAllTys (dataConRepType data_con) in
        let (formal_arg_tys, _)     = splitFunTys formal_tau_ty in 

       vcat [vcat (ilxMapPlaceArgs 0 pushArg env tm_args),
	  sep [	text "newdata",
		nest 2 (pprIlxTyConApp env tycon rep_ty_args <> comma),
		nest 2 (ilxConRef env data_con)
	  ]
        ]
 where
   tycon   = dataConTyCon data_con 
   rep_ty_args = map deepIlxRepType ty_args
   (ty_args,tm_args) = if isAlgTyCon tycon then splitTyArgs (tyConTyVars tycon) args  else splitTyArgs1 args

-- split some type arguments off, throwing away the higher kinded ones for the moment
-- base the higher-kinded checks off a corresponding list of formals
splitTyArgs (htv:ttv) (StgTypeArg h:t) 
   | isIlxTyVar htv = ((h:l), r) 
   | otherwise = trace "splitTyArgs: threw away higher kinded type arg" (l, r) 
   where (l,r) = splitTyArgs ttv t 
splitTyArgs _ l = ([],l)
 
-- split some type arguments off, where none should be higher kinded
splitTyArgs1 (StgTypeArg h:t) 
   = ((h:l), r) 
   where (l,r) = splitTyArgs1 t 
splitTyArgs1 l = ([],l)
 

ilxConRef env data_con
    = pprId data_con <> pprValArgTys pprIlxTypeL env' (map deepIlxRepType (filter (not . isVoidIlxRepType) arg_tys))
  where
    (tyvars, tau_ty) = splitForAllTys (dataConRepType data_con)
    (arg_tys, _)     = splitFunTys tau_ty
    env' 	     = formalIlxEnv env tyvars


tyPrimConTable :: UniqFM (IlxEnv -> [Type] -> SDoc)
tyPrimConTable = listToUFM [(addrPrimTyConKey, 	(\_ _ -> repAddr)),
--			    (fileStreamPrimTyConKey, 	(\_ _ -> repFileStream)),
			    (foreignObjPrimTyConKey, 	(\_ _ -> text "/* ForeignObj */ void *")),
--			    (stablePtrPrimTyConKey, 	(\_ _ -> text "/* StablePtr */ void *")),
			    (charPrimTyConKey, 	(\_ _ -> repChar)),
			    (wordPrimTyConKey, 	(\_ _ -> repWord)),
	                    (byteArrayPrimTyConKey,	(\_ _ -> repByteArray)),
			    (intPrimTyConKey, 	(\_ _ -> repInt)),
			    (int64PrimTyConKey,	(\_ _ -> repInt64)),
			    (word64PrimTyConKey,	(\_ _ -> repWord64)),
			    (floatPrimTyConKey, 	(\_ _ -> text "float32")),
			    (arrayPrimTyConKey, 	(\env [ty] -> pprIlxTypeL env ty <> text "[]")),
			    (mutableArrayPrimTyConKey, 	(\env [_, ty] -> pprIlxTypeL env ty <> text "[]")),
			    (mVarPrimTyConKey, 	(\env [_, ty] -> repMVar (pprIlxTypeL env ty))),
			    (mutVarPrimTyConKey, 	(\env [ty1, ty2] -> repMutVar (pprIlxTypeL env ty1) (pprIlxTypeL env ty2))),
	                    (mutableByteArrayPrimTyConKey,	(\_ _ -> repByteArray)),
	                    (threadIdPrimTyConKey,	(\_ _ -> text "class [mscorlib]System.Threading.Thread /* ThreadId# */ ")),
			    (doublePrimTyConKey,	(\_ _ -> text "float64"))
	     ]


\end{code}


%************************************************************************
%*									*
\subsection{PrimOps}
%*									*
%************************************************************************

\begin{code}



prelGHCReference =
   if preludePackage == opt_InPackage then 
	brackets (text ".module ilx PrelGHC" <> hscOptionQual) 
   else brackets (text "ilx" <+> text (_UNPK_ preludePackage)  <> hscOptionQual)


prelBaseReference =
   if preludePackage == opt_InPackage then 
	brackets (text ".module ilx PrelBase" <> hscOptionQual) 
   else brackets (text "ilx" <+> text (_UNPK_ preludePackage) <> hscOptionQual)


ilxPrimApp env (CCallOp ccall) args ret_ty = ilxCCall env ccall args ret_ty
ilxPrimApp env op 	       args ret_ty = ilxPrimOpTable op env args

ilxMkBool =  text "call class" <+> prelBaseReference <+> 
             text "PrelBase_Bool" <+> 
             prelGHCReference <+> text "GHC.support::mkBool(bool)"
ilxCgt = text "cgt " <+> ilxMkBool
ilxCge = text "clt ldc.i4 0 ceq " <+> ilxMkBool
ilxClt = text "clt " <+> ilxMkBool
ilxCle = text "cgt ldc.i4 0 ceq " <+> ilxMkBool
ilxCeq = text "ceq " <+> ilxMkBool
ilxCne = text "ceq ldc.i4 0 ceq "  <+> ilxMkBool
ilxCgtUn = text "cgt.un " <+> ilxMkBool
ilxCgeUn = text "clt.un ldc.i4 0 ceq " <+> ilxMkBool
ilxCltUn = text "clt.un " <+> ilxMkBool
ilxCleUn = text "cgt.un ldc.i4 0 ceq " <+> ilxMkBool
ldDummyInteger = text " ldc.i4 0 ldnull newobj void" <+> ilxUnboxedPairRep  repInt repByteArray <+> text "::.ctor(!0,!1)"
ldDummyInteger2 = text " ldc.i4 0 ldnull ldc.i4 0 ldnull newobj void" <+> ilxUnboxedQuadRep  repInt repByteArray repInt repByteArray <+> text "::.ctor(!0,!1,!2,!3)"

repByteArray = text "unsigned int8[] /* ByteArr# */ "
repFileStream = text "void * /* FileStream# */ "  -- text "class [mscorlib]System.IO.FileStream"
repInt = text "int32"
repWord = text "unsigned int32"
repAddr = text "/* Addr */ void *"
repInt64 = text "int64"
repWord64 = text "unsigned int64"
repChar = text "/* Char */ unsigned int8"
repInteger = ilxUnboxedPairRep repInt repByteArray
repIntegerPair = ilxUnboxedQuadRep repInt repByteArray repInt repByteArray
repMVar ty = text "class " <+> prelGHCReference <+> text "PrelGHC_MVarzh" <+> ilxTyParams [ty]
repMutVar _ ty2 = text "class " <+> prelGHCReference <+> text "PrelGHC_MutVarzh" <+> ilxTyParams [ty2]
repWeak ty1 = text "class " <+> prelGHCReference <+> text "PrelGHC_Weakzh" <+> ilxTyParams [ty1]

ilxParamsAux [] = empty
ilxParamsAux [h] = h
ilxParamsAux (h:t) = h <> text "," <+> ilxParamsAux t
ilxParams [] = empty
ilxParams l = parens (ilxParamsAux l)

ilxTyParamsAux [] = empty
ilxTyParamsAux [h] = h
ilxTyParamsAux (h:t) = h <> text "," <+> ilxTyParamsAux t
ilxTyParams [] = empty
ilxTyParams l = angleBrackets (ilxTyParamsAux l)

ilxMethA = text "!!0"

ilxTyPair l r = ilxTyParams [l,r]
ilxTyTriple l m r = ilxTyParams [l,m,r]
ilxTyQuad l m1 m2 r = ilxTyParams [l,m1,m2,r]
ilxUnboxedPairRep l r = text "value class" <+> prelGHCReference <+> text "PrelGHC_ZLzhzx2czhZR" <+> ilxTyPair l r
ilxUnboxedTripleRep l m r = text "value class" <+> prelGHCReference <+> text "PrelGHC_ZLzhzx2czx2czhZR" <+> ilxTyTriple l m r
ilxUnboxedQuadRep l m1 m2 r = text "value class" <+> prelGHCReference <+> text "PrelGHC_ZLzhzx2czx2czx2czhZR" <+> ilxTyQuad l m1 m2 r

ilxMethodRef rty cls nm tyargs args = rty <+> cls <+> text "::" <> squotes (text nm) <> ilxTyParams tyargs <> ilxParams args

ilxSupportClass = prelGHCReference <+> text "GHC.support"
ilxSuppMeth rty nm tyargs args = ilxMethodRef rty ilxSupportClass nm tyargs args

ilxPrimOpTable :: PrimOp -> IlxEnv -> [StgArg] -> SDoc
ilxPrimOpTable op
  = case op of
 	CharGtOp    -> simp_op ilxCgt
	CharGeOp    -> simp_op ilxCge
	CharEqOp    -> simp_op ilxCeq
	CharNeOp    -> simp_op ilxCne
	CharLtOp    -> simp_op ilxClt
	CharLeOp    -> simp_op ilxCle

	OrdOp       -> simp_op (text "conv.i4") -- chars represented by UInt32 (u4)
	ChrOp       -> simp_op (text "conv.u4")

	IntGtOp     -> simp_op ilxCgt
	IntGeOp     -> simp_op ilxCge
	IntEqOp     -> simp_op ilxCeq
	IntNeOp     -> simp_op ilxCne
	IntLtOp     -> simp_op ilxClt
	IntLeOp     -> simp_op ilxCle

	WordGtOp     -> simp_op ilxCgtUn -- words represented by UInt32 (u4)
	WordGeOp     -> simp_op ilxCgeUn
	WordEqOp     -> simp_op ilxCeq
	WordNeOp     -> simp_op ilxCne
	WordLtOp     -> simp_op ilxCltUn
	WordLeOp     -> simp_op ilxCleUn

	AddrGtOp     -> simp_op ilxCgt
	AddrGeOp     -> simp_op ilxCge
	AddrEqOp     -> simp_op ilxCeq
	AddrNeOp     -> simp_op ilxCne
	AddrLtOp     -> simp_op ilxClt
	AddrLeOp     -> simp_op ilxCle

	FloatGtOp     -> simp_op ilxCgt
	FloatGeOp     -> simp_op ilxCge
	FloatEqOp     -> simp_op ilxCeq
	FloatNeOp     -> simp_op ilxCne
	FloatLtOp     -> simp_op ilxClt
	FloatLeOp     -> simp_op ilxCle

	DoubleGtOp     -> simp_op ilxCgt
	DoubleGeOp     -> simp_op ilxCge
	DoubleEqOp     -> simp_op ilxCeq
	DoubleNeOp     -> simp_op ilxCne
	DoubleLtOp     -> simp_op ilxClt
	DoubleLeOp     -> simp_op ilxCle

    -- Int#-related ops:
	IntAddOp    -> simp_op (text "add")
	IntSubOp    -> simp_op (text "sub")
	IntMulOp    -> simp_op (text "mul")
	IntQuotOp   -> simp_op (text "div")
	IntNegOp    -> simp_op (text "neg")
	IntRemOp    -> simp_op (text "rem")

	Addr2IntOp  -> simp_op (text "conv.i4") -- Addresses are very dodgy for ILX.  They are used for both C-strings and 
	Int2AddrOp  -> simp_op (text "conv.i")  -- the FFI.  This needs more work.
	ISllOp      -> simp_op (text "shl")
	ISraOp      -> simp_op (text "shr")
	ISrlOp      -> simp_op (text "shr.un")
	IntAddCOp   -> simp_op (text "call" <+> ilxSuppMeth (ilxUnboxedPairRep repInt repInt) "IntAddCOp" [] [repInt, repInt])
	IntSubCOp   -> simp_op (text "call" <+> ilxSuppMeth (ilxUnboxedPairRep repInt repInt) "IntSubCOp" [] [repInt, repInt])
	IntMulCOp   -> simp_op (text "call" <+> ilxSuppMeth (ilxUnboxedPairRep repInt repInt) "IntMulCOp" [] [repInt, repInt])
	IntGcdOp    -> simp_op (text "call" <+> ilxSuppMeth repInt "IntMulCOp" [] [repInt, repInt])


    -- Word#-related ops:
	AndOp  	    -> simp_op (text "and") 
	OrOp   	    -> simp_op (text "or") 
	NotOp  	    -> simp_op (text "not") 
	XorOp  	    -> simp_op (text "xor") 
	SllOp  	    -> simp_op (text "shl") 
	SrlOp 	    -> simp_op (text "shr") 
	Word2IntOp  -> simp_op (text "conv.i4")
	Int2WordOp  -> simp_op (text "conv.u4")

    -- Float#-related ops:
	FloatAddOp   -> simp_op (text "add")
	FloatSubOp   -> simp_op (text "sub")
	FloatMulOp   -> simp_op (text "mul")
	FloatDivOp   -> simp_op (text "div")
	FloatNegOp   -> simp_op (text "neg")
	Float2IntOp  -> simp_op (text "conv.i4")
	Int2FloatOp  -> simp_op (text "conv.r4")

	DoubleAddOp   	-> simp_op (text "add")
	DoubleSubOp   	-> simp_op (text "sub")
	DoubleMulOp   	-> simp_op (text "mul")
	DoubleDivOp   	-> simp_op (text "div")
	DoubleNegOp   	-> simp_op (text "neg")
	Double2IntOp  	-> simp_op (text "conv.i4")
	Int2DoubleOp  	-> simp_op (text "conv.r4")
	Double2FloatOp  -> simp_op (text "conv.r4")
	Float2DoubleOp  -> simp_op (text "conv.r8")
	DoubleDecodeOp  -> simp_op (text "call" <+> ilxSuppMeth (ilxUnboxedTripleRep repInt repInt repByteArray) "decodeDouble" [] [text "float64"])
	FloatDecodeOp   -> simp_op (text "call" <+> ilxSuppMeth (ilxUnboxedTripleRep repInt repInt repByteArray) "decodeFloat" [] [text "float32"])

	FloatExpOp   -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Exp(float64) conv.r4")
	FloatLogOp   -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Log(float64) conv.r4")
	FloatSqrtOp  -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Sqrt(float64) conv.r4")
	FloatSinOp   -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Sin(float64) conv.r4")
	FloatCosOp   -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Cos(float64) conv.r4")
	FloatTanOp   -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Tan(float64) conv.r4")
	FloatAsinOp  -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Asin(float64) conv.r4")
	FloatAcosOp  -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Acos(float64) conv.r4")
	FloatAtanOp  -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Atan(float64) conv.r4")
	FloatSinhOp  -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Sinh(float64) conv.r4")
	FloatCoshOp  -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Cosh(float64) conv.r4")
	FloatTanhOp  -> simp_op (text "conv.r8 call float64 [mscorlib]System.Math::Tanh(float64) conv.r4")
	FloatPowerOp -> simp_op (text "call float64 [mscorlib]System.Math::Pow(float64, float64) conv.r4") -- ** op, make use of implicit cast to r8...

	DoubleExpOp   -> simp_op (text "call float64 [mscorlib]System.Math::Exp(float64)")
	DoubleLogOp   -> simp_op (text "call float64 [mscorlib]System.Math::Log(float64)")
	DoubleSqrtOp  -> simp_op (text "call float64 [mscorlib]System.Math::Sqrt(float64)")
          
	DoubleSinOp   -> simp_op (text "call float64 [mscorlib]System.Math::Sin(float64)")
	DoubleCosOp   -> simp_op (text "call float64 [mscorlib]System.Math::Cos(float64)")
	DoubleTanOp   -> simp_op (text "call float64 [mscorlib]System.Math::Tan(float64)")
          
	DoubleAsinOp   -> simp_op (text "call float64 [mscorlib]System.Math::Asin(float64)")
	DoubleAcosOp   -> simp_op (text "call float64 [mscorlib]System.Math::Acos(float64)")
	DoubleAtanOp   -> simp_op (text "call float64 [mscorlib]System.Math::Atan(float64)")
          
	DoubleSinhOp   -> simp_op (text "call float64 [mscorlib]System.Math::Sinh(float64)")
	DoubleCoshOp   -> simp_op (text "call float64 [mscorlib]System.Math::Cosh(float64)")
	DoubleTanhOp   -> simp_op (text "call float64 [mscorlib]System.Math::Tanh(float64)")
          
	DoublePowerOp  -> simp_op (text "call float64 [mscorlib]System.Math::Pow(float64, float64)")

    -- Integer (and related...) ops: bail out to support routines
--	IntegerNegOp  	   -> simp_op (text "call" <+> ilxSuppMeth repInteger "IntegerNegOp" [] [repInt, repByteArray])
--	Addr2IntegerOp     -> simp_op (text "call" <+> ilxSuppMeth repInteger "Addr2IntegerOp" [] [repAddr])
	IntegerAddOp  	   -> simp_op (text "call" <+> ilxSuppMeth repInteger "IntegerAddOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerSubOp  	   -> simp_op (text "call" <+> ilxSuppMeth repInteger "IntegerSubOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerMulOp  	   -> simp_op (text "call" <+> ilxSuppMeth repInteger "IntegerMulOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerGcdOp  	   -> simp_op (text "call" <+> ilxSuppMeth repInteger "IntegerGcdOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerQuotRemOp   -> simp_op (text "call" <+> ilxSuppMeth repIntegerPair "IntegerQuotRemOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerDivModOp    -> simp_op (text "call" <+> ilxSuppMeth repIntegerPair "IntegerDivModOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerIntGcdOp    -> simp_op (text "call" <+> ilxSuppMeth repInt "IntegerIntGcdOp" [] [repInt, repByteArray, repInt])
	IntegerDivExactOp  -> simp_op (text "call" <+> ilxSuppMeth repInteger "IntegerDivExactOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerQuotOp  	   -> simp_op (text "call" <+> ilxSuppMeth repInteger "IntegerQuotOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerRemOp   	   -> simp_op (text "call" <+> ilxSuppMeth repInteger "IntegerRemOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerCmpOp   	   -> simp_op (text "call" <+> ilxSuppMeth repInt "IntegerCmpOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerCmpIntOp    -> simp_op (text "call" <+> ilxSuppMeth repInt "IntegerCmpIntOp" [] [repInt, repByteArray, repInt])
	Integer2IntOp      -> simp_op (text "call" <+> ilxSuppMeth repInt "Integer2IntOp" [] [repInt, repByteArray])
	Integer2WordOp     -> simp_op (text "call" <+> ilxSuppMeth repWord "Integer2WordOp" [] [repInt, repByteArray])
	Int2IntegerOp  	   -> simp_op (text "call" <+> ilxSuppMeth repInteger "Int2IntegerOp" [] [repInt])
	Word2IntegerOp     -> simp_op (text "call" <+> ilxSuppMeth repInteger "Word2IntegerOp" [] [repWord])
	IntegerToInt64Op   -> simp_op (text "call" <+> ilxSuppMeth repAddr "IntegerToInt64Op" [] [repInt,repByteArray])
	Int64ToIntegerOp   -> simp_op (text "call" <+> ilxSuppMeth repInteger "Int64ToIntegerOp" [] [repInt64])
	IntegerToWord64Op  -> simp_op (text "call" <+> ilxSuppMeth repWord64 "IntegerToWord64Op" [] [repInt,repByteArray])
	Word64ToIntegerOp  -> simp_op (text "call" <+> ilxSuppMeth repInteger "Word64ToIntegerOp" [] [repWord64])

	IndexOffForeignObjOp_Char    -> simp_op (text "add ldind.u1")
	IndexOffForeignObjOp_Int     -> simp_op (text "ldc.i4 4 mul add ldind.i4")
	IndexOffForeignObjOp_Word    -> simp_op (text "ldc.i4 4 mul add ldind.u4")
	IndexOffForeignObjOp_Addr    ->  warn_op "IndexOffForeignObjOp Addr: assuing 32 bit architecture" (simp_op (text "ldc.i4 4 mul add ldind.i  "))
	IndexOffForeignObjOp_Float   -> simp_op (text "ldc.i4 4 mul add ldind.r4")
	IndexOffForeignObjOp_Double  -> simp_op (text "ldc.i4 8 mul add ldind.r8")
	IndexOffForeignObjOp_Int64   -> simp_op (text "ldc.i4 8 mul add ldind.i8")
	IndexOffForeignObjOp_Word64  -> simp_op (text "ldc.i4 8 mul add ldind.u8")

	IndexOffAddrOp_Char    -> simp_op (text "add ldind.u1")
	IndexOffAddrOp_Int     -> simp_op (text "ldc.i4 4 mul add ldind.i4")
	IndexOffAddrOp_Word    -> simp_op (text "ldc.i4 4 mul add ldind.u4")
	IndexOffAddrOp_Addr    -> warn_op "IndexOffAddrOp Addr: assuing 32 bit architecture" (simp_op (text "ldc.i4 4 mul add ldind.i"))
	IndexOffAddrOp_Float   -> simp_op (text "ldc.i4 4 mul add ldind.r4")
	IndexOffAddrOp_Double  -> simp_op (text "ldc.i4 8 mul add ldind.r8")
	IndexOffAddrOp_Int64   -> simp_op (text "ldc.i4 8 mul add ldind.i8")
	IndexOffAddrOp_Word64  -> simp_op (text "ldc.i4 8 mul add ldind.u8")


	WriteOffAddrOp_Char   -> ty1_arg4_op (\sty addr n v s -> addr <+> n <+> text "add" <+> v <+> text "stind.u1")
	WriteOffAddrOp_Int    -> ty1_arg4_op (\sty addr n v s -> addr <+> n <+> text "ldc.i4 4 mul add" <+> v <+> text "stind.i4")
	WriteOffAddrOp_Word   -> ty1_arg4_op (\sty addr n v s -> addr <+> n <+> text "ldc.i4 4 mul add" <+> v <+> text "stind.u4")
	WriteOffAddrOp_Addr   -> ty1_arg4_op (\sty addr n v s -> addr <+> n <+> text "ldc.i4 4 mul add" <+> v <+> text "stind.i")
	WriteOffAddrOp_Float  -> ty1_arg4_op (\sty addr n v s -> addr <+> n <+> text "ldc.i4 4 mul add" <+> v <+> text "stind.r4")
	WriteOffAddrOp_Double -> ty1_arg4_op (\sty addr n v s -> addr <+> n <+> text "ldc.i4 8 mul add" <+> v <+> text "stind.r8")
	WriteOffAddrOp_Int64  -> ty1_arg4_op (\sty addr n v s -> addr <+> n <+> text "ldc.i4 8 mul add" <+> v <+> text "stind.i8")
	WriteOffAddrOp_Word64 -> ty1_arg4_op (\sty addr n v s -> addr <+> n <+> text "ldc.i4 8 mul add" <+> v <+> text "stind.u8")
                  {-    Addr# -> Int# -> Char# -> State# s -> State# s -} 

	ReadOffAddrOp_Char   -> simp_op (text "add ldind.u1")
	ReadOffAddrOp_Int    -> simp_op (text "ldc.i4 4 mul add ldind.i4")
	ReadOffAddrOp_Word   -> simp_op (text "ldc.i4 4 mul add ldind.u4")
	ReadOffAddrOp_Addr   -> simp_op (text "ldc.i4 4 mul add ldind.i")
	ReadOffAddrOp_Float  -> simp_op (text "ldc.i4 4 mul add ldind.r4")
	ReadOffAddrOp_Double -> simp_op (text "ldc.i4 8 mul add ldind.r8")
	ReadOffAddrOp_Int64  -> simp_op (text "ldc.i4 8 mul add ldind.i8")
	ReadOffAddrOp_Word64 -> simp_op (text "ldc.i4 8 mul add ldind.u8")
                  {-    Addr# -> Int# -> Char# -> State# s -> State# s -} 

	IndexByteArrayOp_Char  	   -> simp_op (text "ldelem.u1")
	IndexByteArrayOp_Int   	   -> simp_op (text "ldelem.i4")
	IndexByteArrayOp_Word  	   -> simp_op (text "ldelem.u4")
	IndexByteArrayOp_Addr  	   -> simp_op (text "ldelem.u")
	IndexByteArrayOp_Float 	   -> simp_op (text "ldelem.r4")
	IndexByteArrayOp_Double    -> simp_op (text "ldelem.r8")
	IndexByteArrayOp_StablePtr -> simp_op (text "ldelem.i4")
	IndexByteArrayOp_Int64     -> simp_op (text "ldelem.i8")
	IndexByteArrayOp_Word64    -> simp_op (text "ldelem.u8")

                 {- ByteArr# -> Int# -> Char# -}

	WriteByteArrayOp_Char  	   -> simp_op (text "stelem.u1")
	WriteByteArrayOp_Int   	   -> simp_op (text "stelem.i4")
	WriteByteArrayOp_Word  	   -> simp_op (text "stelem.u4")
	WriteByteArrayOp_Addr  	   -> simp_op (text "stelem.u")
	WriteByteArrayOp_Float 	   -> simp_op (text "stelem.r4")
	WriteByteArrayOp_Double    -> simp_op (text "stelem.r8")
	WriteByteArrayOp_StablePtr -> simp_op (text "stelem.i4")
	WriteByteArrayOp_Int64     -> simp_op (text "stelem.i8")
	WriteByteArrayOp_Word64    -> simp_op (text "stelem.u8")

                 {- MutByteArr# s -> Int# -> Char# -> State# s -> State# s -}

            {- should be monadic??? -}
	ReadByteArrayOp_Char   	  -> simp_op (text "ldelem.u1")
	ReadByteArrayOp_Int    	  -> simp_op (text "ldelem.i4")
	ReadByteArrayOp_Word   	  -> simp_op (text "ldelem.u4")
	ReadByteArrayOp_Addr   	  -> simp_op (text "ldelem.u")
	ReadByteArrayOp_Float  	  -> simp_op (text "ldelem.r4")
	ReadByteArrayOp_Double 	  -> simp_op (text "ldelem.r8")
	ReadByteArrayOp_StablePtr -> simp_op (text "ldelem.i4")
	ReadByteArrayOp_Int64     -> simp_op (text "ldelem.i8")
	ReadByteArrayOp_Word64    -> simp_op (text "ldelem.u8")
                 {-   MutByteArr# s -> Int# -> State# s -> (# State# s, Char# #) -}

            {- should be monadic??? -}
	NewByteArrayOp_Char   	 -> simp_op (text "newarr [mscorlib]System.Byte")
--	NewByteArrayOp_Int    	 -> simp_op (text "newarr [mscorlib]System.Int32")
--	NewByteArrayOp_Word   	 -> simp_op (text "newarr [mscorlib]System.UInt32")
--	NewByteArrayOp_Addr   	 -> simp_op (text "newarr [mscorlib]System.UInt64")
--	NewByteArrayOp_Float  	 -> simp_op (text "newarr [mscorlib]System.Single")
--	NewByteArrayOp_Double 	 -> simp_op (text "newarr [mscorlib]System.Double")
--	NewByteArrayOp_StablePtr -> simp_op (text "newarr [mscorlib]System.UInt32")
--      NewByteArrayOp_Int64     -> simp_op (text "newarr [mscorlib]System.Int64")  TODO: there is no unique for this one -}
--      NewByteArrayOp_Word64    -> simp_op (text "newarr  [mscorlib]System.UInt64") -}
                  {- Int# -> State# s -> (# State# s, MutByteArr# s #) -}

	UnsafeFreezeByteArrayOp ->   ty1_op (\ty1  -> text "nop ")
                  {- MutByteArr# s -> State# s -> (# State# s, ByteArr# #) -}
	SizeofByteArrayOp  -> simp_op (text "ldlen")
                  {- ByteArr# -> Int# -}

	SameMutableByteArrayOp -> ty1_op (\ty1  -> text "ceq " <+> ilxMkBool)
                 {- MutByteArr# s -> MutByteArr# s -> Bool -}
	SizeofMutableByteArrayOp -> ty1_op (\ty1  -> text "ldlen")
                 {- MutByteArr# s -> Int# -}

	SameMutVarOp -> ty2_op (\ty1 ty2 -> text "ceq " <+> ilxMkBool)
                 {- MutVar# s a -> MutVar# s a -> Bool -}
	NewMutVarOp -> ty2_op (\ty1 ty2 -> text "newobj void" <+> repMutVar ty1 ty2 <+> text "::.ctor(!0)")
                 {- a -> State# s -> (# State# s, MutVar# s a #) -}
	ReadMutVarOp -> ty2_op (\ty1 ty2 ->  text "ldfld !0" <+> repMutVar ty1 ty2 <+> text "::contents")
                 {-  MutVar# s a -> State# s -> (# State# s, a #) -}
	WriteMutVarOp -> ty2_op (\ty1 ty2 -> text "stfld !0" <+> repMutVar ty1 ty2 <+> text "::contents")
                 {- MutVar# s a -> a -> State# s -> State# s -}

	NewArrayOp -> ty2_op (\ty1 ty2 -> text "call !!0[] " <+> ilxSupportClass <+> text "::newArray<" <> ty1 <> text ">(" <> repInt <> text ", !!0)")
                 {- Int# -> a -> State# s -> (# State# s, MutArr# s a #) -}
	IndexArrayOp -> ty1_op (\ty1 -> text "ldelem.ref")
                 {- Array# a -> Int# -> (# a #) -}
	WriteArrayOp -> ty2_op (\ty1 ty2 -> text "stelem.ref")
                 {- MutArr# s a -> Int# -> a -> State# s -> State# s -}
	ReadArrayOp -> ty2_op (\ty1 ty2 -> text "ldelem.ref")
                 {- MutArr# s a -> Int# -> State# s -> (# State# s, a #) -}
	UnsafeFreezeArrayOp -> ty2_op (\ty1 ty2 -> text "nop")
                 {-   MutArr# s a -> State# s -> (# State# s, Array# a #) -}
	UnsafeThawArrayOp -> ty2_op (\ty1 ty2 -> text "nop")
                 {-  Array# a -> State# s -> (# State# s, MutArr# s a #) -}

	SameMutableArrayOp -> ty2_op (\ty1 ty2 -> text "ceq " <+> ilxMkBool)
                 {- MutArr# s a -> MutArr# s a -> Bool -}


	RaiseOp -> ty2_op (\ty1 ty2 -> text "throw")
	CatchOp -> ty2_op (\ty1 ty2 -> 
			text "call" <+> ilxSuppMeth ilxMethA "catch" [ty1,ty2] [text "(func (unit) --> !!0)", text "(func (!!1) --> (func (unit) --> !!0))"])
	                    {-        (State# RealWorld -> (# State# RealWorld, a #) )
	                           -> (b -> State# RealWorld -> (# State# RealWorld, a #) ) 
	                           -> State# RealWorld
	                           -> (# State# RealWorld, a #) 
	                     -} 

	BlockAsyncExceptionsOp -> ty1_op (\ty1 -> 
		text "call" <+> ilxSuppMeth ilxMethA "blockAsyncExceptions" [ty1] [text "(func (unit) --> !!0)"])

                {-     (State# RealWorld -> (# State# RealWorld, a #))
                    -> (State# RealWorld -> (# State# RealWorld, a #))
                -}

	UnblockAsyncExceptionsOp -> ty1_op (\ty1 -> 
		text "call" <+> ilxSuppMeth ilxMethA "unblockAsyncExceptions" [ty1] [text "(func (unit) --> !!0)"])

                {-
		    State# RealWorld -> (# State# RealWorld, a #))
                    -> (State# RealWorld -> (# State# RealWorld, a #))
                -}
 
	NewMVarOp -> ty2_op (\sty ty -> 
		text "newobj void " <+> repMVar ty <+> text "::.ctor()")
                 {- State# s -> (# State# s, MVar# s a #) -}

	TakeMVarOp -> ty2_op (\sty ty -> 
		text "call" <+> ilxSuppMeth ilxMethA "takeMVar" [ty] [repMVar ilxMethA])
                  {-  MVar# s a -> State# s -> (# State# s, a #) -}

	-- These aren't yet right
        TryTakeMVarOp -> ty2_op (\sty ty -> 
		text "call" <+> ilxSuppMeth ilxMethA "tryTakeMVar" [ty] [repMVar ilxMethA])
                  {-  MVar# s a -> State# s -> (# State# s, a #) -}

	TryPutMVarOp -> ty2_op (\sty ty -> 
		text "call" <+> ilxSuppMeth ilxMethA "tryPutMVar" [ty] [repMVar ilxMethA])
                  {-  MVar# s a -> State# s -> (# State# s, a #) -}

	PutMVarOp -> ty2_op (\sty ty -> 
		text "call" <+> ilxSuppMeth (text "void") "putMVar" [ty] [repMVar ilxMethA, ilxMethA])
                   {- MVar# s a -> a -> State# s -> State# s -}

	SameMVarOp -> ty2_op (\sty ty -> text "ceq " <+> ilxMkBool)
                   {- MVar# s a -> MVar# s a -> Bool -}

--	TakeMaybeMVarOp -> ty2_op (\sty ty -> 
--		text "call" <+> ilxSuppMeth (ilxUnboxedPairRep repInt ilxMethA) "tryTakeMVar" [ty] [repMVar ilxMethA])
--              {- MVar# s a -> State# s -> (# State# s, Int#, a #) -}

	IsEmptyMVarOp -> ty2_op (\sty ty -> 
		text "call" <+> ilxSuppMeth repInt "isEmptyMVar" [ty] [repMVar ilxMethA])
               {- MVar# s a -> State# s -> (# State# s, Int# #) -}

	DataToTagOp -> ty1_op (\ty1 -> 
		text "call" <+> ilxSuppMeth repInt "dataToTag" [ty1] [ilxMethA])
               {- a -> Int# -}

	TagToEnumOp -> ty1_op (\ty1 -> 
		text "call" <+> ilxSuppMeth ilxMethA "tagToEnum" [ty1] [repInt])
               {- Int# -> a -}

	MakeStablePtrOp -> ty1_op (\ty1 -> text "newobj void class " <+> prelGHCReference <+> text "PrelGHC_StablePtrzh<" <> ty1 <> text ">::.ctor(!0)")
                 {-   a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #) -}

	DeRefStablePtrOp -> ty1_op (\ty1 ->  text "ldfld !0 class " <+> prelGHCReference <+> text "PrelGHC_StablePtrzh<" <> ty1  <> text ">::contents")
                 {-  StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #) -}

	EqStablePtrOp -> ty1_op (\ty1 -> text "ceq " <+> ilxMkBool)
                 {-  StablePtr# a -> StablePtr# a -> Int# -}

	MkWeakOp -> ty3_op (\ty1 ty2 ty3 ->  text "call" <+> ilxMethodRef (repWeak (text "!!1")) (text "class " <+> prelGHCReference <+> text "PrelGHC_Weakzh") "bake" [ty1,ty2,ty3] [text "!!0", text "!!1", text "!!2"])
                 {- o -> b -> c -> State# RealWorld -> (# State# RealWorld, Weak# b #) -}

	DeRefWeakOp -> ty1_op (\ty1 ->  text "call" <+> ilxMethodRef (ilxUnboxedPairRep repInt (text "!0")) (repWeak ty1) "deref" [] [])
	FinalizeWeakOp -> ty1_op (\ty1 ->  text "call" <+> ilxMethodRef (ilxUnboxedPairRep repInt (text "(func (unit) --> class '()')")) (repWeak ty1) "finalizer" [] [])
                   {-    Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, 
	State# RealWorld -> (# State# RealWorld, Unit #)) #) -}

	MkForeignObjOp -> simp_op (text "nop /* newobj void class " <+> prelGHCReference <+> text "PrelGHC_ForeignObjzh::.ctor(void *) */")
	WriteForeignObjOp -> simp_op (text "pop /* stfld void * class " <+> prelGHCReference <+> text "PrelGHC_ForeignObjzh::contents */ ")
             -- (ForeignObjToAddrOp -> simp_op (text "ldfld void * class " <+> prelGHCReference <+> text "PrelGHC_ForeignObjzh::contents"))
	YieldOp -> simp_op (text "call class [mscorlib]System.Threading.Thread class [mscorlib]System.Threading.Thread::get_CurrentThread() 
                                call void class [mscorlib]System.Threading.Thread::Suspend()")
	MyThreadIdOp -> simp_op (text "call default  class [mscorlib]System.Threading.Thread class [mscorlib]System.Threading.Thread::get_CurrentThread() ")
	KillThreadOp -> simp_op (text "call instance void class [mscorlib]System.Threading.Thread::Abort(class [mscorlib]System.Object) ")
              {-   ThreadId# -> a -> State# RealWorld -> State# RealWorld -}

	ForkOp -> ty1_op (\ty -> text "call default class [mscorlib]System.Threading.Thread " <+> ilxSupportClass <+> text "::fork<" <> ty <> text ">(thunk<!0>)")

	ParOp ->  warn_op "ParOp" (simp_op (text "/* ParOp skipped... */ pop ldc.i4 0"))

	DelayOp -> simp_op (text "call void class [mscorlib]System.Threading.Thread::Sleep(int32) ")
                 {-    Int# -> State# s -> State# s -}

	WaitReadOp  -> warn_op "WaitReadOp" (simp_op (text "/* WaitReadOp skipped... */ pop"))
   	WaitWriteOp -> warn_op "WaitWriteOp" (simp_op (text " /* WaitWriteOp skipped... */ pop"))

		-- DEFAULT CASE
	other -> \env args -> ilxComment (simp_op (text "Unknown primop!:" <+> pprId op) env args)



ty1_op  op env ((StgTypeArg ty1):rest)  = 
       vcat (ilxMapPlaceArgs 1 pushArg env rest) $$ op (pprIlxTypeR env (deepIlxRepType ty1)) 
--ty1_after1_op  op env (h:(StgTypeArg ty1):rest)  = 
--       vcat (ilxMapPlaceArgs 0 pushArg env [h]) $$  vcat (ilxMapPlaceArgs 2 pushArg env rest) $$ op (pprIlxTypeR env (deepIlxRepType ty1)) 
ty2_op  op env ((StgTypeArg ty1):(StgTypeArg ty2):rest)  = 
       vcat (ilxMapPlaceArgs 2 pushArg env rest) $$ op (pprIlxTypeR env (deepIlxRepType ty1)) (pprIlxTypeR env (deepIlxRepType ty2))

ty3_op  op env ((StgTypeArg ty1):(StgTypeArg ty2):(StgTypeArg ty3):rest)  = 
       vcat (ilxMapPlaceArgs 2 pushArg env rest) $$ 
      op (pprIlxTypeR env (deepIlxRepType ty1)) 
         (pprIlxTypeR env (deepIlxRepType ty2))
         (pprIlxTypeR env (deepIlxRepType ty3))

ty1_arg4_op  op env [(StgTypeArg ty1), a1, a2, a3, a4] = 
       op (pprIlxTypeR env (deepIlxRepType ty1)) 
          (hd2 (ilxMapPlaceArgs 1 pushArg env [a1]) )
          (hd2 (ilxMapPlaceArgs 2 pushArg env [a2]) )
          (hd2 (ilxMapPlaceArgs 3 pushArg env [a3]) )
          (hd2 (ilxMapPlaceArgs 4 pushArg env [a4]) )

hd (h:t) = h
hd2 (h:t) = h

simp_op  op env args    = vcat (ilxMapPlaceArgs 0 pushArg env args) $$ op
warn_op  warning f args = trace ("WARNING! IlxGen cannot translate primop " ++ warning) (f args)
\end{code}

%************************************************************************
%*									*
\subsection{C Calls}
%*									*
%************************************************************************

\begin{code}

-- We eliminate voids in and around an IL C Call.  We don't yet emit PInvoke stubs.
-- We also do some type-directed translation for pinning Haskell-managed blobs
-- of data as we throw them across the boundary.
ilxCCall env (CCall (StaticTarget c) casm gc cconv) args ret_ty =
   ilxComment (text "C call <+> pprCLabelString c") <+> 
	vcat [vcat (ilxMapPlaceArgs 0 pushCArg env args),
              text "call" <+> retdoc <+> text "class " <+> prelGHCReference <+> text "PrelGHC::" <+> pprCLabelString c  <+> pprTypeArgs pprIlxTypeR env ty_args
                    <+> pprCValArgTys pprIlxTypeL env (map deepIlxRepType (filter (not. isVoidIlxRepType) (map stgArgType tm_args))) ]
  where 
    retdoc = 
          if isVoidIlxRepType ret_ty then text "void" 
          else pprIlxTypeR env (deepIlxRepType ret_ty)
    (ty_args,tm_args) = splitTyArgs1 args 


hasTyCon (TyConApp tc _) tc2 = tc == tc2
hasTyCon _  _ = False

isByteArrayCArgTy ty = hasTyCon ty byteArrayPrimTyCon || hasTyCon ty mutableByteArrayPrimTyCon
isByteArrayCArg v = isByteArrayCArgTy (deepIlxRepType (idType v))
pinCCallArg v = isByteArrayCArg v 

ilxAddrOfPinnedByteArr = text "ldc.i4 0 ldelema unsigned int8"

pushCArg env arg@(StgVarArg v) | isByteArrayCArg v = pushArg env arg <+> text "dup stloc" <+> squotes (ilxEnvQualifyByExact env (ppr v) <> text "pin") <+> ilxAddrOfPinnedByteArr
pushCArg env arg | otherwise = pushArg env arg

pprCValArgTys f env tys = parens (pprSepWithCommas (pprCValArgTy f env) tys)
pprCValArgTy f env ty | isByteArrayCArgTy ty = text "void *" <+> ilxComment (text "interior pointer into ByteArr#")
pprCValArgTy f env ty | otherwise = f env ty


foldR            :: (a -> b -> b) -> [a] -> b -> b
-- foldR _ [] z     =  z
-- foldR f (x:xs) z =  f x (foldR f xs z) 
{-# INLINE foldR #-}
foldR k xs z = go xs
	     where
	       go []     = z
	       go (y:ys) = y `k` go ys

\end{code}


%
\section{Generate .NET extended IL}

\begin{code}
module IlxGen( ilxGen ) where

#include "HsVersions.h"

import Char	( ord, chr )
import StgSyn
import Id	( idType, idName, isDeadBinder, idArity )
import Var	( Var, Id, TyVar, isId, isTyVar, tyVarKind, tyVarName )
import VarEnv
import VarSet   ( isEmptyVarSet )
import TyCon	( TyCon,  tyConPrimRep, isUnboxedTupleTyCon, tyConDataCons, 
		  tyConTyVars, isDataTyCon, isAlgTyCon, tyConArity
		)
import Type	( liftedTypeKind, openTypeKind, unliftedTypeKind,
		  isUnLiftedType, isTyVarTy, mkTyVarTy, predTypeRep, pprType,
		  splitForAllTys, splitFunTys, applyTy, applyTys, eqKind, tyVarsOfTypes
		)
import TypeRep	( Type(..) )
import DataCon	( isUnboxedTupleCon, dataConTyCon, dataConRepType, dataConRepArgTys, DataCon(..) )
import Literal	( Literal(..) )
import PrelNames	-- Lots of keys
import PrimOp		( PrimOp(..) )
import ForeignCall	( CCallConv(..), ForeignCall(..), CCallSpec(..), CCallTarget(..), DNCallSpec(..) )
import TysWiredIn	( mkTupleTy, tupleCon )
import PrimRep		( PrimRep(..) )
import Name		( nameModule, nameOccName, isExternalName, isInternalName, NamedThing(getName) )
import Subst   		( substTyWith )

import Module		( Module, PackageName, ModuleName, moduleName, 
                          modulePackage, basePackage,
			  isHomeModule, isVanillaModule,
                          pprModuleName, mkHomeModule, mkModuleName
			)

import UniqFM
import BasicTypes	( Boxity(..) )
import CStrings		( CLabelString, pprCLabelString )
import Outputable
import Char		( ord )
import List		( partition, elem, insertBy,any  )
import UniqSet

import TysPrim  ( foreignObjPrimTyCon, weakPrimTyCon, byteArrayPrimTyCon, mutableByteArrayPrimTyCon )

-- opt_SimplDoEtaReduction is used to help with assembly naming conventions for different
-- versions of compiled Haskell code.  We add a ".O" to all assembly and module 
-- names when this is set (because that's clue that -O was set).  
-- One day this will be configured by the command line.
import DynFlags	( opt_InPackage, opt_SimplDoEtaReduction )

import Util		( lengthIs, equalLength )

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
  =  vcat [ text ".module '" <> (ppr (moduleName mod)) <> hscOptionQual <> text "o'",
	    text ".assembly extern 'mscorlib' {}",
	    vcat (map (ilxImportPackage topenv) (uniqSetToList import_packages)),
            vcat (map (ilxImportModule topenv) (uniqSetToList import_modules)),
            vcat (map (ilxImportTyCon topenv) (uniqSetToList import_tycons)),
            vcat (map (ilxImportCCall topenv) (map snd (ufmToList import_ccalls))),
            vcat (map (ilxTyCon topenv) data_tycons),
            vcat (map (ilxBindClosures topenv) binds),
	    ilxTopBind mod topenv toppairs
	 ]
    where
      binds = map fst binds_w_srts
      toppairs = ilxPairs binds
      topenv = extendIlxEnvWithTops (emptyIlxEnv False mod) mod toppairs
 	-- Generate info from class decls as well
      (import_packages,import_modules,import_tycons,import_ccalls) = importsBinds topenv binds (importsPrelude emptyImpInfo)
      data_tycons = filter isDataTyCon tycons
\end{code}

%************************************************************************
%*									*
\subsection{Find Imports}
%*									*
%************************************************************************

\begin{code}

importsBinds :: IlxEnv -> [StgBinding] -> ImportsInfo -> ImportsInfo
importsBinds env binds = foldR (importsBind env) binds

importsNone :: ImportsInfo -> ImportsInfo
importsNone sofar = sofar

importsBind :: IlxEnv -> StgBinding -> ImportsInfo -> ImportsInfo
importsBind env (StgNonRec _ b rhs) = importsRhs env rhs.importsVar env b
importsBind env (StgRec _ pairs) = foldR (\(b,rhs) -> importsRhs env rhs . importsVar env b) pairs

importsRhs :: IlxEnv -> StgRhs -> ImportsInfo -> ImportsInfo
importsRhs env (StgRhsCon _ con args) = importsDataCon env con . importsStgArgs env args
importsRhs env (StgRhsClosure _ _ _ _ args body) = importsExpr env body. importsVars env args

importsExpr :: IlxEnv -> StgExpr -> ImportsInfo -> ImportsInfo
importsExpr env (StgLit _) = importsNone
importsExpr env (StgApp f args) = importsVar env f.importsStgArgs env args
importsExpr env (StgConApp con args) = importsDataCon env con.importsStgArgs env args
importsExpr env (StgOpApp (StgFCallOp (CCall (CCallSpec (StaticTarget c) cc _)) _) args rty)
  = addCCallInfo (c,cc, map stgArgType tm_args, rty) . importsStgArgs env args
  where 
    (ty_args,tm_args) = splitTyArgs1 args 

importsExpr env (StgOpApp _ args res_ty) = importsType env res_ty. importsStgArgs env args


importsExpr env (StgSCC _ expr) = importsExpr env expr
importsExpr env (StgCase scrut _ _ bndr _ alts)
  = importsExpr env scrut. imports_alts alts. importsVar env bndr
   where
    imports_alts (StgAlgAlts _ alg_alts deflt) 	-- The Maybe TyCon part is dealt with 
						-- by the case-binder's type
      = foldR imports_alg_alt alg_alts .  imports_deflt deflt
       where
        imports_alg_alt (con, bndrs, _, rhs)
	  = importsExpr env rhs . importsDataCon env con. importsVars env bndrs

    imports_alts (StgPrimAlts _ alg_alts deflt)
      = foldR imports_prim_alt alg_alts . imports_deflt deflt
       where
        imports_prim_alt (_, rhs) = importsExpr env rhs
    imports_deflt StgNoDefault = importsNone
    imports_deflt (StgBindDefault rhs) = importsExpr env rhs


importsExpr env (StgLetNoEscape _ _ bind body) = importsExpr env (StgLet bind body)
importsExpr env (StgLet bind body)
  = importsBind env bind .  importsExpr env body

importsApp env v args = importsVar env v.  importsStgArgs env args
importsStgArgs env args = foldR (importsStgArg env) args

importsStgArg :: IlxEnv -> StgArg -> ImportsInfo -> ImportsInfo
importsStgArg env (StgTypeArg ty) = importsType env ty
importsStgArg env (StgVarArg v) = importsVar env v
importsStgArg env _ = importsNone

importsVars env vs = foldR (importsVar env) vs
importsVar env v = importsName env (idName v). importsType env (idType v)

importsName env n
   | isInternalName n = importsNone
   | ilxEnvModule env == nameModule n = importsNone
   | isHomeModule (nameModule n) =  addModuleImpInfo (moduleName (nameModule n))
-- See HACK below
   | isVanillaModule (nameModule n)  && not inPrelude =  importsPrelude
   | isVanillaModule (nameModule n)  && inPrelude =   addModuleImpInfo (moduleName (nameModule n))
-- End HACK
   | otherwise = addPackageImpInfo (modulePackage (nameModule n))


importsPrelude | inPrelude = addModuleImpInfo (mkModuleName "PrelGHC")
	       | otherwise = addPackageImpInfo basePackage


importsType :: IlxEnv -> Type -> ImportsInfo -> ImportsInfo
importsType env ty = importsType2 env (deepIlxRepType ty)

importsType2 :: IlxEnv -> Type -> ImportsInfo -> ImportsInfo
importsType2 env (AppTy f x) =  importsType2 env f . importsType2 env x
importsType2 env (TyVarTy _) = importsNone
importsType2 env (TyConApp tc args) =importsTyCon env tc . importsTypeArgs2 env args
importsType2 env (FunTy arg res) =  importsType env arg .  importsType2 env res
importsType2 env (ForAllTy tv body_ty) =  importsType2 env body_ty
importsType2 env (NoteTy _ ty) = importsType2 env ty
importsType2 _ _ = panic "IlxGen.lhs: importsType2 ty"
importsTypeArgs2 env tys = foldR (importsType2 env) tys

importsDataCon env dcon = importsTyCon env (dataConTyCon dcon)

importsTyCon env tc | (not (isDataTyCon tc) || 
                   isInternalName (getName tc) || 
                   ilxEnvModule env == nameModule (getName tc)) = importsNone
importsTyCon env tc | otherwise = importsName env (getName tc) . addTyConImpInfo tc .
				    foldR (importsTyConDataCon env) (tyConDataCons tc)


importsTyConDataCon :: IlxEnv -> DataCon -> ImportsInfo -> ImportsInfo
importsTyConDataCon env dcon = foldR (importsTyConDataConType env) (filter (not . isVoidIlxRepType) (dataConRepArgTys dcon))

importsTyConDataConType :: IlxEnv -> Type -> ImportsInfo -> ImportsInfo
importsTyConDataConType env ty = importsTyConDataConType2 env (deepIlxRepType ty)

importsTyConDataConType2 :: IlxEnv -> Type -> ImportsInfo -> ImportsInfo
importsTyConDataConType2 env (AppTy f x) =  importsTyConDataConType2 env f . importsTyConDataConType2 env x
importsTyConDataConType2 env (TyVarTy _) = importsNone
importsTyConDataConType2 env (TyConApp tc args) = importsTyConDataConTypeTyCon env tc . importsTyConDataConTypeArgs2 env args
importsTyConDataConType2 env (FunTy arg res) = importsTyConDataConType env arg .  importsTyConDataConType2 env res
importsTyConDataConType2 env (ForAllTy tv body_ty) = importsTyConDataConType2 env body_ty
importsTyConDataConType2 env (NoteTy _ ty) = importsTyConDataConType2 env ty
importsTyConDataConType2 _ _ = panic "IlxGen.lhs: importsTyConDataConType2 ty"
importsTyConDataConTypeArgs2 env tys = foldR (importsTyConDataConType2 env) tys

importsTyConDataConTypeTyCon env tc | (not (isDataTyCon tc) || 
                   isInternalName (getName tc) || 
                   ilxEnvModule env == nameModule (getName tc)) = importsNone
importsTyConDataConTypeTyCon env tc | otherwise = importsName env (getName tc)


type StaticCCallInfo = (CLabelString,CCallConv,[Type],Type)
type ImportsInfo = (UniqSet PackageName, UniqSet ModuleName, UniqSet TyCon, UniqFM StaticCCallInfo)
   -- (Packages, Modules, Datatypes, Imported CCalls)

emptyImpInfo :: ImportsInfo
emptyImpInfo = (emptyUniqSet, emptyUniqSet, emptyUniqSet, emptyUFM)
addPackageImpInfo p (w,x,y,z) = (addOneToUniqSet w p, x, y,z)
addModuleImpInfo m (w,x,y,z) = (w, addOneToUniqSet x m, y,z)
addTyConImpInfo tc (w,x,y,z) = (w, x, addOneToUniqSet y tc,z)
addCCallInfo info@(nm,a,b,c) (w,x,y,z) = (w, x, y,addToUFM z nm info)

ilxImportTyCon :: IlxEnv -> TyCon -> SDoc
ilxImportTyCon env tycon | isDataTyCon tycon = ilxTyConDef True env tycon
ilxImportTyCon _ _ | otherwise =  empty

ilxImportPackage :: IlxEnv -> PackageName -> SDoc
ilxImportPackage _ p = text ".assembly extern" <+> singleQuotes (ppr p <> hscOptionQual) <+> text "{ }"

ilxImportModule :: IlxEnv -> ModuleName -> SDoc
ilxImportModule _ m = text ".module extern" <+> singleQuotes (ppr m <> hscOptionQual <> text "o")

-- Emit a P/Invoke declaration for the imported C function
-- TODO: emit the right DLL name
ilxImportCCall :: IlxEnv -> StaticCCallInfo -> SDoc
ilxImportCCall env (c,cc,args,ret) = 
    text ".method static assembly pinvokeimpl" <+> 
    parens (doubleQuotes (text "HSstd_cbits.dll") <+> text "cdecl") <+> retdoc <+> singleQuotes (pprCLabelString c) <+> 
    pprCValArgTys ilxTypeL env (map deepIlxRepType (filter (not. isVoidIlxRepType) args)) <+> 
    text "unmanaged preservesig { }"
  where 
    retdoc = 
          if isVoidIlxRepType ret then text "void" 
          else ilxTypeR env (deepIlxRepType ret)


\end{code}

%************************************************************************
%*									*
\subsection{Type declarations}
%*									*
%************************************************************************

\begin{code}


ilxTyCon :: IlxEnv -> TyCon -> SDoc
ilxTyCon env tycon =  ilxTyConDef False env tycon

-- filter to get only dataTyCons?
ilxTyConDef importing env tycon = 
	vcat [empty $$ line,
	      text ".classunion" <+> (if importing then text "import" else empty) <+> tycon_ref <+> tyvars_text <+> super_text   <+> alts_text]
   where
     tycon_ref =  nameReference env (getName tycon)  <> (ppr tycon)
     super_text = if importing then empty else text "extends thunk" <> angleBrackets (text "class" <+> tycon_ref)
     tyvars = tyConTyVars tycon
     (ilx_tvs, _) = categorizeTyVars tyvars
     alts_env = extendIlxEnvWithFormalTyVars env ilx_tvs 
     tyvars_text = pprTyVarBinders alts_env ilx_tvs 
     alts = vcat (map (pprIlxDataCon alts_env) (tyConDataCons tycon))
     alts_text = nest 2 (braces alts)

pprIlxDataCon env dcon =
        text ".alternative" <+> pprId dcon <+> 
        parens (pprSepWithCommas (ilxTypeL env) (map deepIlxRepType (filter (not. isVoidIlxRepType) (dataConRepArgTys dcon))))
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
ilxRhsClosures _ (_, StgRhsCon _ _ _)
  = empty

ilxRhsClosures env (bndr, StgRhsClosure _ _ fvs upd args rhs)
  = vcat [ilxExprClosures next_env rhs,

	 empty $$ line,
	 kind_text <+> singleQuotes cloname <+>  free_vs_text,
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
			  otherwise -> ".closure"
    kind_text = text kind_of_thing 
		
    cloname = ilxEnvQualifyByModule env (ppr bndr)
    next_env = ilxPlaceStgRhsClosure env bndr 
    (free_vs_text,env_with_fvs) = pprFreeBinders next_env fvs


    closure_sig_text =     
      vcat [ text "()",
             (case args of 
               []        -> empty
               otherwise -> args_text),
             text "-->" <+>  rty_text]

    (args_text,env_with_args) = pprArgBinders env_with_fvs args

        -- Find the type returned, from the no. of args and the type of "bndr"
    rty_text = 
      case retType env_with_fvs (idIlxRepType bndr) args of
       Just (env,ty) -> 
          if isVoidIlxRepType ty  then  (text "void")
          else ilxTypeR env ty 
       Nothing -> trace "WARNING!  IlxGen.trace could not find return type - see generated ILX for context where this occurs." (text "// Could not find return type:" <+> ilxTypeR env_with_fvs (idIlxRepType bndr)<+> text ", non representation: " <+> ilxTypeR env_with_fvs (idType bndr))

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

pprIlxLocal env (LocalId v,_) = ilxTypeL env (idIlxRepType v) <+> pprId v
pprIlxLocal env (LocalSDoc (ty,doc,pin),_) = ilxTypeL env (deepIlxRepType ty) <+> (if pin then text "pinned" else empty) <+> doc


pprFreeBinders env fvs 
    = (ilx_tvs_text <+> vs_text, env2)
    where   
       (free_ilx_tvs, _,free_vs) = categorizeVars fvs
       real_free_vs = filter (not . isVoidIlxRepId) free_vs
        -- ignore the higher order type parameters for the moment
       env1 = extendIlxEnvWithFreeTyVars env free_ilx_tvs 
       ilx_tvs_text = pprTyVarBinders env1 free_ilx_tvs
       vs_text = parens (pprSepWithCommas ppr_id real_free_vs)
       ppr_id v = ilxTypeL env1 (idIlxRepType v) <+> pprId v 
       env2 = extendIlxEnvWithFreeVars env1 real_free_vs 

pprIdBinder env v = parens (ilxTypeL env (idIlxRepType v) <+> pprId v)

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
  | isVoidIlxRepId arg = (text "()", extendIlxEnvWithArgs env [arg])
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
ilxExprLocals env (StgOpApp (StgFCallOp fcall _) args _) 
     = concat (ilxMapPlaceArgs 0 ilxCCallArgLocals env args)
ilxExprLocals _ _  = []

-- Generate locals to use for pinning arguments as we cross the boundary
-- to C.
ilxCCallArgLocals env (StgVarArg v) | pinCCallArg v = 
   [(LocalSDoc (idType v, ilxEnvQualifyByExact env (ppr v) <> text "pin", True), Nothing)]
ilxCCallArgLocals _ _ | otherwise = []

ilxBindLocals env (StgNonRec _ b rhs) = [(LocalId b,Just (env, rhs))]
ilxBindLocals env (StgRec _ pairs)    = map (\(x,y) -> (LocalId x,Just (env, y))) pairs

ilxAltsLocals env (StgAlgAlts  _ alts deflt) = ilxDefltLocals env deflt ++ concat (ilxMapPlaceAlts ilxAlgAltLocals env alts)
ilxAltsLocals env (StgPrimAlts _ alts deflt) = ilxDefltLocals env deflt ++ concat (ilxMapPlaceAlts ilxPrimAltLocals env alts)

ilxAlgAltLocals env (_, bndrs, _, rhs) = map (\x -> (LocalId x,Nothing)) (filter (\v -> isId v && not (isDeadBinder v)) bndrs) ++ ilxExprLocals env rhs
ilxPrimAltLocals env (_, rhs)          = ilxExprLocals env rhs

ilxDefltLocals _ StgNoDefault 	= []
ilxDefltLocals env (StgBindDefault rhs) = ilxExprLocals (ilxPlaceStgBindDefault env) rhs

--------------
ilxExprClosures :: IlxEnv -> StgExpr -> SDoc
ilxExprClosures env (StgApp _ args)
  = vcat (ilxMapPlaceArgs 0 (ilxArgClosures) env args)  -- get strings
ilxExprClosures env (StgConApp _ args)
  = vcat (ilxMapPlaceArgs 0 (ilxArgClosures) env args) -- get strings
ilxExprClosures env (StgOpApp _ args _)
  = vcat (ilxMapPlaceArgs 0 (ilxArgClosures) env args) -- get strings
ilxExprClosures env (StgLet bind body)
  = ilxBindClosures env bind $$ ilxExprClosures (extendIlxEnvWithBinds env (ilxPairs1 bind)) body
ilxExprClosures env (StgLetNoEscape _ _ bind body)  -- TO DO????
  = ilxBindClosures env bind $$ ilxExprClosures (extendIlxEnvWithBinds env (ilxPairs1 bind)) body
ilxExprClosures env (StgCase scrut _ _ _ _ alts)
  = ilxExprClosures (ilxPlaceStgCaseScrut env) scrut $$ ilxAltsClosures env alts 
ilxExprClosures env (StgLit lit) 
  = ilxGenLit env lit 
ilxExprClosures _ _ 
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
ilxDefltClosures _ StgNoDefault	  = empty

ilxArgClosures env (StgLitArg lit) = ilxGenLit env lit 
ilxArgClosures _ _ = empty



ilxGenLit env (MachStr fs) 
  = vcat [text ".field static assembly char "  <+> singleQuotes nm <+> text "at" <+> nm <> text "L",
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

ilxExpr (IlxEEnv env _) (StgApp fun args) sequel
  = ilxFunApp env fun args (isReturn sequel) $$ ilxSequel sequel

-- ilxExpr eenv (StgLit lit) sequel
ilxExpr (IlxEEnv env _) (StgLit lit) sequel
  = pushLit env lit $$ ilxSequel sequel

-- ilxExpr eenv (StgConApp data_con args) sequel
ilxExpr (IlxEEnv env _) (StgConApp data_con args) sequel
  = text " /* ilxExpr:StgConApp */ " <+>  ilxConApp env data_con args $$ ilxSequel sequel

-- ilxExpr eenv (StgPrimApp primop args _) sequel
ilxExpr (IlxEEnv env _) (StgOpApp (StgFCallOp fcall _) args ret_ty) sequel
  = ilxFCall env fcall args ret_ty $$ ilxSequel sequel

ilxExpr (IlxEEnv env _) (StgOpApp (StgPrimOp primop) args ret_ty) sequel
  = ilxPrimOpTable primop args env $$ ilxSequel sequel

--BEGIN TEMPORARY
-- The following are versions of a peephole optimizations for "let t = \[] t2[fvs] in t"
-- I think would be subsumed by a general treatmenet of let-no-rec bindings??
ilxExpr eenv@(IlxEEnv env _) (StgLet (StgNonRec _ bndr (StgRhsClosure _ _ _ _ [] rhs)) (StgApp fun [])) sequel 
              | (bndr == fun && null (ilxExprLocals env rhs)) -- TO DO???
  = ilxExpr eenv rhs sequel
ilxExpr eenv@(IlxEEnv env _) (StgLetNoEscape _ _ (StgNonRec _ bndr (StgRhsClosure _ _ _ _ [] rhs)) (StgApp fun [])) sequel 
              | (bndr == fun && null (ilxExprLocals env rhs)) -- TO DO???
  = ilxExpr eenv rhs sequel
--END TEMPORARY

ilxExpr eenv (StgLet bind body) sequel
  = ilxBind eenv bind $$ ilxExpr eenv body sequel


ilxExpr eenv (StgLetNoEscape _ _ bind body) sequel -- TO DO???
  = ilxBind eenv bind $$ ilxExpr eenv body sequel

-- StgCase: Special case 1 to avoid spurious branch.
ilxExpr eenv@(IlxEEnv env live) (StgCase (StgApp fun args) live_in_case _live_in_alts bndr _ alts) sequel
  = vcat [ilxWipe env (uniqSetToList (live `minusUniqSet` live_in_case)),
	  ilxFunApp (ilxPlaceStgCaseScrut env) fun args False,
          --ilxWipe env (uniqSetToList (live_in_case `minusUniqSet` _live_in_alts)),
	  --ilxAlts (IlxEEnv env _live_in_alts) bndr alts sequel
	  ilxAlts (IlxEEnv env live_in_case) bndr alts sequel
    ]

-- StgCase: Special case 2 to avoid spurious branch.
ilxExpr eenv@(IlxEEnv env live) (StgCase (StgOpApp (StgPrimOp primop) args ret_ty) live_in_case _live_in_alts bndr _ alts) sequel
  = vcat [ilxWipe env (uniqSetToList (live `minusUniqSet` live_in_case)),
	  ilxPrimOpTable primop args (ilxPlaceStgCaseScrut env),
          --ilxWipe env (uniqSetToList (live_in_case `minusUniqSet` _live_in_alts)),
	  --ilxAlts (IlxEEnv env _live_in_alts) bndr alts sequel
	  ilxAlts (IlxEEnv env live_in_case) bndr alts sequel
    ]

-- StgCase: Normal case.
ilxExpr eenv@(IlxEEnv env live) (StgCase scrut live_in_case _live_in_alts bndr _ alts) sequel
  = vcat [ilxWipe env (uniqSetToList (live `minusUniqSet` live_in_case)),
	  ilxExpr (IlxEEnv (ilxPlaceStgCaseScrut env) live_in_case) scrut (Jump join_lbl),
	  ilxLabel join_lbl,
          --ilxWipe env (uniqSetToList (live_in_case `minusUniqSet` _live_in_alts)),
	  --ilxAlts (IlxEEnv env _live_in_alts) bndr alts sequel
	  ilxAlts (IlxEEnv env live_in_case) bndr alts sequel
    ]
  where
    join_lbl = mkJoinLabel bndr

ilxExpr _ _ _ 
  = panic "ilxExpr:  Patterns not matched:(IlxEEnv _ _) (StgSCC _ _) _ (IlxEEnv _ _) (StgLam _ _ _) _"


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
	  Just Local  -> text "ldloca " <+> pprId id <+> text "initobj.any" <+> (ilxTypeL env (idIlxRepType id))
	  Just Arg   -> text "deadarg " <+> pprId id <+> text "," <+> (ilxTypeL env (idIlxRepType id))
	  Just (CloVar _)  -> ilxComment (text "not yet wiping closure variable" <+> pprId id )
	  _ -> ilxComment (text "cannot wipe non-local/non-argument" <+> pprId id )
  where 
      

----------------------

ilxAlts :: IlxEEnv -> Id -> StgCaseAlts -> Sequel -> SDoc
ilxAlts (IlxEEnv env live) bndr alts sequel
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
            = vcat [text "castdata" <+> sep [ilxTypeR env scrut_rep_ty <> comma,
		  			     ilxConRef env data_con],
 		do_alg_alt (IlxEEnv (ilxPlaceAlt env i) live) alt
	      ]

    do_alg_alts alts deflt
	= vcat [text "datacase" <+> sep [ilxTypeR env scrut_rep_ty,text ",",
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
			      ilxTypeR alt_env scrut_rep_ty <> text "::fld" <> integer reduced_fld_no]
      | otherwise 
      = text "lddata" <+> sep [ilxTypeR alt_env scrut_rep_ty <> comma, 
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
    vcat [pushId env fun,ilxFunAppAfterPush env fun args tail_call]

ilxFunAppAfterPush env fun args tail_call 
  =	-- For example:
        --	ldloc f		function of type forall a. a->a
	--	ldloc x		arg of type Int
	--	.tail callfunc <Int32> (!0) --> !0
	--
    vcat [ilxFunAppArgs env 0 (idIlxRepType fun) args tail_call known_clo]
  where
    known_clo :: KnownClosure
    known_clo =
      case lookupIlxBindEnv env fun of
	  Just (_, StgRhsClosure  _ _ _ Updatable _ _)   -> Nothing 
	  Just (place, StgRhsClosure  _ _ fvs _ args _)  -> Just (place,fun,args,fvs)
	  _ -> Nothing -- trace (show fun ++ " --> " ++ show (idArity fun))

type KnownClosure = Maybe (  IlxEnv	-- Of the binding site of the function
			   , Id		-- The function
			   , [Var]	-- Binders
			   , [Var])	-- Free vars of the closure

-- Push as many arguments as ILX allows us to in one go, and call the function
-- Recurse until we're done.
-- The function is already on the stack
ilxFunAppArgs :: IlxEnv
	      -> Int		-- Number of args already pushed (zero is a special case;
				--	otherwise used only for place generation)
	      -> Type		-- Type of the function
	      -> [StgArg]	-- The arguments
	      -> Bool		-- True <=> tail call please
	      -> KnownClosure	-- Information about the function we're calling
	      -> SDoc

ilxFunAppArgs env num_sofar funty args tail_call known_clo
 =   vcat [vcat (ilxMapPlaceArgs num_sofar pushArgWithVoids env now_args),
	   call_instr <+> (if num_sofar == 0 then text "() /* first step in every Haskell app. is to a thunk */ " else empty)
                     <+> now_args_text
                     <+> text "-->" 
                     <+> later_ty_text,
           later
          ]
  where
    now_args_text = 
      case now_arg_tys of
        [] -> empty
        _ -> hsep (map (pprIlxArgInfo env_after_now_tyvs) now_arg_tys)

    later_ty_text
        | isVoidIlxRepType later_ty = text "void"
        | otherwise = ilxTypeR env_after_now_tyvs later_ty

    (now_args,now_arg_tys,env_after_now_tyvs,later_args,later_ty) = 
	case args of
          (StgTypeArg v:rest) -> get_type_args ilxBestTypeArity args env funty
          _ -> get_term_args 0 ilxBestTermArity args env funty

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
            let rest_ty = deepIlxRepType (substTyWith [tv] [v] rem_funty) in 
            let (now,now_tys,env3,later,later_ty) = get_type_args (max - 1) rest env rest_ty in 
            let arg_ty = mkTyVarTy tv in 
            (arg:now,(arg,arg_ty):now_tys,env2, later, later_ty)
          else 
             get_type_args max rest env rem_funty  -- ? subst??
    get_type_args _ (StgTypeArg _:_) _ _ = trace "IlxGen Internal Error: get_type_args could not get ForAllTy for corresponding arg" ([],[],env,[],funty)
    get_type_args _ args env funty = ([],[],env,args,funty)

    get_term_args n max args env (NoteTy _ ty)
       -- Skip NoteTy types 
       = trace "IlxGen Internal Error: non representation type passed to get_term_args" (get_term_args n max args env ty)
    get_term_args n 0 args env funty
       -- Stop if we've hit the maximum number of ILX arguments to apply n one hit.
       = ([],[],env,args,funty)
    get_term_args n max args env funty
      | (case known_clo of
           Just (_,_,needed,_) -> needed `lengthIs` n
           Nothing -> False)
       -- Stop if we have the optimal number for a direct call
       = ([],[],env,args,funty)
    get_term_args _ _ (args@(StgTypeArg _:_)) env funty 
       -- Stop if we hit a type arg.
       = ([],[],env,args,funty)
    get_term_args n max (h:t) env (FunTy dom ran)
       -- Take an argument.
       = let (now,now_tys,env2,later,later_ty) = get_term_args (n+1) (max - 1) t env ran in 
         (h:now, (h,dom):now_tys,env2,later,later_ty)
    get_term_args _ max (h:t) env funty = trace "IlxGen Internal Error: get_term_args could not get FunTy or ForAllTy for corresponding arg" ([],[],env,[],funty)
    get_term_args _ max args env funty = ([],[],env,args,funty)

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
        Just (known_env,fun,needed,fvs) | (equalLength needed now_args) && 
                                          all (\x -> elemIlxTyEnv x env) free_ilx_tvs -> 
           vcat [text "callclo class",
                 nameReference env (idName fun) <+> singleQuotes (ilxEnvQualifyByModule env (ppr fun)),
                 pprTypeArgs ilxTypeR env (map mkTyVarTy free_ilx_tvs)]
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
    angleBrackets (ilxTypeR env (deepIlxRepType arg) <+> ilxComment (text "actual for tyvar")) <+> text "<class [mscorlib] System.Object>" 
pprIlxArgInfo env (_,ty) =  
    parens (ilxTypeL env ty)


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
    clotext = pprIlxNamedTyConApp env (ilxEnvQualifyByModule env (ppr bndr)) (map mkTyVarTy free_ilx_tvs)

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
    clotext = pprIlxNamedTyConApp env (ilxEnvQualifyByModule env (ppr bndr)) (map mkTyVarTy free_ilx_tvs)



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
	  text "newclo" <+> pprIlxNamedTyConApp env (ilxEnvQualifyByModule env (ppr bndr)) (map mkTyVarTy free_ilx_tvs),
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
  =  ilxTypeL env ty <+> moduleReference env mod <+> pprId mod <> text "::" <> pprId id

ilxTopRhsStorage mod env (bndr, StgRhsClosure _ _ _ _ _ _) 
  =   text ".field public static " <+> ilxTypeL env bndTy <+> pprId bndr
  where
    bndTy = idIlxRepType bndr
ilxTopRhsStorage mod env (bndr, StgRhsCon _ _ _) 
  =   text ".field public static " <+> ilxTypeL env bndTy <+> pprId bndr
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
   /* if voids then  text "ldunit" else */ ilxComment (text "pushId: void rep skipped")
pushId_aux _ env var 
  = case lookupIlxVarEnv env var of
	  Just Arg    -> text "ldarg"    <+> pprId var
	  Just (CloVar n) -> text "ldenv" <+> int n
	  Just Local  -> text "ldloc"    <+> pprId var
	  Just (Top m)  -> 
             vcat [ilxComment (text "pushId (Top) " <+> pprId m), 
                   text "ldsfld" <+> ilxTypeL env (idIlxRepType var)
                      <+> moduleReference env m <+> pprId (moduleName m) <> text "::" <> pprId var]

	  Nothing ->  
             vcat [ilxComment (text "pushId (import) " <+> pprIlxTopVar env var), 
                   text "ldsfld" <+> ilxTypeL env (idIlxRepType var) 
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
pushLit env (MachNullAddr)  = text "ldc.i4 0"
pushLit env (MachLabel l _) = trace "WARNING: Cannot compile MachLabel to ILX in IlxGen.lhs" (text "// MachLabel!!!  Not valid in ILX!!")

pprIlxTopVar env v
  | isExternalName n = (nameReference env n) <> pprId (nameModule n) <> text "::" <> singleQuotes (ppr (nameModule n) <> text "_" <> ppr (nameOccName n))
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
  = isUnboxedTupleTyCon tc && null (filter (not. isVoidIlxRepType) tys)
isVoidIlxRepType _ = False

isVoidIlxRepId id = isVoidIlxRepType (idType id)



-- Get rid of all NoteTy and NewTy artifacts
deepIlxRepType :: Type -> Type
deepIlxRepType (FunTy l r)
  = FunTy (deepIlxRepType l) (deepIlxRepType r)

deepIlxRepType ty@(TyConApp tc tys) 
  =        -- collapse UnboxedTupleTyCon down when it contains VoidRep types.
	   -- e.g. 	(# State#, Int#, Int# #)  ===>   (# Int#, Int# #)
            if isUnboxedTupleTyCon tc then 
               let tys' = map deepIlxRepType (filter (not. isVoidIlxRepType) tys) in 
               case tys' of
                  [h] -> h
                  _ -> mkTupleTy Unboxed (length tys') tys'
            else 
              TyConApp tc (map deepIlxRepType tys)
deepIlxRepType (AppTy f x)     = AppTy (deepIlxRepType f) (deepIlxRepType x)
deepIlxRepType (ForAllTy b ty) = ForAllTy b (deepIlxRepType ty)
deepIlxRepType (NoteTy   _ ty) = deepIlxRepType ty
deepIlxRepType (PredTy p)      = deepIlxRepType (predTypeRep p)
deepIlxRepType ty@(TyVarTy tv) = ty

idIlxRepType id = deepIlxRepType (idType id)

--------------------------
-- Some primitive type constructors are not thunkable.
-- Everything else needs to be marked thunkable.
ilxTypeL :: IlxEnv -> Type -> SDoc

ilxTypeL env ty | isUnLiftedType ty ||  isVoidIlxRepType ty = ilxTypeR env ty
ilxTypeL env ty = text "thunk" <> angleBrackets (ilxTypeR env ty)


--------------------------
-- Print non-thunkable version of type.
--

ilxTypeR :: IlxEnv -> Type -> SDoc
ilxTypeR env ty | isVoidIlxRepType ty = text "/* unit skipped */"
ilxTypeR env ty@(AppTy f _) | isTyVarTy f    = ilxComment (text "type app:" <+> pprType ty) <+> (text "class [mscorlib]System.Object")
ilxTypeR env ty@(AppTy f x)     = trace "ilxTypeR: should I be beta reducing types?!" (ilxComment (text "ilxTypeR: should I be beta reducing types?!") <+> ilxTypeR env (applyTy f x))
ilxTypeR env (TyVarTy tv)       = ilxTyVar env tv

-- The following is a special rule for types constructed out of 
-- higher kinds, e.g. Monad f or Functor f.  
--
-- The code below is not as general as it should be, but as I
-- have no idea if this approach will even work, I'm going to
-- just try it out on some simple cases arising from the prelude.
ilxTypeR env ty@(TyConApp tc (h:t)) | isAlgTyCon tc && null (tyConTyVars tc)
   = ilxComment (text "what on earth? 2") <+> (ilxTypeR env (TyConApp tc t))
ilxTypeR env ty@(TyConApp tc (h:t)) | isAlgTyCon tc && not (isIlxTyVar (hd (tyConTyVars tc)))
   = ilxTypeR env (TyConApp tc t)
ilxTypeR env (TyConApp tc args) = ilxTyConApp env tc args

  -- nb. the only legitimate place for VoidIlxRepTypes to occur in normalized IlxRepTypes 
  -- is on the left of an arrow
  --  We could probably eliminate all but a final occurrence of these.
ilxTypeR env (FunTy arg res)| isVoidIlxRepType res 
    = pprIlxFunTy (ilxTypeL env arg) (text "void")
ilxTypeR env (FunTy arg res)
    = pprIlxFunTy (ilxTypeL env arg) (ilxTypeR env res)

ilxTypeR env ty@(ForAllTy tv body_ty) | isIlxTyVar tv
  = parens (text "forall" <+> pprTyVarBinders env' [tv] <+> nest 2 (ilxTypeR env' body_ty))
    where
       env' = extendIlxEnvWithFormalTyVars env [tv]

ilxTypeR env ty@(ForAllTy tv body_ty) | otherwise
  = ilxComment (text "higher order type var " <+> pprId tv) <+>
    pprIlxFunTy (text "class [mscorlib]System.Object") (ilxTypeR env body_ty)

ilxTypeR env (NoteTy _ ty)       
   = trace "WARNING! non-representation type given to ilxTypeR: see generated ILX for context where this occurs"
     (vcat [text "/* WARNING! non-representation type given to ilxTypeR! */",
           ilxTypeR env ty ])

pprIlxFunTy dom ran = parens (hsep [text "func",parens dom,text "-->", ran])

ilxTyConApp env tcon args =
   case lookupUFM tyPrimConTable (getUnique tcon) of
	Just f  -> f args env
        Nothing -> 
            (if isUnboxedTupleTyCon tcon then pprIlxUnboxedTupleTyConApp else pprIlxBoxedTyConApp)
              env tcon args

pprIlxTyCon env tcon = nameReference env (getName tcon) <> ppr tcon
pprIlxUnboxedTupleTyConApp env tcon args 
  = text "/* unboxed */ value class" <+> pprIlxTyCon env tcon' <> pprTypeArgs ilxTypeL env non_void
  where 
   non_void = filter (not . isVoidIlxRepType) args
   tcon' = dataConTyCon (tupleCon Unboxed (length non_void)) 
pprIlxBoxedTyConApp env tcon args 
  = pprIlxNamedTyConApp env (pprIlxTyCon env tcon) args
pprIlxNamedTyConApp env tcon_text args 
  = text "class" <+> tcon_text <> pprTypeArgs ilxTypeR env args

-- Returns e.g: <Int32, Bool>
-- Void-sized type arguments are _always_ eliminated, everywhere.
-- If the type constructor is an unboxed tuple type then it should already have
-- been adjusted to be the correct constructor.
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
                         text ":" <+> ilxTypeR env (tyVarKind tv)) <+>
             ilxComment (text "omitted")
             -- parens (text "class [mscorlib]System.Object" <+> pprId tv)


pprTyVarBinder_aux env tv = 
   ilxComment (text "tyvar" <+> pprId tv <+> text ":" <+> 
                        ilxTypeR env (tyVarKind tv)) <+>
             (text "class [mscorlib]System.Object")

-- Only a subset of Haskell types can be generalized using the type quantification
-- of ILX
isIlxForAllKind h = 
        ( h `eqKind` liftedTypeKind) ||
        ( h `eqKind` unliftedTypeKind) ||
        ( h `eqKind` openTypeKind)

isIlxTyVar v = isTyVar v && isIlxForAllKind (tyVarKind v)

categorizeVars fvs = (ilx_tvs, non_ilx_tvs, vs)
         where
           (tvs, vs) = partition isTyVar fvs
           (ilx_tvs, non_ilx_tvs) = categorizeTyVars tvs

categorizeTyVars tyvs = partition isIlxTyVar tyvs

pprValArgTys ppr_ty env tys = parens (pprSepWithCommas (ppr_ty env) tys)

pprId id = singleQuotes (ppr id)

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

ilxTyVar  env tv
  = go 0 (ilxEnvTyEnv env)
  where
    go n [] 		    
      = pprTrace "ilxTyVar" (pprId tv <+> text "tv_env = { "
           <+> pprSepWithCommas
	         (\x -> pprId x <+> text ":" <+> ilxTypeR env (tyVarKind x)) 
               (ilxEnvTyEnv env) <+> text "}") 
        (char '!' <> pprId tv) 
    go n (x:xs)
      = {- pprTrace "go" (ppr (tyVarName tv) <+> ppr (tyVarName x)) -}
        (if tyVarName x== tyVarName tv then  char '!' <> int n <+> ilxComment (char '!' <> pprId tv) 
         else go (n+1) xs)

emptyIlxEnv :: Bool -> Module -> IlxEnv
emptyIlxEnv trace mod = IlxEnv (mod, emptyIlxTyEnv, emptyVarEnv, emptyVarEnv, (ppr mod,empty),trace)

nextPlace place sdoc = place <> sdoc
usePlace  place sdoc = place <> sdoc

ilxEnvModule (IlxEnv (m, _, _,  _, _,_)) = m
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

ilxEnvTyEnv :: IlxEnv -> IlxTyEnv
ilxEnvTyEnv (IlxEnv (_, tv_env, _,_,_,_)) = tv_env 
elemIlxTyEnv var env = elem var (ilxEnvTyEnv env )
elemIlxVarEnv var (IlxEnv (_, _, id_env,_,_,_)) = elemVarEnv var id_env 
lookupIlxVarEnv (IlxEnv (_, _, id_env,_,_,_)) var = lookupVarEnv id_env var
lookupIlxBindEnv (IlxEnv (_, _, _, bind_env,_,_)) var = lookupVarEnv bind_env var

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

hscOptionQual = text ".i_"

nameReference env n
  | isInternalName n = empty
  | ilxEnvModule env == nameModule n  = text ""
  | isHomeModule (nameModule n)   = moduleNameReference (moduleName (nameModule n))
-- HACK: no Vanilla modules should be around, but they are!!  This
-- gets things working for the scenario "standard library linked as one
-- assembly with multiple modules + a one module program running on top of this"
-- Same applies to all other mentions of Vailla modules in this file
  | isVanillaModule (nameModule n)  && not inPrelude =  basePackageReference
  | isVanillaModule (nameModule n)  && inPrelude =   moduleNameReference (moduleName (nameModule n))
-- end hack
  | otherwise = packageReference (modulePackage (nameModule n))

packageReference p = brackets (singleQuotes (ppr p  <> hscOptionQual))
moduleNameReference m = brackets ((text ".module") <+> (singleQuotes (pprModuleName m <> hscOptionQual <> text "o")))

moduleReference env m
  | ilxEnvModule env   == m = text ""
  | isHomeModule m = moduleNameReference (moduleName m)
  -- See hack above
  | isVanillaModule m && not inPrelude =  basePackageReference
  | isVanillaModule m && inPrelude =  moduleNameReference (moduleName m)
  -- end hack
  | otherwise  =  packageReference (modulePackage m)

basePackageReference = packageReference basePackage
inPrelude = basePackage == opt_InPackage

------------------------------------------------
-- This code is copied from absCSyn/CString.lhs,
-- and modified to do the correct thing!  It's
-- still a mess though.  Also, still have to do the
-- right thing for embedded nulls.

pprFSInILStyle :: FastString -> SDoc
pprFSInILStyle fs = doubleQuotes (text (stringToC (unpackFS fs)))

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
  		        ilxTyConApp env tycon' rep_ty_args',
                        text "::.ctor",
                        pprValArgTys ilxTypeR formal_env' (map deepIlxRepType formal_arg_tys')
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
		nest 2 (ilxTyConApp env tycon rep_ty_args <> comma),
		nest 2 (ilxConRef env data_con)
	  ]
        ]
 where
   tycon   = dataConTyCon data_con 
   rep_ty_args = map deepIlxRepType ty_args
   (ty_args,tm_args) = if isAlgTyCon tycon then splitTyArgs (tyConTyVars tycon) args  else splitTyArgs1 args

-- Split some type arguments off, throwing away the higher kinded ones for the moment.
-- Base the higher-kinded checks off a corresponding list of formals.
splitTyArgs :: [Var] 		-- Formals
	    -> [StgArg]		-- Actuals
	    -> ([Type], [StgArg])
splitTyArgs (htv:ttv) (StgTypeArg h:t) 
   | isIlxTyVar htv = ((h:l), r) 
   | otherwise = trace "splitTyArgs: threw away higher kinded type arg" (l, r) 
   where (l,r) = splitTyArgs ttv t 
splitTyArgs _ l = ([],l)
 
-- Split some type arguments off, where none should be higher kinded
splitTyArgs1 :: [StgArg] -> ([Type], [StgArg])
splitTyArgs1 (StgTypeArg ty : args) = (ty:tys, args')
				    where
				      (tys, args') = splitTyArgs1 args
splitTyArgs1 args		    = ([], args)

ilxConRef env data_con
 | isUnboxedTupleCon data_con
    = let data_con' = tupleCon Unboxed (length non_void_args)in 
      pprId data_con' <> arg_text
 | otherwise 
    = pprId data_con <> arg_text
  where
    arg_text = pprValArgTys ilxTypeL env' (map deepIlxRepType non_void_args)
    non_void_args = filter (not . isVoidIlxRepType) arg_tys
    (tyvars, tau_ty) = splitForAllTys (dataConRepType data_con)
    (arg_tys, _)     = splitFunTys tau_ty
    env' 	     = formalIlxEnv env tyvars




\end{code}


%************************************************************************
%*									*
\subsection{PrimOps and Prim Representations}				*
%************************************************************************

\begin{code}

ilxPrimApp env op 	       args ret_ty = ilxPrimOpTable op args env


type IlxTyFrag = IlxEnv -> SDoc
ilxType s env = text s

ilxLift ty env = text "thunk" <> angleBrackets (ty env)

ilxTypeSeq :: [IlxTyFrag] -> IlxTyFrag
ilxTypeSeq ops env = hsep (map (\x -> x env) ops)

tyPrimConTable :: UniqFM ([Type] -> IlxTyFrag)
tyPrimConTable = 
  listToUFM [(addrPrimTyConKey, 	(\_ -> repAddr)),
--	     (fileStreamPrimTyConKey, 	(\_ -> repFileStream)),
	     (foreignObjPrimTyConKey, 	(\_ -> repForeign)),
             (stablePtrPrimTyConKey, 	(\[ty] -> repStablePtr {- (ilxTypeL2 ty) -})),
             (stableNamePrimTyConKey, 	(\[ty] -> repStableName {- (ilxTypeL2 ty) -} )),
             (charPrimTyConKey, 	(\_ -> repChar)),
	     (wordPrimTyConKey, 	(\_ -> repWord)),
	     (byteArrayPrimTyConKey,	(\_ -> repByteArray)),
	     (intPrimTyConKey, 	        (\_ -> repInt)),
	     (int64PrimTyConKey,	(\_ -> repInt64)),
	     (word64PrimTyConKey,	(\_ -> repWord64)),
	     (floatPrimTyConKey, 	(\_ -> repFloat)),
	     (doublePrimTyConKey,	(\_ -> repDouble)),
              -- These can all also accept unlifted parameter types so we explicitly lift.
	     (arrayPrimTyConKey, 	(\[ty] -> repArray (ilxTypeL2 ty))),
	     (mutableArrayPrimTyConKey, 	(\[_, ty] -> repMutArray (ilxTypeL2 ty))),
	     (weakPrimTyConKey, 	(\[ty] -> repWeak (ilxTypeL2 ty))),
	     (mVarPrimTyConKey, 	(\[_, ty] -> repMVar (ilxTypeL2 ty))),
	     (mutVarPrimTyConKey, 	(\[ty1, ty2] -> repMutVar (ilxTypeL2 ty1) (ilxTypeL2 ty2))),
	     (mutableByteArrayPrimTyConKey,	(\_ -> repByteArray)),
	     (threadIdPrimTyConKey,	(\_ -> repThread)),
	     (bcoPrimTyConKey,	(\_ -> repBCO))
	     ]

ilxTypeL2 :: Type -> IlxTyFrag
ilxTypeL2 ty env = ilxTypeL env ty
ilxTypeR2 :: Type -> IlxTyFrag
ilxTypeR2 ty env = ilxTypeR env ty

ilxMethTyVarA = ilxType "!!0"
ilxMethTyVarB = ilxType "!!1"
prelGHCReference :: IlxTyFrag
prelGHCReference env =
   if ilxEnvModule env == mkHomeModule (mkModuleName "PrelGHC") then empty
   else if inPrelude then moduleNameReference (mkModuleName "PrelGHC")
   else basePackageReference

prelBaseReference :: IlxTyFrag
prelBaseReference env =
   if ilxEnvModule env == mkHomeModule (mkModuleName "PrelBase") then empty
   else if inPrelude then moduleNameReference (mkModuleName "PrelBase")
   else basePackageReference

repThread = ilxType "class [mscorlib]System.Threading.Thread /* ThreadId# */ "
repByteArray = ilxType "unsigned int8[] /* ByteArr# */ "
--repFileStream = text "void * /* FileStream# */ "  -- text "class [mscorlib]System.IO.FileStream"
repInt = ilxType "int32"
repWord = ilxType "unsigned int32"
repAddr =ilxType "/* Addr */ void *"
repInt64 = ilxType "int64"
repWord64 = ilxType "unsigned int64"
repFloat = ilxType "float32"
repDouble = ilxType "float64"
repChar = ilxType "/* Char */ unsigned int8"
repForeign = ilxTypeSeq [ilxType "class ",prelGHCReference,ilxType "PrelGHC_Foreignzh"]
repInteger = ilxUnboxedPairRep repInt repByteArray
repIntegerPair = ilxUnboxedQuadRep repInt repByteArray repInt repByteArray
repArray ty = ilxTypeSeq [ty,ilxType "[]"]
repMutArray ty = ilxTypeSeq [ty,ilxType "[]"]
repMVar ty = ilxTypeSeq [ilxType "class ",prelGHCReference,ilxType "PrelGHC_MVarzh",ilxTyParams [ty]]
repMutVar _ ty2 = ilxTypeSeq [ilxType "class ",prelGHCReference,ilxType "PrelGHC_MutVarzh",ilxTyParams [ty2]]
repWeak ty1 = ilxTypeSeq [ilxType "class ",prelGHCReference,ilxType "PrelGHC_Weakzh",ilxTyParams [ty1]]
repStablePtr {- ty1 -} = ilxTypeSeq [ilxType "class ",prelGHCReference,ilxType "PrelGHC_StablePtrzh" {- ,ilxTyParams [ty1] -} ]
repStableName {- ty1 -}  = ilxTypeSeq [ilxType "class ",prelGHCReference,ilxType "PrelGHC_StableNamezh" {- ,ilxTyParams [ty1] -} ]
classWeak = ilxTypeSeq [ilxType "class ",prelGHCReference,ilxType "PrelGHC_Weakzh"]
repBCO = ilxTypeSeq [ilxType "class ",prelGHCReference,ilxType "PrelGHC_BCOzh"]

ilxTyPair l r = ilxTyParams [l,r]
ilxTyTriple l m r = ilxTyParams [l,m,r]
ilxTyQuad l m1 m2 r = ilxTyParams [l,m1,m2,r]
ilxUnboxedEmptyRep = ilxTypeSeq [ilxType "value class",prelGHCReference,ilxType "PrelGHC_Z1H"]
ilxUnboxedPairRep l r = ilxTypeSeq [ilxType "value class",prelGHCReference,ilxType "PrelGHC_Z2H",ilxTyPair l r]
ilxUnboxedTripleRep l m r = ilxTypeSeq [ilxType "value class",prelGHCReference,ilxType "PrelGHC_Z3H",ilxTyTriple l m r]
ilxUnboxedQuadRep l m1 m2 r = ilxTypeSeq [ilxType "value class",prelGHCReference,ilxType "PrelGHC_Z4H",ilxTyQuad l m1 m2 r]

ilxTyIO b = ilxTypeSeq [ilxType "(func ( /* unit skipped */ ) --> ", b, ilxType ")"]

ilxTyParams :: [IlxTyFrag] -> IlxTyFrag
ilxTyParams [] env = empty
ilxTyParams l env = angleBrackets (ilxTyParamsAux l env)
  where
   ilxTyParamsAux [] env = empty
   ilxTyParamsAux [h] env = h env
   ilxTyParamsAux (h:t) env = h env <> text "," <+> ilxTyParamsAux t env
   ilxTyParams [] env = empty


type IlxOpFrag = IlxEnv -> SDoc
ilxOp :: String -> IlxOpFrag
ilxOp s env = text s
ilxOpSeq :: [IlxOpFrag] -> IlxOpFrag
ilxOpSeq ops env = hsep (map (\x -> x env) ops)

ilxParams :: [IlxOpFrag] -> IlxOpFrag
ilxParams l env = parens (ilxParamsAux l env)
  where
   ilxParamsAux [] env = empty
   ilxParamsAux [h] env = h env
   ilxParamsAux (h:t) env = h env <> text "," <+> ilxParamsAux t env


ilxMethodRef rty cls nm tyargs args = 
    ilxOpSeq [rty,cls,ilxOp "::",ilxOp nm,
              ilxTyParams tyargs,ilxParams args]

ilxCall m = ilxOpSeq [ilxOp "call", m]

ilxSupportClass = ilxOpSeq [prelGHCReference, ilxOp "'GHC.support'"]
ilxSuppMeth rty nm tyargs args = ilxMethodRef rty ilxSupportClass nm tyargs args

ilxCallSuppMeth rty nm tyargs args  = ilxCall (ilxSuppMeth rty nm tyargs args)

ilxMkBool :: IlxOpFrag
ilxMkBool =  ilxOpSeq [ilxOp "call class",prelBaseReference,
                       ilxOp "PrelBase_Bool",
                       prelGHCReference,ilxOp "GHC.support::mkBool(bool)"]
ilxCgt = ilxOpSeq [ilxOp "cgt",ilxMkBool]
ilxCge = ilxOpSeq [ilxOp "clt ldc.i4 0 ceq ",ilxMkBool]
ilxClt = ilxOpSeq [ilxOp "clt ",ilxMkBool]
ilxCle = ilxOpSeq [ilxOp "cgt ldc.i4 0 ceq ",ilxMkBool]
ilxCeq = ilxOpSeq [ilxOp "ceq ",ilxMkBool]
ilxCne = ilxOpSeq [ilxOp "ceq ldc.i4 0 ceq " ,ilxMkBool]
ilxCgtUn = ilxOpSeq [ilxOp "cgt.un ",ilxMkBool]
ilxCgeUn  = ilxOpSeq [ilxOp "clt.un ldc.i4 0 ceq ",ilxMkBool]
ilxCltUn = ilxOpSeq [ilxOp "clt.un ",ilxMkBool]
ilxCleUn = ilxOpSeq [ilxOp "cgt.un ldc.i4 0 ceq ",ilxMkBool]

ilxAddrOfForeignOp = ilxOpSeq [ilxOp "ldfld void *" , repForeign, ilxOp "::contents"]
ilxAddrOfByteArrOp = ilxOp "ldc.i4 0 ldelema unsigned int8"

ilxPrimOpTable :: PrimOp -> [StgArg] -> IlxOpFrag
ilxPrimOpTable op
  = case op of
 	CharGtOp    -> simp_op ilxCgt
	CharGeOp    -> simp_op ilxCge
	CharEqOp    -> simp_op ilxCeq
	CharNeOp    -> simp_op ilxCne
	CharLtOp    -> simp_op ilxClt
	CharLeOp    -> simp_op ilxCle

	OrdOp       -> simp_op (ilxOp "conv.i4") -- chars represented by UInt32 (u4)
	ChrOp       -> simp_op (ilxOp "conv.u4")

	IntGtOp     -> simp_op ilxCgt
	IntGeOp     -> simp_op ilxCge
	IntEqOp     -> simp_op ilxCeq
	IntNeOp     -> simp_op ilxCne
	IntLtOp     -> simp_op ilxClt
	IntLeOp     -> simp_op ilxCle

        Narrow8IntOp   -> simp_op  (ilxOp"conv.i1")
        Narrow16IntOp  -> simp_op (ilxOp "conv.i2")
        Narrow32IntOp  -> simp_op (ilxOp "conv.i4")
        Narrow8WordOp  -> simp_op (ilxOp "conv.u1")
        Narrow16WordOp -> simp_op (ilxOp "conv.u2")
        Narrow32WordOp -> simp_op (ilxOp "conv.u4")

	WordGtOp     -> simp_op ilxCgtUn
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
	IntAddOp    -> simp_op (ilxOp "add")
	IntSubOp    -> simp_op (ilxOp "sub")
	IntMulOp    -> simp_op (ilxOp "mul")
	IntQuotOp   -> simp_op (ilxOp "div")
	IntNegOp    -> simp_op (ilxOp "neg")
	IntRemOp    -> simp_op (ilxOp "rem")

    -- Addr# ops:
        AddrAddOp  -> simp_op (ilxOp "add")
	AddrSubOp  -> simp_op (ilxOp "sub")
	AddrRemOp  -> simp_op (ilxOp "rem")
	Int2AddrOp -> warn_op "int2Addr" (simp_op (ilxOp "/* PrimOp int2Addr */ "))
	Addr2IntOp -> warn_op "addr2Int" (simp_op (ilxOp "/* PrimOp addr2Int */ "))

    -- Word#-related ops:
	WordAddOp    -> simp_op (ilxOp "add")
	WordSubOp    -> simp_op (ilxOp "sub")
	WordMulOp    -> simp_op (ilxOp "mul")
	WordQuotOp   -> simp_op (ilxOp "div")
	WordRemOp    -> simp_op (ilxOp "rem")

	ISllOp      -> simp_op (ilxOp "shl")
	ISraOp      -> simp_op (ilxOp "shr")
	ISrlOp      -> simp_op (ilxOp "shr.un")
	IntAddCOp   -> simp_op (ilxCallSuppMeth (ilxUnboxedPairRep repInt repInt) "IntAddCOp" [] [repInt, repInt])
	IntSubCOp   -> simp_op (ilxCallSuppMeth (ilxUnboxedPairRep repInt repInt) "IntSubCOp" [] [repInt, repInt])
	IntGcdOp    -> simp_op (ilxCallSuppMeth repInt "IntGcdOp" [] [repInt, repInt])


    -- Word#-related ops:
	AndOp  	    -> simp_op (ilxOp "and") 
	OrOp   	    -> simp_op (ilxOp "or") 
	NotOp  	    -> simp_op (ilxOp "not") 
	XorOp  	    -> simp_op (ilxOp "xor") 
	SllOp  	    -> simp_op (ilxOp "shl") 
	SrlOp 	    -> simp_op (ilxOp "shr") 
	Word2IntOp  -> simp_op (ilxOp "conv.i4")
	Int2WordOp  -> simp_op (ilxOp "conv.u4")

    -- Float#-related ops:
	FloatAddOp   -> simp_op (ilxOp "add")
	FloatSubOp   -> simp_op (ilxOp "sub")
	FloatMulOp   -> simp_op (ilxOp "mul")
	FloatDivOp   -> simp_op (ilxOp "div")
	FloatNegOp   -> simp_op (ilxOp "neg")
	Float2IntOp  -> simp_op (ilxOp "conv.i4")
	Int2FloatOp  -> simp_op (ilxOp "conv.r4")

	DoubleAddOp   	-> simp_op (ilxOp "add")
	DoubleSubOp   	-> simp_op (ilxOp "sub")
	DoubleMulOp   	-> simp_op (ilxOp "mul")
	DoubleDivOp   	-> simp_op (ilxOp "div")
	DoubleNegOp   	-> simp_op (ilxOp "neg")
	Double2IntOp  	-> simp_op (ilxOp "conv.i4")
	Int2DoubleOp  	-> simp_op (ilxOp "conv.r4")
	Double2FloatOp  -> simp_op (ilxOp "conv.r4")
	Float2DoubleOp  -> simp_op (ilxOp "conv.r8")
	DoubleDecodeOp  -> simp_op (ilxCallSuppMeth (ilxUnboxedTripleRep repInt repInt repByteArray) "decodeDouble" [] [ilxType "float64"])
	FloatDecodeOp   -> simp_op (ilxCallSuppMeth (ilxUnboxedTripleRep repInt repInt repByteArray) "decodeFloat" [] [ilxType "float32"])

	FloatExpOp   -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Exp(float64) conv.r4")
	FloatLogOp   -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Log(float64) conv.r4")
	FloatSqrtOp  -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Sqrt(float64) conv.r4")
	FloatSinOp   -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Sin(float64) conv.r4")
	FloatCosOp   -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Cos(float64) conv.r4")
	FloatTanOp   -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Tan(float64) conv.r4")
	FloatAsinOp  -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Asin(float64) conv.r4")
	FloatAcosOp  -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Acos(float64) conv.r4")
	FloatAtanOp  -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Atan(float64) conv.r4")
	FloatSinhOp  -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Sinh(float64) conv.r4")
	FloatCoshOp  -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Cosh(float64) conv.r4")
	FloatTanhOp  -> simp_op (ilxOp "conv.r8 call float64 [mscorlib]System.Math::Tanh(float64) conv.r4")
	FloatPowerOp -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Pow(float64, float64) conv.r4") -- ** op, make use of implicit cast to r8...

	DoubleExpOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Exp(float64)")
	DoubleLogOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Log(float64)")
	DoubleSqrtOp  -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Sqrt(float64)")
          
	DoubleSinOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Sin(float64)")
	DoubleCosOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Cos(float64)")
	DoubleTanOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Tan(float64)")
          
	DoubleAsinOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Asin(float64)")
	DoubleAcosOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Acos(float64)")
	DoubleAtanOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Atan(float64)")
          
	DoubleSinhOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Sinh(float64)")
	DoubleCoshOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Cosh(float64)")
	DoubleTanhOp   -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Tanh(float64)")
          
	DoublePowerOp  -> simp_op (ilxOp "call float64 [mscorlib]System.Math::Pow(float64, float64)")

    -- Integer (and related...) ops: bail out to support routines
	IntegerAndOp  	   -> simp_op (ilxCallSuppMeth repInteger "IntegerAndOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerOrOp  	   -> simp_op (ilxCallSuppMeth repInteger "IntegerOrOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerXorOp  	   -> simp_op (ilxCallSuppMeth repInteger "IntegerXorOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerComplementOp -> simp_op (ilxCallSuppMeth repInteger "IntegerComplementOp" [] [repInt, repByteArray])
	IntegerAddOp  	   -> simp_op (ilxCallSuppMeth repInteger "IntegerAddOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerSubOp  	   -> simp_op (ilxCallSuppMeth repInteger "IntegerSubOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerMulOp  	   -> simp_op (ilxCallSuppMeth repInteger "IntegerMulOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerGcdOp  	   -> simp_op (ilxCallSuppMeth repInteger "IntegerGcdOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerQuotRemOp   -> simp_op (ilxCallSuppMeth repIntegerPair "IntegerQuotRemOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerDivModOp    -> simp_op (ilxCallSuppMeth repIntegerPair "IntegerDivModOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerIntGcdOp    -> simp_op (ilxCallSuppMeth repInt "IntegerIntGcdOp" [] [repInt, repByteArray, repInt])
	IntegerDivExactOp  -> simp_op (ilxCallSuppMeth repInteger "IntegerDivExactOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerQuotOp  	   -> simp_op (ilxCallSuppMeth repInteger "IntegerQuotOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerRemOp   	   -> simp_op (ilxCallSuppMeth repInteger "IntegerRemOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerCmpOp   	   -> simp_op (ilxCallSuppMeth repInt "IntegerCmpOp" [] [repInt, repByteArray, repInt, repByteArray])
	IntegerCmpIntOp    -> simp_op (ilxCallSuppMeth repInt "IntegerCmpIntOp" [] [repInt, repByteArray, repInt])
	Integer2IntOp      -> simp_op (ilxCallSuppMeth repInt "Integer2IntOp" [] [repInt, repByteArray])
	Integer2WordOp     -> simp_op (ilxCallSuppMeth repWord "Integer2WordOp" [] [repInt, repByteArray])
	Int2IntegerOp  	   -> simp_op (ilxCallSuppMeth repInteger "Int2IntegerOp" [] [repInt])
	Word2IntegerOp     -> simp_op (ilxCallSuppMeth repInteger "Word2IntegerOp" [] [repWord])
--	IntegerToInt64Op   -> simp_op (ilxCallSuppMeth repInt64 "IntegerToInt64Op" [] [repInt,repByteArray])
	Int64ToIntegerOp   -> simp_op (ilxCallSuppMeth repInteger "Int64ToIntegerOp" [] [repInt64])
--	IntegerToWord64Op  -> simp_op (ilxCallSuppMeth repWord64 "IntegerToWord64Op" [] [repInt,repByteArray])
	Word64ToIntegerOp  -> simp_op (ilxCallSuppMeth repInteger "Word64ToIntegerOp" [] [repWord64])



	IndexByteArrayOp_Char  	   -> simp_op (ilxOp "ldelem.u1")
	IndexByteArrayOp_WideChar  -> simp_op (ilxOp "ldelem.u4")
	IndexByteArrayOp_Int   	   -> simp_op (ilxOp "ldelem.i4")
	IndexByteArrayOp_Word  	   -> simp_op (ilxOp "ldelem.u4")
	IndexByteArrayOp_Addr  	   -> simp_op (ilxOp "ldelem.u")
	IndexByteArrayOp_Float 	   -> simp_op (ilxOp "ldelem.r4")
	IndexByteArrayOp_Double    -> simp_op (ilxOp "ldelem.r8")
	IndexByteArrayOp_StablePtr -> simp_op (ilxOp "ldelem.ref")
	IndexByteArrayOp_Int8     -> simp_op (ilxOp "ldelem.i1")
	IndexByteArrayOp_Int16     -> simp_op (ilxOp "ldelem.i2")
	IndexByteArrayOp_Int32     -> simp_op (ilxOp "ldelem.i4")
	IndexByteArrayOp_Int64     -> simp_op (ilxOp "ldelem.i8")
	IndexByteArrayOp_Word8    -> simp_op (ilxOp "ldelem.u1")
	IndexByteArrayOp_Word16    -> simp_op (ilxOp "ldelem.u2")
	IndexByteArrayOp_Word32    -> simp_op (ilxOp "ldelem.u4")
	IndexByteArrayOp_Word64    -> simp_op (ilxOp "ldelem.u8")

            {- should be monadic??? -}
	ReadByteArrayOp_Char   	  -> simp_op (ilxOp "ldelem.u1")
	ReadByteArrayOp_WideChar  -> simp_op (ilxOp "ldelem.u4")
	ReadByteArrayOp_Int    	  -> simp_op (ilxOp "ldelem.i4")
	ReadByteArrayOp_Word   	  -> simp_op (ilxOp "ldelem.u4")
	ReadByteArrayOp_Addr   	  -> simp_op (ilxOp "ldelem.u")
	ReadByteArrayOp_Float  	  -> simp_op (ilxOp "ldelem.r4")
	ReadByteArrayOp_Double 	  -> simp_op (ilxOp "ldelem.r8")
	ReadByteArrayOp_StablePtr -> simp_op (ilxOp "ldelem.ref")
	ReadByteArrayOp_Int8     -> simp_op (ilxOp "ldelem.i1")
	ReadByteArrayOp_Int16     -> simp_op (ilxOp "ldelem.i2")
	ReadByteArrayOp_Int32     -> simp_op (ilxOp "ldelem.i4")
	ReadByteArrayOp_Int64     -> simp_op (ilxOp "ldelem.i8")
	ReadByteArrayOp_Word8    -> simp_op (ilxOp "ldelem.u1")
	ReadByteArrayOp_Word16    -> simp_op (ilxOp "ldelem.u2")
	ReadByteArrayOp_Word32    -> simp_op (ilxOp "ldelem.u4")
	ReadByteArrayOp_Word64    -> simp_op (ilxOp "ldelem.u8")
                 {-   MutByteArr# s -> Int# -> State# s -> (# State# s, Char# #) -}
                 {- ByteArr# -> Int# -> Char# -}


	WriteByteArrayOp_Char  	   -> simp_op (ilxOp "stelem.u1")
	WriteByteArrayOp_WideChar   -> simp_op (ilxOp "stelem.u4")
	WriteByteArrayOp_Int   	   -> simp_op (ilxOp "stelem.i4")
	WriteByteArrayOp_Word  	   -> simp_op (ilxOp "stelem.u4")
	WriteByteArrayOp_Addr  	   -> simp_op (ilxOp "stelem.u")
	WriteByteArrayOp_Float 	   -> simp_op (ilxOp "stelem.r4")
	WriteByteArrayOp_Double    -> simp_op (ilxOp "stelem.r8")
	WriteByteArrayOp_StablePtr -> simp_op (ilxOp "stelem.ref")
	WriteByteArrayOp_Int8     -> simp_op (ilxOp "stelem.i1")
	WriteByteArrayOp_Int16     -> simp_op (ilxOp "stelem.i2")
	WriteByteArrayOp_Int32     -> simp_op (ilxOp "stelem.i4")
	WriteByteArrayOp_Int64     -> simp_op (ilxOp "stelem.i8")
	WriteByteArrayOp_Word8    -> simp_op (ilxOp "stelem.u1")
	WriteByteArrayOp_Word16    -> simp_op (ilxOp "stelem.u2")
	WriteByteArrayOp_Word32    -> simp_op (ilxOp "stelem.u4")
	WriteByteArrayOp_Word64    -> simp_op (ilxOp "stelem.i8 /* nb. no stelem.u8 */")
                 {- MutByteArr# s -> Int# -> Char# -> State# s -> State# s -}

	IndexOffAddrOp_Char    -> simp_op (ilxOp "sizeof unsigned int8 mul add ldind.u1")
	IndexOffAddrOp_WideChar    -> simp_op (ilxOp "sizeof int32 mul add ldind.u4")
	IndexOffAddrOp_Int     -> simp_op (ilxOp "sizeof int32 mul add ldind.i4")
	IndexOffAddrOp_Word    -> simp_op (ilxOp "sizeof int32 mul add ldind.u4")
	IndexOffAddrOp_Addr    -> simp_op (ilxOp "sizeof native unsigned int mul add ldind.i")
	IndexOffAddrOp_StablePtr   -> simp_op (ilxOp "sizeof native unsigned int mul add ldind.ref")
	IndexOffAddrOp_Float   -> simp_op (ilxOp "sizeof float32 mul add ldind.r4")
	IndexOffAddrOp_Double  -> simp_op (ilxOp "sizeof float64 mul add ldind.r8")
	IndexOffAddrOp_Int8   -> simp_op (ilxOp "sizeof int8 mul add ldind.i1")
	IndexOffAddrOp_Int16   -> simp_op (ilxOp "sizeof int16 mul add ldind.i2")
	IndexOffAddrOp_Int32   -> simp_op (ilxOp "sizeof int32 mul add ldind.i4")
	IndexOffAddrOp_Int64   -> simp_op (ilxOp "sizeof int64 mul add ldind.i8")
	IndexOffAddrOp_Word8  -> simp_op (ilxOp "sizeof unsigned int8 mul add ldind.u1")
	IndexOffAddrOp_Word16 -> simp_op (ilxOp "sizeof unsigned int16 mul add ldind.u2")
	IndexOffAddrOp_Word32  -> simp_op (ilxOp "sizeof unsigned int32 mul add ldind.u4")
	IndexOffAddrOp_Word64  -> simp_op (ilxOp "sizeof int64 mul add ldind.u8")

	-- ForeignObj: load the address inside the object first
        -- TODO: is this remotely right?
	EqForeignObj                 -> warn_op "eqForeignObj" (simp_op (ilxOp "pop /* PrimOp eqForeignObj */ "))
        IndexOffForeignObjOp_Char    -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof unsigned int8 mul add ldind.u1"])
	IndexOffForeignObjOp_WideChar    -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof int32 mul add ldind.u4"])
	IndexOffForeignObjOp_Int     -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof int32 mul add ldind.i4"])
	IndexOffForeignObjOp_Word    -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof unsigned int32 mul add ldind.u4"])
	IndexOffForeignObjOp_Addr    ->  arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof native unsigned int mul add ldind.i  "])
	IndexOffForeignObjOp_StablePtr    ->  ty1_arg2_op (\ty fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof native unsigned int mul add ldind.ref  "])
	IndexOffForeignObjOp_Float   -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof float32 mul add ldind.r4"])
	IndexOffForeignObjOp_Double  -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof float64 mul add ldind.r8"])
	IndexOffForeignObjOp_Int8   -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof int8 mul add ldind.i1"])
	IndexOffForeignObjOp_Int16   -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof int16 mul add ldind.i2"])
	IndexOffForeignObjOp_Int32   -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof int32 mul add ldind.i4"])
	IndexOffForeignObjOp_Int64   -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof int64 mul add ldind.i8"])
	IndexOffForeignObjOp_Word8  -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof unsigned int8 mul add ldind.u1"])
	IndexOffForeignObjOp_Word16  -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof unsigned int16 mul add ldind.u2"])
	IndexOffForeignObjOp_Word32  -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof unsigned int32 mul add ldind.u4"])
	IndexOffForeignObjOp_Word64  -> arg2_op (\fobj n -> ilxOpSeq [fobj, ilxAddrOfForeignOp, n, ilxOp "sizeof unsigned int64 mul add ldind.u8"])

	ReadOffAddrOp_Char   -> simp_op (ilxOp "sizeof unsigned int8 mul add ldind.u1")
	ReadOffAddrOp_WideChar -> simp_op (ilxOp "sizeof int32 mul add ldind.u4")
	ReadOffAddrOp_Int    -> simp_op (ilxOp "sizeof int32 mul add ldind.i4")
	ReadOffAddrOp_Word   -> simp_op (ilxOp "sizeof unsigned int32 mul add ldind.u4")
	ReadOffAddrOp_Addr   -> simp_op (ilxOp "sizeof native unsigned int mul add ldind.i")
	ReadOffAddrOp_Float  -> simp_op (ilxOp "sizeof float32 mul add ldind.r4")
	ReadOffAddrOp_Double -> simp_op (ilxOp "sizeof float64 mul add ldind.r8")
	ReadOffAddrOp_StablePtr  -> simp_op (ilxOp "sizeof native unsigned int mul add ldind.ref")
	ReadOffAddrOp_Int8  -> simp_op (ilxOp "sizeof int8 mul add ldind.i1")
	ReadOffAddrOp_Int16  -> simp_op (ilxOp "sizeof int16 mul add ldind.i2")
	ReadOffAddrOp_Int32  -> simp_op (ilxOp "sizeof int32 mul add ldind.i4")
	ReadOffAddrOp_Int64  -> simp_op (ilxOp "sizeof int64 mul add ldind.i8")
	ReadOffAddrOp_Word8 -> simp_op (ilxOp "sizeof unsigned int8 mul add ldind.u1")
	ReadOffAddrOp_Word16 -> simp_op (ilxOp "sizeof unsigned int16 mul add ldind.u2")
	ReadOffAddrOp_Word32 -> simp_op (ilxOp "sizeof unsigned int32 mul add ldind.u4")
	ReadOffAddrOp_Word64 -> simp_op (ilxOp "sizeof unsigned int64 mul add ldind.u8")
                  {-    Addr# -> Int# -> Char# -> State# s -> State# s -} 

	WriteOffAddrOp_Char   -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr, n, ilxOp "add", v, ilxOp "stind.u1"])
	WriteOffAddrOp_WideChar   -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr, n, ilxOp "sizeof int32 mul add", v, ilxOp "stind.u4"])
	WriteOffAddrOp_Int    -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr, n, ilxOp "sizeof int32 mul add", v, ilxOp "stind.i4"])
	WriteOffAddrOp_Word   -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr, n, ilxOp "sizeof int32 mul add", v, ilxOp "stind.u4"])
	WriteOffAddrOp_Addr   -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr, n, ilxOp "sizeof native unsigned int mul add", v, ilxOp "stind.i"])
	WriteOffAddrOp_ForeignObj   -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr, n, ilxOp "sizeof native unsigned int mul add", v, ilxOp "stind.ref"])
	WriteOffAddrOp_Float  -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr, n, ilxOp "sizeof float32 mul add", v,ilxOp "stind.r4"])
	WriteOffAddrOp_StablePtr   -> ty2_arg4_op (\ty1 sty addr n v s -> ilxOpSeq [addr, n, ilxOp "sizeof native unsigned int mul add", v, ilxOp "stind.ref"])
	WriteOffAddrOp_Double -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof float64 mul add",v,ilxOp "stind.r8"])
	WriteOffAddrOp_Int8  -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof int8 mul add",v,ilxOp "stind.i1"])
	WriteOffAddrOp_Int16  -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof int16 mul add",v,ilxOp "stind.i2"])
	WriteOffAddrOp_Int32  -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof int32 mul add",v,ilxOp "stind.i4"])
	WriteOffAddrOp_Int64  -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof int64 mul add",v,ilxOp "stind.i8"])
	WriteOffAddrOp_Word8 -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof unsigned int8 mul add",v,ilxOp "stind.u1"])
	WriteOffAddrOp_Word16 -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof unsigned int16 mul add",v,ilxOp "stind.u2"])
	WriteOffAddrOp_Word32 -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof unsigned int32 mul add",v,ilxOp "stind.u4"])
	WriteOffAddrOp_Word64 -> ty1_arg4_op (\sty addr n v s -> ilxOpSeq [addr,n,ilxOp "sizeof unsigned int64 mul add",v,ilxOp "stind.u8"])
                  {-    Addr# -> Int# -> Char# -> State# s -> State# s -} 

            {- should be monadic??? -}
	NewPinnedByteArrayOp_Char -> warn_op "newPinnedByteArray" (simp_op (ilxOp "newarr [mscorlib]System.Byte "))
	NewByteArrayOp_Char   	 -> simp_op (ilxOp "newarr [mscorlib]System.Byte")
--	NewByteArrayOp_Int    	 -> simp_op (ilxOp "newarr [mscorlib]System.Int32")
--	NewByteArrayOp_Word   	 -> simp_op (ilxOp "newarr [mscorlib]System.UInt32")
--	NewByteArrayOp_Addr   	 -> simp_op (ilxOp "newarr [mscorlib]System.UInt64")
--	NewByteArrayOp_Float  	 -> simp_op (ilxOp "newarr [mscorlib]System.Single")
--	NewByteArrayOp_Double 	 -> simp_op (ilxOp "newarr [mscorlib]System.Double")
--	NewByteArrayOp_StablePtr -> simp_op (ilxOp "newarr [mscorlib]System.UInt32")
--      NewByteArrayOp_Int64     -> simp_op (ilxOp "newarr [mscorlib]System.Int64")  TODO: there is no unique for this one -}
--      NewByteArrayOp_Word64    -> simp_op (ilxOp "newarr  [mscorlib]System.UInt64") -}
                  {- Int# -> State# s -> (# State# s, MutByteArr# s #) -}
	ByteArrayContents_Char   -> warn_op "byteArrayContents" (simp_op ilxAddrOfByteArrOp)

	UnsafeFreezeByteArrayOp ->   ty1_op (\ty1  -> ilxOp "nop ")
                  {- MutByteArr# s -> State# s -> (# State# s, ByteArr# #) -}
	SizeofByteArrayOp  -> simp_op (ilxOp "ldlen")
                  {- ByteArr# -> Int# -}

	SameMutableByteArrayOp -> ty1_op (\ty1  -> ilxCeq)
                 {- MutByteArr# s -> MutByteArr# s -> Bool -}
	SizeofMutableByteArrayOp -> ty1_op (\ty1  -> ilxOp "ldlen")
                 {- MutByteArr# s -> Int# -}

	SameMutVarOp -> ty2_op (\ty1 ty2 -> ilxCeq)
                 {- MutVar# s a -> MutVar# s a -> Bool -}
	NewMutVarOp -> ty2_op (\ty1 ty2 -> ilxOpSeq [ilxOp "newobj void" , repMutVar ty1 ty2 , ilxOp "::.ctor(!0)"])
                 {- a -> State# s -> (# State# s, MutVar# s a #) -}
	ReadMutVarOp -> ty2_op (\ty1 ty2 ->  ilxOpSeq [ilxOp "ldfld !0" , repMutVar ty1 ty2 , ilxOp "::contents"])
                 {-  MutVar# s a -> State# s -> (# State# s, a #) -}
	WriteMutVarOp -> ty2_op (\ty1 ty2 -> ilxOpSeq [ilxOp "stfld !0" , repMutVar ty1 ty2 , ilxOp "::contents"])
                 {- MutVar# s a -> a -> State# s -> State# s -}

	NewArrayOp -> ty2_op (\ty1 ty2 -> ilxCallSuppMeth (ilxType "!!0[]") "newArray" [ty1] [repInt,ilxMethTyVarA])
                 {- Int# -> a -> State# s -> (# State# s, MutArr# s a #) -}
	IndexArrayOp -> ty1_op (\ty1 -> ilxOp "ldelem.ref")
                 {- Array# a -> Int# -> (# a #) -}
	WriteArrayOp -> ty2_op (\ty1 ty2 -> ilxOp "stelem.ref")
                 {- MutArr# s a -> Int# -> a -> State# s -> State# s -}
	ReadArrayOp -> ty2_op (\ty1 ty2 -> ilxOp "ldelem.ref")
                 {- MutArr# s a -> Int# -> State# s -> (# State# s, a #) -}
	UnsafeFreezeArrayOp -> ty2_op (\ty1 ty2 -> ilxOp "nop")
                 {-   MutArr# s a -> State# s -> (# State# s, Array# a #) -}
	UnsafeThawArrayOp -> ty2_op (\ty1 ty2 -> ilxOp "nop")
                 {-  Array# a -> State# s -> (# State# s, MutArr# s a #) -}

	SameMutableArrayOp -> ty2_op (\ty1 ty2 -> ilxCeq)
                 {- MutArr# s a -> MutArr# s a -> Bool -}


	RaiseOp -> ty2_op (\ty1 ty2 -> ilxOp "throw")
	CatchOp -> ty2_op (\ty1 ty2 -> 
		ilxCallSuppMeth ilxMethTyVarA "'catch'" [ty1,ty2] [ilxLift (ilxTyIO (ilxType "!!0")), 
                                                              ilxOp "thunk<(func (!!1) --> (func ( /* unit skipped */ ) --> !!0))>"])
	                    {-        (State# RealWorld -> (# State# RealWorld, a #) )
	                           -> (b -> State# RealWorld -> (# State# RealWorld, a #) ) 
	                           -> State# RealWorld
	                           -> (# State# RealWorld, a #) 
	                     -} 

	BlockAsyncExceptionsOp -> ty1_op (\ty1 -> 
		ilxCallSuppMeth ilxMethTyVarA "blockAsyncExceptions" [ty1] [ilxLift (ilxTyIO (ilxType "!!0"))])

                {-     (State# RealWorld -> (# State# RealWorld, a #))
                    -> (State# RealWorld -> (# State# RealWorld, a #))
                -}

	UnblockAsyncExceptionsOp -> ty1_op (\ty1 -> 
		ilxCallSuppMeth ilxMethTyVarA "unblockAsyncExceptions" [ty1] [ilxLift (ilxTyIO (ilxType "!!0"))])

                {-
		    State# RealWorld -> (# State# RealWorld, a #))
                    -> (State# RealWorld -> (# State# RealWorld, a #))
                -}
 
	NewMVarOp -> ty2_op (\sty ty -> 
		ilxOpSeq [ilxOp "newobj void " , repMVar ty , ilxOp "::.ctor()"])
                 {- State# s -> (# State# s, MVar# s a #) -}

	TakeMVarOp -> ty2_op (\sty ty -> 
		ilxCallSuppMeth ilxMethTyVarA "takeMVar" [ty] [repMVar ilxMethTyVarA])
                  {-  MVar# s a -> State# s -> (# State# s, a #) -}

	-- These aren't yet right
        TryTakeMVarOp -> ty2_op (\sty ty -> 
		ilxCallSuppMeth (ilxUnboxedPairRep repInt ilxMethTyVarA) "tryTakeMVar" [ty] [repMVar ilxMethTyVarA])
                  {-  MVar# s a -> State# s -> (# State# s, a #) -}

	TryPutMVarOp -> ty2_op (\sty ty -> 
		ilxCallSuppMeth repInt "tryPutMVar" [ty] [repMVar ilxMethTyVarA,ilxMethTyVarA])
                  {-  MVar# s a -> State# s -> (# State# s, a #) -}

	PutMVarOp -> ty2_op (\sty ty -> 
		ilxCallSuppMeth (ilxOp "void") "putMVar" [ty] [repMVar ilxMethTyVarA, ilxMethTyVarA])
                   {- MVar# s a -> a -> State# s -> State# s -}

	SameMVarOp -> ty2_op (\sty ty -> ilxCeq)
                   {- MVar# s a -> MVar# s a -> Bool -}

--	TakeMaybeMVarOp -> ty2_op (\sty ty -> 
--		(ilxCallSuppMeth (ilxUnboxedPairRep repInt ilxMethTyVarA) "tryTakeMVar" [ty] [repMVar ilxMethTyVarA]))
--              {- MVar# s a -> State# s -> (# State# s, Int#, a #) -}

	IsEmptyMVarOp -> ty2_op (\sty ty -> 
		ilxCallSuppMeth repInt "isEmptyMVar" [ty] [repMVar ilxMethTyVarA])
               {- MVar# s a -> State# s -> (# State# s, Int# #) -}

	TouchOp -> warn_op "touch" (ty1_op (\ty1 -> ilxOp "pop /* PrimOp touch */ "))

               {- a -> Int# -}
	DataToTagOp -> ty1_op (\ty1 -> 
		ilxCallSuppMeth repInt "dataToTag" [ty1] [ilxMethTyVarA])
               {- a -> Int# -}

	TagToEnumOp -> ty1_op (\ty1 -> 
		ilxCallSuppMeth ilxMethTyVarA "tagToEnum" [ty1] [repInt])
               {- Int# -> a -}

	MakeStablePtrOp -> ty1_op (\ty1 -> ilxOpSeq [ilxOp "box", ty1, ilxOp "newobj void", repStablePtr {- ty1 -}, ilxOp "::.ctor(class [mscorlib]System.Object)"])
                 {-   a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #) -}
	MakeStableNameOp -> ty1_op (\ty1 -> ilxOpSeq [ilxOp "pop newobj void", repStableName {- ty1 -}, ilxOp "::.ctor()"])
                        -- primOpInfo MakeStableNameOp = mkGenPrimOp SLIT("makeStableName#")  [alphaTyVar] [alphaTy, mkStatePrimTy realWorldTy] ((mkTupleTy Unboxed 2 [mkStatePrimTy realWorldTy, mkStableNamePrimTy alphaTy]))

        EqStableNameOp -> ty1_op (\ty1 -> ilxOp "ceq")
               -- [alphaTyVar] [mkStableNamePrimTy alphaTy, mkStableNamePrimTy alphaTy] (intPrimTy)
        StableNameToIntOp -> warn_op "StableNameToIntOp" (ty1_op (\ty1 -> ilxOp "pop ldc.i4 0"))
               -- [alphaTyVar] [mkStableNamePrimTy alphaTy] (intPrimTy)

	DeRefStablePtrOp -> ty1_op (\ty1 -> ilxOpSeq [ilxOp "ldfld class [mscorlib]System.Object", repStablePtr {- ty1 -}, ilxOp "::contents"])
                 {-  StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #) -}

	EqStablePtrOp -> ty1_op (\ty1 -> ilxOp "ceq")
                 {-  StablePtr# a -> StablePtr# a -> Int# -}

            -- The 3rd argument to MkWeakOp is always a IO Monad action, i.e. passed as () --> ()
	MkWeakOp -> ty3_op (\ty1 ty2 ty3 ->  ilxCall (ilxMethodRef (repWeak ilxMethTyVarB) classWeak "bake" [ilxLift ty1,ilxLift ty2] [ilxMethTyVarA, ilxMethTyVarB, ilxLift (ilxTyIO ilxUnboxedEmptyRep)]))
                 {- o -> b -> c -> State# RealWorld -> (# State# RealWorld, Weak# b #) -}

	DeRefWeakOp -> ty1_op (\ty1 ->  ilxCall (ilxMethodRef (ilxUnboxedPairRep repInt ilxMethTyVarA) classWeak "deref" [ty1] [repWeak ilxMethTyVarA]))
	FinalizeWeakOp -> ty1_op (\ty1 ->  ilxCall (ilxMethodRef (ilxUnboxedPairRep repInt (ilxTyIO ilxUnboxedEmptyRep)) classWeak "finalizer" [ty1] [repWeak ilxMethTyVarA]))
                   {-    Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, 
	State# RealWorld -> (# State# RealWorld, Unit #)) #) -}

	MkForeignObjOp -> simp_op (ilxOpSeq [ilxOp "newobj void", repForeign, ilxOp "::.ctor(void *)"])
	WriteForeignObjOp -> ty1_op (\sty -> ilxOpSeq [ilxOp "stfld void *", repForeign, ilxOp "::contents"])
        ForeignObjToAddrOp -> simp_op ilxAddrOfForeignOp
	YieldOp -> simp_op (ilxOpSeq [ilxOp "call class [mscorlib]System.Threading.Thread class [mscorlib]System.Threading.Thread::get_CurrentThread() 
                                call instance void class [mscorlib]System.Threading.Thread::Suspend()"])
	MyThreadIdOp -> simp_op (ilxOpSeq [ilxOp "call default  class [mscorlib]System.Threading.Thread class [mscorlib]System.Threading.Thread::get_CurrentThread() "])
	-- This pushes a THUNK across as the exception value.
        -- This is the correct Haskell semantics...  TODO: we should probably
        -- push across an HaskellThreadAbortException object that wraps this
        -- thunk, but which is still actually an exception of
        -- an appropriate type.
        KillThreadOp -> ty1_op (\ty -> ilxOpSeq [ilxOp "call instance void class [mscorlib]System.Threading.Thread::Abort(class [mscorlib]System.Object) "])
              {-   ThreadId# -> a -> State# RealWorld -> State# RealWorld -}

	ForkOp -> warn_op "ForkOp" (simp_op (ilxOp "/* ForkOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	ParOp ->  warn_op "ParOp" (simp_op (ilxOp "/* ParOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	DelayOp -> simp_op (ilxOp "call void class [mscorlib]System.Threading.Thread::Sleep(int32) ")
                 {-    Int# -> State# s -> State# s -}

	WaitReadOp  -> warn_op "WaitReadOp" (simp_op (ilxOp "/* WaitReadOp skipped... */ pop"))
   	WaitWriteOp -> warn_op "WaitWriteOp" (simp_op (ilxOp " /* WaitWriteOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	ParAtForNowOp -> warn_op "ParAtForNowOp" (simp_op (ilxOp " /* ParAtForNowOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	ParAtRelOp -> warn_op "ParAtRelOp" (simp_op (ilxOp " /* ParAtRelOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	ParAtAbsOp -> warn_op "ParAtAbsOp" (simp_op (ilxOp " /* ParAtAbsOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	ParAtOp -> warn_op "ParAtOp" (simp_op (ilxOp " /* ParAtOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	ParLocalOp -> warn_op "ParLocalOp" (simp_op (ilxOp " /* ParLocalOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	ParGlobalOp -> warn_op "ParGlobalOp" (simp_op (ilxOp " /* ParGlobalOp skipped... */ newobj void [mscorlib]System.Object::.ctor() throw"))
	SeqOp -> warn_op "SeqOp" (simp_op (ilxOp " newobj void [mscorlib]System.Object::.ctor() throw "))
	AddrToHValueOp -> warn_op "AddrToHValueOp" (simp_op (ilxOp "newobj void [mscorlib]System.Object::.ctor() throw"))
--	ReallyUnsafePtrEqualityOp -> simp_op (ilxOp "ceq")

        MkApUpd0_Op ->  warn_op "MkApUpd0_Op" (simp_op (ilxOp " newobj void [mscorlib]System.Object::.ctor() throw"))
        NewBCOOp ->  warn_op "NewBCOOp" (simp_op (ilxOp " newobj void [mscorlib]System.Object::.ctor() throw"))
                  -- ("newBCO#")  [alphaTyVar, deltaTyVar] [byteArrayPrimTy, byteArrayPrimTy, mkArrayPrimTy alphaTy, byteArrayPrimTy, mkStatePrimTy deltaTy] ((mkTupleTy Unboxed 2 [mkStatePrimTy deltaTy, bcoPrimTy]))
        _        -> pprPanic "Unimplemented primop" (ppr op)


ty1_op :: (IlxTyFrag -> IlxOpFrag) -> [StgArg] ->  IlxOpFrag 
ty1_op  op ((StgTypeArg ty1):rest)  = 
      ilxOpSeq [getArgsStartingAt 1 rest, 
                op (ilxTypeR2 (deepIlxRepType ty1))]

ty2_op :: (IlxTyFrag -> IlxTyFrag -> IlxOpFrag) -> [StgArg] ->  IlxOpFrag 
ty2_op  op ((StgTypeArg ty1):(StgTypeArg ty2):rest)  = 
      ilxOpSeq [getArgsStartingAt 2 rest, 
                op (ilxTypeR2 (deepIlxRepType ty1)) 
                   (ilxTypeR2 (deepIlxRepType ty2))]

ty3_op :: (IlxTyFrag -> IlxTyFrag -> IlxTyFrag -> IlxOpFrag) -> [StgArg] ->  IlxOpFrag 
ty3_op  op ((StgTypeArg ty1):(StgTypeArg ty2):(StgTypeArg ty3):rest) = 
      ilxOpSeq [getArgsStartingAt 3 rest, 
                op (ilxTypeR2 (deepIlxRepType ty1)) 
                   (ilxTypeR2 (deepIlxRepType ty2))
                   (ilxTypeR2 (deepIlxRepType ty3))]

arg2_op :: (IlxTyFrag -> IlxOpFrag -> IlxOpFrag) -> [StgArg] ->  IlxOpFrag 
arg2_op  op [a1, a2] = 
       op (getAsArg 1 a1)
          (getAsArg 2 a2)

ty1_arg2_op :: (IlxTyFrag -> IlxOpFrag ->  IlxOpFrag -> IlxOpFrag) -> [StgArg] ->  IlxOpFrag 
ty1_arg2_op  op [(StgTypeArg ty1), a1, a2] = 
       op (ilxTypeR2 (deepIlxRepType ty1)) 
          (getAsArg 1 a1)
          (getAsArg 2 a2)

ty1_arg4_op :: (IlxTyFrag -> IlxOpFrag -> IlxOpFrag -> IlxOpFrag -> IlxOpFrag -> IlxOpFrag) -> [StgArg] ->  IlxOpFrag 
ty1_arg4_op  op [(StgTypeArg ty1), a1, a2, a3, a4] = 
       op (ilxTypeR2 (deepIlxRepType ty1)) 
          (getAsArg 1 a1)
          (getAsArg 2 a2)
          (getAsArg 3 a3)
          (getAsArg 4 a4)

ty2_arg4_op :: (IlxTyFrag -> IlxTyFrag -> IlxOpFrag -> IlxOpFrag -> IlxOpFrag -> IlxOpFrag -> IlxOpFrag) -> [StgArg] ->  IlxOpFrag 
ty2_arg4_op  op [(StgTypeArg ty1), (StgTypeArg ty2),a1, a2, a3, a4] = 
       op (ilxTypeR2 (deepIlxRepType ty1)) 
          (ilxTypeR2 (deepIlxRepType ty2)) 
          (getAsArg 2 a1)
          (getAsArg 3 a2)
          (getAsArg 4 a3)
          (getAsArg 5 a4)

hd (h:t) = h

getAsArg n a env = hd (ilxMapPlaceArgs n pushArg env [a])
getArgsStartingAt n a env = vcat (ilxMapPlaceArgs n pushArg env a)

simp_op :: IlxOpFrag -> [StgArg] -> IlxOpFrag
simp_op  op args env    = vcat (ilxMapPlaceArgs 0 pushArg env args) $$ op env
warn_op  warning f args = trace ("WARNING! IlxGen cannot translate primop " ++ warning) (f args)
\end{code}

%************************************************************************
%*									*
\subsection{C Calls}
%*									*
%************************************************************************

\begin{code}
-- Call the P/Invoke stub wrapper generated in the import section.
-- We eliminate voids in and around an IL C Call.  
-- We also do some type-directed translation for pinning Haskell-managed blobs
-- of data as we throw them across the boundary.
ilxFCall env (CCall (CCallSpec (StaticTarget c) cconv gc)) args ret_ty
 = ilxComment ((text "C call") <+> pprCLabelString c) <+> 
	vcat [vcat (ilxMapPlaceArgs 0 pushCArg env args),
              text "call" <+> retdoc <+> pprCLabelString c <+> tyarg_doc
                    <+> pprCValArgTys ilxTypeL env (map deepIlxRepType (filter (not. isVoidIlxRepType) (map stgArgType tm_args))) ]
  where 
    retdoc | isVoidIlxRepType ret_ty = text "void"
	   | otherwise		     = ilxTypeR env (deepIlxRepType ret_ty)
    (ty_args,tm_args) = splitTyArgs1 args
    tyarg_doc | not (isEmptyVarSet (tyVarsOfTypes ty_args)) = text "/* type variable found */"
	      | otherwise = pprTypeArgs ilxTypeR env ty_args

ilxFCall env (DNCall (DNCallSpec call_instr)) args ret_ty
  = ilxComment (text "IL call") <+> 
    vcat [vcat (ilxMapPlaceArgs 0 pushEvalArg env tm_args), 
	  ptext call_instr
		-- In due course we'll need to pass the type arguments
		-- and to do that we'll need to have more than just a string
		-- for call_instr
    ]
  where
    (ty_args,tm_args) = splitTyArgs1 args 

-- Push and argument and force its evaluation if necessary.
pushEvalArg _ (StgTypeArg _) = empty
pushEvalArg env (StgVarArg arg) = ilxFunApp env arg [] False
pushEvalArg env (StgLitArg lit) = pushLit env lit


hasTyCon (TyConApp tc _) tc2 = tc == tc2
hasTyCon _  _ = False

isByteArrayCArgTy ty = hasTyCon ty byteArrayPrimTyCon || hasTyCon ty mutableByteArrayPrimTyCon
isByteArrayCArg v = isByteArrayCArgTy (deepIlxRepType (idType v))

isForeignObjCArgTy ty = hasTyCon ty foreignObjPrimTyCon
isForeignObjCArg v = isForeignObjCArgTy (deepIlxRepType (idType v))

pinCCallArg v = isByteArrayCArg v || isForeignObjCArg v  

pinCArg  env arg v = pushArg env arg <+> text "dup stloc" <+> singleQuotes (ilxEnvQualifyByExact env (ppr v) <> text "pin") 
pushCArg  env arg@(StgVarArg v) | isByteArrayCArg v = pinCArg env arg v <+> ilxAddrOfByteArrOp env
pushCArg env arg@(StgVarArg v) | isForeignObjCArg v = pinCArg env arg v <+> ilxAddrOfForeignOp env
pushCArg env arg | otherwise = pushArg env arg

pprCValArgTys f env tys = parens (pprSepWithCommas (pprCValArgTy f env) tys)
pprCValArgTy f env ty | isByteArrayCArgTy ty = text "void *" <+> ilxComment (text "interior pointer into ByteArr#")
pprCValArgTy f env ty | isForeignObjCArgTy ty = text "void *" <+> ilxComment (text "foreign object")
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


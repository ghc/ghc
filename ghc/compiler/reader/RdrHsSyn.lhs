%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @RdrName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
module RdrHsSyn (
	RdrNameArithSeqInfo,
	RdrNameBangType,
	RdrNameClassOpSig,
	RdrNameConDecl,
	RdrNameContext,
	RdrNameSpecDataSig,
	RdrNameDefaultDecl,
	RdrNameForeignDecl,
	RdrNameGRHS,
	RdrNameGRHSs,
	RdrNameHsBinds,
	RdrNameHsDecl,
	RdrNameHsExpr,
	RdrNameHsModule,
	RdrNameIE,
	RdrNameImportDecl,
	RdrNameInstDecl,
	RdrNameMatch,
	RdrNameMonoBinds,
	RdrNamePat,
	RdrNameHsType,
	RdrNameSig,
	RdrNameStmt,
	RdrNameTyClDecl,

	RdrNameClassOpPragmas,
	RdrNameClassPragmas,
	RdrNameDataPragmas,
	RdrNameGenPragmas,
	RdrNameInstancePragmas,
	extractHsTyVars, extractHsCtxtTyVars, extractPatsTyVars,

	RdrName(..),
	qual, varQual, tcQual, varUnqual,
	dummyRdrVarName, dummyRdrTcName,
	isUnqual, isQual,
	rdrNameOcc, rdrNameModule, ieOcc,
	cmpRdr,
	mkOpApp, mkClassDecl

    ) where

#include "HsVersions.h"

import HsSyn
import BasicTypes	( IfaceFlavour(..), Unused )
import Name		( NamedThing(..), 
			  Module, pprModule, mkModuleFS,
			  OccName, srcTCOcc, srcVarOcc, isTvOcc,
			  pprOccName, mkClassTyConOcc, mkClassDataConOcc
			)
import PrelMods		( mkTupNameStr, mkUbxTupNameStr )
import Util		( thenCmp )
import HsPragmas	( GenPragmas, ClassPragmas, DataPragmas, ClassOpPragmas, InstancePragmas )
import List		( nub )
import Outputable
\end{code}

\begin{code}
type RdrNameArithSeqInfo	= ArithSeqInfo		RdrName RdrNamePat
type RdrNameBangType		= BangType		RdrName
type RdrNameClassOpSig		= Sig			RdrName
type RdrNameConDecl		= ConDecl		RdrName
type RdrNameContext		= Context 		RdrName
type RdrNameHsDecl		= HsDecl		RdrName RdrNamePat
type RdrNameSpecDataSig		= SpecDataSig		RdrName
type RdrNameDefaultDecl		= DefaultDecl		RdrName
type RdrNameForeignDecl		= ForeignDecl		RdrName
type RdrNameGRHS		= GRHS			RdrName RdrNamePat
type RdrNameGRHSs		= GRHSs			RdrName RdrNamePat
type RdrNameHsBinds		= HsBinds		RdrName RdrNamePat
type RdrNameHsExpr		= HsExpr		RdrName RdrNamePat
type RdrNameHsModule		= HsModule		RdrName RdrNamePat
type RdrNameIE			= IE			RdrName
type RdrNameImportDecl 		= ImportDecl		RdrName
type RdrNameInstDecl		= InstDecl		RdrName RdrNamePat
type RdrNameMatch		= Match			RdrName RdrNamePat
type RdrNameMonoBinds		= MonoBinds		RdrName RdrNamePat
type RdrNamePat			= InPat			RdrName
type RdrNameHsType		= HsType		RdrName
type RdrNameSig			= Sig			RdrName
type RdrNameStmt		= Stmt			RdrName RdrNamePat
type RdrNameTyClDecl		= TyClDecl		RdrName RdrNamePat

type RdrNameClassOpPragmas	= ClassOpPragmas	RdrName
type RdrNameClassPragmas	= ClassPragmas		RdrName
type RdrNameDataPragmas		= DataPragmas		RdrName
type RdrNameGenPragmas		= GenPragmas		RdrName
type RdrNameInstancePragmas	= InstancePragmas	RdrName
\end{code}

@extractHsTyVars@ looks just for things that could be type variables.
It's used when making the for-alls explicit.

\begin{code}
extractHsTyVars :: HsType RdrName -> [RdrName]
extractHsTyVars ty = nub (extract_ty ty [])

extractHsCtxtTyVars :: Context RdrName -> [RdrName]
extractHsCtxtTyVars ty = nub (extract_ctxt ty [])

extract_ctxt ctxt acc = foldr extract_ass acc ctxt
		      where
			extract_ass (cls, tys) acc = foldr extract_ty acc tys

extract_ty (MonoTyApp ty1 ty2)	    acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (MonoListTy ty)	    acc = extract_ty ty acc
extract_ty (MonoTupleTy tys _)      acc = foldr extract_ty acc tys
extract_ty (MonoFunTy ty1 ty2)	    acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (MonoDictTy cls tys)	    acc = foldr extract_ty acc tys
extract_ty (MonoTyVar tv)           acc = insertTV tv acc
extract_ty (HsForAllTy tvs ctxt ty) acc = acc ++
					  (filter (`notElem` locals) $
				           extract_ctxt ctxt (extract_ty ty []))
				        where
				          locals = map getTyVarName tvs

insertTV name@(Unqual occ) acc | isTvOcc occ = name : acc
insertTV other 	           acc 		     = acc

extractPatsTyVars :: [RdrNamePat] -> [RdrName]
extractPatsTyVars pats = nub (foldr extract_pat [] pats)

extract_pat (SigPatIn pat ty)	   acc = extract_ty ty acc
extract_pat WildPatIn	      	   acc = acc
extract_pat (VarPatIn var)         acc = acc
extract_pat (LitPatIn _)	   acc = acc
extract_pat (LazyPatIn pat)        acc = extract_pat pat acc
extract_pat (AsPatIn a pat)        acc = extract_pat pat acc
extract_pat (NPlusKPatIn n _)      acc = acc
extract_pat (ConPatIn c pats)      acc = foldr extract_pat acc pats
extract_pat (ConOpPatIn p1 c f p2) acc = extract_pat p1 (extract_pat p2 acc)
extract_pat (NegPatIn  pat)        acc = extract_pat pat acc
extract_pat (ParPatIn  pat)        acc = extract_pat pat acc
extract_pat (ListPatIn pats)       acc = foldr extract_pat acc pats
extract_pat (TuplePatIn pats _)    acc = foldr extract_pat acc pats
extract_pat (RecPatIn c fields)    acc = foldr (\ (f,pat,_) acc -> extract_pat pat acc) acc fields
\end{code}


A useful function for building @OpApps@.  The operator is always a variable,
and we don't know the fixity yet.

\begin{code}
mkOpApp e1 op e2 = OpApp e1 (HsVar op) (error "mkOpApp:fixity") e2
\end{code}

mkClassDecl builds a RdrClassDecl, filling in the names for tycon and datacon
by deriving them from the name of the class.

\begin{code}
mkClassDecl cxt cname tyvars sigs mbinds prags loc
  = ClassDecl cxt cname tyvars sigs mbinds prags tname dname loc
  where
  -- The datacon and tycon are called "_DC" and "_TC", where the class is C
  -- This prevents name clashes with user-defined tycons or datacons C
    (dname, tname) = case cname of
	  	       Qual m occ hif -> (Qual m (mkClassDataConOcc occ) hif,
					  Qual m (mkClassTyConOcc   occ) hif)
		       Unqual occ     -> (Unqual (mkClassDataConOcc occ),
					  Unqual (mkClassTyConOcc   occ))
\end{code}

%************************************************************************
%*									*
\subsection[RdrName]{The @RdrName@ datatype; names read from files}
%*									*
%************************************************************************

\begin{code}
data RdrName
  = Unqual OccName
  | Qual   Module OccName IfaceFlavour	-- HiBootFile for M!.t (interface files only), 
					-- HiFile for the common M.t

-- These ones are used for making RdrNames for known-key things,
-- Or in code constructed from derivings
qual     (m,n) = Qual m n HiFile
tcQual   (m,n) = Qual m (srcTCOcc n) HiFile
varQual  (m,n) = Qual m (srcVarOcc n) HiFile
varUnqual n    = Unqual (srcVarOcc n)

	-- This guy is used by the reader when HsSyn has a slot for
	-- an implicit name that's going to be filled in by
	-- the renamer.  We can't just put "error..." because
	-- we sometimes want to print out stuff after reading but
	-- before renaming
dummyRdrVarName = Unqual (srcVarOcc SLIT("V-DUMMY"))
dummyRdrTcName  = Unqual (srcVarOcc SLIT("TC-DUMMY"))


isUnqual (Unqual _)   = True
isUnqual (Qual _ _ _) = False

isQual (Unqual _)   = False
isQual (Qual _ _ _) = True


cmpRdr (Unqual  n1) (Unqual  n2)     = n1 `compare` n2
cmpRdr (Unqual  n1) (Qual m2 n2 _)   = LT
cmpRdr (Qual m1 n1 _) (Unqual  n2)   = GT
cmpRdr (Qual m1 n1 _) (Qual m2 n2 _) = (n1 `compare` n2) `thenCmp` (m1 `compare` m2)
				   -- always compare module-names *second*

rdrNameOcc :: RdrName -> OccName
rdrNameOcc (Unqual occ)   = occ
rdrNameOcc (Qual _ occ _) = occ

rdrNameModule :: RdrName -> Module
rdrNameModule (Qual m _ _) = m

ieOcc :: RdrNameIE -> OccName
ieOcc ie = rdrNameOcc (ieName ie)

instance Show RdrName where -- debugging
    showsPrec p rn = showsPrecSDoc p (ppr rn)

instance Eq RdrName where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

instance Ord RdrName where
    a <= b = case (a `compare` b) of { LT -> True;	EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;	EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpRdr a b

instance Outputable RdrName where
    ppr (Unqual n)   = pprOccName n
    ppr (Qual m n _) = hcat [pprModule m, char '.', pprOccName n]

instance NamedThing RdrName where		-- Just so that pretty-printing of expressions works
    getOccName = rdrNameOcc
    getName = panic "no getName for RdrNames"
\end{code}


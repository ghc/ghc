%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @RdrName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
module RdrHsSyn (
	RdrNameArithSeqInfo,
	RdrNameBangType,
	RdrNameClassDecl,
	RdrNameClassOpSig,
	RdrNameConDecl,
	RdrNameContext,
	RdrNameSpecDataSig,
	RdrNameDefaultDecl,
	RdrNameFixityDecl,
	RdrNameGRHS,
	RdrNameGRHSsAndBinds,
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
	RdrNameTyDecl,

	RdrNameClassOpPragmas,
	RdrNameClassPragmas,
	RdrNameDataPragmas,
	RdrNameGenPragmas,
	RdrNameInstancePragmas,
	extractHsTyVars, extractHsCtxtTyVars,

	RdrName(..),
	qual, varQual, tcQual, varUnqual, lexVarQual, lexTcQual,
	dummyRdrVarName, dummyRdrTcName,
	isUnqual, isQual,
	showRdr, rdrNameOcc, rdrNameModule, ieOcc,
	cmpRdr, prefixRdrName,
	mkOpApp, mkClassDecl, isClassDataConRdrName

    ) where

#include "HsVersions.h"

import HsSyn
import Lex
import BasicTypes	( Module(..), IfaceFlavour(..), Unused )
import Name		( pprModule, OccName(..), pprOccName, 
			  prefixOccName, NamedThing(..) )
import Util		( thenCmp )
import HsPragmas	( GenPragmas, ClassPragmas, DataPragmas, ClassOpPragmas, InstancePragmas )
import List		( nub )
import Outputable

import Char		( isUpper )
\end{code}

\begin{code}
type RdrNameArithSeqInfo	= ArithSeqInfo		Unused RdrName RdrNamePat
type RdrNameBangType		= BangType		RdrName
type RdrNameClassDecl		= ClassDecl		Unused RdrName RdrNamePat
type RdrNameClassOpSig		= Sig			RdrName
type RdrNameConDecl		= ConDecl		RdrName
type RdrNameContext		= Context 		RdrName
type RdrNameHsDecl		= HsDecl		Unused RdrName RdrNamePat
type RdrNameSpecDataSig		= SpecDataSig		RdrName
type RdrNameDefaultDecl		= DefaultDecl		RdrName
type RdrNameFixityDecl		= FixityDecl		RdrName
type RdrNameGRHS		= GRHS			Unused RdrName RdrNamePat
type RdrNameGRHSsAndBinds	= GRHSsAndBinds		Unused RdrName RdrNamePat
type RdrNameHsBinds		= HsBinds		Unused RdrName RdrNamePat
type RdrNameHsExpr		= HsExpr		Unused RdrName RdrNamePat
type RdrNameHsModule		= HsModule		Unused RdrName RdrNamePat
type RdrNameIE			= IE			RdrName
type RdrNameImportDecl 		= ImportDecl		RdrName
type RdrNameInstDecl		= InstDecl		Unused RdrName RdrNamePat
type RdrNameMatch		= Match			Unused RdrName RdrNamePat
type RdrNameMonoBinds		= MonoBinds		Unused RdrName RdrNamePat
type RdrNamePat			= InPat			RdrName
type RdrNameHsType		= HsType		RdrName
type RdrNameSig			= Sig			RdrName
type RdrNameStmt		= Stmt			Unused RdrName RdrNamePat
type RdrNameTyDecl		= TyDecl		RdrName

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

extract_ctxt ctxt acc = foldr extract_ass [] ctxt
		      where
			extract_ass (cls, tys) acc = foldr extract_ty acc tys

extract_ty (MonoTyApp ty1 ty2)	 acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (MonoListTy tc ty)	 acc = extract_ty ty acc
extract_ty (MonoTupleTy tc tys)	 acc = foldr extract_ty acc tys
extract_ty (MonoFunTy ty1 ty2)	 acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (MonoDictTy cls tys)	 acc = foldr extract_ty acc tys
extract_ty (MonoTyVar tv)        acc = insert tv acc

	-- In (All a => a -> a) -> Int, there are no free tyvars
	-- We just assume that we quantify over all type variables mentioned in the context.
extract_ty (HsPreForAllTy ctxt ty)  acc = filter (`notElem` locals) (extract_ty ty [])
				          ++ acc
				        where
				          locals = extract_ctxt ctxt []

extract_ty (HsForAllTy tvs ctxt ty) acc = acc ++
					  (filter (`notElem` locals) $
				           extract_ctxt ctxt (extract_ty ty []))
				        where
				          locals = map getTyVarName tvs


insert (Qual _ _ _)	  acc = acc
insert (Unqual (TCOcc _)) acc = acc
insert other 	          acc = other : acc
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
  -- The datacon and tycon are called ":C" where the class is C
  -- This prevents name clashes with user-defined tycons or datacons C
    (dname, tname) = case cname of
	  	       Qual m (TCOcc s) hif -> (Qual m (VarOcc s1) hif, Qual m (TCOcc s1) hif)
					    where
					       s1 = SLIT(":") _APPEND_ s

		       Unqual (TCOcc s)     -> (Unqual (VarOcc s1),     Unqual (TCOcc s1))
					    where
					       s1 = SLIT(":") _APPEND_ s

-- This nasty little function tests for whether a RdrName was 
-- constructed by the above process.  It's used only for filtering
-- out duff error messages.  Maybe there's a tidier way of doing this
-- but I can't work up the energy to find it.

isClassDataConRdrName rdr_name
 = case rdrNameOcc rdr_name of
	TCOcc s -> case _UNPK_ s of
			':' : c : _ -> isUpper c
			other	    -> False
	other -> False
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

qual     (m,n) = Qual m n HiFile
tcQual   (m,n) = Qual m (TCOcc n) HiFile
varQual  (m,n) = Qual m (VarOcc n) HiFile

lexTcQual  (m,n,hif) = Qual m (TCOcc n) hif
lexVarQual (m,n,hif) = Qual m (VarOcc n) hif

	-- This guy is used by the reader when HsSyn has a slot for
	-- an implicit name that's going to be filled in by
	-- the renamer.  We can't just put "error..." because
	-- we sometimes want to print out stuff after reading but
	-- before renaming
dummyRdrVarName = Unqual (VarOcc SLIT("V-DUMMY"))
dummyRdrTcName = Unqual (VarOcc SLIT("TC-DUMMY"))


varUnqual n = Unqual (VarOcc n)

isUnqual (Unqual _)   = True
isUnqual (Qual _ _ _) = False

isQual (Unqual _)   = False
isQual (Qual _ _ _) = True

	-- Used for adding a prefix to a RdrName
prefixRdrName :: FAST_STRING -> RdrName -> RdrName
prefixRdrName prefix (Qual m n hif) = Qual m (prefixOccName prefix n) hif
prefixRdrName prefix (Unqual n)     = Unqual (prefixOccName prefix n)

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

instance Text RdrName where -- debugging
    showsPrec _ rn = showString (showSDoc (ppr rn))

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

showRdr rdr = showSDoc (ppr rdr)
\end{code}


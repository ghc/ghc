%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @RdrName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
#include "HsVersions.h"

module RdrHsSyn (
	SYN_IE(RdrNameArithSeqInfo),
	SYN_IE(RdrNameBangType),
	SYN_IE(RdrNameBind),
	SYN_IE(RdrNameClassDecl),
	SYN_IE(RdrNameClassOpSig),
	SYN_IE(RdrNameConDecl),
	SYN_IE(RdrNameContext),
	SYN_IE(RdrNameSpecDataSig),
	SYN_IE(RdrNameDefaultDecl),
	SYN_IE(RdrNameFixityDecl),
	SYN_IE(RdrNameGRHS),
	SYN_IE(RdrNameGRHSsAndBinds),
	SYN_IE(RdrNameHsBinds),
	SYN_IE(RdrNameHsDecl),
	SYN_IE(RdrNameHsExpr),
	SYN_IE(RdrNameHsModule),
	SYN_IE(RdrNameIE),
	SYN_IE(RdrNameImportDecl),
	SYN_IE(RdrNameInstDecl),
	SYN_IE(RdrNameMatch),
	SYN_IE(RdrNameMonoBinds),
	SYN_IE(RdrNamePat),
	SYN_IE(RdrNameHsType),
	SYN_IE(RdrNameQual),
	SYN_IE(RdrNameSig),
	SYN_IE(RdrNameSpecInstSig),
	SYN_IE(RdrNameStmt),
	SYN_IE(RdrNameTyDecl),

	SYN_IE(RdrNameClassOpPragmas),
	SYN_IE(RdrNameClassPragmas),
	SYN_IE(RdrNameDataPragmas),
	SYN_IE(RdrNameGenPragmas),
	SYN_IE(RdrNameInstancePragmas),
	SYN_IE(RdrNameCoreExpr),
	extractHsTyVars,

	RdrName(..),
	qual, varQual, tcQual, varUnqual,
	dummyRdrVarName, dummyRdrTcName,
	isUnqual, isQual,
	showRdr, rdrNameOcc,
	cmpRdr,
	mkOpApp

    ) where

IMP_Ubiq()

import HsSyn
import Lex
import PrelMods		( pRELUDE )
import Name		( ExportFlag(..), Module(..), pprModule,
			  OccName(..), pprOccName )
import Pretty		
import PprStyle		( PprStyle(..) )
import Util		( cmpPString, panic, thenCmp )
\end{code}

\begin{code}
type RdrNameArithSeqInfo	= ArithSeqInfo		Fake Fake RdrName RdrNamePat
type RdrNameBangType		= BangType		RdrName
type RdrNameBind		= Bind			Fake Fake RdrName RdrNamePat
type RdrNameClassDecl		= ClassDecl		Fake Fake RdrName RdrNamePat
type RdrNameClassOpSig		= Sig			RdrName
type RdrNameConDecl		= ConDecl		RdrName
type RdrNameContext		= Context 		RdrName
type RdrNameHsDecl		= HsDecl		Fake Fake RdrName RdrNamePat
type RdrNameSpecDataSig		= SpecDataSig		RdrName
type RdrNameDefaultDecl		= DefaultDecl		RdrName
type RdrNameFixityDecl		= FixityDecl		RdrName
type RdrNameGRHS		= GRHS			Fake Fake RdrName RdrNamePat
type RdrNameGRHSsAndBinds	= GRHSsAndBinds		Fake Fake RdrName RdrNamePat
type RdrNameHsBinds		= HsBinds		Fake Fake RdrName RdrNamePat
type RdrNameHsExpr		= HsExpr		Fake Fake RdrName RdrNamePat
type RdrNameHsModule		= HsModule		Fake Fake RdrName RdrNamePat
type RdrNameIE			= IE			RdrName
type RdrNameImportDecl 		= ImportDecl		RdrName
type RdrNameInstDecl		= InstDecl		Fake Fake RdrName RdrNamePat
type RdrNameMatch		= Match			Fake Fake RdrName RdrNamePat
type RdrNameMonoBinds		= MonoBinds		Fake Fake RdrName RdrNamePat
type RdrNamePat			= InPat			RdrName
type RdrNameHsType		= HsType		RdrName
type RdrNameQual		= Qualifier		Fake Fake RdrName RdrNamePat
type RdrNameSig			= Sig			RdrName
type RdrNameSpecInstSig		= SpecInstSig 		RdrName
type RdrNameStmt		= Stmt			Fake Fake RdrName RdrNamePat
type RdrNameTyDecl		= TyDecl		RdrName

type RdrNameClassOpPragmas	= ClassOpPragmas	RdrName
type RdrNameClassPragmas	= ClassPragmas		RdrName
type RdrNameDataPragmas		= DataPragmas		RdrName
type RdrNameGenPragmas		= GenPragmas		RdrName
type RdrNameInstancePragmas	= InstancePragmas	RdrName
type RdrNameCoreExpr		= GenCoreExpr		RdrName RdrName RdrName RdrName 
\end{code}

@extractHsTyVars@ looks just for things that could be type variables.
It's used when making the for-alls explicit.

\begin{code}
extractHsTyVars :: HsType RdrName -> [RdrName]
extractHsTyVars ty
  = get ty []
  where
    get (MonoTyApp ty1 ty2)	 acc = get ty1 (get ty2 acc)
    get (MonoListTy tc ty)	 acc = get ty acc
    get (MonoTupleTy tc tys)	 acc = foldr get acc tys
    get (MonoFunTy ty1 ty2)	 acc = get ty1 (get ty2 acc)
    get (MonoDictTy cls ty)	 acc = get ty acc
    get (MonoTyVar tv) 	         acc = insert tv acc
    get (HsPreForAllTy ctxt ty)  acc = foldr (get . snd) (get ty acc) ctxt
    get (HsForAllTy tvs ctxt ty) acc = filter (`notElem` locals) $
				       foldr (get . snd) (get ty acc) ctxt
				     where
				       locals = map getTyVarName tvs

    insert (Qual _ _)	      acc = acc
    insert (Unqual (TCOcc _)) acc = acc
    insert other 	      acc | other `elem` acc = acc
				  | otherwise	     = other : acc
\end{code}


A useful function for building @OpApps@.  The operator is always a variable,
and we don't know the fixity yet.

\begin{code}
mkOpApp e1 op e2 = OpApp e1 (HsVar op) (error "mkOpApp:fixity") e2
\end{code}


%************************************************************************
%*									*
\subsection[RdrName]{The @RdrName@ datatype; names read from files}
%*									*
%************************************************************************

\begin{code}
data RdrName
  = Unqual OccName
  | Qual   Module OccName

qual     (m,n) = Qual m n
tcQual   (m,n) = Qual m (TCOcc n)
varQual  (m,n) = Qual m (VarOcc n)

	-- This guy is used by the reader when HsSyn has a slot for
	-- an implicit name that's going to be filled in by
	-- the renamer.  We can't just put "error..." because
	-- we sometimes want to print out stuff after reading but
	-- before renaming
dummyRdrVarName = Unqual (VarOcc SLIT("V-DUMMY"))
dummyRdrTcName = Unqual (VarOcc SLIT("TC-DUMMY"))

varUnqual n = Unqual (VarOcc n)

isUnqual (Unqual _) = True
isUnqual (Qual _ _) = False

isQual (Unqual _) = False
isQual (Qual _ _) = True

cmpRdr (Unqual  n1) (Unqual  n2) = n1 `cmp` n2
cmpRdr (Unqual  n1) (Qual m2 n2) = LT_
cmpRdr (Qual m1 n1) (Unqual  n2) = GT_
cmpRdr (Qual m1 n1) (Qual m2 n2) = (n1 `cmp` n2) `thenCmp` (_CMP_STRING_ m1 m2)
				   -- always compare module-names *second*

rdrNameOcc :: RdrName -> OccName
rdrNameOcc (Unqual occ) = occ
rdrNameOcc (Qual _ occ) = occ

instance Text RdrName where -- debugging
    showsPrec _ rn = showString (ppShow 80 (ppr PprDebug rn))

instance Eq RdrName where
    a == b = case (a `cmp` b) of { EQ_ -> True;  _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False; _ -> True }

instance Ord RdrName where
    a <= b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }

instance Ord3 RdrName where
    cmp = cmpRdr

instance Outputable RdrName where
    ppr sty (Unqual n) = pprOccName sty n
    ppr sty (Qual m n) = ppBesides [pprModule sty m, ppStr ".", pprOccName sty n]

instance NamedThing RdrName where		-- Just so that pretty-printing of expressions works
    getOccName = rdrNameOcc
    getName = panic "no getName for RdrNames"

showRdr sty rdr = ppShow 100 (ppr sty rdr)
\end{code}


%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
#include "HsVersions.h"

module RnHsSyn where

import Ubiq

import HsSyn

import Name		( isLocalName, nameUnique, Name, RdrName )
import Id		( GenId, Id(..) )
import Outputable	( Outputable(..) )
import PprType		( GenType, GenTyVar, TyCon )
import PprStyle		( PprStyle(..) )
import Pretty
import TyCon		( TyCon )
import TyVar		( GenTyVar )
import Unique		( Unique )
import Util		( panic, pprPanic )
\end{code}

\begin{code}
data RnName
  = WiredInId       Id
  | WiredInTyCon    TyCon
  | RnName          Name        -- funtions/binders/tyvars
  | RnSyn           Name        -- type synonym
  | RnData          Name [Name] -- data type   (with constrs)
  | RnConstr        Name  Name  -- constructor (with data type)
  | RnClass         Name [Name] -- class       (with class ops)
  | RnClassOp       Name  Name  -- class op    (with class)
  | RnImplicit      Name      	-- implicitly imported
  | RnImplicitTyCon Name      	-- implicitly imported
  | RnImplicitClass Name      	-- implicitly imported
  | RnUnbound	    RdrName    	-- place holder

mkRnName          = RnName
mkRnImplicit      = RnImplicit
mkRnImplicitTyCon = RnImplicitTyCon
mkRnImplicitClass = RnImplicitClass
mkRnUnbound       = RnUnbound

isRnWired (WiredInId _)    = True
isRnWired (WiredInTyCon _) = True
isRnWired _ 	           = False

isRnLocal (RnName n) = isLocalName n
isRnLocal _ 	     = False


isRnTyCon (WiredInTyCon _)    = True
isRnTyCon (RnSyn _)    	      = True
isRnTyCon (RnData _ _) 	      = True
isRnTyCon (RnImplicitTyCon _) = True
isRnTyCon _            	      = False

isRnClass (RnClass _ _)       = True
isRnClass (RnImplicitClass _) = True
isRnClass _                   = False

isRnClassOp cls (RnClassOp _ op_cls) = eqUniqsNamed cls op_cls
isRnClassOp cls (RnImplicit _)	     = True	-- ho hummm ...
isRnClassOp cls _		     = False

isRnImplicit (RnImplicit _)      = True
isRnImplicit (RnImplicitTyCon _) = True
isRnImplicit (RnImplicitClass _) = True
isRnImplicit _			 = False

isRnUnbound (RnUnbound _) = True
isRnUnbound _		  = False

-- Very general NamedThing comparison, used when comparing
-- Uniquable things with different types

eqUniqsNamed  n1 n2 = uniqueOf n1  ==   uniqueOf n2
cmpUniqsNamed n1 n2 = uniqueOf n1 `cmp` uniqueOf n2

instance Eq RnName where
    a == b = eqUniqsNamed a b

instance Ord3 RnName where
    a `cmp` b = cmpUniqsNamed a b

instance Uniquable RnName where
    uniqueOf = nameUnique . getName

instance NamedThing RnName where
    getName (WiredInId id)    = getName id
    getName (WiredInTyCon tc) = getName tc
    getName (RnName n)	      = n
    getName (RnSyn n)	      = n
    getName (RnData n _)      = n
    getName (RnConstr n _)    = n
    getName (RnClass n _)     = n
    getName (RnClassOp n _)   = n
    getName (RnImplicit n)    = n
    getName (RnUnbound occ)   = pprPanic "getRnName:RnUnbound" (ppr PprDebug occ)

instance Outputable RnName where
#ifdef DEBUG
    ppr sty@PprShowAll (RnData n cs)   = ppBesides [ppr sty n, ppStr "{-", ppr sty cs, ppStr "-}"]
    ppr sty@PprShowAll (RnConstr n d)  = ppBesides [ppr sty n, ppStr "{-", ppr sty d, ppStr "-}"]
    ppr sty@PprShowAll (RnClass n ops) = ppBesides [ppr sty n, ppStr "{-", ppr sty ops, ppStr "-}"]
    ppr sty@PprShowAll (RnClassOp n c) = ppBesides [ppr sty n, ppStr "{-", ppr sty c, ppStr "-}"]
#endif
    ppr sty (WiredInId id)      = ppr sty id
    ppr sty (WiredInTyCon tycon)= ppr sty tycon
    ppr sty (RnUnbound occ)	= ppBeside (ppr sty occ) (ppPStr SLIT("{-UNBOUND-}"))
    ppr sty rn_name		= ppr sty (getName rn_name)
\end{code}

\begin{code}
type RenamedArithSeqInfo	= ArithSeqInfo		Fake Fake RnName RenamedPat
type RenamedBind		= Bind			Fake Fake RnName RenamedPat
type RenamedClassDecl		= ClassDecl		Fake Fake RnName RenamedPat
type RenamedClassOpSig		= Sig			RnName
type RenamedConDecl		= ConDecl		RnName
type RenamedContext		= Context 		RnName
type RenamedSpecDataSig		= SpecDataSig		RnName
type RenamedDefaultDecl		= DefaultDecl		RnName
type RenamedFixityDecl		= FixityDecl		RnName
type RenamedGRHS		= GRHS			Fake Fake RnName RenamedPat
type RenamedGRHSsAndBinds	= GRHSsAndBinds		Fake Fake RnName RenamedPat
type RenamedHsBinds		= HsBinds		Fake Fake RnName RenamedPat
type RenamedHsExpr		= HsExpr		Fake Fake RnName RenamedPat
type RenamedHsModule		= HsModule		Fake Fake RnName RenamedPat
type RenamedInstDecl		= InstDecl		Fake Fake RnName RenamedPat
type RenamedMatch		= Match			Fake Fake RnName RenamedPat
type RenamedMonoBinds		= MonoBinds		Fake Fake RnName RenamedPat
type RenamedMonoType		= MonoType		RnName
type RenamedPat			= InPat			RnName
type RenamedPolyType		= PolyType		RnName
type RenamedRecordBinds		= HsRecordBinds		Fake Fake RnName RenamedPat
type RenamedQual		= Qual			Fake Fake RnName RenamedPat
type RenamedSig			= Sig			RnName
type RenamedSpecInstSig		= SpecInstSig 		RnName
type RenamedStmt		= Stmt			Fake Fake RnName RenamedPat
type RenamedTyDecl		= TyDecl		RnName

type RenamedClassOpPragmas	= ClassOpPragmas	RnName
type RenamedClassPragmas	= ClassPragmas		RnName
type RenamedDataPragmas		= DataPragmas		RnName
type RenamedGenPragmas		= GenPragmas		RnName
type RenamedInstancePragmas	= InstancePragmas	RnName
\end{code}

\begin{code}
collectQualBinders :: [RenamedQual] -> [RnName]

collectQualBinders quals
  = concat (map collect quals)
  where
    collect (GeneratorQual pat _) = collectPatBinders pat
    collect (FilterQual expr)	  = []
    collect (LetQual    binds)	  = collectTopLevelBinders binds
\end{code}


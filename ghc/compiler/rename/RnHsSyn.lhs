%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
#include "HsVersions.h"

module RnHsSyn where

IMP_Ubiq()

import HsSyn

import Id		( isDataCon, GenId, SYN_IE(Id) )
import Name		( isLocalName, nameUnique, Name, RdrName(..),
			  mkLocalName
			)
import Outputable	( Outputable(..){-instance * []-} )
import PprStyle		( PprStyle(..) )
import PprType		( GenType, GenTyVar, TyCon )
import Pretty
import TyCon		( TyCon )
import TyVar		( GenTyVar )
import Unique		( mkAlphaTyVarUnique, Unique )
import Util		( panic, pprPanic{-, pprTrace ToDo:rm-} )
\end{code}

\begin{code}
data RnName
  = WiredInId       Id
  | WiredInTyCon    TyCon
  | RnName          Name        	-- functions/binders/tyvars
  | RnSyn           Name        	-- type synonym
  | RnData          Name [Name] [Name]	-- data type   (with constrs and fields)
  | RnConstr        Name  Name		-- constructor (with data type)
  | RnField	    Name  Name	  	-- field       (with data type)
  | RnClass         Name [Name] 	-- class       (with class ops)
  | RnClassOp       Name  Name  	-- class op    (with class)
  | RnImplicit      Name      		-- implicitly imported
  | RnImplicitTyCon Name      		-- implicitly imported
  | RnImplicitClass Name      		-- implicitly imported
  | RnUnbound	    RdrName    		-- place holder

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
isRnTyCon (RnData _ _ _)      = True
isRnTyCon (RnImplicitTyCon _) = True
isRnTyCon _            	      = False

isRnClass (RnClass _ _)       = True
isRnClass (RnImplicitClass _) = True
isRnClass _                   = False

-- a common need: isRnTyCon || isRnClass:
isRnTyConOrClass (WiredInTyCon _)    = True
isRnTyConOrClass (RnSyn _)    	     = True
isRnTyConOrClass (RnData _ _ _)	     = True
isRnTyConOrClass (RnImplicitTyCon _) = True
isRnTyConOrClass (RnClass _ _)       = True
isRnTyConOrClass (RnImplicitClass _) = True
isRnTyConOrClass _                   = False

isRnConstr (RnConstr _ _) = True
isRnConstr (WiredInId id) = isDataCon id
isRnConstr  _		  = False

isRnField  (RnField _ _)  = True
isRnField  _		  = False

isRnClassOp cls (RnClassOp _ op_cls) = eqUniqsNamed cls op_cls
isRnClassOp cls n		     = True -- pprTrace "isRnClassOp:" (ppr PprShowAll n) $ True -- let it past anyway

isRnImplicit (RnImplicit _)      = True
isRnImplicit (RnImplicitTyCon _) = True
isRnImplicit (RnImplicitClass _) = True
isRnImplicit _			 = False

isRnUnbound (RnUnbound _) = True
isRnUnbound _		  = False

isRnEntity (WiredInId _)       = True
isRnEntity (WiredInTyCon _)    = True
isRnEntity (RnName n)	       = not (isLocalName n)
isRnEntity (RnSyn _)           = True
isRnEntity (RnData _ _ _)      = True
isRnEntity (RnClass _ _)       = True
isRnEntity _                   = False

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
    getName (WiredInId id)      = getName id
    getName (WiredInTyCon tc)   = getName tc
    getName (RnName n)	        = n
    getName (RnSyn n)	        = n
    getName (RnData n _ _)      = n
    getName (RnConstr n _)      = n
    getName (RnField n _)       = n
    getName (RnClass n _)       = n
    getName (RnClassOp n _)     = n
    getName (RnImplicit n)      = n
    getName (RnImplicitTyCon n) = n
    getName (RnImplicitClass n) = n
    getName (RnUnbound occ)     = --pprTrace "getRnName:RnUnbound: " (ppr PprDebug occ)
				  (case occ of
				     Unqual n -> mkLocalName bottom n False bottom2
				     Qual m n -> mkLocalName bottom n False bottom2)
			        where bottom = mkAlphaTyVarUnique 0 -- anything; just something that will print
				      bottom2 = panic "getRnName: srcloc"

instance Outputable RnName where
#ifdef DEBUG
    ppr sty@PprShowAll (RnData n cs fs)  = ppBesides [ppr sty n, ppStr "{-", ppr sty cs, ppr sty fs, ppStr "-}"]
    ppr sty@PprShowAll (RnConstr n d)    = ppBesides [ppr sty n, ppStr "{-", ppr sty d, ppStr "-}"]
    ppr sty@PprShowAll (RnField  n d)    = ppBesides [ppr sty n, ppStr "{-", ppr sty d, ppStr "-}"]
    ppr sty@PprShowAll (RnClass n ops)   = ppBesides [ppr sty n, ppStr "{-", ppr sty ops, ppStr "-}"]
    ppr sty@PprShowAll (RnClassOp n c)   = ppBesides [ppr sty n, ppStr "{-", ppr sty c, ppStr "-}"]
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
type RenamedQual		= Qualifier		Fake Fake RnName RenamedPat
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

fixDeclName :: FixityDecl name -> name
fixDeclName (InfixL name i) = name
fixDeclName (InfixR name i) = name
fixDeclName (InfixN name i) = name
\end{code}


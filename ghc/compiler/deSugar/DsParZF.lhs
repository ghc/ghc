%************************************************************************
%*									*
\section[DsParZF]{Desugaring Parallel ZF expressisions}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"
module DsParZF	where

IMPORT_Trace		-- ToDo: rm

import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer
import AbsPrel		( mkFunTy , eRROR_ID , integerTy,
			  fromDomainId , toDomainId)
import DsExpr		( dsExpr )
import DsUtils		( mkSelectorBinds , EquationInfo(..))
import Match		( match )
import FiniteMap	-- WAS: Set
import FreeVars
import SrcLoc
import BasicLit		 ( BasicLit(..) )
import Util
\end{code}

The purpose of the module is to convert the abstract syntax representation
of parallel ZF expressions into the core syntax representation. The two 
representations differ in that the core syntax only contains binders in 
drawn and index from generators. 

\begin{description}
\item[The ``Idea''] For each pattern in a generator we apply the function 
$\lambda hole\ .\ {\cal D}[[{\tt (\\pat ->}\ hole {\tt )x}]]$ to 
{\em every} expression in an inner scope than that of the definition of 
the pattern; {\tt x} represents the binder in the generator after translation,
${\cal D}[[exp]]$ represents the desugaring of the expression $exp$.

\item[Optimising the ``Idea''] We catagorise each pattern into two types;
simple patterns in which their are no binders, and complex patterns. We
only apply simple patterns to the left handside of a ZF expressions, and 
complex patterns to expressions in which the intersection of the free
variables of the expression, and the binders of the pattern is non-empty.
\end{description}

%************************************************************************
%*									*
\subsection[dsParallelZF]{Interface to the outside world}
%*									*
%************************************************************************

\begin{code}
dsParallelZF::TypecheckedExpr -> TypecheckedParQuals -> DsM PlainCoreExpr
dsParallelZF expr quals 
  = dsParQuals quals			`thenDs`	(\ (quals',hf)	 ->
    dsExpr expr				`thenDs`	( \ expr'	 ->
    let_1_0 (typeOfCoreExpr expr')			( \ ty		 ->
    returnDs (CoZfExpr (applyHoleLhsExpr ty expr' hf) quals') )))
\end{code}

%************************************************************************
%*									*
\subsection[dsZF_datatype]{DataType used to represent ``HoleFunction''}
%*									*
%************************************************************************

\begin{code}
type HoleFunction = (UniType -> PlainCoreExpr -> PlainCoreExpr,
		     [(PlainCoreExpr -> Bool,
		       UniType -> PlainCoreExpr -> PlainCoreExpr)])
\end{code}

\begin{code}
combine fn fn' = \t e -> fn t (fn' t e)
\end{code}

\begin{code}
combineHoles:: HoleFunction -> HoleFunction -> HoleFunction
combineHoles (lhs,rhs) (lhs',rhs') 
   = (combine lhs lhs',rhs++rhs')
\end{code}

\begin{code}
identityHole::HoleFunction
identityHole = (\t e -> e,[])
\end{code}

\begin{code}
applyHoleLhsExpr:: UniType	
		-> PlainCoreExpr 
		-> HoleFunction 
		-> PlainCoreExpr
applyHoleLhsExpr ty expr (lhs,rhs)
   = (combine lhs (foldr combine (\t e -> e) (map snd rhs))) ty expr
\end{code}

\begin{code}
applyHoleRhsExpr ty expr (_,rhs)
   = (foldr combine (\t e -> e) [ y | (x,y) <- rhs, (x expr)]) ty expr
\end{code}

\begin{code}
applyHoleFunction :: PlainCoreParQuals
		  -> HoleFunction
		  -> PlainCoreParQuals
applyHoleFunction (CoAndQuals left right) hf
   = CoAndQuals (applyHoleFunction left hf) (applyHoleFunction right hf)

applyHoleFunction (CoParFilter expr) hf
   = CoParFilter (applyHoleRhsExpr (typeOfCoreExpr expr) expr hf)

applyHoleFunction (CoDrawnGen pats pat expr) hf
   = CoDrawnGen pats pat (applyHoleRhsExpr (typeOfCoreExpr expr) expr hf)

applyHoleFunction (CoIndexGen exprs pat expr) hf
   = CoIndexGen (map (\x -> applyHoleRhsExpr (typeOfCoreExpr x) x hf) exprs) 
		pat 
		(applyHoleRhsExpr (typeOfCoreExpr expr) expr hf)
\end{code}

%************************************************************************
%*									*
\subsection[dsParQuals]{Desugaring the qualifiers}
%*									*
%************************************************************************

\begin{code}
dsParQuals::TypecheckedParQuals 
	   -> DsM (PlainCoreParQuals,HoleFunction)
\end{code}

\begin{code}
dsParQuals (AndParQuals left right) 
   = dsParQuals left		`thenDs`      (\ (left', hfleft)  ->
     dsParQuals right		`thenDs`      (\ (right',hfright) ->
     returnDs (CoAndQuals left'	 (applyHoleFunction right' hfleft), 
	       combineHoles hfleft hfright) ))
 
\end{code}

\begin{code}
dsParQuals (ParFilter expr)
   = dsExpr expr		`thenDs`	(\ expr' ->
     returnDs (CoParFilter expr', identityHole) )

dsParQuals (DrawnGenOut pats convs pat dRHS) 
   = listDs  (map dsExpr convs)	`thenDs`	(\ convs'	   ->
     listDs  (map prettyNewLocalDs pats)	
				`thenDs`	(\ binders	   ->
     listDs (zipWith3 dsPid  pats binders convs')	
				`thenDs`	(\ hfList	   ->
     let_1_0 (foldr1 (combineHoles) hfList)	(\ hf		   ->
     prettyNewLocalDs pat	`thenDs`	(\ iden		   ->  
     duplicateLocalDs iden	`thenDs`	(\ binder	   ->
     dsPid pat binder (CoLam [iden] (CoVar iden))
				`thenDs`	(\ hf'		   ->
     dsExpr dRHS		`thenDs`	(\ dRHS'	   ->
     returnDs (CoDrawnGen binders binder dRHS', 
	       combineHoles hf hf') ))))))))


dsParQuals (IndexGen exprs pat iRHS)
   = listDs (map dsExpr exprs)	`thenDs`	(\ exprs'	 ->
     prettyNewLocalDs pat	`thenDs`	(\ binder	   -> 
     duplicateLocalDs binder	`thenDs`	(\ iden	       ->
     dsPid pat binder (CoLam [iden] (CoVar iden))
				`thenDs`	(\ hf		 ->
     dsExpr iRHS		`thenDs`	(\ iRHS'	 ->
     returnDs (CoIndexGen exprs' binder iRHS' ,hf) )))))	

\end{code}

\begin{code}
dsPid:: TypecheckedPat			-- Pattern to be desugared
     -> Id				-- Patterns desugared binder
     -> PlainCoreExpr			-- Conversion function
     -> DsM HoleFunction			

dsPid pat binder conv
  = duplicateLocalDs binder	`thenDs`		(\ lambdaBind	 ->
    getSrcLocDs			`thenDs`		(\ (sfile,sline) ->
    let_1_0 ("\""++sfile++"\", line "++sline++" : "++
	     "Processor not defined\n")			( \ errorStr	 ->
    getUniqueSupplyDs		`thenDs`		(\ us		 ->
    let_1_0 (collectTypedPatBinders pat)		(\ patBinders	 ->
    case (null patBinders) of
    True  -> returnDs (mkHole lambdaBind errorStr us,[])
    False -> 
       returnDs (\t e -> e, [(mkPredicate patBinders,
			      mkHole lambdaBind errorStr us)]) )))))
   
  where
     mkPredicate b e
	= let_1_0 (freeStuff b e)	(\ ((fvSet,_),_) ->
	  let_1_0 (mkSet b)		(\ bSet		 ->
	  not (isEmptySet (intersect fvSet bSet)) ))

     mkHole lambdaBind errorStr us
	= \ ty expr ->
	     (CoApp
		(CoLam
		   [lambdaBind]
		   (snd (initDs
			   us
			   nullIdEnv
			   (\ _ -> False)	-- Hack alert!!!
			   (panic "mkHole: module name")
			   (match [lambdaBind] [([pat], \x -> expr)] 
				  (CoApp 
				     (mkCoTyApp (CoVar eRROR_ID) ty) 
				     (CoLit (NoRepStr (_PK_ errorStr))))))))
		(CoApp conv (CoVar binder)))
\end{code} 

In the mkHole function we need to conjure up some state so we can
use the match function...
%************************************************************************
%*									*
\subsection[prettyLocals]{Make a new binder; try and keep names nice :-)}
%*									*
%************************************************************************

\begin{code}
prettyNewLocalDs::TypecheckedPat -> DsM Id
prettyNewLocalDs (VarPat id)  = duplicateLocalDs id
prettyNewLocalDs (AsPat id _) = duplicateLocalDs id
preetyNewLocalDs pat	      = let_1_0 (typeOfPat pat)		(\ pat_ty->
				newSysLocalDs pat_ty
				)
\end{code}

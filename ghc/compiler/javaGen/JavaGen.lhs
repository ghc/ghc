%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Generate Java}

\begin{code}
module JavaGen( javaGen ) where

import Java

import Literal	( Literal(..) )
import Id	( Id, isDataConId_maybe, isId, idName, isDeadBinder )
import Name	( NamedThing(..), getOccString, isGlobalName )
import DataCon	( DataCon, dataConRepArity, dataConId )
import qualified CoreSyn 
import CoreSyn	( CoreBind, CoreExpr, CoreAlt, CoreBndr,
		  Bind(..), Alt, AltCon(..), collectBinders, isValArg
		)
import CoreUtils( exprIsValue, exprIsTrivial )
import Module	( Module, moduleString )
import TyCon	( TyCon, isDataTyCon, tyConDataCons )
import Outputable

#include "HsVersions.h"

\end{code}


\begin{code}
javaGen :: Module -> [Module] -> [TyCon] -> [CoreBind] -> CompilationUnit

javaGen mod import_mods tycons binds
  = Package [moduleString mod] decls
  where
    decls = [Import [moduleString mod] | mod <- import_mods] ++
	    concat (map javaTyCon (filter isDataTyCon tycons)) ++ 
	    concat (map javaTopBind binds)
\end{code}


%************************************************************************
%*									*
\subsection{Type declarations}
%*									*
%************************************************************************

\begin{code}
javaTyCon :: TyCon -> [Decl]
--  	public class List {}
--
--	public class $wCons extends List {
--		Object f1; Object f2
--	}
--	public class $wNil extends List {}

javaTyCon tycon 
  = tycon_jclass : map constr_class constrs
  where
    constrs = tyConDataCons tycon
    tycon_jclass_jname = javaName tycon
    tycon_jclass = Class [Public] tycon_jclass_jname [] [] []

    constr_class data_con
	= Class [Public] constr_jname [tycon_jclass_jname] [] field_decls
	where
	  constr_jname = javaConstrWkrName data_con
	  enter_meth   = Method [Public] objectType enterName [] stmts
	  n_val_args   = dataConRepArity data_con
	  field_names  = map fieldName [1..n_val_args]
	  field_decls  = [Field [Public] objectType f Nothing | f <- field_names]
	  stmts	       = vmCOLLECT n_val_args (Var thisName) ++
		     	 [var [Final] objectType f vmPOP | f <- field_names] ++
		     	 [Return (New constr_jname (map Var field_names) Nothing)]
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
javaTopBind :: CoreBind -> [Decl]
javaTopBind (NonRec bndr rhs) = [java_top_bind bndr rhs]
javaTopBind (Rec prs) 	      = [java_top_bind bndr rhs | (bndr,rhs) <- prs]

java_top_bind :: Id -> CoreExpr -> Decl
-- 	public class f implements Code {
--	  public Object ENTER() { ...translation of rhs... }
--	}
java_top_bind bndr rhs
  = Class [Public] (javaName bndr) [] [codeName] [enter_meth]
  where
    enter_meth = Method [Public] objectType enterName [] (javaExpr rhs)
\end{code}


%************************************************************************
%*									*
\subsection{Expressions}
%*									*
%************************************************************************

\begin{code}
javaVar :: Id -> Expr
javaVar v | isGlobalName (idName v) = New (javaName v) [] Nothing
	  | otherwise	  	    = Var (javaName v)


javaLit :: Literal.Literal -> Lit
javaLit (MachInt i)  = UIntLit (fromInteger i)
javaLit (MachChar c) = UCharLit c
javaLit other	     = pprPanic "javaLit" (ppr other)

javaExpr :: CoreExpr -> [Statement]
-- Generate code to apply the value of 
-- the expression to the arguments aleady on the stack
javaExpr (CoreSyn.Var v)   = [Return (javaVar v)]
javaExpr (CoreSyn.Lit l)   = [Return (Literal (javaLit l))]
javaExpr (CoreSyn.App f a) = javaApp f [a]
javaExpr e@(CoreSyn.Lam _ _) = javaLam (collectBinders e)
javaExpr (CoreSyn.Case e x alts) = javaCase e x alts
javaExpr (CoreSyn.Let bind body) = javaBind bind ++ javaExpr body
javaExpr (CoreSyn.Note _ e)	 = javaExpr e

javaCase :: CoreExpr -> Id -> [CoreAlt] -> [Statement]
-- 	case e of x { Nil      -> r1
--		      Cons p q -> r2 }
-- ==>
--	final Object x = VM.WHNF(...code for e...)
--	else if x instance_of Nil {
--		...translation of r1...
--	} else if x instance_of Cons {
--		final Object p = ((Cons) x).f1
--		final Object q = ((Cons) x).f2
--		...translation of r2...
--	} else return null

javaCase e x alts
  =  [var [Final] objectType (javaName x) (vmWHNF (javaArg e)),
      IfThenElse (map mk_alt alts) Nothing]
  where
     mk_alt (DEFAULT, [], rhs)   = (true, 	    Block (javaExpr rhs))
     mk_alt (DataAlt d, bs, rhs) = (instanceOf x d, Block (bind_args d bs ++ javaExpr rhs))
     mk_alt alt@(LitAlt _, _, _) = pprPanic "mk_alt" (ppr alt)

     bind_args d bs = [var [Final] objectType (javaName b) 
			   (Access (Cast (Type (javaConstrWkrName d)) (javaVar x)) f)
		      | (b, f) <- filter isId bs `zip` map fieldName [1..],
			not (isDeadBinder b)
		      ]

javaBind (NonRec x rhs)
{-
	x = ...rhs_x...
  ==>
	final Object x = new Thunk( new Code() { ...code for rhs_x... } )
-}
  = [var [Final] objectType (javaName x) (javaArg rhs)]

javaBind (Rec prs)
{- 	rec { x = ...rhs_x...; y = ...rhs_y... }
  ==>
	class x implements Code {
	  Code x, y;
	  public Object ENTER() { ...code for rhs_x...}
	}
	...ditto for y...

	final x x_inst = new x();
	...ditto for y...

	final Thunk x = new Thunk( x_inst );
	...ditto for y...

	x_inst.x = x;
	x_inst.y = y;
	...ditto for y...
-}
  = (map mk_class prs) ++ (map mk_inst prs) ++ 
    (map mk_thunk prs) ++ concat (map mk_knot prs)
  where
    mk_class (b,r) = Declaration (Class [] (javaName b) [] [codeName] stmts)
		   where
		     stmts = [Field [] codeType (javaName b) Nothing | (b,_) <- prs] ++
			     [Method [Public] objectType enterName [] (javaExpr r)]	

    mk_inst (b,r) = var [Final] (Type (javaName b)) (javaInstName b)
			(New (javaName b) [] Nothing)

    mk_thunk (b,r) = var [Final] thunkType (javaName b)
			 (New thunkName [Var (javaInstName b)] Nothing)

    mk_knot (b,_) = [ExprStatement (Assign lhs rhs) 
		    | (b',_) <- prs,
		      let lhs = Access (Var (javaInstName b)) (javaName b'),
		      let rhs = Var (javaName b')
		    ]
		
javaLam :: ([CoreBndr], CoreExpr) -> [Statement]
javaLam (bndrs, body)
  | null val_bndrs = javaExpr body
  | otherwise
  =  vmCOLLECT (length val_bndrs) (Var thisName)
  ++ [var [Final] objectType (javaName n) vmPOP | n <- val_bndrs]
  ++ javaExpr body
  where
    val_bndrs = filter isId bndrs

javaApp :: CoreExpr -> [CoreExpr] -> [Statement]
javaApp (CoreSyn.App f a) as = javaApp f (a:as)
javaApp (CoreSyn.Var f) as
  = case isDataConId_maybe f of {
	Just dc | length as == dataConRepArity dc
		-> 	-- Saturated constructors
		   [Return (New (javaName f) (javaArgs as) Nothing)]

    ; other ->   -- Not a saturated constructor
	java_apply (CoreSyn.Var f) as
    }
	
javaApp f as = java_apply f as

java_apply :: CoreExpr -> [CoreExpr] -> [Statement]
java_apply f as = [ExprStatement (vmPUSH arg) | arg <- javaArgs as] ++ javaExpr f

javaArgs :: [CoreExpr] -> [Expr]
javaArgs args = [javaArg a | a <- args, isValArg a]

javaArg :: CoreExpr -> Expr
javaArg (CoreSyn.Type t) = pprPanic "javaArg" (ppr t)
javaArg e | exprIsValue e || exprIsTrivial e = newCode (javaExpr e)
	  | otherwise	 		     = newThunk (newCode (javaExpr e))
\end{code}

%************************************************************************
%*									*
\subsection{Helper functions}
%*									*
%************************************************************************

\begin{code}
true, this :: Expr
this = Var thisName

true = Var ["true"]

vmCOLLECT :: Int -> Expr -> [Statement]
vmCOLLECT 0 e = []
vmCOLLECT n e = [ExprStatement (Call (Var vmName) ["COLLECT"] [Literal (IntLit n), e])]

vmPOP :: Expr
vmPOP = Call (Var vmName) ["POP"] []

vmPUSH :: Expr -> Expr
vmPUSH e = Call (Var vmName) ["PUSH"] [e]

var :: [Modifier] -> Type -> Name -> Expr -> Statement
var ms ty field_name value = Declaration (Field ms ty field_name (Just value))

vmWHNF :: Expr -> Expr
vmWHNF e = Call (Var vmName) ["WHNF"] [e]

instanceOf :: Id -> DataCon -> Expr
instanceOf x data_con
  = InstanceOf (Var (javaName x)) (Type (javaConstrWkrName data_con))

newCode :: [Statement] -> Expr
newCode [Return e] = e
newCode stmts	   = New codeName [] (Just [Method [Public] objectType enterName [] stmts])

newThunk :: Expr -> Expr
newThunk e = New thunkName [e] Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Name mangling}
%*									*
%************************************************************************

\begin{code}
codeName, enterName, vmName :: Name
codeName  = ["Code"]
thunkName = ["Thunk"]
enterName = ["ENTER"]
vmName    = ["VM"]
thisName  = ["this"]

fieldName :: Int -> Name	-- Names for fields of a constructor
fieldName n = ["f" ++ show n]

javaName :: NamedThing a => a -> Name
javaName n = [getOccString n]

javaConstrWkrName :: DataCon ->  Name
-- The function that makes the constructor
javaConstrWkrName con = [getOccString (dataConId con)]

javaInstName :: NamedThing a => a -> Name
-- Makes x_inst for Rec decls
javaInstName n = [getOccString n ++ "_inst"]
\end{code}

%************************************************************************
%*									*
\subsection{Type mangling}
%*									*
%************************************************************************

\begin{code}
codeType, thunkType, objectType :: Type
objectType = Type ["Object"]
codeType  = Type codeName
thunkType = Type thunkName
\end{code}


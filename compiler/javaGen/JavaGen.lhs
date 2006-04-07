%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\section{Generate Java}

Name mangling for Java.
~~~~~~~~~~~~~~~~~~~~~~

Haskell has a number of namespaces. The Java translator uses
the standard Haskell mangles (see OccName.lhs), and some extra
mangles.

All names are hidden inside packages.

module name:
  - becomes a first level java package.
  - can not clash with java, because haskell modules are upper case,
     java default packages are lower case.

function names: 
  - these turn into classes
  - java keywords (eg. private) have the suffix "zdk" ($k) added.

data *types*
  - These have a base class, so need to appear in the 
    same name space as other object. for example data Foo = Foo
  - We add a postfix to types: "zdc" ($c)
  - Types are upper case, so never clash with keywords

data constructors
  - There are tWO classes for each Constructor
   (1) - Class with the payload extends the relevent datatype baseclass.
       - This class has the prefix zdw ($w)
   (2) - Constructor *wrapper* just use their own name.
    - Constructors are upper case, so never clash with keywords
    - So Foo would become 2 classes.
	* Foo		-- the constructor wrapper
	* zdwFoo	-- the worker, with the payload


$i  for instances.
$k  for keyword nameclash avoidance.

\begin{code}
module JavaGen( javaGen ) where

import Java

import Literal	( Literal(..) )
import Id	( Id, isDataConWorkId_maybe, isId, idName, isDeadBinder, idPrimRep
		, isPrimOpId_maybe )
import Name	( NamedThing(..), getOccString, isExternalName, isInternalName
		, nameModule )
import PrimRep  ( PrimRep(..) )
import DataCon	( DataCon, dataConRepArity, dataConRepArgTys, dataConWorkId )
import qualified Type
import qualified CoreSyn
import CoreSyn	( CoreBind, CoreExpr, CoreAlt, CoreBndr,
		  Bind(..), AltCon(..), collectBinders, isValArg
		)
import TysWiredIn	( boolTy, trueDataCon, falseDataCon )
import qualified CoreUtils
import Module	( Module, moduleString )
import TyCon	( TyCon, isDataTyCon, tyConDataCons )
import Outputable

import Maybe
import PrimOp
import Util     ( lengthIs, notNull )

#include "HsVersions.h"

\end{code}


\begin{code}
javaGen :: Module -> [Module] -> [TyCon] -> [CoreBind] -> CompilationUnit

javaGen mod import_mods tycons binds
  = liftCompilationUnit package
  where
    decls = [Import "haskell.runtime.*"] ++
	    [Import (moduleString mod) | mod <- import_mods] ++
	    concat (map javaTyCon (filter isDataTyCon tycons)) ++ 
	    concat (map javaTopBind binds)
    package = Package (moduleString mod) decls
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
  = tycon_jclass : concat (map constr_class constrs)
  where
    constrs = tyConDataCons tycon
    tycon_jclass_jname =  javaTyConTypeName tycon ++ "zdc"
    tycon_jclass = Class [Public] (shortName tycon_jclass_jname) [] [] []

    constr_class data_con
	= [ Class [Public] constr_jname [tycon_jclass_jname] []
				(field_decls ++ [cons_meth,debug_meth])
	  ]
	where
	  constr_jname = shortName (javaConstrWkrName data_con)

	  field_names  = constrToFields data_con
	  field_decls  = [ Field [Public] n Nothing 
			 | n <- field_names
			 ]

	  cons_meth    = mkCons constr_jname field_names

	  debug_meth   = Method [Public] (Name "toString" stringType)
					 []
					 []
		       (  [ Declaration (Field [] txt Nothing) ]
		       ++ [ ExprStatement
				(Assign (Var txt)
					    (mkStr
						("( " ++ 
						  getOccString data_con ++ 
						  " ")
				       	     )
				)
			  ]
		       ++ [ ExprStatement
				(Assign (Var txt)
				   (Op (Var txt)
				        "+" 
				       (Op (Var n) "+" litSp)
				   )
				)
			  | n <- field_names
			  ]
		       ++ [ Return (Op (Var txt)
				        "+" 
				      (mkStr ")")
				   )
			  ]
		       )

	  litSp    = mkStr " "
	  txt      = Name "__txt" stringType
	 

-- This checks to see the type is reasonable to call new with.
-- primitives might use a static method later.
mkNew :: Type -> [Expr] -> Expr
mkNew t@(PrimType primType) _  = error "new primitive -- fix it???"
mkNew t@(Type _)            es = New t es Nothing
mkNew _                     _  = error "new with strange arguments"

constrToFields :: DataCon -> [Name]
constrToFields cons = 
	[ fieldName i t 
	| (i,t) <- zip [1..] (map primRepToType
			          (map Type.typePrimRep
				       (dataConRepArgTys cons)
				  )
			     )
	]

mkCons :: TypeName -> [Name] -> Decl
mkCons name args = Constructor [Public] name
	[ Parameter [] n | n <- args ]
	[ ExprStatement (Assign 
			   (Access this n)
			   (Var n)
			 )
		    | n <- args ]

mkStr :: String -> Expr
mkStr str = Literal (StringLit str)
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
  = Class [Public] (shortName (javaIdTypeName bndr))
		[] [codeName] [enter_meth]
  where
    enter_meth = Method [Public]
			enterName
			[vmArg]
			[excName]
			(javaExpr vmRETURN rhs)
\end{code}

%************************************************************************
%*									*
\subsection{Expressions}
%*									*
%************************************************************************

\begin{code}
javaVar :: Id -> Expr
javaVar v | isExternalName (idName v) = mkNew (javaIdType v) []
	  | otherwise	  	    =   Var (javaName v)

javaLit :: Literal.Literal -> Expr
javaLit (MachInt i)  = Literal (IntLit (fromInteger i))
javaLit (MachChar c) = Literal (CharLit c)
javaLit (MachStr fs) = Literal (StringLit str)
   where
	str = concatMap renderString (unpackFS fs) ++ "\\000"
	-- This should really handle all the chars 0..31.
	renderString '\NUL' = "\\000"
	renderString other  = [other]

javaLit other	     = pprPanic "javaLit" (ppr other)

-- Pass in the 'shape' of the result.
javaExpr :: (Expr -> Statement) -> CoreExpr -> [Statement]
-- Generate code to apply the value of 
-- the expression to the arguments aleady on the stack
javaExpr r (CoreSyn.Var v)   = [r (javaVar v)]
javaExpr r (CoreSyn.Lit l)   = [r (javaLit l)]
javaExpr r (CoreSyn.App f a) = javaApp r f [a]
javaExpr r e@(CoreSyn.Lam _ _) = javaLam r (collectBinders e)
javaExpr r (CoreSyn.Case e x alts) = javaCase r e x alts
javaExpr r (CoreSyn.Let bind body) = javaBind bind ++ javaExpr r body
javaExpr r (CoreSyn.Note _ e)	 = javaExpr r e

javaCase :: (Expr -> Statement) -> CoreExpr -> Id -> [CoreAlt] -> [Statement]
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
--	} else throw java.lang.Exception

-- This first special case happens a lot, typically
-- during dictionary deconstruction.
-- We need to access at least *one* field, to check to see
-- if we have correct constructor.
-- If we've got the wrong one, this is _|_, and the
-- casting will catch this with an exception.

javaCase r e x [(DataAlt d,bs,rhs)] | notNull bs
  = java_expr PushExpr e ++
    [ var [Final] (javaName x)
	          (whnf primRep (vmPOP (primRepToType primRep))) ] ++
    bind_args d bs ++
    javaExpr r rhs
   where      
     primRep = idPrimRep x
     whnf PtrRep = vmWHNF	-- needs evaluation
     whnf _      = id		-- anything else does notg

     bind_args d bs = [var [Final] (javaName b) 
			   (Access (Cast (javaConstrWkrType d) (javaVar x)
				   ) f
			   )
		      | (b,f) <- filter isId bs `zip` (constrToFields d)
		      , not (isDeadBinder b)
		      ]
   
javaCase r e x alts
  | isIfThenElse && isPrimCmp
  = javaIfThenElse r (fromJust maybePrim) tExpr fExpr
  | otherwise
  = java_expr PushExpr e ++
       [ var [Final] (javaName x)
		           (whnf primRep (vmPOP (primRepToType primRep)))
       , IfThenElse (map mk_alt con_alts) (Just default_code)
       ]
  where
     isIfThenElse = CoreUtils.exprType e `Type.eqType` boolTy
		    -- also need to check that x is not free in
		    -- any of the branches.
     maybePrim    = findCmpPrim e []
     isPrimCmp    = isJust maybePrim
     (_,_,tExpr)  = CoreUtils.findAlt (DataAlt trueDataCon) alts 
     (_,_,fExpr)  = CoreUtils.findAlt (DataAlt falseDataCon) alts 

     primRep = idPrimRep x
     whnf PtrRep = vmWHNF	-- needs evaluation
     whnf _      = id

     (con_alts, maybe_default) = CoreUtils.findDefault alts
     default_code = case maybe_default of
			Nothing  -> ExprStatement (Raise excName [Literal (StringLit "case failure")])
			Just rhs -> Block (javaExpr r rhs)

     mk_alt (DataAlt d,  bs, rhs) = (instanceOf x d, Block (bind_args d bs ++ javaExpr r rhs))
     mk_alt (LitAlt lit, bs, rhs) = (eqLit lit     , Block (javaExpr r rhs))


     eqLit (MachInt n) = Op (Literal (IntLit n))

			    "=="
			    (Var (javaName x))
     eqLit (MachChar n) = Op (Literal (CharLit n))
			    "=="
			    (Var (javaName x))
     eqLit other       = pprPanic "eqLit" (ppr other)

     bind_args d bs = [var [Final] (javaName b) 
			   (Access (Cast (javaConstrWkrType d) (javaVar x)
				   ) f
			   )
		      | (b,f) <- filter isId bs `zip` (constrToFields d)
		      , not (isDeadBinder b)
		      ]

javaIfThenElse r cmp tExpr fExpr 
{-
 - Now what we need to do is generate code for the if/then/else.
 - [all arguments are already check for simpleness (Var or Lit).]
 - 
 - if (<prim> arg1 arg2 arg3 ...) {
 -	trueCode
 -  } else {
 -	falseCode
 - }
 -}
 = [IfThenElse [(cmp,j_tExpr)] (Just j_fExpr)]
 where
   j_tExpr, j_fExpr :: Statement
   j_tExpr = Block (javaExpr r tExpr)
   j_fExpr = Block (javaExpr r fExpr)

javaBind (NonRec x rhs)
{-
	x = ...rhs_x...
  ==>
	final Object x = new Thunk( new Code() { ...code for rhs_x... } )
-}

  = java_expr (SetVar name) rhs
  where
    name = case coreTypeToType rhs of
	    ty@(PrimType _) -> javaName x `withType` ty
	    _               -> javaName x `withType` codeType

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
    mk_class (b,r) = Declaration (Class [] class_name [] [codeName] stmts)
		   where
		     class_name = javaIdTypeName b
		     stmts = [Field [] (javaName b `withType` codeType) Nothing | (b,_) <- prs] ++
			     [Method [Public] enterName [vmArg] [excName] (javaExpr vmRETURN r)]	

    mk_inst (b,r) = var [Final] name (mkNew ty [])
	where
	   name@(Name _ ty)  = javaInstName b

    mk_thunk (b,r) = var [Final] (javaName b `withType` codeType)
			 (mkNew thunkType [Var (javaInstName b)])

    mk_knot (b,_) = [ ExprStatement (Assign lhs rhs) 
		    | (b',_) <- prs,
		      let lhs = Access (Var (javaInstName b)) (javaName b'),
		      let rhs = Var (javaName b')
		    ]

javaLam :: (Expr -> Statement) -> ([CoreBndr], CoreExpr) -> [Statement]
javaLam r (bndrs, body)
  | null val_bndrs = javaExpr r body
  | otherwise
  =  vmCOLLECT (length val_bndrs) this
  ++ [var [Final] n (vmPOP t) | n@(Name _ t) <- val_bndrs]
  ++ javaExpr r body
  where
    val_bndrs = map javaName (filter isId bndrs)

javaApp :: (Expr -> Statement) -> CoreExpr -> [CoreExpr] -> [Statement]
javaApp r (CoreSyn.App f a) as 
	| isValArg a = javaApp r f (a:as)
	| otherwise  = javaApp r f as
javaApp r (CoreSyn.Var f) as 
  = case isDataConWorkId_maybe f of {
	Just dc | as `lengthIs` dataConRepArity dc
	 -- NOTE: Saturated constructors never returning a primitive at this point
	 --
	 -- We push the arguments backwards, because we are using
	 -- the (ugly) semantics of the order of evaluation of arguments,
	 -- to avoid making up local names. Oh to have a namesupply...
	 --
		-> javaArgs (reverse as) ++
		   [r (New (javaIdType f)
			   (javaPops as)
			   Nothing
		       )
		   ]
	        | otherwise ->
		   --  build a local 
		   let stmts = 
			  vmCOLLECT (dataConRepArity dc) this ++
			[ vmRETURN
			   (New (javaIdType f)
				[ vmPOP ty | (Name _ ty) <- constrToFields dc ]
				Nothing
			    )
			]
		   in javaArgs (reverse as) ++ [r (newCode stmts)]
    ; other -> java_apply r (CoreSyn.Var f) as
    }
	
javaApp r f as = java_apply r f as

-- This means, given a expression an a list of arguments,
-- generate code for "pushing the arguments on the stack,
--  and the executing the expression."

java_apply :: (Expr -> Statement) -> CoreExpr -> [CoreExpr] -> [Statement]
java_apply r f as = javaArgs as ++ javaExpr r f

-- This generates statements that have the net effect
-- of pushing values (perhaps thunks) onto the stack.

javaArgs :: [CoreExpr] -> [Statement]
javaArgs args = concat [ java_expr PushExpr a | a <- args, isValArg a]

javaPops :: [CoreExpr] -> [Expr]
javaPops args = [ vmPOP (primRepToType (Type.typePrimRep (CoreUtils.exprType a)))
		| a <- args 
		, isValArg a
		]


-- The result is a list of statments that have the effect of
-- pushing onto the stack (via one of the VM.PUSH* commands)
-- the argument, (or returning, or setting a variable)
-- perhaps thunked.

{- This is mixing two things.
 (1) Optimizations for things like primitives, whnf calls, etc.
 (2) If something needs a thunk constructor round it.
 - Seperate them at some point!
 -}
data ExprRetStyle = SetVar Name | PushExpr | ReturnExpr

java_expr :: ExprRetStyle -> CoreExpr -> [Statement]
java_expr _ (CoreSyn.Type t) = pprPanic "java_expr" (ppr t)
java_expr ret e
   | isPrimCall = [push (fromJust maybePrim)]
	-- This is a shortcut, 
	-- basic names and literals do not need a code block
	-- to compute the value.
   | isPrim primty && CoreUtils.exprIsTrivial e = javaExpr push e
   | isPrim primty =
 	  let expr  = javaExpr vmRETURN e
	      code  = access (vmWHNF (newCode expr)) (primRepToType primty)
	  in [push code]
   | otherwise =
 	  let expr  = javaExpr vmRETURN e
	      code  = newCode expr
	      code' = if CoreUtils.exprIsValue e 
		      || CoreUtils.exprIsTrivial e 
		      || isPrim primty
		      then code
		      else newThunk code
	  in [push code']
   where
	maybePrim  = findFnPrim e []
	isPrimCall = isJust maybePrim

	push e = case ret of
		  SetVar name -> var [Final] name e
		  PushExpr -> vmPUSH e
		  ReturnExpr -> vmRETURN e
	corety = CoreUtils.exprType e
	primty = Type.typePrimRep corety
	isPrim PtrRep  = False	-- only this needs updated
	isPrim _       = True

coreTypeToType = primRepToType . Type.typePrimRep . CoreUtils.exprType

renameForKeywords :: (NamedThing name) => name -> String
renameForKeywords name 
  | str `elem` keywords = "zdk" ++ str
  | otherwise            = str
  where
	str = getOccString name

keywords :: [String]
keywords =
	[ "return"
	, "if"
	, "then"
	, "else"
	, "class"
	, "instance"
	, "import"
	, "throw"
	, "try"
	]

\end{code}

%************************************************************************
%*									*
\subsection{Helper functions}
%*									*
%************************************************************************

\begin{code}
true, this,javaNull :: Expr
this = Var thisName 
true = Var (Name "true" (PrimType PrimBoolean))
javaNull = Var (Name "null" objectType)

vmCOLLECT :: Int -> Expr -> [Statement]
vmCOLLECT 0 e = []
vmCOLLECT n e = [ExprStatement 
		    (Call varVM collectName
			[ Literal (IntLit (toInteger n))
			, e
			]
		    )
		]

vmPOP :: Type -> Expr 
vmPOP ty = Call varVM (Name ("POP" ++ suffix ty) ty) []

vmPUSH :: Expr -> Statement
vmPUSH e = ExprStatement 
	     (Call varVM (Name ("PUSH" ++ suffix (exprType e)) void) [e])

vmRETURN :: Expr -> Statement
vmRETURN e = Return (
     case ty of
	PrimType _ -> Call varVM (Name ("RETURN" ++ suffix ty)
				       valueType
				 ) [e]
	_ -> e)
  where
	ty = exprType e

var :: [Modifier] -> Name -> Expr -> Statement
var ms field_name@(Name _ ty) value 
   | exprType value == ty = Declaration (Field ms field_name (Just value))
   | otherwise            = var ms field_name (Cast ty value)

vmWHNF :: Expr -> Expr
vmWHNF e = Call varVM whnfName [e]

suffix :: Type -> String
suffix (PrimType t) = primName t
suffix _            = ""

primName :: PrimType -> String
primName PrimInt       = "int"
primName PrimChar      = "char"
primName PrimByte      = "byte"
primName PrimBoolean   = "boolean"
primName _             = error "unsupported primitive"

varVM :: Expr
varVM = Var vmName 

instanceOf :: Id -> DataCon -> Expr
instanceOf x data_con
  = InstanceOf (Var (javaName x)) (javaConstrWkrType data_con)

newCode :: [Statement] -> Expr
newCode [Return e] = e
newCode stmts	   = New codeType [] (Just [Method [Public] enterName [vmArg] [excName] stmts])

newThunk :: Expr -> Expr
newThunk e = New thunkType [e] Nothing

vmArg :: Parameter
vmArg = Parameter [Final] vmName

-- This is called with boolean compares, checking 
-- to see if we can do an obvious shortcut.
-- If there is, we return a (GOO) expression for doing this,

-- So if, we have case (#< x y) of { True -> e1; False -> e2 },
-- we will call findCmpFn with (#< x y), this return Just (Op x "<" y)

findCmpPrim :: CoreExpr -> [Expr] -> Maybe Expr
findCmpPrim (CoreSyn.App f a) as =
     case a of
	CoreSyn.Var v -> findCmpPrim f (javaVar v:as)
	CoreSyn.Lit l -> findCmpPrim f (javaLit l:as)
	_ -> Nothing
findCmpPrim (CoreSyn.Var p)   as = 
	case isPrimOpId_maybe p of
	  Just prim -> find_cmp_prim prim as
	  Nothing   -> Nothing
findCmpPrim _                 as = Nothing

find_cmp_prim cmpPrim args@[a,b] = 
   case cmpPrim of
     IntGtOp -> fn ">"
     IntGeOp -> fn ">="
     IntEqOp -> fn "=="
     IntNeOp -> fn "/="
     IntLtOp -> fn "<"
     IntLeOp -> fn "<="
     _ -> Nothing
  where
	fn op = Just (Op a op b)
find_cmp_prim _ _ = Nothing

findFnPrim :: CoreExpr -> [Expr] -> Maybe Expr
findFnPrim (CoreSyn.App f a) as =
     case a of
	CoreSyn.Var v -> findFnPrim f (javaVar v:as)
	CoreSyn.Lit l -> findFnPrim f (javaLit l:as)
	_ -> Nothing
findFnPrim (CoreSyn.Var p)   as = 
	case isPrimOpId_maybe p of
	  Just prim -> find_fn_prim prim as
	  Nothing   -> Nothing
findFnPrim _                 as = Nothing

find_fn_prim cmpPrim args@[a,b] = 
   case cmpPrim of
     IntAddOp -> fn "+"
     IntSubOp -> fn "-"
     IntMulOp -> fn "*"
     _ -> Nothing
  where
	fn op = Just (Op a op b)
find_fn_prim _ _ = Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Haskell to Java Types}
%*									*
%************************************************************************

\begin{code}
exprType (Var (Name _ t)) = t
exprType (Literal lit)    = litType lit
exprType (Cast t _)       = t
exprType (New t _ _)      = t
exprType (Call _ (Name _ t) _) = t
exprType (Access _ (Name _ t)) = t
exprType (Raise t _)           = error "do not know the type of raise!"
exprType (Op _ op _) | op `elem` ["==","/=","<","<=","=>",">"]
		     = PrimType PrimBoolean
exprType (Op x op _) | op `elem` ["+","-","*"]
		     = exprType x
exprType expr = error ("can't figure out an expression type: " ++ show expr)

litType (IntLit i)    = PrimType PrimInt
litType (CharLit i)   = PrimType PrimChar
litType (StringLit i) = stringType	-- later, might use char array?
\end{code}

%************************************************************************
%*									*
\subsection{Name mangling}
%*									*
%************************************************************************

\begin{code}
codeName, excName, thunkName :: TypeName
codeName  = "haskell.runtime.Code"
thunkName = "haskell.runtime.Thunk"
excName   = "java.lang.Exception"

enterName, vmName,thisName,collectName, whnfName :: Name
enterName   = Name "ENTER"   objectType
vmName      = Name "VM"      vmType
thisName    = Name "this"    (Type "<this>")
collectName = Name "COLLECT" void
whnfName    = Name "WHNF"    objectType

fieldName :: Int -> Type -> Name	-- Names for fields of a constructor
fieldName n ty = Name ("f" ++ show n) ty

withType :: Name -> Type -> Name
withType (Name n _) t = Name n t

-- This maps (local only) names Ids to Names, 
-- using the same string as the Id.
javaName :: Id -> Name
javaName n 
  | isExternalName (idName n) = error "useing javaName on global"
  | otherwise = Name (getOccString n)
		     (primRepToType (idPrimRep n))

-- TypeName's are almost always global. This would typically return something
-- like Test.foo or Test.Foozdc or PrelBase.foldr.
-- Local might use locally bound types, (which do not have '.' in them).

javaIdTypeName :: Id -> TypeName
javaIdTypeName n
    | isInternalName n' = renameForKeywords n'
    | otherwise      = moduleString (nameModule n') ++ "." ++ renameForKeywords n'
  where
	     n' = getName n

-- There is no such thing as a local type constructor.

javaTyConTypeName :: TyCon -> TypeName
javaTyConTypeName n = (moduleString (nameModule n') ++ "." ++ renameForKeywords n')
  where
	     n' = getName n

-- this is used for getting the name of a class when defining it.
shortName :: TypeName -> TypeName
shortName = reverse . takeWhile (/= '.') . reverse

-- The function that makes the constructor name
-- The constructor "Foo ..." in module Test,
-- would return the name "Test.Foo".

javaConstrWkrName :: DataCon -> TypeName
javaConstrWkrName = javaIdTypeName . dataConWorkId

-- Makes x_inst for Rec decls
-- They are *never* is primitive
-- and always have local (type) names.
javaInstName :: Id -> Name
javaInstName n = Name (renameForKeywords n ++ "zdi_inst")
		      (Type (renameForKeywords n))
\end{code}

%************************************************************************
%*									*
\subsection{Types and type mangling}
%*									*
%************************************************************************

\begin{code}
-- Haskell RTS types
codeType, thunkType, valueType :: Type
codeType   = Type codeName
thunkType  = Type thunkName
valueType  = Type "haskell.runtime.Value"
vmType     = Type "haskell.runtime.VMEngine"

-- Basic Java types
objectType, stringType :: Type
objectType = Type "java.lang.Object"
stringType = Type "java.lang.String"

void :: Type
void = PrimType PrimVoid

inttype :: Type
inttype = PrimType PrimInt

chartype :: Type
chartype = PrimType PrimChar

bytetype :: Type
bytetype = PrimType PrimByte

-- This lets you get inside a possible "Value" type,
-- to access the internal unboxed object.
access :: Expr -> Type -> Expr
access expr (PrimType prim) = accessPrim (Cast valueType expr) prim
access expr other           = expr

accessPrim expr PrimInt  = Call expr (Name "intValue" inttype) []
accessPrim expr PrimChar = Call expr (Name "charValue" chartype) []
accessPrim expr PrimByte = Call expr (Name "byteValue" bytetype) []
accessPrim expr other    = pprPanic "accessPrim" (text (show other))

-- This is where we map from typename to types,
-- allowing to match possible primitive types.
mkType :: TypeName -> Type
mkType "PrelGHC.Intzh"  = inttype
mkType "PrelGHC.Charzh" = chartype
mkType other            = Type other

-- Turns a (global) Id into a Type (fully qualified name).
javaIdType :: Id -> Type
javaIdType = mkType . javaIdTypeName

javaLocalIdType :: Id -> Type
javaLocalIdType = primRepToType . idPrimRep

primRepToType ::PrimRep -> Type
primRepToType PtrRep  = objectType
primRepToType IntRep  = inttype
primRepToType CharRep = chartype
primRepToType Int8Rep = bytetype
primRepToType AddrRep = objectType
primRepToType other   = pprPanic "primRepToType" (ppr other)

-- The function that makes the constructor name
javaConstrWkrType :: DataCon -> Type
javaConstrWkrType con = Type (javaConstrWkrName con)
\end{code}

%************************************************************************
%*									*
\subsection{Class Lifting}
%*									*
%************************************************************************

This is a very simple class lifter. It works by carrying inwards a
list of bound variables (things that might need to be passed to a
lifted inner class). 
 * Any variable references is check with this list, and if it is
   bound, then it is not top level, external reference. 
 * This means that for the purposes of lifting, it might be free
   inside a lifted inner class.
 * We remember these "free inside the inner class" values, and 
   use this list (which is passed, via the monad, outwards)
   when lifting.

\begin{code}
type Bound = [Name]
type Frees = [Name]

combine :: [Name] -> [Name] -> [Name]
combine []           names          = names
combine names        []             = names
combine (name:names) (name':names') 
	| name < name' = name  : combine names (name':names')
	| name > name' = name' : combine (name:names) names'
	| name == name = name  : combine names names'
	| otherwise    = error "names are not a total order"

both :: [Name] -> [Name] -> [Name]
both []           names          = []
both names        []             = []
both (name:names) (name':names') 
	| name < name' = both names (name':names')
	| name > name' = both (name:names) names'
	| name == name = name  : both names names'
	| otherwise    = error "names are not a total order"

combineEnv :: Env -> [Name] -> Env
combineEnv (Env bound env) new = Env (bound `combine` new) env

addTypeMapping :: TypeName -> TypeName -> [Name] -> Env -> Env
addTypeMapping origName newName frees (Env bound env)
	= Env bound ((origName,(newName,frees)) : env)

-- This a list of bound vars (with types)
-- and a mapping from old class name 
--     to inner class name (with a list of frees that need passed
--	                    to the inner class.)

data Env = Env Bound [(TypeName,(TypeName,[Name]))]

newtype LifterM a = 
  	LifterM { unLifterM ::
		     TypeName ->		-- this class name
		     Int -> 			-- uniq supply
			  ( a			--  *
			    , Frees		-- frees
			    , [Decl]		-- lifted classes
			    , Int		-- The uniqs
			    )
		}

instance Monad LifterM where
	return a = LifterM (\ n s -> (a,[],[],s))
	(LifterM m) >>= fn = LifterM (\ n s ->
	  case m n s of
	    (a,frees,lifted,s) 
	         -> case unLifterM (fn a) n s of
	 	     (a,frees2,lifted2,s) -> ( a
					     , combine frees frees2
					     , lifted ++ lifted2
					     , s)
	  )

liftAccess :: Env -> Name -> LifterM ()
liftAccess env@(Env bound _) name 
	| name `elem` bound = LifterM (\ n s -> ((),[name],[],s))
	| otherwise         = return ()

scopedName :: TypeName -> LifterM a -> LifterM a
scopedName name (LifterM m) =
   LifterM (\ _ s -> 
      case m name 1 of
	(a,frees,lifted,_) -> (a,frees,lifted,s)
      )

genAnonInnerClassName :: LifterM TypeName
genAnonInnerClassName = LifterM (\ n s ->
	( n ++ "$" ++ show s
	, []
	, []
	, s + 1
	)
    )

genInnerClassName :: TypeName -> LifterM TypeName
genInnerClassName name = LifterM (\ n s ->
	( n ++ "$" ++ name 
	, []
	, []
	, s
	)
    )

getFrees  :: LifterM a -> LifterM (a,Frees)
getFrees (LifterM m) = LifterM (\ n s ->
	case m n s of
	  (a,frees,lifted,n) -> ((a,frees),frees,lifted,n)
    )

rememberClass :: Decl -> LifterM ()
rememberClass decl = LifterM (\ n s -> ((),[],[decl],s))


liftCompilationUnit :: CompilationUnit -> CompilationUnit
liftCompilationUnit (Package name ds) = 
    Package name (concatMap liftCompilationUnit' ds)

liftCompilationUnit' :: Decl -> [Decl]
liftCompilationUnit' decl = 
    case unLifterM (liftDecls True (Env [] []) [decl]) [] 1 of
      (ds,_,ds',_) -> ds ++ ds'


-- The bound vars for the current class have
-- already be captured before calling liftDecl,
-- because they are in scope everywhere inside the class.

liftDecl :: Bool -> Env -> Decl -> LifterM Decl
liftDecl = \ top env decl ->
  case decl of
    { Import n -> return (Import n)
    ; Field mfs n e -> 
      do { e <- liftMaybeExpr env e
	 ; return (Field mfs (liftName env n) e)
	 }
    ; Constructor mfs n as ss -> 
      do { let newBound = getBoundAtParameters as
	 ; (ss,_) <- liftStatements (combineEnv env newBound) ss
	 ; return (Constructor mfs n (liftParameters env as) ss)
	 }
    ; Method mfs n as ts ss -> 
      do { let newBound = getBoundAtParameters as
	 ; (ss,_) <- liftStatements (combineEnv env newBound) ss
	 ; return (Method mfs (liftName env n) (liftParameters env as) ts ss)
	 }
    ; Comment s -> return (Comment s)
    ; Interface mfs n is ms -> error "interfaces not supported"
    ; Class mfs n x is ms -> 
      do { let newBound = getBoundAtDecls ms
	 ; ms <- scopedName n
		    (liftDecls False (combineEnv env newBound) ms)
	 ; return (Class mfs n x is ms)
	 }
    }

liftDecls :: Bool -> Env -> [Decl] -> LifterM [Decl]
liftDecls top env = mapM (liftDecl top env)

getBoundAtDecls :: [Decl] -> Bound
getBoundAtDecls = foldr combine [] . map getBoundAtDecl

getBoundAtDecl :: Decl -> Bound
getBoundAtDecl (Field _ n _) = [n]
getBoundAtDecl _             = []

getBoundAtParameters :: [Parameter] -> Bound
getBoundAtParameters = foldr combine [] . map getBoundAtParameter

-- TODO
getBoundAtParameter :: Parameter -> Bound
getBoundAtParameter (Parameter _ n) = [n]


liftStatement :: Env -> Statement -> LifterM (Statement,Env)
liftStatement = \ env stmt ->
  case stmt of 
    { Skip -> return (stmt,env)
    ; Return e -> do { e <- liftExpr env e
		     ; return (Return e,env)
		     } 
    ; Block ss -> do { (ss,env) <- liftStatements env ss
		     ; return (Block ss,env)
		     }
    ; ExprStatement e -> do { e <- liftExpr env e
			    ; return (ExprStatement e,env)
			    }
    ; Declaration decl@(Field mfs n e) ->
      do { e <- liftMaybeExpr env e
	 ; return ( Declaration (Field mfs (liftName env n) e)
		  , env `combineEnv` getBoundAtDecl decl
		  )
	 }
    ; Declaration decl@(Class mfs n x is ms) ->
      do { innerName <- genInnerClassName n
	 ; frees <- liftClass env innerName ms x is
	 ; return ( Declaration (Comment ["lifted " ++  n])
		  , addTypeMapping n innerName frees env
		  )
	 }
    ; Declaration d -> error "general Decl not supported"
    ; IfThenElse ecs s -> ifthenelse env ecs s
    ; Switch e as d -> error "switch not supported"
    } 

ifthenelse :: Env 
	   -> [(Expr,Statement)] 
	   -> (Maybe Statement) 
	   -> LifterM (Statement,Env)
ifthenelse env pairs may_stmt =
  do { let (exprs,stmts) = unzip pairs
     ; exprs <- liftExprs env exprs
     ; (stmts,_) <- liftStatements env stmts
     ; may_stmt <- case may_stmt of
		      Just stmt -> do { (stmt,_) <- liftStatement env stmt
				      ; return (Just stmt)
				      }
		      Nothing -> return Nothing
     ; return (IfThenElse (zip exprs stmts) may_stmt,env)
     }

liftStatements :: Env -> [Statement] -> LifterM ([Statement],Env)
liftStatements env []     = return ([],env)
liftStatements env (s:ss) = 
	do { (s,env) <- liftStatement env s
	   ; (ss,env) <- liftStatements env ss
	   ; return (s:ss,env) 
	   }

liftExpr :: Env -> Expr -> LifterM Expr
liftExpr = \ env expr ->
 case expr of
   { Var n -> do { liftAccess env n 
		 ; return (Var (liftName env n))
	         }
   ; Literal l -> return expr
   ; Cast t e -> do { e <- liftExpr env e
	            ; return (Cast (liftType env t) e) 
	            }
   ; Access e n -> do { e <- liftExpr env e 
			-- do not consider n as an access, because
			-- this is a indirection via a reference
		      ; return (Access e n) 
		      }
   ; Assign l r -> do { l <- liftExpr env l
		      ; r <- liftExpr env r
		      ; return (Assign l r)
		      } 
   ; InstanceOf e t -> do { e <- liftExpr env e
			  ; return (InstanceOf e (liftType env t))
			  }	    
   ; Raise n es -> do { es <- liftExprs env es
		      ; return (Raise n es)
		      }
   ; Call e n es -> do { e <- liftExpr env e
		       ; es <- mapM (liftExpr env) es
		       ; return (Call e n es) 
		       }
   ; Op e1 o e2 -> do { e1 <- liftExpr env e1
		      ;	e2 <- liftExpr env e2
		      ; return (Op e1 o e2)
		      }
   ; New n es ds -> new env n es ds
   }

liftParameter env (Parameter ms n) = Parameter ms (liftName env n)
liftParameters env = map (liftParameter env)

liftName env (Name n t) = Name n (liftType env t)

liftExprs :: Env -> [Expr] -> LifterM [Expr]
liftExprs = mapM . liftExpr


liftMaybeExpr :: Env -> (Maybe Expr) -> LifterM (Maybe Expr)
liftMaybeExpr env Nothing     = return Nothing
liftMaybeExpr env (Just stmt) = do { stmt <- liftExpr env stmt
				     ; return (Just stmt)
				     }



new :: Env -> Type -> [Expr] -> Maybe [Decl] -> LifterM Expr
new env@(Env _ pairs) typ args Nothing =
  do { args <- liftExprs env args
     ; return (liftNew env typ args)
     }
new env typ [] (Just inner) =
  -- anon. inner class
  do { innerName <- genAnonInnerClassName 
     ; frees <- liftClass env innerName inner [] [unType typ]
     ; return (New (Type (innerName)) 
	           (map Var frees) 
	            Nothing)
     }
  where unType (Type name) = name
	unType _             = error "incorrect type style"
new env typ _ (Just inner) = error "cant handle inner class with args"


liftClass :: Env -> TypeName -> [Decl] -> [TypeName] -> [TypeName] -> LifterM [ Name ]
liftClass env@(Env bound _) innerName inner xs is =
  do { let newBound = getBoundAtDecls inner
     ; (inner,frees) <- 
	   getFrees (liftDecls False (env `combineEnv` newBound) inner)
     ; let trueFrees = filter (\ (Name xs _) -> xs /= "VM") (both frees bound)
     ; let freeDefs = [ Field [Final] n Nothing | n <- trueFrees ]
     ; let cons = mkCons innerName trueFrees
     ; let innerClass = Class [] innerName xs is (freeDefs ++ [cons] ++ inner)
     ; rememberClass innerClass
     ; return trueFrees
     }

liftType :: Env -> Type -> Type
liftType (Env _ env) typ@(Type name) 
   = case lookup name env of
	Nothing     -> typ
	Just (nm,_) -> Type nm
liftType _           typ = typ

liftNew :: Env -> Type -> [Expr] -> Expr
liftNew (Env _ env) typ@(Type name) exprs
   = case lookup name env of
	Nothing                     -> New typ exprs Nothing
	Just (nm,args) | null exprs 
		-> New (Type nm) (map Var args) Nothing
	_ -> error "pre-lifted constructor with arguments"
\end{code}

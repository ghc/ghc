%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Generate Java}

\begin{code}
module JavaGen( javaGen ) where

import Java

import Literal	( Literal(..) )
import Id	( Id, isDataConId_maybe, isId, idName, isDeadBinder )
import Name	( NamedThing(..), getOccString, isGlobalName 
		, nameModule )
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
  = liftCompilationUnit package
  where
    decls = [Import ["haskell","runtime","*"]] ++
	    [Import [moduleString mod] | mod <- import_mods] ++
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
  = tycon_jclass : map constr_class constrs
  where
    constrs = tyConDataCons tycon
    tycon_jclass_jname = javaName tycon
    tycon_jclass = Class [Public] tycon_jclass_jname [] [] []

    constr_class data_con
	= Class [Public] constr_jname [tycon_jclass_jname] [] field_decls
	where
	  constr_jname = javaConstrWkrName data_con
	  constr_jtype = javaConstrWkrType data_con
	  enter_meth   = Method [Public] objectType enterName [] [papExcName] stmts
	  n_val_args   = dataConRepArity data_con
	  field_names  = map fieldName [1..n_val_args]
	  field_decls  = [Field [Public] objectType f Nothing | f <- field_names]
	  stmts	       = vmCOLLECT n_val_args (Var thisName) ++
		     	 [var [Final] objectType f vmPOP | f <- field_names] ++
		     	 [Return (New constr_jtype (map Var field_names) Nothing)]
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
  = Class [Public] (javaShortName bndr) [] [codeName] [enter_meth]
  where
    enter_meth = Method [Public] objectType enterName [] [papExcName] 
			(javaExpr rhs)
\end{code}


%************************************************************************
%*									*
\subsection{Expressions}
%*									*
%************************************************************************

\begin{code}
javaVar :: Id -> Expr
javaVar v | isGlobalName (idName v) = New (javaType v) [] Nothing
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
			   (Access (Cast (javaConstrWkrType d) (javaVar x)) f)
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
			     [Method [Public] objectType enterName [] [papExcName] (javaExpr r)]	

    mk_inst (b,r) = var [Final] (javaType b) (javaInstName b)
			(New (javaType b) [] Nothing)

    mk_thunk (b,r) = var [Final] thunkType (javaName b)
			 (New thunkType [Var (javaInstName b)] Nothing)

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
		   [Return (New (javaType f) (javaArgs as) Nothing)]

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

true = Var "true"

vmCOLLECT :: Int -> Expr -> [Statement]
vmCOLLECT 0 e = []
vmCOLLECT n e = [ExprStatement (Call (Var vmName) "COLLECT" [Literal (IntLit n), e])]

vmPOP :: Expr
vmPOP = Call (Var vmName) "POP" []

vmPUSH :: Expr -> Expr
vmPUSH e = Call (Var vmName) "PUSH" [e]

var :: [Modifier] -> Type -> Name -> Expr -> Statement
var ms ty field_name value = Declaration (Field ms ty field_name (Just value))

vmWHNF :: Expr -> Expr
vmWHNF e = Call (Var vmName) "WHNF" [e]

instanceOf :: Id -> DataCon -> Expr
instanceOf x data_con
  = InstanceOf (Var (javaName x)) (javaConstrWkrType data_con)

newCode :: [Statement] -> Expr
newCode [Return e] = e
newCode stmts	   = New codeType [] (Just [Method [Public] objectType enterName [] [papExcName] stmts])

newThunk :: Expr -> Expr
newThunk e = New thunkType [e] Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Name mangling}
%*									*
%************************************************************************

\begin{code}
codeName, enterName, vmName,papExcName :: Name
codeName  = "Code"
thunkName = "Thunk"
enterName = "ENTER"
vmName    = "VM"
thisName  = "this"
papExcName = "PartialApplicationException"

fieldName :: Int -> Name	-- Names for fields of a constructor
fieldName n = "f" ++ show n

javaName :: NamedThing a => a -> Name
javaName n = if isGlobalName n'
	     then moduleString (nameModule n') ++ "." ++ getOccString n
	     else getOccString n
  where
	     n' = getName n

-- this is used for getting the name of a class when defining it.
javaShortName n = getOccString n

javaConstrWkrName :: DataCon -> Name
-- The function that makes the constructor
javaConstrWkrName con = getOccString (dataConId con)

javaInstName :: NamedThing a => a -> Name
-- Makes x_inst for Rec decls
javaInstName n = getOccString n ++ "_inst"
\end{code}

%************************************************************************
%*									*
\subsection{Type mangling}
%*									*
%************************************************************************

\begin{code}
javaType :: NamedThing a => a -> Type
javaType n = Type [javaName n]

javaConstrWkrType :: DataCon -> Type
-- The function that makes the constructor
javaConstrWkrType con = Type [javaConstrWkrName con]

codeType, thunkType, objectType :: Type
objectType = Type ["Object"]
codeType   = Type [codeName]
thunkType  = Type [thunkName]
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

addTypeMapping :: Name -> Name -> [Name] -> Env -> Env
addTypeMapping origName newName frees (Env bound env) 
	= Env bound ((origName,(newName,frees)) : env)

data Env = Env Bound [(Name,(Name,[Name]))]

newtype LifterM a = 
  	LifterM { unLifterM ::
		     Name ->
		     Int -> ( a			-- *
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

access :: Env -> Name -> LifterM ()
access env@(Env bound _) name 
	| name `elem` bound = LifterM (\ n s -> ((),[name],[],s))
	| otherwise         = return ()

scopedName :: Name -> LifterM a -> LifterM a
scopedName name (LifterM m) =
   LifterM (\ _ s -> 
      case m name 1 of
	(a,frees,lifted,_) -> (a,frees,lifted,s)
      )

genAnonInnerClassName :: LifterM Name
genAnonInnerClassName = LifterM (\ n s ->
	( n ++ "$" ++ show s
	, []
	, []
	, s + 1
	)
    )

genInnerClassName :: Name -> LifterM Name
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
    ; Field mfs t n e -> 
      do { e <- liftMaybeExpr env e
	 ; return (Field mfs (liftType env t) n e)
	 }
    ; Constructor mfs n as ss -> 
      do { let newBound = getBoundAtParameters as
	 ; (ss,_) <- liftStatements (combineEnv env newBound) ss
	 ; return (Constructor mfs n (liftParameters env as) ss)
	 }
    ; Method mfs t n as ts ss -> 
      do { let newBound = getBoundAtParameters as
	 ; (ss,_) <- liftStatements (combineEnv env newBound) ss
	 ; return (Method mfs (liftType env t) n (liftParameters env as) ts ss)
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

-- TODO
getBoundAtDecl :: Decl -> Bound
getBoundAtDecl (Field _ _ n _) = [n]
getBoundAtDecl _               = []

getBoundAtParameters :: [Parameter] -> Bound
getBoundAtParameters = foldr combine [] . map getBoundAtParameter

-- TODO
getBoundAtParameter :: Parameter -> Bound
getBoundAtParameter (Parameter _ _ n) = [n]

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
   ; Declaration decl@(Field mfs t n e) ->
      do { e <- liftMaybeExpr env e
	 ; return ( Declaration (Field mfs t n e)
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
   { Var n -> do { access env n 
		 ; return (Var n)
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
   ; Call e n es -> do { e <- liftExpr env e
		       ; es <- mapM (liftExpr env) es
		       ; return (Call e n es) 
		       }
   ; Op e1 o e2 -> do { e1 <- liftExpr env e1
		      ;	e2 <- liftExpr env e1
		      ; return (Op e1 o e2)
		      }
   ; New n es ds -> new env n es ds
   ; NewArray n es -> error "array not (yet) supported"
   }

liftParameter env (Parameter ms t n) = Parameter ms (liftType env t) n
liftParameters env = map (liftParameter env)

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
     ; return (mkNew env typ args)
     }
new env typ [] (Just inner) =
  -- anon. inner class
  do { innerName <- genAnonInnerClassName 
     ; frees <- liftClass env innerName inner [] [unType typ]
     ; return (New (Type [innerName]) [ Var name | name <- frees ] Nothing)
     }
  where unType (Type [name]) = name
	unType _             = error "incorrect type style"
	
new env typ _ (Just inner) = error "cant handle inner class with args"

liftClass :: Env -> Name -> [Decl] -> [Name] -> [Name] -> LifterM [ Name ]
liftClass env@(Env bound _) innerName inner xs is =
  do { let newBound = getBoundAtDecls inner
     ; (inner,frees) <- 
	   getFrees (liftDecls False (env `combineEnv` newBound) inner)
     ; let trueFrees = both frees bound
     ; let mirrorFrees = [ "_" ++ name ++ "_" | name <- trueFrees ]
     ; let freeDefs = [ Field [Final] objectType n Nothing | n <- trueFrees ]
     ; let cons = Constructor [Public] innerName 
		    [ Parameter [] objectType name | name <- mirrorFrees ]
		    [ ExprStatement (Assign (Var true) (Var mirror))
		    | (true,mirror) <- zip trueFrees mirrorFrees
		    ]
     ; let innerClass = Class [] innerName xs is (freeDefs ++ [cons] ++ inner)
     ; rememberClass innerClass
     ; return trueFrees
     }

liftType :: Env -> Type -> Type
liftType (Env _ env) typ@(Type [name]) 
   = case lookup name env of
	Nothing     -> typ
	Just (nm,_) -> Type [nm]
liftType _           typ = typ

mkNew :: Env -> Type -> [Expr] -> Expr
mkNew (Env _ env) typ@(Type [name]) exprs
   = case lookup name env of
	Nothing                     -> New typ exprs Nothing
	Just (nm,args) | null exprs 
		-> New (Type [nm]) (map Var args) Nothing
	_ -> error "pre-lifted constructor with arguments"
mkNew _           typ exprs = New typ exprs Nothing
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsExpr]{Abstract Haskell syntax: expressions}

\begin{code}
module HsExpr where

#include "HsVersions.h"

-- friends:
import HsDecls		( HsDecl )
import HsBinds		( HsBinds(..), nullBinds )
import HsPat		( Pat )
import HsLit		( HsLit, HsOverLit )
import HsTypes		( HsType, PostTcType, SyntaxName )
import HsImpExp		( isOperator, pprHsVar )

-- others:
import ForeignCall	( Safety )
import PprType		( pprParendType )
import Type		( Type )
import Var		( TyVar, Id )
import Name		( Name )
import DataCon		( DataCon )
import CStrings		( CLabelString, pprCLabelString )
import BasicTypes	( IPName, Boxity, tupleParens, Fixity(..) )
import SrcLoc		( SrcLoc )
import Outputable	
import FastString
\end{code}

%************************************************************************
%*									*
\subsection{Expressions proper}
%*									*
%************************************************************************

\begin{code}
data HsExpr id
  = HsVar	id		-- variable
  | HsIPVar	(IPName id)	-- implicit parameter
  | HsOverLit	HsOverLit	-- Overloaded literals; eliminated by type checker
  | HsLit	HsLit		-- Simple (non-overloaded) literals

  | HsLam	(Match  id)	-- lambda
  | HsApp	(HsExpr id)	-- application
		(HsExpr id)

  -- Operator applications:
  -- NB Bracketed ops such as (+) come out as Vars.

  -- NB We need an expr for the operator in an OpApp/Section since
  -- the typechecker may need to apply the operator to a few types.

  | OpApp	(HsExpr id)	-- left operand
		(HsExpr id)	-- operator
		Fixity				-- Renamer adds fixity; bottom until then
		(HsExpr id)	-- right operand

  -- We preserve prefix negation and parenthesis for the precedence parser.
  -- They are eventually removed by the type checker.

  | NegApp	(HsExpr id)	-- negated expr
		SyntaxName	-- Name of 'negate' (see RnEnv.lookupSyntaxName)

  | HsPar	(HsExpr id)	-- parenthesised expr

  | SectionL	(HsExpr id)	-- operand
		(HsExpr id)	-- operator
  | SectionR	(HsExpr id)	-- operator
		(HsExpr id)	-- operand
				
  | HsCase	(HsExpr id)
		[Match id]
		SrcLoc

  | HsIf	(HsExpr id)	--  predicate
		(HsExpr id)	--  then part
		(HsExpr id)	--  else part
		SrcLoc

  | HsLet	(HsBinds id)	-- let(rec)
		(HsExpr  id)

  | HsWith	(HsExpr id)	-- implicit parameter binding
  		[(IPName id, HsExpr id)]
		Bool		-- True <=> this was a 'with' binding
				--  (tmp, until 'with' is removed)

  | HsDo	HsDoContext
		[Stmt id]	-- "do":one or more stmts
		[id]		-- Ids for [return,fail,>>=,>>]
				--	Brutal but simple
				-- Before type checking, used for rebindable syntax
		PostTcType	-- Type of the whole expression
		SrcLoc

  | ExplicitList		-- syntactic list
		PostTcType	-- Gives type of components of list
		[HsExpr id]

  | ExplicitPArr		-- syntactic parallel array: [:e1, ..., en:]
		PostTcType	-- type of elements of the parallel array
		[HsExpr id]

  | ExplicitTuple		-- tuple
		[HsExpr id]
				-- NB: Unit is ExplicitTuple []
				-- for tuples, we can get the types
				-- direct from the components
		Boxity


	-- Record construction
  | RecordCon	id				-- The constructor
		(HsRecordBinds id)

  | RecordConOut DataCon
		(HsExpr id)		-- Data con Id applied to type args
		(HsRecordBinds id)


	-- Record update
  | RecordUpd	(HsExpr id)
		(HsRecordBinds id)

  | RecordUpdOut (HsExpr id)	-- TRANSLATION
		 Type			-- Type of *input* record
		 Type			-- Type of *result* record (may differ from
					-- 	type of input record)
		 (HsRecordBinds id)

  | ExprWithTySig			-- signature binding
		(HsExpr id)
		(HsType id)
  | ArithSeqIn				-- arithmetic sequence
		(ArithSeqInfo id)
  | ArithSeqOut
		(HsExpr id)		-- (typechecked, of course)
		(ArithSeqInfo id)
  | PArrSeqIn           		-- arith. sequence for parallel array
		(ArithSeqInfo id)	-- [:e1..e2:] or [:e1, e2..e3:]
  | PArrSeqOut
		(HsExpr id)		-- (typechecked, of course)
		(ArithSeqInfo id)

  | HsCCall	CLabelString	-- call into the C world; string is
		[HsExpr id]	-- the C function; exprs are the
				-- arguments to pass.
		Safety		-- True <=> might cause Haskell
				-- garbage-collection (must generate
				-- more paranoid code)
		Bool		-- True <=> it's really a "casm"
				-- NOTE: this CCall is the *boxed*
				-- version; the desugarer will convert
				-- it into the unboxed "ccall#".
		PostTcType	-- The result type; will be *bottom*
				-- until the typechecker gets ahold of it

  | HsSCC	FastString	-- "set cost centre" (_scc_) annotation
		(HsExpr id) 	-- expr whose cost is to be measured
		
  -- MetaHaskell Extensions
  | HsBracket    (HsBracket id)

  | HsBracketOut (HsBracket Name)	-- Output of the type checker is the *original*
		 [PendingSplice]	-- renamed expression, plus *typechecked* splices
					-- to be pasted back in by the desugarer

  | HsSplice id (HsExpr id )		-- $z  or $(f 4)
					-- The id is just a unique name to 
					-- identify this splice point
\end{code}


These constructors only appear temporarily in the parser.
The renamer translates them into the Right Thing.

\begin{code}
  | EWildPat			-- wildcard

  | EAsPat	id		-- as pattern
		(HsExpr id)

  | ELazyPat	(HsExpr id) -- ~ pattern

  | HsType      (HsType id)     -- Explicit type argument; e.g  f {| Int |} x y
\end{code}

Everything from here on appears only in typechecker output.

\begin{code}
  | TyLam			-- TRANSLATION
		[TyVar]
		(HsExpr id)
  | TyApp			-- TRANSLATION
		(HsExpr id) -- generated by Spec
		[Type]

  -- DictLam and DictApp are "inverses"
  |  DictLam
		[id]
		(HsExpr id)
  |  DictApp
		(HsExpr id)
		[id]

type PendingSplice = (Name, HsExpr Id)	-- Typechecked splices, waiting to be 
					-- pasted back in by the desugarer
\end{code}


A @Dictionary@, unless of length 0 or 1, becomes a tuple.  A
@ClassDictLam dictvars methods expr@ is, therefore:
\begin{verbatim}
\ x -> case x of ( dictvars-and-methods-tuple ) -> expr
\end{verbatim}

\begin{code}
instance OutputableBndr id => Outputable (HsExpr id) where
    ppr expr = pprExpr expr
\end{code}

\begin{code}
pprExpr :: OutputableBndr id => HsExpr id -> SDoc

pprExpr  e = pprDeeper (ppr_expr e)
pprBinds b = pprDeeper (ppr b)

ppr_expr (HsVar v)	 = pprHsVar v
ppr_expr (HsIPVar v)     = ppr v
ppr_expr (HsLit lit)     = ppr lit
ppr_expr (HsOverLit lit) = ppr lit

ppr_expr (HsLam match)
  = hsep [char '\\', nest 2 (pprMatch LambdaExpr match)]

ppr_expr expr@(HsApp e1 e2)
  = let (fun, args) = collect_args expr [] in
    (ppr_expr fun) <+> (sep (map ppr_expr args))
  where
    collect_args (HsApp fun arg) args = collect_args fun (arg:args)
    collect_args fun		 args = (fun, args)

ppr_expr (OpApp e1 op fixity e2)
  = case op of
      HsVar v -> pp_infixly v
      _	      -> pp_prefixly
  where
    pp_e1 = pprParendExpr e1		-- Add parens to make precedence clear
    pp_e2 = pprParendExpr e2

    pp_prefixly
      = hang (pprExpr op) 4 (sep [pp_e1, pp_e2])

    pp_infixly v
      = sep [pp_e1, hsep [pp_v_op, pp_e2]]
      where
	ppr_v = ppr v
        pp_v_op | isOperator ppr_v = ppr_v
		| otherwise        = char '`' <> ppr_v <> char '`'
	        -- Put it in backquotes if it's not an operator already

ppr_expr (NegApp e _) = char '-' <+> pprParendExpr e

ppr_expr (HsPar e) = parens (ppr_expr e)

ppr_expr (SectionL expr op)
  = case op of
      HsVar v -> pp_infixly v
      _	      -> pp_prefixly
  where
    pp_expr = pprParendExpr expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
		       4 (hsep [pp_expr, ptext SLIT("x_ )")])
    pp_infixly v = parens (sep [pp_expr, ppr v])

ppr_expr (SectionR op expr)
  = case op of
      HsVar v -> pp_infixly v
      _	      -> pp_prefixly
  where
    pp_expr = pprParendExpr expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, ptext SLIT("x_")])
		       4 ((<>) pp_expr rparen)
    pp_infixly v
      = parens (sep [ppr v, pp_expr])

ppr_expr (HsCase expr matches _)
  = sep [ sep [ptext SLIT("case"), nest 4 (pprExpr expr), ptext SLIT("of")],
	    nest 2 (pprMatches CaseAlt matches) ]

ppr_expr (HsIf e1 e2 e3 _)
  = sep [hsep [ptext SLIT("if"), nest 2 (pprExpr e1), ptext SLIT("then")],
	   nest 4 (pprExpr e2),
	   ptext SLIT("else"),
	   nest 4 (pprExpr e3)]

-- special case: let ... in let ...
ppr_expr (HsLet binds expr@(HsLet _ _))
  = sep [hang (ptext SLIT("let")) 2 (hsep [pprBinds binds, ptext SLIT("in")]),
	 pprExpr expr]

ppr_expr (HsLet binds expr)
  = sep [hang (ptext SLIT("let")) 2 (pprBinds binds),
	 hang (ptext SLIT("in"))  2 (ppr expr)]

ppr_expr (HsWith expr binds is_with)
  = sep [hang (ptext SLIT("let")) 2 (pp_ipbinds binds),
	 hang (ptext SLIT("in"))  2 (ppr expr)]

ppr_expr (HsDo do_or_list_comp stmts _ _ _) = pprDo do_or_list_comp stmts

ppr_expr (ExplicitList _ exprs)
  = brackets (fsep (punctuate comma (map ppr_expr exprs)))

ppr_expr (ExplicitPArr _ exprs)
  = pa_brackets (fsep (punctuate comma (map ppr_expr exprs)))

ppr_expr (ExplicitTuple exprs boxity)
  = tupleParens boxity (sep (punctuate comma (map ppr_expr exprs)))

ppr_expr (RecordCon con_id rbinds)
  = pp_rbinds (ppr con_id) rbinds
ppr_expr (RecordConOut data_con con rbinds)
  = pp_rbinds (ppr con) rbinds

ppr_expr (RecordUpd aexp rbinds)
  = pp_rbinds (pprParendExpr aexp) rbinds
ppr_expr (RecordUpdOut aexp _ _ rbinds)
  = pp_rbinds (pprParendExpr aexp) rbinds

ppr_expr (ExprWithTySig expr sig)
  = hang (nest 2 (ppr_expr expr) <+> dcolon)
	 4 (ppr sig)

ppr_expr (ArithSeqIn info)
  = brackets (ppr info)
ppr_expr (ArithSeqOut expr info)
  = brackets (ppr info)

ppr_expr (PArrSeqIn info)
  = pa_brackets (ppr info)
ppr_expr (PArrSeqOut expr info)
  = pa_brackets (ppr info)

ppr_expr EWildPat = char '_'
ppr_expr (ELazyPat e) = char '~' <> pprParendExpr e
ppr_expr (EAsPat v e) = ppr v <> char '@' <> pprParendExpr e

ppr_expr (HsCCall fun args _ is_asm result_ty)
  = hang (if is_asm
	  then ptext SLIT("_casm_ ``") <> pprCLabelString fun <> ptext SLIT("''")
	  else ptext SLIT("_ccall_") <+> pprCLabelString fun)
       4 (sep (map pprParendExpr args))

ppr_expr (HsSCC lbl expr)
  = sep [ ptext SLIT("_scc_") <+> doubleQuotes (ftext lbl), pprParendExpr expr ]

ppr_expr (TyLam tyvars expr)
  = hang (hsep [ptext SLIT("/\\"), 
		hsep (map (pprBndr LambdaBind) tyvars), 
		ptext SLIT("->")])
	 4 (ppr_expr expr)

ppr_expr (TyApp expr [ty])
  = hang (ppr_expr expr) 4 (pprParendType ty)

ppr_expr (TyApp expr tys)
  = hang (ppr_expr expr)
	 4 (brackets (interpp'SP tys))

ppr_expr (DictLam dictvars expr)
  = hang (hsep [ptext SLIT("\\{-dict-}"), 
	  	hsep (map (pprBndr LambdaBind) dictvars), 
		ptext SLIT("->")])
	 4 (ppr_expr expr)

ppr_expr (DictApp expr [dname])
  = hang (ppr_expr expr) 4 (ppr dname)

ppr_expr (DictApp expr dnames)
  = hang (ppr_expr expr)
	 4 (brackets (interpp'SP dnames))

ppr_expr (HsType id) = ppr id

ppr_expr (HsSplice n e)      = char '$' <> brackets (ppr n) <> pprParendExpr e
ppr_expr (HsBracket b)       = pprHsBracket b
ppr_expr (HsBracketOut e ps) = ppr e $$ ptext SLIT("where") <+> ppr ps

-- add parallel array brackets around a document
--
pa_brackets :: SDoc -> SDoc
pa_brackets p = ptext SLIT("[:") <> p <> ptext SLIT(":]")    
\end{code}

Parenthesize unless very simple:
\begin{code}
pprParendExpr :: OutputableBndr id => HsExpr id -> SDoc

pprParendExpr expr
  = let
	pp_as_was = pprExpr expr
    in
    case expr of
      HsLit l		    -> ppr l
      HsOverLit l 	    -> ppr l

      HsVar _		    -> pp_as_was
      HsIPVar _		    -> pp_as_was
      ExplicitList _ _      -> pp_as_was
      ExplicitPArr _ _      -> pp_as_was
      ExplicitTuple _ _	    -> pp_as_was
      HsPar _		    -> pp_as_was

      _			    -> parens pp_as_was
\end{code}

%************************************************************************
%*									*
\subsection{Record binds}
%*									*
%************************************************************************

\begin{code}
type HsRecordBinds id = [(id, HsExpr id)]

recBindFields :: HsRecordBinds id -> [id]
recBindFields rbinds = [field | (field,_) <- rbinds]

pp_rbinds :: OutputableBndr id => SDoc -> HsRecordBinds id -> SDoc

pp_rbinds thing rbinds
  = hang thing 
	 4 (braces (sep (punctuate comma (map (pp_rbind) rbinds))))
  where
    pp_rbind (v, e) = hsep [pprBndr LetBind v, char '=', ppr e]
\end{code}

\begin{code}
pp_ipbinds :: OutputableBndr id => [(IPName id, HsExpr id)] -> SDoc
pp_ipbinds pairs = hsep (punctuate semi (map pp_item pairs))
		 where
		   pp_item (id,rhs) = pprBndr LetBind id <+> equals <+> ppr_expr rhs
\end{code}


%************************************************************************
%*									*
\subsection{@Match@, @GRHSs@, and @GRHS@ datatypes}
%*									*
%************************************************************************

@Match@es are sets of pattern bindings and right hand sides for
functions, patterns or case branches. For example, if a function @g@
is defined as:
\begin{verbatim}
g (x,y) = y
g ((x:ys),y) = y+1,
\end{verbatim}
then \tr{g} has two @Match@es: @(x,y) = y@ and @((x:ys),y) = y+1@.

It is always the case that each element of an @[Match]@ list has the
same number of @pats@s inside it.  This corresponds to saying that
a function defined by pattern matching must have the same number of
patterns in each equation.

\begin{code}
data Match id
  = Match
	[Pat id]		-- The patterns
	(Maybe (HsType id))	-- A type signature for the result of the match
				--	Nothing after typechecking

	(GRHSs id)

-- GRHSs are used both for pattern bindings and for Matches
data GRHSs id	
  = GRHSs [GRHS id]		-- Guarded RHSs
	  (HsBinds id)		-- The where clause
	  PostTcType		-- Type of RHS (after type checking)

data GRHS id
  = GRHS  [Stmt id]		-- The RHS is the final ResultStmt
	  SrcLoc

mkSimpleMatch :: [Pat id] -> HsExpr id -> Type -> SrcLoc -> Match id
mkSimpleMatch pats rhs rhs_ty locn
  = Match pats Nothing (GRHSs (unguardedRHS rhs locn) EmptyBinds rhs_ty)

unguardedRHS :: HsExpr id -> SrcLoc -> [GRHS id]
unguardedRHS rhs loc = [GRHS [ResultStmt rhs loc] loc]
\end{code}

@getMatchLoc@ takes a @Match@ and returns the
source-location gotten from the GRHS inside.
THis is something of a nuisance, but no more.

\begin{code}
getMatchLoc :: Match id -> SrcLoc
getMatchLoc (Match _ _ (GRHSs (GRHS _ loc : _) _ _)) = loc
\end{code}

We know the list must have at least one @Match@ in it.

\begin{code}
pprMatches :: (OutputableBndr id) => HsMatchContext id -> [Match id] -> SDoc
pprMatches ctxt matches = vcat (map (pprMatch ctxt) matches)

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprFunBind :: (OutputableBndr id) => id -> [Match id] -> SDoc
pprFunBind fun matches = pprMatches (FunRhs fun) matches

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprPatBind :: (OutputableBndr id)
	   => Pat id -> GRHSs id -> SDoc
pprPatBind pat grhss = sep [ppr pat, nest 4 (pprGRHSs PatBindRhs grhss)]


pprMatch :: OutputableBndr id => HsMatchContext id -> Match id -> SDoc
pprMatch ctxt (Match pats maybe_ty grhss)
  = pp_name ctxt <+> sep [sep (map ppr pats), 
		     ppr_maybe_ty,
		     nest 2 (pprGRHSs ctxt grhss)]
  where
    pp_name (FunRhs fun) = ppr fun	-- Not pprBndr; the AbsBinds will
					-- have printed the signature
    pp_name other	 = empty
    ppr_maybe_ty = case maybe_ty of
			Just ty -> dcolon <+> ppr ty
			Nothing -> empty


pprGRHSs :: OutputableBndr id => HsMatchContext id -> GRHSs id -> SDoc
pprGRHSs ctxt (GRHSs grhss binds ty)
  = vcat (map (pprGRHS ctxt) grhss)
    $$
    (if nullBinds binds then empty
     else text "where" $$ nest 4 (pprDeeper (ppr binds)))


pprGRHS :: OutputableBndr id => HsMatchContext id -> GRHS id -> SDoc

pprGRHS ctxt (GRHS [ResultStmt expr _] locn)
 =  pp_rhs ctxt expr

pprGRHS ctxt (GRHS guarded locn)
 = sep [char '|' <+> interpp'SP guards, pp_rhs ctxt expr]
 where
    ResultStmt expr _ = last guarded	-- Last stmt should be a ResultStmt for guards
    guards	      = init guarded

pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)
\end{code}



%************************************************************************
%*									*
\subsection{Do stmts and list comprehensions}
%*									*
%************************************************************************

\begin{code}
data Stmt id
  = BindStmt	(Pat id) (HsExpr id) SrcLoc
  | LetStmt	(HsBinds id)
  | ResultStmt	(HsExpr id)	SrcLoc			-- See notes that follow
  | ExprStmt	(HsExpr id)	PostTcType SrcLoc	-- See notes that follow
	-- The type is the *element type* of the expression
  | ParStmt	[[Stmt id]]				-- List comp only: parallel set of quals
  | ParStmtOut	[([id], [Stmt id])]			-- PLC after renaming; the ids are the binders
							-- bound by the stmts
\end{code}

ExprStmts and ResultStmts are a bit tricky, because what they mean
depends on the context.  Consider the following contexts:

	A do expression of type (m res_ty)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	* ExprStmt E any_ty:   do { ....; E; ... }
		E :: m any_ty
	  Translation: E >> ...
	
	* ResultStmt E:   do { ....; E }
		E :: m res_ty
	  Translation: E
	
	A list comprehensions of type [elt_ty]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	* ExprStmt E Bool:   [ .. | .... E ]
			[ .. | ..., E, ... ]
			[ .. | .... | ..., E | ... ]
		E :: Bool
	  Translation: if E then fail else ...

	* ResultStmt E:   [ E | ... ]
		E :: elt_ty
	  Translation: return E
	
	A guard list, guarding a RHS of type rhs_ty
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	* ExprStmt E Bool:   f x | ..., E, ... = ...rhs...
		E :: Bool
	  Translation: if E then fail else ...
	
	* ResultStmt E:   f x | ...guards... = E
		E :: rhs_ty
	  Translation: E

Array comprehensions are handled like list comprehensions -=chak

\begin{code}
consLetStmt :: HsBinds id -> [Stmt id] -> [Stmt id]
consLetStmt EmptyBinds stmts = stmts
consLetStmt binds      stmts = LetStmt binds : stmts
\end{code}

\begin{code}
instance OutputableBndr id => Outputable (Stmt id) where
    ppr stmt = pprStmt stmt

pprStmt (BindStmt pat expr _) = hsep [ppr pat, ptext SLIT("<-"), ppr expr]
pprStmt (LetStmt binds)       = hsep [ptext SLIT("let"), pprBinds binds]
pprStmt (ExprStmt expr _ _)   = ppr expr
pprStmt (ResultStmt expr _)   = ppr expr
pprStmt (ParStmt stmtss)
 = hsep (map (\stmts -> ptext SLIT("| ") <> ppr stmts) stmtss)
pprStmt (ParStmtOut stmtss)
 = hsep (map (\stmts -> ptext SLIT("| ") <> ppr stmts) stmtss)

pprDo :: OutputableBndr id => HsDoContext -> [Stmt id] -> SDoc
pprDo DoExpr stmts   = hang (ptext SLIT("do")) 2 (vcat (map ppr stmts))
pprDo ListComp stmts = pprComp brackets   stmts
pprDo PArrComp stmts = pprComp pa_brackets stmts

pprComp :: OutputableBndr id => (SDoc -> SDoc) -> [Stmt id] -> SDoc
pprComp brack stmts = brack $
		      hang (pprExpr expr <+> char '|')
			 4 (interpp'SP quals)
		    where
		      ResultStmt expr _ = last stmts  -- Last stmt should
		      quals	        = init stmts  -- be an ResultStmt
\end{code}

%************************************************************************
%*									*
		Template Haskell quotation brackets
%*									*
%************************************************************************

\begin{code}
data HsBracket id = ExpBr (HsExpr id)
		  | PatBr (Pat id)
		  | DecBr [HsDecl id]
		  | TypBr (HsType id)

instance OutputableBndr id => Outputable (HsBracket id) where
  ppr = pprHsBracket


pprHsBracket (ExpBr e) = thBrackets empty (ppr e)
pprHsBracket (PatBr p) = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBr d) = thBrackets (char 'd') (vcat (map ppr d))
pprHsBracket (TypBr t) = thBrackets (char 't') (ppr t)


thBrackets pp_kind pp_body = char '[' <> pp_kind <> char '|' <+> 
			     pp_body <+> ptext SLIT("|]")
\end{code}

%************************************************************************
%*									*
\subsection{Enumerations and list comprehensions}
%*									*
%************************************************************************

\begin{code}
data ArithSeqInfo id
  = From	    (HsExpr id)
  | FromThen 	    (HsExpr id)
		    (HsExpr id)
  | FromTo	    (HsExpr id)
		    (HsExpr id)
  | FromThenTo	    (HsExpr id)
		    (HsExpr id)
		    (HsExpr id)
\end{code}

\begin{code}
instance OutputableBndr id => Outputable (ArithSeqInfo id) where
    ppr (From e1)		= hcat [ppr e1, pp_dotdot]
    ppr (FromThen e1 e2)	= hcat [ppr e1, comma, space, ppr e2, pp_dotdot]
    ppr (FromTo e1 e3)	= hcat [ppr e1, pp_dotdot, ppr e3]
    ppr (FromThenTo e1 e2 e3)
      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot, ppr e3]

pp_dotdot = ptext SLIT(" .. ")
\end{code}


%************************************************************************
%*									*
\subsection{HsMatchCtxt}
%*									*
%************************************************************************

\begin{code}
data HsMatchContext id	-- Context of a Match or Stmt
  = DoCtxt HsDoContext	-- Do-stmt or list comprehension
  | FunRhs id		-- Function binding for f
  | CaseAlt		-- Guard on a case alternative
  | LambdaExpr		-- Lambda
  | PatBindRhs		-- Pattern binding
  | RecUpd		-- Record update
  deriving ()

data HsDoContext = ListComp 
		 | DoExpr 
		 | PArrComp	-- parallel array comprehension
\end{code}

\begin{code}
isDoExpr (DoCtxt DoExpr) = True
isDoExpr other 		 = False
\end{code}

\begin{code}
matchSeparator (FunRhs _)   = ptext SLIT("=")
matchSeparator CaseAlt      = ptext SLIT("->") 
matchSeparator LambdaExpr   = ptext SLIT("->") 
matchSeparator PatBindRhs   = ptext SLIT("=") 
matchSeparator (DoCtxt _)   = ptext SLIT("<-")  
matchSeparator RecUpd       = panic "When is this used?"
\end{code}

\begin{code}
pprMatchContext (FunRhs fun) 	  = ptext SLIT("In the definition of") <+> quotes (ppr fun)
pprMatchContext CaseAlt	     	  = ptext SLIT("In a case alternative")
pprMatchContext RecUpd	     	  = ptext SLIT("In a record-update construct")
pprMatchContext PatBindRhs   	  = ptext SLIT("In a pattern binding")
pprMatchContext LambdaExpr   	  = ptext SLIT("In a lambda abstraction")
pprMatchContext (DoCtxt DoExpr)   = ptext SLIT("In a 'do' expression pattern binding")
pprMatchContext (DoCtxt ListComp) = 
  ptext SLIT("In a 'list comprehension' pattern binding")
pprMatchContext (DoCtxt PArrComp) = 
  ptext SLIT("In an 'array comprehension' pattern binding")

-- Used to generate the string for a *runtime* error message
matchContextErrString (FunRhs fun)    	= "function " ++ showSDoc (ppr fun)
matchContextErrString CaseAlt	      	= "case"
matchContextErrString PatBindRhs      	= "pattern binding"
matchContextErrString RecUpd	      	= "record update"
matchContextErrString LambdaExpr      	=  "lambda"
matchContextErrString (DoCtxt DoExpr)   = "'do' expression"
matchContextErrString (DoCtxt ListComp) = "list comprehension"
matchContextErrString (DoCtxt PArrComp) = "array comprehension"
\end{code}

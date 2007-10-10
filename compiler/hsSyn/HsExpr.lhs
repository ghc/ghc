%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

HsExpr: Abstract Haskell syntax: expressions

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module HsExpr where

#include "HsVersions.h"

-- friends:
import HsDecls
import HsPat
import HsLit
import HsTypes
import HsImpExp
import HsBinds

-- others:
import Var
import Name
import BasicTypes
import DataCon
import SrcLoc
import Outputable	
import FastString
\end{code}


%************************************************************************
%*									*
\subsection{Expressions proper}
%*									*
%************************************************************************

\begin{code}
type LHsExpr id = Located (HsExpr id)

-------------------------
-- PostTcExpr is an evidence expression attached to the
-- syntax tree by the type checker (c.f. postTcType)
-- We use a PostTcTable where there are a bunch of pieces of 
-- evidence, more than is convenient to keep individually
type PostTcExpr  = HsExpr Id
type PostTcTable = [(Name, Id)]

noPostTcExpr :: PostTcExpr
noPostTcExpr = HsLit (HsString FSLIT("noPostTcExpr"))

noPostTcTable :: PostTcTable
noPostTcTable = []

-------------------------
-- SyntaxExpr is like PostTcExpr, but it's filled in a little earlier,
-- by the renamer.  It's used for rebindable syntax.  
-- E.g. (>>=) is filled in before the renamer by the appropriate Name
--      for (>>=), and then instantiated by the type checker with its
--	type args tec

type SyntaxExpr id = HsExpr id

noSyntaxExpr :: SyntaxExpr id	-- Before renaming, and sometimes after,
				-- (if the syntax slot makes no sense)
noSyntaxExpr = HsLit (HsString FSLIT("noSyntaxExpr"))


type SyntaxTable id = [(Name, SyntaxExpr id)]
--	*** Currently used only for CmdTop (sigh) ***
-- * Before the renamer, this list is noSyntaxTable
--
-- * After the renamer, it takes the form [(std_name, HsVar actual_name)]
--   For example, for the 'return' op of a monad
--	normal case:		(GHC.Base.return, HsVar GHC.Base.return)
--	with rebindable syntax:	(GHC.Base.return, return_22)
--		where return_22 is whatever "return" is in scope
--
-- * After the type checker, it takes the form [(std_name, <expression>)]
--	where <expression> is the evidence for the method

noSyntaxTable :: SyntaxTable id
noSyntaxTable = []


-------------------------
data HsExpr id
  = HsVar	id		-- variable
  | HsIPVar	(IPName id)	-- implicit parameter
  | HsOverLit	(HsOverLit id) -- Overloaded literals

  | HsLit	HsLit		-- Simple (non-overloaded) literals

  | HsLam	(MatchGroup  id)	-- Currently always a single match

  | HsApp	(LHsExpr id)		-- Application
		(LHsExpr id)

  -- Operator applications:
  -- NB Bracketed ops such as (+) come out as Vars.

  -- NB We need an expr for the operator in an OpApp/Section since
  -- the typechecker may need to apply the operator to a few types.

  | OpApp	(LHsExpr id)	-- left operand
		(LHsExpr id)	-- operator
		Fixity		-- Renamer adds fixity; bottom until then
		(LHsExpr id)	-- right operand

  | NegApp	(LHsExpr id)	-- negated expr
		(SyntaxExpr id)	-- Name of 'negate'

  | HsPar	(LHsExpr id)	-- parenthesised expr

  | SectionL	(LHsExpr id)	-- operand
		(LHsExpr id)	-- operator
  | SectionR	(LHsExpr id)	-- operator
		(LHsExpr id)	-- operand
				
  | HsCase	(LHsExpr id)
		(MatchGroup id)

  | HsIf	(LHsExpr id)	--  predicate
		(LHsExpr id)	--  then part
		(LHsExpr id)	--  else part

  | HsLet	(HsLocalBinds id) -- let(rec)
		(LHsExpr  id)

  | HsDo	(HsStmtContext Name)	-- The parameterisation is unimportant
					-- because in this context we never use
					-- the PatGuard or ParStmt variant
		[LStmt id]		-- "do":one or more stmts
		(LHsExpr id)		-- The body; the last expression in the 'do'
					--	     of [ body | ... ] in a list comp
		PostTcType		-- Type of the whole expression

  | ExplicitList		-- syntactic list
		PostTcType	-- Gives type of components of list
		[LHsExpr id]

  | ExplicitPArr		-- syntactic parallel array: [:e1, ..., en:]
		PostTcType	-- type of elements of the parallel array
		[LHsExpr id]

  | ExplicitTuple		-- tuple
		[LHsExpr id]
				-- NB: Unit is ExplicitTuple []
				-- for tuples, we can get the types
				-- direct from the components
		Boxity


	-- Record construction
  | RecordCon	(Located id)		-- The constructor.  After type checking
					-- it's the dataConWrapId of the constructor
		PostTcExpr		-- Data con Id applied to type args
		(HsRecordBinds id)

	-- Record update
  | RecordUpd	(LHsExpr id)
		(HsRecordBinds id)
		[DataCon]		-- Filled in by the type checker to the *non-empty*
					-- list of DataCons that have all the upd'd fields
		[PostTcType]		-- Argument types of *input* record type
		[PostTcType]		-- 		and  *output* record type
	-- For a type family, the arg types are of the *instance* tycon, not the family tycon

  | ExprWithTySig			-- e :: type
		(LHsExpr id)
		(LHsType id)

  | ExprWithTySigOut			-- TRANSLATION
		(LHsExpr id)
		(LHsType Name)		-- Retain the signature for round-tripping purposes

  | ArithSeq				-- arithmetic sequence
		PostTcExpr
		(ArithSeqInfo id)

  | PArrSeq	           		-- arith. sequence for parallel array
		PostTcExpr		-- [:e1..e2:] or [:e1, e2..e3:]
		(ArithSeqInfo id)

  | HsSCC	FastString	-- "set cost centre" (_scc_) annotation
		(LHsExpr id) 	-- expr whose cost is to be measured

  | HsCoreAnn   FastString      -- hdaume: core annotation
                (LHsExpr id)
		
  -----------------------------------------------------------
  -- MetaHaskell Extensions

  | HsBracket    (HsBracket id)

  | HsBracketOut (HsBracket Name)	-- Output of the type checker is the *original*
		 [PendingSplice]	-- renamed expression, plus *typechecked* splices
					-- to be pasted back in by the desugarer

  | HsSpliceE (HsSplice id) 

  -----------------------------------------------------------
  -- Arrow notation extension

  | HsProc	(LPat id)		-- arrow abstraction, proc
		(LHsCmdTop id)		-- body of the abstraction
					-- always has an empty stack

  ---------------------------------------
  -- The following are commands, not expressions proper

  | HsArrApp	-- Arrow tail, or arrow application (f -< arg)
	(LHsExpr id)	-- arrow expression, f
	(LHsExpr id)	-- input expression, arg
	PostTcType	-- type of the arrow expressions f,
			-- of the form a t t', where arg :: t
	HsArrAppType	-- higher-order (-<<) or first-order (-<)
	Bool		-- True => right-to-left (f -< arg)
			-- False => left-to-right (arg >- f)

  | HsArrForm	-- Command formation,  (| e cmd1 .. cmdn |)
	(LHsExpr id)	-- the operator
			-- after type-checking, a type abstraction to be
			-- applied to the type of the local environment tuple
	(Maybe Fixity)	-- fixity (filled in by the renamer), for forms that
			-- were converted from OpApp's by the renamer
	[LHsCmdTop id]	-- argument commands


  ---------------------------------------
  -- Haskell program coverage (Hpc) Support

  | HsTick 
     Int				-- module-local tick number
     [id]                               -- variables in scope
     (LHsExpr id)			-- sub-expression

  | HsBinTick
     Int				-- module-local tick number for True
     Int				-- module-local tick number for False
     (LHsExpr id)			-- sub-expression

  | HsTickPragma			-- A pragma introduced tick
     (FastString,(Int,Int),(Int,Int))   -- external span for this tick    
     (LHsExpr id)     

  ---------------------------------------
  -- These constructors only appear temporarily in the parser.
  -- The renamer translates them into the Right Thing.

  | EWildPat			-- wildcard

  | EAsPat	(Located id)	-- as pattern
		(LHsExpr id)

  | EViewPat	(LHsExpr id)	-- view pattern
		(LHsExpr id)

  | ELazyPat	(LHsExpr id) -- ~ pattern

  | HsType      (LHsType id)     -- Explicit type argument; e.g  f {| Int |} x y

  ---------------------------------------
  -- Finally, HsWrap appears only in typechecker output

  |  HsWrap	HsWrapper 	-- TRANSLATION
		(HsExpr id)

type PendingSplice = (Name, LHsExpr Id)	-- Typechecked splices, waiting to be 
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
-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper; 
-- the underscore versions do not
pprLExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprLExpr (L _ e) = pprExpr e

pprExpr :: OutputableBndr id => HsExpr id -> SDoc
pprExpr e | isAtomicHsExpr e || isQuietHsExpr e =            ppr_expr e
          | otherwise                           = pprDeeper (ppr_expr e)

isQuietHsExpr :: HsExpr id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsExpr (HsPar _) = True
-- applications don't display anything themselves
isQuietHsExpr (HsApp _ _) = True
isQuietHsExpr (OpApp _ _ _ _) = True
isQuietHsExpr _ = False

pprBinds :: (OutputableBndr idL, OutputableBndr idR) => HsLocalBindsLR idL idR -> SDoc
pprBinds b = pprDeeper (ppr b)

-----------------------
ppr_lexpr :: OutputableBndr id => LHsExpr id -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)

ppr_expr :: OutputableBndr id => HsExpr id -> SDoc
ppr_expr (HsVar v)	 = pprHsVar v
ppr_expr (HsIPVar v)     = ppr v
ppr_expr (HsLit lit)     = ppr lit
ppr_expr (HsOverLit lit) = ppr lit
ppr_expr (HsPar e)	 = parens (ppr_lexpr e)

ppr_expr (HsCoreAnn s e)
  = vcat [ptext SLIT("HsCoreAnn") <+> ftext s, ppr_lexpr e]

ppr_expr (HsApp e1 e2)
  = let (fun, args) = collect_args e1 [e2] in
    hang (ppr_lexpr fun) 2 (sep (map pprParendExpr args))
  where
    collect_args (L _ (HsApp fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)

ppr_expr (OpApp e1 op fixity e2)
  = case unLoc op of
      HsVar v -> pp_infixly v
      _	      -> pp_prefixly
  where
    pp_e1 = pprDebugParendExpr e1   -- In debug mode, add parens 
    pp_e2 = pprDebugParendExpr e2   -- to make precedence clear

    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])

    pp_infixly v
      = sep [nest 2 pp_e1, pprInfix v, nest 2 pp_e2]

ppr_expr (NegApp e _) = char '-' <+> pprDebugParendExpr e

ppr_expr (SectionL expr op)
  = case unLoc op of
      HsVar v -> pp_infixly v
      _	      -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
		       4 (hsep [pp_expr, ptext SLIT("x_ )")])
    pp_infixly v = (sep [pp_expr, pprInfix v])

ppr_expr (SectionR op expr)
  = case unLoc op of
      HsVar v -> pp_infixly v
      _	      -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, ptext SLIT("x_")])
		       4 ((<>) pp_expr rparen)
    pp_infixly v
      = (sep [pprInfix v, pp_expr])

ppr_expr (HsLam matches :: HsExpr id) 
  = pprMatches (LambdaExpr :: HsMatchContext id) matches

ppr_expr (HsCase expr matches :: HsExpr id)
  = sep [ sep [ptext SLIT("case"), nest 4 (ppr expr), ptext SLIT("of")],
	    nest 2 (pprMatches (CaseAlt :: HsMatchContext id) matches) ]

ppr_expr (HsIf e1 e2 e3)
  = sep [hsep [ptext SLIT("if"), nest 2 (ppr e1), ptext SLIT("then")],
	   nest 4 (ppr e2),
	   ptext SLIT("else"),
	   nest 4 (ppr e3)]

-- special case: let ... in let ...
ppr_expr (HsLet binds expr@(L _ (HsLet _ _)))
  = sep [hang (ptext SLIT("let")) 2 (hsep [pprBinds binds, ptext SLIT("in")]),
	 ppr_lexpr expr]

ppr_expr (HsLet binds expr)
  = sep [hang (ptext SLIT("let")) 2 (pprBinds binds),
	 hang (ptext SLIT("in"))  2 (ppr expr)]

ppr_expr (HsDo do_or_list_comp stmts body _) = pprDo do_or_list_comp stmts body

ppr_expr (ExplicitList _ exprs)
  = brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (ExplicitPArr _ exprs)
  = pa_brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (ExplicitTuple exprs boxity)
  = tupleParens boxity (sep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (RecordCon con_id con_expr rbinds)
  = hang (ppr con_id) 2 (ppr rbinds)

ppr_expr (RecordUpd aexp rbinds _ _ _)
  = hang (pprParendExpr aexp) 2 (ppr rbinds)

ppr_expr (ExprWithTySig expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
	 4 (ppr sig)
ppr_expr (ExprWithTySigOut expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
	 4 (ppr sig)

ppr_expr (ArithSeq expr info) = brackets (ppr info)
ppr_expr (PArrSeq expr info)  = pa_brackets (ppr info)

ppr_expr EWildPat     = char '_'
ppr_expr (ELazyPat e) = char '~' <> pprParendExpr e
ppr_expr (EAsPat v e) = ppr v <> char '@' <> pprParendExpr e

ppr_expr (HsSCC lbl expr)
  = sep [ ptext SLIT("_scc_") <+> doubleQuotes (ftext lbl), pprParendExpr expr ]

ppr_expr (HsWrap co_fn e) = pprHsWrapper (pprExpr e) co_fn
ppr_expr (HsType id)	    = ppr id

ppr_expr (HsSpliceE s)       = pprSplice s
ppr_expr (HsBracket b)       = pprHsBracket b
ppr_expr (HsBracketOut e []) = ppr e	
ppr_expr (HsBracketOut e ps) = ppr e $$ ptext SLIT("pending") <+> ppr ps

ppr_expr (HsProc pat (L _ (HsCmdTop cmd _ _ _)))
  = hsep [ptext SLIT("proc"), ppr pat, ptext SLIT("->"), ppr cmd]

ppr_expr (HsTick tickId vars exp)
  = hcat [ptext SLIT("tick<"), ppr tickId,ptext SLIT(">("), hsep (map pprHsVar vars), ppr exp,ptext SLIT(")")]
ppr_expr (HsBinTick tickIdTrue tickIdFalse exp)
  = hcat [ptext SLIT("bintick<"), 
	  ppr tickIdTrue,
	  ptext SLIT(","),
	  ppr tickIdFalse,
	  ptext SLIT(">("), 
	  ppr exp,ptext SLIT(")")]
ppr_expr (HsTickPragma externalSrcLoc exp)
  = hcat [ptext SLIT("tickpragma<"), ppr externalSrcLoc,ptext SLIT(">("), ppr exp,ptext SLIT(")")]

ppr_expr (HsArrApp arrow arg _ HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, ptext SLIT("-<"), ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg _ HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, ptext SLIT(">-"), ppr_lexpr arrow]
ppr_expr (HsArrApp arrow arg _ HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, ptext SLIT("-<<"), ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg _ HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, ptext SLIT(">>-"), ppr_lexpr arrow]

ppr_expr (HsArrForm (L _ (HsVar v)) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfix v, pprCmdArg (unLoc arg2)]]
ppr_expr (HsArrForm op _ args)
  = hang (ptext SLIT("(|") <> ppr_lexpr op)
	 4 (sep (map (pprCmdArg.unLoc) args) <> ptext SLIT("|)"))

pprCmdArg :: OutputableBndr id => HsCmdTop id -> SDoc
pprCmdArg (HsCmdTop cmd@(L _ (HsArrForm _ Nothing [])) _ _ _)
  = ppr_lexpr cmd
pprCmdArg (HsCmdTop cmd _ _ _)
  = parens (ppr_lexpr cmd)

-- Put a var in backquotes if it's not an operator already
pprInfix :: Outputable name => name -> SDoc
pprInfix v | isOperator ppr_v = ppr_v
	   | otherwise        = char '`' <> ppr_v <> char '`'
	   where
	     ppr_v = ppr v

-- add parallel array brackets around a document
--
pa_brackets :: SDoc -> SDoc
pa_brackets p = ptext SLIT("[:") <> p <> ptext SLIT(":]")    
\end{code}

HsSyn records exactly where the user put parens, with HsPar.
So generally speaking we print without adding any parens.
However, some code is internally generated, and in some places
parens are absolutely required; so for these places we use
pprParendExpr (but don't print double parens of course).

For operator applications we don't add parens, because the oprerator
fixities should do the job, except in debug mode (-dppr-debug) so we
can see the structure of the parse tree.

\begin{code}
pprDebugParendExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprDebugParendExpr expr
  = getPprStyle (\sty ->
    if debugStyle sty then pprParendExpr expr
		      else pprLExpr      expr)
  
pprParendExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprParendExpr expr
  = let
	pp_as_was = pprLExpr expr
	-- Using pprLExpr makes sure that we go 'deeper'
	-- I think that is usually (always?) right
    in
    case unLoc expr of
      HsLit l		   -> pp_as_was
      HsOverLit l 	   -> pp_as_was
      HsVar _		   -> pp_as_was
      HsIPVar _		   -> pp_as_was
      ExplicitList _ _     -> pp_as_was
      ExplicitPArr _ _     -> pp_as_was
      ExplicitTuple _ _	   -> pp_as_was
      HsPar _		   -> pp_as_was
      HsBracket _	   -> pp_as_was
      HsBracketOut _ []	   -> pp_as_was
      HsDo sc _ _ _
       | isListCompExpr sc -> pp_as_was
      _			   -> parens pp_as_was

isAtomicHsExpr :: HsExpr id -> Bool	-- A single token
isAtomicHsExpr (HsVar {})     = True
isAtomicHsExpr (HsLit {})     = True
isAtomicHsExpr (HsOverLit {}) = True
isAtomicHsExpr (HsIPVar {})   = True
isAtomicHsExpr (HsWrap _ e)   = isAtomicHsExpr e
isAtomicHsExpr (HsPar e)      = isAtomicHsExpr (unLoc e)
isAtomicHsExpr e	      = False
\end{code}

%************************************************************************
%*									*
\subsection{Commands (in arrow abstractions)}
%*									*
%************************************************************************

We re-use HsExpr to represent these.

\begin{code}
type HsCmd id = HsExpr id

type LHsCmd id = LHsExpr id

data HsArrAppType = HsHigherOrderApp | HsFirstOrderApp
\end{code}

The legal constructors for commands are:

  = HsArrApp ...		-- as above

  | HsArrForm ...		-- as above

  | HsApp	(HsCmd id)
		(HsExpr id)

  | HsLam	(Match  id)	-- kappa

  -- the renamer turns this one into HsArrForm
  | OpApp	(HsExpr id)	-- left operand
		(HsCmd id)	-- operator
		Fixity		-- Renamer adds fixity; bottom until then
		(HsCmd id)	-- right operand

  | HsPar	(HsCmd id)	-- parenthesised command

  | HsCase	(HsExpr id)
		[Match id]	-- bodies are HsCmd's
		SrcLoc

  | HsIf	(HsExpr id)	--  predicate
		(HsCmd id)	--  then part
		(HsCmd id)	--  else part
		SrcLoc

  | HsLet	(HsLocalBinds id)	-- let(rec)
		(HsCmd  id)

  | HsDo	(HsStmtContext Name)	-- The parameterisation is unimportant
					-- because in this context we never use
					-- the PatGuard or ParStmt variant
		[Stmt id]	-- HsExpr's are really HsCmd's
		PostTcType	-- Type of the whole expression
		SrcLoc

Top-level command, introducing a new arrow.
This may occur inside a proc (where the stack is empty) or as an
argument of a command-forming operator.

\begin{code}
type LHsCmdTop id = Located (HsCmdTop id)

data HsCmdTop id
  = HsCmdTop	(LHsCmd id)
		[PostTcType]	-- types of inputs on the command's stack
		PostTcType	-- return type of the command
		(SyntaxTable id)
				-- after type checking:
				-- names used in the command's desugaring
\end{code}

%************************************************************************
%*									*
\subsection{Record binds}
%*									*
%************************************************************************

\begin{code}
type HsRecordBinds id = HsRecFields id (LHsExpr id)
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
data MatchGroup id 
  = MatchGroup 
	[LMatch id] 	-- The alternatives
	PostTcType	-- The type is the type of the entire group
			-- 	t1 -> ... -> tn -> tr
			-- where there are n patterns

type LMatch id = Located (Match id)

data Match id
  = Match
	[LPat id]		-- The patterns
	(Maybe (LHsType id))	-- A type signature for the result of the match
				--	Nothing after typechecking
	(GRHSs id)

matchGroupArity :: MatchGroup id -> Arity
matchGroupArity (MatchGroup [] _) 
  = panic "matchGroupArity"	-- MatchGroup is never empty
matchGroupArity (MatchGroup (match:matches) _)
  = ASSERT( all ((== n_pats) . length . hsLMatchPats) matches )
	-- Assertion just checks that all the matches have the same number of pats
    n_pats
  where
    n_pats = length (hsLMatchPats match)

hsLMatchPats :: LMatch id -> [LPat id]
hsLMatchPats (L _ (Match pats _ _)) = pats

-- GRHSs are used both for pattern bindings and for Matches
data GRHSs id	
  = GRHSs [LGRHS id]		-- Guarded RHSs
	  (HsLocalBinds id)	-- The where clause

type LGRHS id = Located (GRHS id)

data GRHS id = GRHS [LStmt id]		-- Guards
		    (LHsExpr id)	-- Right hand side
\end{code}

We know the list must have at least one @Match@ in it.

\begin{code}
pprMatches :: (OutputableBndr idL, OutputableBndr idR) => HsMatchContext idL -> MatchGroup idR -> SDoc
pprMatches ctxt (MatchGroup matches ty) = vcat (map (pprMatch ctxt) (map unLoc matches))
                                          -- Don't print the type; it's only 
                                          -- a place-holder before typechecking

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprFunBind :: (OutputableBndr idL, OutputableBndr idR) => idL -> Bool -> MatchGroup idR -> SDoc
pprFunBind fun inf matches = pprMatches (FunRhs fun inf) matches

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprPatBind :: (OutputableBndr bndr, OutputableBndr id)
	   => LPat bndr -> GRHSs id -> SDoc
pprPatBind pat (grhss :: GRHSs id) = sep [ppr pat, nest 4 (pprGRHSs (PatBindRhs :: HsMatchContext id) grhss)]


pprMatch :: (OutputableBndr idL, OutputableBndr idR) => HsMatchContext idL -> Match idR -> SDoc
pprMatch ctxt (Match pats maybe_ty grhss)
  = herald <+> sep [sep (map ppr other_pats), 
		    ppr_maybe_ty, 
		    nest 2 (pprGRHSs ctxt grhss)]
  where
    (herald, other_pats) 
	= case ctxt of
	    FunRhs fun is_infix
		| not is_infix -> (ppr fun, pats)
			-- f x y z = e
			-- Not pprBndr; the AbsBinds will
			-- have printed the signature

		| null pats3 -> (pp_infix, [])
			-- x &&& y = e

		| otherwise -> (parens pp_infix, pats3)
			-- (x &&& y) z = e
		where
		  (pat1:pat2:pats3) = pats
		  pp_infix = ppr pat1 <+> ppr fun <+> ppr pat2

	    LambdaExpr -> (char '\\', pats)
	    other      -> (empty,     pats)

    ppr_maybe_ty = case maybe_ty of
			Just ty -> dcolon <+> ppr ty
			Nothing -> empty


pprGRHSs :: (OutputableBndr idL, OutputableBndr idR) => HsMatchContext idL -> GRHSs idR -> SDoc
pprGRHSs ctxt (GRHSs grhss binds)
  = vcat (map (pprGRHS ctxt . unLoc) grhss)
 $$ if isEmptyLocalBinds binds then empty
                               else text "where" $$ nest 4 (pprBinds binds)

pprGRHS :: (OutputableBndr idL, OutputableBndr idR) => HsMatchContext idL -> GRHS idR -> SDoc

pprGRHS ctxt (GRHS [] expr)
 =  pp_rhs ctxt expr

pprGRHS ctxt (GRHS guards expr)
 = sep [char '|' <+> interpp'SP guards, pp_rhs ctxt expr]

pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)
\end{code}

%************************************************************************
%*									*
\subsection{Do stmts and list comprehensions}
%*									*
%************************************************************************

\begin{code}
type LStmt id = Located (StmtLR id id)
type LStmtLR idL idR = Located (StmtLR idL idR)

type Stmt id = StmtLR id id

-- The SyntaxExprs in here are used *only* for do-notation, which
-- has rebindable syntax.  Otherwise they are unused.
data StmtLR idL idR
  = BindStmt	(LPat idL)		
		(LHsExpr idR) 
		(SyntaxExpr idR)		-- The (>>=) operator
		(SyntaxExpr idR)		-- The fail operator 
		-- The fail operator is noSyntaxExpr 
		-- if the pattern match can't fail

  | ExprStmt	(LHsExpr idR)
		(SyntaxExpr idR)		-- The (>>) operator
		PostTcType		-- Element type of the RHS (used for arrows)

  | LetStmt	(HsLocalBindsLR idL idR)	

	-- ParStmts only occur in a list comprehension
  | ParStmt	[([LStmt idL], [idR])] -- After renaming, the ids are the binders
					 -- bound by the stmts and used subsequently

	-- Recursive statement (see Note [RecStmt] below)
  | RecStmt  [LStmtLR idL idR] 
		--- The next two fields are only valid after renaming
	     [idR] 	-- The ids are a subset of the variables bound by the stmts
	     	 	-- that are used in stmts that follow the RecStmt

	     [idR]	-- Ditto, but these variables are the "recursive" ones, that 
			-- are used before they are bound in the stmts of the RecStmt
			-- From a type-checking point of view, these ones have to be monomorphic

		--- These fields are only valid after typechecking
	     [PostTcExpr]	-- These expressions correspond
				-- 1-to-1 with the "recursive" [id], and are the expresions that 
				-- should be returned by the recursion.  They may not quite be the
				-- Ids themselves, because the Id may be *polymorphic*, but
				-- the returned thing has to be *monomorphic*.
	     (DictBinds idR)	-- Method bindings of Ids bound by the RecStmt,
				-- and used afterwards
\end{code}

ExprStmts are a bit tricky, because what they mean
depends on the context.  Consider the following contexts:

	A do expression of type (m res_ty)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	* ExprStmt E any_ty:   do { ....; E; ... }
		E :: m any_ty
	  Translation: E >> ...
	
	A list comprehensions of type [elt_ty]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	* ExprStmt E Bool:   [ .. | .... E ]
			[ .. | ..., E, ... ]
			[ .. | .... | ..., E | ... ]
		E :: Bool
	  Translation: if E then fail else ...

	A guard list, guarding a RHS of type rhs_ty
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	* ExprStmt E Bool:   f x | ..., E, ... = ...rhs...
		E :: Bool
	  Translation: if E then fail else ...
	
Array comprehensions are handled like list comprehensions -=chak

Note [RecStmt]
~~~~~~~~~~~~~~
Example:
	HsDo [ BindStmt x ex

	     , RecStmt [a::forall a. a -> a, b] 
		       [a::Int -> Int,       c] 
		       [ BindStmt b (return x)
		       , LetStmt a = ea
		       , BindStmt c ec ]

	     , return (a b) ]

Here, the RecStmt binds a,b,c; but 
  - Only a,b are used in the stmts *following* the RecStmt, 
	This 'a' is *polymorphic'
  - Only a,c are used in the stmts *inside* the RecStmt
	*before* their bindings
	This 'a' is monomorphic

Nota Bene: the two a's have different types, even though they
have the same Name.


\begin{code}
instance (OutputableBndr idL, OutputableBndr idR) => Outputable (StmtLR idL idR) where
    ppr stmt = pprStmt stmt

pprStmt :: (OutputableBndr idL, OutputableBndr idR) => (StmtLR idL idR) -> SDoc
pprStmt (BindStmt pat expr _ _)	  = hsep [ppr pat, ptext SLIT("<-"), ppr expr]
pprStmt (LetStmt binds)       	  = hsep [ptext SLIT("let"), pprBinds binds]
pprStmt (ExprStmt expr _ _)   	  = ppr expr
pprStmt (ParStmt stmtss)          = hsep (map (\stmts -> ptext SLIT("| ") <> ppr stmts) stmtss)
pprStmt (RecStmt segment _ _ _ _) = ptext SLIT("rec") <+> braces (vcat (map ppr segment))

pprDo :: OutputableBndr id => HsStmtContext any -> [LStmt id] -> LHsExpr id -> SDoc
pprDo DoExpr      stmts body = ptext SLIT("do")  <+> pprDeeperList vcat (map ppr stmts ++ [ppr body])
pprDo (MDoExpr _) stmts body = ptext SLIT("mdo") <+> pprDeeperList vcat (map ppr stmts ++ [ppr body])
pprDo ListComp    stmts body = pprComp brackets    stmts body
pprDo PArrComp    stmts body = pprComp pa_brackets stmts body
pprDo other	  stmts body = panic "pprDo" 	-- PatGuard, ParStmtCxt

pprComp :: OutputableBndr id => (SDoc -> SDoc) -> [LStmt id] -> LHsExpr id -> SDoc
pprComp brack quals body
  = brack $
	hang (ppr body <+> char '|')
	     4 (interpp'SP quals)
\end{code}

%************************************************************************
%*									*
		Template Haskell quotation brackets
%*									*
%************************************************************************

\begin{code}
data HsSplice id  = HsSplice 	--  $z  or $(f 4)
			id 		-- The id is just a unique name to 
			(LHsExpr id) 	-- identify this splice point
					
instance OutputableBndr id => Outputable (HsSplice id) where
  ppr = pprSplice

pprSplice :: OutputableBndr id => HsSplice id -> SDoc
pprSplice (HsSplice n e) = char '$' <> ifPprDebug (brackets (ppr n)) <> pprParendExpr e


data HsBracket id = ExpBr (LHsExpr id)		-- [|  expr  |]
		  | PatBr (LPat id)		-- [p| pat   |]
		  | DecBr (HsGroup id)		-- [d| decls |]
		  | TypBr (LHsType id)		-- [t| type  |]
		  | VarBr id			-- 'x, ''T

instance OutputableBndr id => Outputable (HsBracket id) where
  ppr = pprHsBracket


pprHsBracket (ExpBr e) = thBrackets empty (ppr e)
pprHsBracket (PatBr p) = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBr d) = thBrackets (char 'd') (ppr d)
pprHsBracket (TypBr t) = thBrackets (char 't') (ppr t)
pprHsBracket (VarBr n) = char '\'' <> ppr n
	-- Infelicity: can't show ' vs '', because
	-- we can't ask n what its OccName is, because the 
	-- pretty-printer for HsExpr doesn't ask for NamedThings
	-- But the pretty-printer for names will show the OccName class

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
  = From	    (LHsExpr id)
  | FromThen 	    (LHsExpr id)
		    (LHsExpr id)
  | FromTo	    (LHsExpr id)
		    (LHsExpr id)
  | FromThenTo	    (LHsExpr id)
		    (LHsExpr id)
		    (LHsExpr id)
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
data HsMatchContext id	-- Context of a Match
  = FunRhs id Bool		-- Function binding for f; True <=> written infix
  | CaseAlt			-- Guard on a case alternative
  | LambdaExpr			-- Pattern of a lambda
  | ProcExpr			-- Pattern of a proc
  | PatBindRhs			-- Pattern binding
  | RecUpd			-- Record update [used only in DsExpr to tell matchWrapper
				-- 	what sort of runtime error message to generate]
  | StmtCtxt (HsStmtContext id)	-- Pattern of a do-stmt or list comprehension
  deriving ()

data HsStmtContext id
  = ListComp 
  | DoExpr 
  | MDoExpr PostTcTable			-- Recursive do-expression
					-- (tiresomely, it needs table
					--  of its return/bind ops)
  | PArrComp				-- Parallel array comprehension
  | PatGuard (HsMatchContext id)	-- Pattern guard for specified thing
  | ParStmtCtxt (HsStmtContext id)	-- A branch of a parallel stmt 
\end{code}

\begin{code}
isDoExpr :: HsStmtContext id -> Bool
isDoExpr DoExpr      = True
isDoExpr (MDoExpr _) = True
isDoExpr _           = False

isListCompExpr :: HsStmtContext id -> Bool
isListCompExpr ListComp = True
isListCompExpr PArrComp = True
isListCompExpr _        = False
\end{code}

\begin{code}
matchSeparator (FunRhs {})  = ptext SLIT("=")
matchSeparator CaseAlt      = ptext SLIT("->") 
matchSeparator LambdaExpr   = ptext SLIT("->") 
matchSeparator ProcExpr     = ptext SLIT("->") 
matchSeparator PatBindRhs   = ptext SLIT("=") 
matchSeparator (StmtCtxt _) = ptext SLIT("<-")  
matchSeparator RecUpd       = panic "unused"
\end{code}

\begin{code}
pprMatchContext (FunRhs fun _) 	  = ptext SLIT("the definition of") <+> quotes (ppr fun)
pprMatchContext CaseAlt	     	  = ptext SLIT("a case alternative")
pprMatchContext RecUpd	     	  = ptext SLIT("a record-update construct")
pprMatchContext PatBindRhs   	  = ptext SLIT("a pattern binding")
pprMatchContext LambdaExpr   	  = ptext SLIT("a lambda abstraction")
pprMatchContext ProcExpr   	  = ptext SLIT("an arrow abstraction")
pprMatchContext (StmtCtxt ctxt)   = ptext SLIT("a pattern binding in") $$ pprStmtContext ctxt

pprStmtContext (ParStmtCtxt c) = sep [ptext SLIT("a parallel branch of"), pprStmtContext c]
pprStmtContext (PatGuard ctxt) = ptext SLIT("a pattern guard for") $$ pprMatchContext ctxt
pprStmtContext DoExpr          = ptext SLIT("a 'do' expression")
pprStmtContext (MDoExpr _)     = ptext SLIT("an 'mdo' expression")
pprStmtContext ListComp        = ptext SLIT("a list comprehension")
pprStmtContext PArrComp        = ptext SLIT("an array comprehension")

{- 
pprMatchRhsContext (FunRhs fun) = ptext SLIT("a right-hand side of function") <+> quotes (ppr fun)
pprMatchRhsContext CaseAlt	= ptext SLIT("the body of a case alternative")
pprMatchRhsContext PatBindRhs	= ptext SLIT("the right-hand side of a pattern binding")
pprMatchRhsContext LambdaExpr	= ptext SLIT("the body of a lambda")
pprMatchRhsContext ProcExpr	= ptext SLIT("the body of a proc")
pprMatchRhsContext other	= panic "pprMatchRhsContext"	-- RecUpd, StmtCtxt

-- Used for the result statement of comprehension
-- e.g. the 'e' in	[ e | ... ]
--	or the 'r' in   f x = r
pprStmtResultContext (PatGuard ctxt) = pprMatchRhsContext ctxt
pprStmtResultContext other	     = ptext SLIT("the result of") <+> pprStmtContext other
-}

-- Used to generate the string for a *runtime* error message
matchContextErrString (FunRhs fun _)   	      	 = "function " ++ showSDoc (ppr fun)
matchContextErrString CaseAlt	      	      	 = "case"
matchContextErrString PatBindRhs      	      	 = "pattern binding"
matchContextErrString RecUpd	      	      	 = "record update"
matchContextErrString LambdaExpr      	      	 = "lambda"
matchContextErrString ProcExpr      	      	 = "proc"
matchContextErrString (StmtCtxt (ParStmtCtxt c)) = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (PatGuard _)) 	 = "pattern guard"
matchContextErrString (StmtCtxt DoExpr)       	 = "'do' expression"
matchContextErrString (StmtCtxt (MDoExpr _))   	 = "'mdo' expression"
matchContextErrString (StmtCtxt ListComp)     	 = "list comprehension"
matchContextErrString (StmtCtxt PArrComp)     	 = "array comprehension"
\end{code}

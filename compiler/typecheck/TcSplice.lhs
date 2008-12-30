%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcSplice: Template Haskell splices

\begin{code}
{-# OPTIONS -fno-warn-unused-imports -fno-warn-unused-binds #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcSplice( tcSpliceExpr, tcSpliceDecls, tcBracket,
                 lookupThName_maybe,
                 runQuasiQuoteExpr, runQuasiQuotePat, runAnnotation ) where

#include "HsVersions.h"

import HscMain
import TcRnDriver
	-- These imports are the reason that TcSplice 
	-- is very high up the module hierarchy

import HsSyn
import Convert
import RnExpr
import RnEnv
import RdrName
import RnTypes
import TcExpr
import TcHsSyn
import TcSimplify
import TcUnify
import TcType
import TcEnv
import TcMType
import TcHsType
import TcIface
import TypeRep
import Name
import NameEnv
import PrelNames
import HscTypes
import OccName
import Var
import Module
import Annotations
import TcRnMonad
import Class
import Inst
import TyCon
import DataCon
import Id
import IdInfo
import TysWiredIn
import DsMeta
import DsExpr
import DsMonad hiding (Splice)
import Serialized
import ErrUtils
import SrcLoc
import Outputable
import Unique
import Maybe
import BasicTypes
import Panic
import FastString
import Exception

import qualified Language.Haskell.TH as TH
-- THSyntax gives access to internal functions and data types
import qualified Language.Haskell.TH.Syntax as TH

#ifdef GHCI
-- Because GHC.Desugar might not be in the base library of the bootstrapping compiler
import GHC.Desugar      ( AnnotationWrapper(..) )
#endif

import GHC.Exts		( unsafeCoerce#, Int#, Int(..) )
import System.IO.Error
\end{code}

Note [Template Haskell levels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Imported things are impLevel (= 0)

* In GHCi, variables bound by a previous command are treated
  as impLevel, because we have bytecode for them.

* Variables are bound at the "current level"

* The current level starts off at topLevel (= 1)

* The level is decremented by splicing $(..)
	       incremented by brackets [| |]
	       incremented by name-quoting 'f

When a variable is used, we compare 
	bind:  binding level, and
	use:   current level at usage site

  Generally
	bind > use	Always error (bound later than used)
			[| \x -> $(f x) |]
			
	bind = use	Always OK (bound same stage as used)
			[| \x -> $(f [| x |]) |]

	bind < use	Inside brackets, it depends
			Inside splice, OK
			Inside neither, OK

  For (bind < use) inside brackets, there are three cases:
    - Imported things	OK	f = [| map |]
    - Top-level things	OK	g = [| f |]
    - Non-top-level 	Only if there is a liftable instance
				h = \(x:Int) -> [| x |]

See Note [What is a top-level Id?]

Note [Quoting names]
~~~~~~~~~~~~~~~~~~~~
A quoted name 'n is a bit like a quoted expression [| n |], except that we 
have no cross-stage lifting (c.f. TcExpr.thBrackId).  So, after incrementing
the use-level to account for the brackets, the cases are:

	bind > use			Error
	bind = use			OK
	bind < use	
		Imported things		OK
		Top-level things	OK
		Non-top-level		Error

See Note [What is a top-level Id?] in TcEnv.  Examples:

  f 'map	-- OK; also for top-level defns of this module

  \x. f 'x	-- Not ok (whereas \x. f [| x |] might have been ok, by
		--				 cross-stage lifting

  \y. [| \x. $(f 'y) |]	-- Not ok (same reason)

  [| \x. $(f 'x) |]	-- OK


Note [What is a top-level Id?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the level-control criteria above, we need to know what a "top level Id" is.
There are three kinds:
  * Imported from another module		(GlobalId, ExternalName)
  * Bound at the top level of this module	(ExternalName)
  * In GHCi, bound by a previous stmt		(GlobalId)
It's strange that there is no one criterion tht picks out all three, but that's
how it is right now.  (The obvious thing is to give an ExternalName to GHCi Ids 
bound in an earlier Stmt, but what module would you choose?  See 
Note [Interactively-bound Ids in GHCi] in TcRnDriver.)

The predicate we use is TcEnv.thTopLevelId.


%************************************************************************
%*									*
\subsection{Main interface + stubs for the non-GHCI case
%*									*
%************************************************************************

\begin{code}
tcBracket     :: HsBracket Name -> BoxyRhoType -> TcM (LHsExpr TcId)
tcSpliceDecls :: LHsExpr Name -> TcM [LHsDecl RdrName]
tcSpliceExpr  :: HsSplice Name -> BoxyRhoType -> TcM (HsExpr TcId)
kcSpliceType  :: HsSplice Name -> TcM (HsType Name, TcKind)
	-- None of these functions add constraints to the LIE

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)

runQuasiQuoteExpr :: HsQuasiQuote Name -> TcM (LHsExpr RdrName)
runQuasiQuotePat  :: HsQuasiQuote Name -> TcM (LPat RdrName)
runAnnotation     :: CoreAnnTarget -> LHsExpr Name -> TcM Annotation

#ifndef GHCI
tcBracket     x _ = pprPanic "Cant do tcBracket without GHCi"     (ppr x)
tcSpliceExpr  e   = pprPanic "Cant do tcSpliceExpr without GHCi"  (ppr e)
tcSpliceDecls x   = pprPanic "Cant do tcSpliceDecls without GHCi" (ppr x)
kcSpliceType  x   = pprPanic "Cant do kcSpliceType without GHCi"  (ppr x)

lookupThName_maybe n = pprPanic "Cant do lookupThName_maybe without GHCi" (ppr n)

runQuasiQuoteExpr q = pprPanic "Cant do runQuasiQuoteExpr without GHCi" (ppr q)
runQuasiQuotePat  q = pprPanic "Cant do runQuasiQuotePat without GHCi" (ppr q)
runAnnotation   _ q = pprPanic "Cant do runAnnotation without GHCi" (ppr q)
#else
\end{code}

%************************************************************************
%*									*
\subsection{Quoting an expression}
%*									*
%************************************************************************

Note [Handling brackets]
~~~~~~~~~~~~~~~~~~~~~~~~
Source:		f = [| Just $(g 3) |]
  The [| |] part is a HsBracket

Typechecked:	f = [| Just ${s7}(g 3) |]{s7 = g Int 3}
  The [| |] part is a HsBracketOut, containing *renamed* (not typechecked) expression
  The "s7" is the "splice point"; the (g Int 3) part is a typechecked expression

Desugared:	f = do { s7 <- g Int 3
		       ; return (ConE "Data.Maybe.Just" s7) }

\begin{code}
tcBracket brack res_ty = do
   level <- getStage
   case bracketOK level of {
	Nothing         -> failWithTc (illegalBracket level) ;
	Just next_level -> do

   	-- Typecheck expr to make sure it is valid,
	-- but throw away the results.  We'll type check
	-- it again when we actually use it.
    recordThUse
    pending_splices <- newMutVar []
    lie_var <- getLIEVar

    (meta_ty, lie) <- setStage (Brack next_level pending_splices lie_var)
                               (getLIE (tc_bracket next_level brack))
    tcSimplifyBracket lie

	-- Make the expected type have the right shape
    boxyUnify meta_ty res_ty

	-- Return the original expression, not the type-decorated one
    pendings <- readMutVar pending_splices
    return (noLoc (HsBracketOut brack pendings))
    }

tc_bracket :: ThLevel -> HsBracket Name -> TcM TcType
tc_bracket use_lvl (VarBr name) 	-- Note [Quoting names]
  = do	{ thing <- tcLookup name
	; case thing of
    	    AGlobal _ -> return ()
    	    ATcId { tct_level = bind_lvl, tct_id = id }
		| thTopLevelId id	-- C.f thTopLevelId case of
		-> keepAliveTc id 	--     TcExpr.thBrackId
		| otherwise
		-> do { checkTc (use_lvl == bind_lvl)
				(quotedNameStageErr name) }
	    _ -> pprPanic "th_bracket" (ppr name)

	; tcMetaTy nameTyConName 	-- Result type is Var (not Q-monadic)
	}

tc_bracket _ (ExpBr expr) 
  = do	{ any_ty <- newFlexiTyVarTy liftedTypeKind
	; tcMonoExpr expr any_ty
	; tcMetaTy expQTyConName }
	-- Result type is Expr (= Q Exp)

tc_bracket _ (TypBr typ) 
  = do	{ tcHsSigType ExprSigCtxt typ
	; tcMetaTy typeQTyConName }
	-- Result type is Type (= Q Typ)

tc_bracket _ (DecBr decls)
  = do	{  tcTopSrcDecls emptyModDetails decls
	-- Typecheck the declarations, dicarding the result
	-- We'll get all that stuff later, when we splice it in

	; decl_ty <- tcMetaTy decTyConName
	; q_ty    <- tcMetaTy qTyConName
	; return (mkAppTy q_ty (mkListTy decl_ty))
	-- Result type is Q [Dec]
    }

tc_bracket _ (PatBr _)
  = failWithTc (ptext (sLit "Tempate Haskell pattern brackets are not supported yet"))

quotedNameStageErr :: Name -> SDoc
quotedNameStageErr v 
  = sep [ ptext (sLit "Stage error: the non-top-level quoted name") <+> ppr (VarBr v)
	, ptext (sLit "must be used at the same stage at which is is bound")]
\end{code}


%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
tcSpliceExpr (HsSplice name expr) res_ty
  = setSrcSpan (getLoc expr) 	$ do
    level <- getStage
    case spliceOK level of {
	Nothing 	-> failWithTc (illegalSplice level) ;
	Just next_level -> 

     case level of {
	Comp _ 		       -> do { e <- tcTopSplice expr res_ty
				     ; return (unLoc e) } ;
	Brack _ ps_var lie_var -> do

	-- A splice inside brackets
  	-- NB: ignore res_ty, apart from zapping it to a mono-type
	-- e.g.   [| reverse $(h 4) |]
	-- Here (h 4) :: Q Exp
	-- but $(h 4) :: forall a.a 	i.e. anything!

      unBox res_ty
      meta_exp_ty <- tcMetaTy expQTyConName
      expr' <- setStage (Splice next_level) (
                 setLIEVar lie_var    $
                 tcMonoExpr expr meta_exp_ty
               )

	-- Write the pending splice into the bucket
      ps <- readMutVar ps_var
      writeMutVar ps_var ((name,expr') : ps)

      return (panic "tcSpliceExpr")	-- The returned expression is ignored

     ; Splice {} -> panic "tcSpliceExpr Splice"
     }} 

-- tcTopSplice used to have this:
-- Note that we do not decrement the level (to -1) before 
-- typechecking the expression.  For example:
--	f x = $( ...$(g 3) ... )
-- The recursive call to tcMonoExpr will simply expand the 
-- inner escape before dealing with the outer one

tcTopSplice :: LHsExpr Name -> BoxyRhoType -> TcM (LHsExpr Id)
tcTopSplice expr res_ty = do
    meta_exp_ty <- tcMetaTy expQTyConName

        -- Typecheck the expression
    zonked_q_expr <- tcTopSpliceExpr expr meta_exp_ty

        -- Run the expression
    traceTc (text "About to run" <+> ppr zonked_q_expr)
    expr2 <- runMetaE convertToHsExpr zonked_q_expr

    traceTc (text "Got result" <+> ppr expr2)

    showSplice "expression" 
               zonked_q_expr (ppr expr2)

        -- Rename it, but bale out if there are errors
        -- otherwise the type checker just gives more spurious errors
    (exp3, _fvs) <- checkNoErrs (rnLExpr expr2)

    tcMonoExpr exp3 res_ty


tcTopSpliceExpr :: LHsExpr Name -> TcType -> TcM (LHsExpr Id)
-- Type check an expression that is the body of a top-level splice
--   (the caller will compile and run it)
tcTopSpliceExpr expr meta_ty 
  = checkNoErrs $  -- checkNoErrs: must not try to run the thing
                   -- if the type checker fails!
    do { (expr', const_binds) <- tcSimplifyStagedExpr topSpliceStage $
                                 (recordThUse >> tcMonoExpr expr meta_ty)
          -- Zonk it and tie the knot of dictionary bindings
       ; zonkTopLExpr (mkHsDictLet const_binds expr') }
\end{code}


%************************************************************************
%*									*
	Annotations
%*									*
%************************************************************************

\begin{code}
runAnnotation target expr = do
    expr_ty <- newFlexiTyVarTy liftedTypeKind
    
    -- Find the classes we want instances for in order to call toAnnotationWrapper
    data_class <- tcLookupClass dataClassName
    
    -- Check the instances we require live in another module (we want to execute it..)
    -- and check identifiers live in other modules using TH stage checks. tcSimplifyStagedExpr
    -- also resolves the LIE constraints to detect e.g. instance ambiguity
    ((wrapper, expr'), const_binds) <- tcSimplifyStagedExpr topAnnStage $ do
                expr' <- tcPolyExprNC expr expr_ty
                -- By instantiating the call >here< it gets registered in the 
		-- LIE consulted by tcSimplifyStagedExpr
                -- and hence ensures the appropriate dictionary is bound by const_binds
                wrapper <- instCall AnnOrigin [expr_ty] [mkClassPred data_class [expr_ty]]
                return (wrapper, expr')

    -- We manually wrap the typechecked expression in a call to toAnnotationWrapper
    loc <- getSrcSpanM
    to_annotation_wrapper_id <- tcLookupId toAnnotationWrapperName
    let specialised_to_annotation_wrapper_expr = L loc (HsWrap wrapper (HsVar to_annotation_wrapper_id))
        wrapped_expr' = mkHsDictLet const_binds $
                        L loc (HsApp specialised_to_annotation_wrapper_expr expr')

    -- If we have type checking problems then potentially zonking 
    -- (and certainly compilation) may fail. Give up NOW!
    failIfErrsM

    -- Zonk the type variables out of that raw expression. Note that
    -- in particular we don't call recordThUse, since we don't
    -- necessarily use any code or definitions from that package.
    zonked_wrapped_expr' <- zonkTopLExpr wrapped_expr'

    -- Run the appropriately wrapped expression to get the value of
    -- the annotation and its dictionaries. The return value is of
    -- type AnnotationWrapper by construction, so this conversion is
    -- safe
    flip runMetaAW zonked_wrapped_expr' $ \annotation_wrapper ->
        case annotation_wrapper of
            AnnotationWrapper value | let serialized = toSerialized serializeWithData value ->
                -- Got the value and dictionaries: build the serialized value and 
		-- call it a day. We ensure that we seq the entire serialized value 
		-- in order that any errors in the user-written code for the
                -- annotation are exposed at this point.  This is also why we are 
		-- doing all this stuff inside the context of runMeta: it has the 
		-- facilities to deal with user error in a meta-level expression
                seqSerialized serialized `seq` Annotation { 
                    ann_target = target,
                    ann_value = serialized
                }
\end{code}


%************************************************************************
%*									*
	Quasi-quoting
%*									*
%************************************************************************

Note [Quasi-quote overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GHC "quasi-quote" extension is described by Geoff Mainland's paper
"Why it's nice to be quoted: quasiquoting for Haskell" (Haskell
Workshop 2007).

Briefly, one writes
	[:p| stuff |]
and the arbitrary string "stuff" gets parsed by the parser 'p', whose
type should be Language.Haskell.TH.Quote.QuasiQuoter.  'p' must be
defined in another module, because we are going to run it here.  It's
a bit like a TH splice:
	$(p "stuff")

However, you can do this in patterns as well as terms.  Becuase of this,
the splice is run by the *renamer* rather than the type checker.

\begin{code}
runQuasiQuote :: Outputable hs_syn
              => HsQuasiQuote Name	-- Contains term of type QuasiQuoter, and the String
              -> Name			-- Of type QuasiQuoter -> String -> Q th_syn
              -> String			-- Documentation string only
              -> Name			-- Name of th_syn type  
              -> (SrcSpan -> th_syn -> Either Message hs_syn)
              -> TcM hs_syn
runQuasiQuote (HsQuasiQuote _name quoter q_span quote) quote_selector desc meta_ty convert
  = do	{ -- Check that the quoter is not locally defined, otherwise the TH
          -- machinery will not be able to run the quasiquote.
        ; this_mod <- getModule
        ; let is_local = case nameModule_maybe quoter of
                           Just mod | mod == this_mod -> True
                                    | otherwise       -> False
                           Nothing -> True
	; traceTc (text "runQQ" <+> ppr quoter <+> ppr is_local)
        ; checkTc (not is_local) (quoteStageError quoter)

	  -- Build the expression 
      	; let quoterExpr = L q_span $! HsVar $! quoter
      	; let quoteExpr = L q_span $! HsLit $! HsString quote
      	; let expr = L q_span $
      	             HsApp (L q_span $
      	                    HsApp (L q_span (HsVar quote_selector)) quoterExpr) quoteExpr
      	; recordThUse
      	; meta_exp_ty <- tcMetaTy meta_ty

      	-- Typecheck the expression
      	; zonked_q_expr <- tcTopSpliceExpr expr meta_exp_ty

      	-- Run the expression
      	; traceTc (text "About to run" <+> ppr zonked_q_expr)
      	; result <- runMetaQ convert zonked_q_expr
      	; traceTc (text "Got result" <+> ppr result)
      	; showSplice desc zonked_q_expr (ppr result)
      	; return result
      	}

runQuasiQuoteExpr quasiquote
    = runQuasiQuote quasiquote quoteExpName "expression" expQTyConName convertToHsExpr

runQuasiQuotePat quasiquote
    = runQuasiQuote quasiquote quotePatName "pattern" patQTyConName convertToPat

quoteStageError :: Name -> SDoc
quoteStageError quoter
  = sep [ptext (sLit "GHC stage restriction:") <+> ppr quoter,
         nest 2 (ptext (sLit "is used in a quasiquote, and must be imported, not defined locally"))]
\end{code}


%************************************************************************
%*									*
		Splicing a type
%*									*
%************************************************************************

Very like splicing an expression, but we don't yet share code.

\begin{code}
kcSpliceType (HsSplice name hs_expr)
  = setSrcSpan (getLoc hs_expr) $ do 	
	{ level <- getStage
	; case spliceOK level of {
		Nothing 	-> failWithTc (illegalSplice level) ;
		Just next_level -> do 

	{ case level of {
		Comp _ 		       -> do { (t,k) <- kcTopSpliceType hs_expr 
					     ; return (unLoc t, k) } ;
		Brack _ ps_var lie_var -> do

	{ 	-- A splice inside brackets
	; meta_ty <- tcMetaTy typeQTyConName
	; expr' <- setStage (Splice next_level) $
		   setLIEVar lie_var	   	$
		   tcMonoExpr hs_expr meta_ty

		-- Write the pending splice into the bucket
	; ps <- readMutVar ps_var
	; writeMutVar ps_var ((name,expr') : ps)

	-- e.g.   [| Int -> $(h 4) |]
	-- Here (h 4) :: Q Type
	-- but $(h 4) :: forall a.a 	i.e. any kind
	; kind <- newKindVar
	; return (panic "kcSpliceType", kind)	-- The returned type is ignored
    }
        ; Splice {} -> panic "kcSpliceType Splice"
    }}}}

kcTopSpliceType :: LHsExpr Name -> TcM (LHsType Name, TcKind)
kcTopSpliceType expr
  = do	{ meta_ty <- tcMetaTy typeQTyConName

	-- Typecheck the expression
	; zonked_q_expr <- tcTopSpliceExpr expr meta_ty

	-- Run the expression
	; traceTc (text "About to run" <+> ppr zonked_q_expr)
	; hs_ty2 <- runMetaT convertToHsType zonked_q_expr
  
	; traceTc (text "Got result" <+> ppr hs_ty2)

	; showSplice "type" zonked_q_expr (ppr hs_ty2)

	-- Rename it, but bale out if there are errors
	-- otherwise the type checker just gives more spurious errors
	; let doc = ptext (sLit "In the spliced type") <+> ppr hs_ty2
	; hs_ty3 <- checkNoErrs (rnLHsType doc hs_ty2)

	; kcHsType hs_ty3 }
\end{code}

%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
-- Always at top level
-- Type sig at top of file:
-- 	tcSpliceDecls :: LHsExpr Name -> TcM [LHsDecl RdrName]
tcSpliceDecls expr
  = do	{ meta_dec_ty <- tcMetaTy decTyConName
	; meta_q_ty <- tcMetaTy qTyConName
	; let list_q = mkAppTy meta_q_ty (mkListTy meta_dec_ty)
	; zonked_q_expr <- tcTopSpliceExpr expr list_q

		-- Run the expression
	; traceTc (text "About to run" <+> ppr zonked_q_expr)
	; decls <- runMetaD convertToHsDecls zonked_q_expr

	; traceTc (text "Got result" <+> vcat (map ppr decls))
	; showSplice "declarations"
	  	     zonked_q_expr 
		     (ppr (getLoc expr) $$ (vcat (map ppr decls)))
	; return decls }
\end{code}


%************************************************************************
%*									*
\subsection{Running an expression}
%*									*
%************************************************************************

\begin{code}
runMetaAW :: (AnnotationWrapper -> output)
          -> LHsExpr Id         -- Of type AnnotationWrapper
          -> TcM output
runMetaAW k = runMeta False (\_ -> return . Right . k)
    -- We turn off showing the code in meta-level exceptions because doing so exposes
    -- the toAnnotationWrapper function that we slap around the users code

runQThen :: (SrcSpan -> input -> Either Message output)
         -> SrcSpan
         -> TH.Q input
         -> TcM (Either Message output)
runQThen f expr_span what = TH.runQ what >>= (return . f expr_span)

runMetaQ :: (SrcSpan -> input -> Either Message output)
	 -> LHsExpr Id
	 -> TcM output
runMetaQ = runMeta True . runQThen

runMetaE :: (SrcSpan -> TH.Exp -> Either Message (LHsExpr RdrName))
	 -> LHsExpr Id 		-- Of type (Q Exp)
	 -> TcM (LHsExpr RdrName)
runMetaE = runMetaQ

runMetaP :: (SrcSpan -> TH.Pat -> Either Message (Pat RdrName))
         -> LHsExpr Id          -- Of type (Q Pat)
         -> TcM (Pat RdrName)
runMetaP = runMetaQ

runMetaT :: (SrcSpan -> TH.Type -> Either Message (LHsType RdrName))
	 -> LHsExpr Id 		-- Of type (Q Type)
	 -> TcM (LHsType RdrName)	
runMetaT = runMetaQ

runMetaD :: (SrcSpan -> [TH.Dec] -> Either Message [LHsDecl RdrName])
	 -> LHsExpr Id 		-- Of type Q [Dec]
	 -> TcM [LHsDecl RdrName]
runMetaD = runMetaQ

runMeta :: Bool                 -- Whether code should be printed in the exception message
        -> (SrcSpan -> input -> TcM (Either Message output))
	-> LHsExpr Id 		-- Of type X
	-> TcM output		-- Of type t
runMeta show_code run_and_convert expr
  = do	{ 	-- Desugar
	  ds_expr <- initDsTc (dsLExpr expr)
	-- Compile and link it; might fail if linking fails
	; hsc_env <- getTopEnv
	; src_span <- getSrcSpanM
	; either_hval <- tryM $ liftIO $
			 HscMain.compileExpr hsc_env src_span ds_expr
	; case either_hval of {
	    Left exn   -> failWithTc (mk_msg "compile and link" exn) ;
	    Right hval -> do

	{ 	-- Coerce it to Q t, and run it

		-- Running might fail if it throws an exception of any kind (hence tryAllM)
		-- including, say, a pattern-match exception in the code we are running
		--
		-- We also do the TH -> HS syntax conversion inside the same
		-- exception-cacthing thing so that if there are any lurking 
		-- exceptions in the data structure returned by hval, we'll
		-- encounter them inside the try
		--
		-- See Note [Exceptions in TH] 
	  let expr_span = getLoc expr
	; either_tval <- tryAllM $
            		 setSrcSpan expr_span $	-- Set the span so that qLocation can
						-- see where this splice is
	     do	{ mb_result <- run_and_convert expr_span (unsafeCoerce# hval)
		; case mb_result of
		    Left err     -> failWithTc err
		    Right result -> return $! result }

	; case either_tval of
	    Right v -> return v
	    Left se ->
                    case fromException se of
                    Just IOEnvFailure ->
                        failM -- Error already in Tc monad
                    _ -> failWithTc (mk_msg "run" se)	-- Exception
        }}}
  where
    mk_msg s exn = vcat [text "Exception when trying to" <+> text s <+> text "compile-time code:",
			 nest 2 (text (Panic.showException exn)),
			 if show_code then nest 2 (text "Code:" <+> ppr expr) else empty]
\end{code}

Note [Exceptions in TH]
~~~~~~~~~~~~~~~~~~~~~~~
Supppose we have something like this 
	$( f 4 )
where
	f :: Int -> Q [Dec]
	f n | n>3       = fail "Too many declarations"
	    | otherwise = ...

The 'fail' is a user-generated failure, and should be displayed as a
perfectly ordinary compiler error message, not a panic or anything
like that.  Here's how it's processed:

  * 'fail' is the monad fail.  The monad instance for Q in TH.Syntax
    effectively transforms (fail s) to 
	qReport True s >> fail
    where 'qReport' comes from the Quasi class and fail from its monad
    superclass.

  * The TcM monad is an instance of Quasi (see TcSplice), and it implements
    (qReport True s) by using addErr to add an error message to the bag of errors.
    The 'fail' in TcM raises an IOEnvFailure exception

  * So, when running a splice, we catch all exceptions; then for 
	- an IOEnvFailure exception, we assume the error is already 
		in the error-bag (above)
	- other errors, we add an error to the bag
    and then fail


To call runQ in the Tc monad, we need to make TcM an instance of Quasi:

\begin{code}
instance TH.Quasi (IOEnv (Env TcGblEnv TcLclEnv)) where
  qNewName s = do { u <- newUnique 
		  ; let i = getKey u
		  ; return (TH.mkNameU s i) }

  qReport True msg  = addErr (text msg)
  qReport False msg = addReport (text msg)

  qLocation = do { m <- getModule
		 ; l <- getSrcSpanM
		 ; return (TH.Loc { TH.loc_filename = unpackFS (srcSpanFile l)
				  , TH.loc_module   = moduleNameString (moduleName m)
				  , TH.loc_package  = packageIdString (modulePackageId m)
				  , TH.loc_start = (srcSpanStartLine l, srcSpanStartCol l)
				  , TH.loc_end = (srcSpanEndLine   l, srcSpanEndCol   l) }) }
		
  qReify v = reify v

	-- For qRecover, discard error messages if 
	-- the recovery action is chosen.  Otherwise
	-- we'll only fail higher up.  c.f. tryTcLIE_
  qRecover recover main = do { (msgs, mb_res) <- tryTcErrs main
			     ; case mb_res of
		  	         Just val -> do { addMessages msgs	-- There might be warnings
				   	        ; return val }
		  	         Nothing  -> recover			-- Discard all msgs
			  }

  qRunIO io = liftIO io
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
showSplice :: String -> LHsExpr Id -> SDoc -> TcM ()
showSplice what before after = do
    loc <- getSrcSpanM
    traceSplice (vcat [ppr loc <> colon <+> text "Splicing" <+> text what, 
		       nest 2 (sep [nest 2 (ppr before),
				    text "======>",
				    nest 2 after])])

illegalBracket :: ThStage -> SDoc
illegalBracket level
  = ptext (sLit "Illegal bracket at level") <+> ppr level

illegalSplice :: ThStage -> SDoc
illegalSplice level
  = ptext (sLit "Illegal splice at level") <+> ppr level

#endif 	/* GHCI */
\end{code}


%************************************************************************
%*									*
			Reification
%*									*
%************************************************************************


\begin{code}
reify :: TH.Name -> TcM TH.Info
reify th_name
  = do	{ name <- lookupThName th_name
	; thing <- tcLookupTh name
		-- ToDo: this tcLookup could fail, which would give a
		-- 	 rather unhelpful error message
	; traceIf (text "reify" <+> text (show th_name) <+> brackets (ppr_ns th_name) <+> ppr name)
	; reifyThing thing
    }
  where
    ppr_ns (TH.Name _ (TH.NameG TH.DataName _pkg _mod)) = text "data"
    ppr_ns (TH.Name _ (TH.NameG TH.TcClsName _pkg _mod)) = text "tc"
    ppr_ns (TH.Name _ (TH.NameG TH.VarName _pkg _mod)) = text "var"
    ppr_ns _ = panic "reify/ppr_ns"

lookupThName :: TH.Name -> TcM Name
lookupThName th_name = do
    mb_name <- lookupThName_maybe th_name
    case mb_name of
        Nothing   -> failWithTc (notInScope th_name)
        Just name -> return name

lookupThName_maybe th_name
  =  do { names <- mapMaybeM lookup (thRdrNameGuesses th_name)
          -- Pick the first that works
	  -- E.g. reify (mkName "A") will pick the class A in preference to the data constructor A
	; return (listToMaybe names) }	
  where
    lookup rdr_name
	= do { 	-- Repeat much of lookupOccRn, becase we want
		-- to report errors in a TH-relevant way
	     ; rdr_env <- getLocalRdrEnv
  	     ; case lookupLocalRdrEnv rdr_env rdr_name of
		 Just name -> return (Just name)
	         Nothing   -> lookupGlobalOccRn_maybe rdr_name }

tcLookupTh :: Name -> TcM TcTyThing
-- This is a specialised version of TcEnv.tcLookup; specialised mainly in that
-- it gives a reify-related error message on failure, whereas in the normal
-- tcLookup, failure is a bug.
tcLookupTh name
  = do	{ (gbl_env, lcl_env) <- getEnvs
	; case lookupNameEnv (tcl_env lcl_env) name of {
		Just thing -> return thing;
		Nothing    -> do
	{ if nameIsLocalOrFrom (tcg_mod gbl_env) name
	  then	-- It's defined in this module
	      case lookupNameEnv (tcg_type_env gbl_env) name of
		Just thing -> return (AGlobal thing)
		Nothing	   -> failWithTc (notInEnv name)
	 
	  else do 		-- It's imported
	{ (eps,hpt) <- getEpsAndHpt
        ; dflags <- getDOpts
	; case lookupType dflags hpt (eps_PTE eps) name of 
	    Just thing -> return (AGlobal thing)
	    Nothing    -> do { thing <- tcImportDecl name
			     ; return (AGlobal thing) }
		-- Imported names should always be findable; 
		-- if not, we fail hard in tcImportDecl
    }}}}

notInScope :: TH.Name -> SDoc
notInScope th_name = quotes (text (TH.pprint th_name)) <+> 
		     ptext (sLit "is not in scope at a reify")
	-- Ugh! Rather an indirect way to display the name

notInEnv :: Name -> SDoc
notInEnv name = quotes (ppr name) <+> 
		     ptext (sLit "is not in the type environment at a reify")

------------------------------
reifyThing :: TcTyThing -> TcM TH.Info
-- The only reason this is monadic is for error reporting,
-- which in turn is mainly for the case when TH can't express
-- some random GHC extension

reifyThing (AGlobal (AnId id))
  = do	{ ty <- reifyType (idType id)
	; fix <- reifyFixity (idName id)
	; let v = reifyName id
	; case globalIdDetails id of
	    ClassOpId cls    -> return (TH.ClassOpI v ty (reifyName cls) fix)
	    _                -> return (TH.VarI     v ty Nothing fix)
    }

reifyThing (AGlobal (ATyCon tc))  = reifyTyCon tc
reifyThing (AGlobal (AClass cls)) = reifyClass cls
reifyThing (AGlobal (ADataCon dc))
  = do	{ let name = dataConName dc
	; ty <- reifyType (idType (dataConWrapId dc))
	; fix <- reifyFixity name
	; return (TH.DataConI (reifyName name) ty (reifyName (dataConTyCon dc)) fix) }

reifyThing (ATcId {tct_id = id, tct_type = ty}) 
  = do	{ ty1 <- zonkTcType ty	-- Make use of all the info we have, even
				-- though it may be incomplete
	; ty2 <- reifyType ty1
	; fix <- reifyFixity (idName id)
	; return (TH.VarI (reifyName id) ty2 Nothing fix) }

reifyThing (ATyVar tv ty) 
  = do	{ ty1 <- zonkTcType ty
	; ty2 <- reifyType ty1
	; return (TH.TyVarI (reifyName tv) ty2) }

reifyThing (AThing {}) = panic "reifyThing AThing"

------------------------------
reifyTyCon :: TyCon -> TcM TH.Info
reifyTyCon tc
  | isFunTyCon tc  = return (TH.PrimTyConI (reifyName tc) 2 		  False)
  | isPrimTyCon tc = return (TH.PrimTyConI (reifyName tc) (tyConArity tc) (isUnLiftedTyCon tc))
  | isSynTyCon tc
  = do { let (tvs, rhs) = synTyConDefn tc 
       ; rhs' <- reifyType rhs
       ; return (TH.TyConI $ 
		   TH.TySynD (reifyName tc) (reifyTyVars tvs) rhs') }

reifyTyCon tc
  = do 	{ cxt <- reifyCxt (tyConStupidTheta tc)
	; let tvs = tyConTyVars tc
	; cons <- mapM (reifyDataCon (mkTyVarTys tvs)) (tyConDataCons tc)
	; let name = reifyName tc
	      r_tvs  = reifyTyVars tvs
	      deriv = []	-- Don't know about deriving
	      decl | isNewTyCon tc = TH.NewtypeD cxt name r_tvs (head cons) deriv
		   | otherwise	   = TH.DataD    cxt name r_tvs cons 	  deriv
	; return (TH.TyConI decl) }

reifyDataCon :: [Type] -> DataCon -> TcM TH.Con
reifyDataCon tys dc
  | isVanillaDataCon dc
  = do 	{ arg_tys <- reifyTypes (dataConInstOrigArgTys dc tys)
	; let stricts = map reifyStrict (dataConStrictMarks dc)
	      fields  = dataConFieldLabels dc
	      name    = reifyName dc
	      [a1,a2] = arg_tys
	      [s1,s2] = stricts
	; ASSERT( length arg_tys == length stricts )
          if not (null fields) then
	     return (TH.RecC name (zip3 (map reifyName fields) stricts arg_tys))
	  else
	  if dataConIsInfix dc then
	     ASSERT( length arg_tys == 2 )
	     return (TH.InfixC (s1,a1) name (s2,a2))
	  else
	     return (TH.NormalC name (stricts `zip` arg_tys)) }
  | otherwise
  = failWithTc (ptext (sLit "Can't reify a non-Haskell-98 data constructor:") 
		<+> quotes (ppr dc))

------------------------------
reifyClass :: Class -> TcM TH.Info
reifyClass cls 
  = do	{ cxt <- reifyCxt theta
	; ops <- mapM reify_op op_stuff
	; return (TH.ClassI $ TH.ClassD cxt (reifyName cls) (reifyTyVars tvs) fds' ops) }
  where
    (tvs, fds, theta, _, _, op_stuff) = classExtraBigSig cls
    fds' = map reifyFunDep fds
    reify_op (op, _) = do { ty <- reifyType (idType op)
			  ; return (TH.SigD (reifyName op) ty) }

------------------------------
reifyType :: TypeRep.Type -> TcM TH.Type
reifyType (TyVarTy tv)	    = return (TH.VarT (reifyName tv))
reifyType (TyConApp tc tys) = reify_tc_app (reifyName tc) tys
reifyType (AppTy t1 t2)     = do { [r1,r2] <- reifyTypes [t1,t2] ; return (r1 `TH.AppT` r2) }
reifyType (FunTy t1 t2)     = do { [r1,r2] <- reifyTypes [t1,t2] ; return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2) }
reifyType ty@(ForAllTy _ _) = do { cxt' <- reifyCxt cxt; 
				 ; tau' <- reifyType tau 
				 ; return (TH.ForallT (reifyTyVars tvs) cxt' tau') }
			    where
				(tvs, cxt, tau) = tcSplitSigmaTy ty
reifyType (PredTy {}) = panic "reifyType PredTy"

reifyTypes :: [Type] -> TcM [TH.Type]
reifyTypes = mapM reifyType
reifyCxt :: [PredType] -> TcM [TH.Type]
reifyCxt   = mapM reifyPred

reifyFunDep :: ([TyVar], [TyVar]) -> TH.FunDep
reifyFunDep (xs, ys) = TH.FunDep (map reifyName xs) (map reifyName ys)

reifyTyVars :: [TyVar] -> [TH.Name]
reifyTyVars = map reifyName

reify_tc_app :: TH.Name -> [TypeRep.Type] -> TcM TH.Type
reify_tc_app tc tys = do { tys' <- reifyTypes tys 
			 ; return (foldl TH.AppT (TH.ConT tc) tys') }

reifyPred :: TypeRep.PredType -> TcM TH.Type
reifyPred (ClassP cls tys) = reify_tc_app (reifyName cls) tys
reifyPred p@(IParam _ _)   = noTH (sLit "implicit parameters") (ppr p)
reifyPred (EqPred {})      = panic "reifyPred EqPred"


------------------------------
reifyName :: NamedThing n => n -> TH.Name
reifyName thing
  | isExternalName name = mk_varg pkg_str mod_str occ_str
  | otherwise	        = TH.mkNameU occ_str (getKey (getUnique name))
	-- Many of the things we reify have local bindings, and 
	-- NameL's aren't supposed to appear in binding positions, so
	-- we use NameU.  When/if we start to reify nested things, that
	-- have free variables, we may need to generate NameL's for them.
  where
    name    = getName thing
    mod     = ASSERT( isExternalName name ) nameModule name
    pkg_str = packageIdString (modulePackageId mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = occNameString occ
    occ     = nameOccName name
    mk_varg | OccName.isDataOcc occ = TH.mkNameG_d
	    | OccName.isVarOcc  occ = TH.mkNameG_v
	    | OccName.isTcOcc   occ = TH.mkNameG_tc
	    | otherwise		    = pprPanic "reifyName" (ppr name)

------------------------------
reifyFixity :: Name -> TcM TH.Fixity
reifyFixity name
  = do	{ fix <- lookupFixityRn name
	; return (conv_fix fix) }
    where
      conv_fix (BasicTypes.Fixity i d) = TH.Fixity i (conv_dir d)
      conv_dir BasicTypes.InfixR = TH.InfixR
      conv_dir BasicTypes.InfixL = TH.InfixL
      conv_dir BasicTypes.InfixN = TH.InfixN

reifyStrict :: BasicTypes.StrictnessMark -> TH.Strict
reifyStrict MarkedStrict    = TH.IsStrict
reifyStrict MarkedUnboxed   = TH.IsStrict
reifyStrict NotMarkedStrict = TH.NotStrict

------------------------------
noTH :: LitString -> SDoc -> TcM a
noTH s d = failWithTc (hsep [ptext (sLit "Can't represent") <+> ptext s <+> 
				ptext (sLit "in Template Haskell:"),
		 	     nest 2 d])
\end{code}

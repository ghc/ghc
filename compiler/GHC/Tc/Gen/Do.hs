
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
(c) The University of Iowa 2023

-}

-- | Expand @Do@ block statements into @(>>=)@, @(>>)@ and @let@s
--   After renaming but right ebefore type checking
module GHC.Tc.Gen.Do (expandDoStmts) where

import GHC.Prelude

import GHC.Rename.Utils ( wrapGenSpan, genHsExpApps, genHsApp, genHsLet,
                          genHsLamDoExp, genHsCaseAltDoExp )
import GHC.Rename.Env   ( irrefutableConLikeRn )

import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcMType

import GHC.Hs

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Driver.DynFlags ( DynFlags, getDynFlags )
import GHC.Driver.Ppr (showPpr)

import GHC.Types.SrcLoc
import GHC.Types.Basic
import qualified GHC.LanguageExtensions as LangExt

import Data.List ((\\))


{-
************************************************************************
*                                                                      *
\subsection{XXExprGhcRn for Do Statements}
*                                                                      *
************************************************************************
-}
-- | Expand the `do`-statments into expressions right after renaming
--   so that they can be typechecked.
--   See Note [Expanding HsDo with XXExprGhcRn] below for `HsDo` specific commentary
--   and Note [Handling overloaded and rebindable constructs] for high level commentary
expandDoStmts :: HsDoFlavour -> [ExprLStmt GhcRn] -> TcM (LHsExpr GhcRn)
expandDoStmts doFlav stmts = expand_do_stmts doFlav stmts

-- | The main work horse for expanding do block statements into applications of binds and thens
--   See Note [Expanding HsDo with XXExprGhcRn]
expand_do_stmts :: HsDoFlavour -> [ExprLStmt GhcRn] -> TcM (LHsExpr GhcRn)

expand_do_stmts ListComp _ =
  pprPanic "expand_do_stmts: impossible happened. ListComp" empty
        -- handled by `GHC.Tc.Gen.Match.tcLcStmt`

expand_do_stmts _ (stmt@(L _ (TransStmt {})):_) =
  pprPanic "expand_do_stmts: TransStmt" $ ppr stmt
  -- handeled by `GHC.Tc.Gen.Match.tcLcStmt`

expand_do_stmts _ (stmt@(L _ (ParStmt {})):_) =
  pprPanic "expand_do_stmts: ParStmt" $ ppr stmt
  -- handeled by `GHC.Tc.Gen.Match.tcLcStmt`

expand_do_stmts _ (stmt@(L _ (XStmtLR ApplicativeStmt{})): _) =
  pprPanic "expand_do_stmts: Applicative Stmt" $ ppr stmt
  -- Handeled by tcSyntaxOp see `GHC.Tc.Gen.Match.tcStmtsAndThen`

expand_do_stmts _ [] = pprPanic "expand_do_stmts: impossible happened. Empty stmts" empty

expand_do_stmts flav [stmt@(L _loc (LastStmt _ (L body_loc body) _ ret_expr))]
-- See  Note [Expanding HsDo with XXExprGhcRn] Equation (5) below
-- last statement of a list comprehension, needs to explicitly return it
-- See `checkLastStmt` and `Syntax.Expr.StmtLR.LastStmt`
   | NoSyntaxExprRn <- ret_expr
   -- Last statement is just body if we are not in ListComp context. See Syntax.Expr.LastStmt
   = return $ L body_loc (mkExpandedStmt stmt flav (HsPar noExtField (L body_loc body)))
   | SyntaxExprRn ret <- ret_expr
   --
   --    ------------------------------------------------
   --               return e  ~~> return e
   -- to make T18324 work
   = do let expansion = genHsApp ret (L body_loc body)
        return $ L body_loc (mkExpandedStmt stmt flav (HsPar noExtField (L body_loc expansion)))

expand_do_stmts doFlavour (stmt@(L loc (LetStmt _ bs)) : lstmts) =
-- See  Note [Expanding HsDo with XXExprGhcRn] Equation (3) below
--                      stmts ~~> stmts'
--    ------------------------------------------------
--       let x = e ; stmts ~~> let x = e in stmts'
  do expand_stmts_expr <- expand_do_stmts doFlavour lstmts
     let expansion = genHsLet bs (genPopErrCtxtExpr expand_stmts_expr)
     return $ L loc (mkExpandedStmt stmt doFlavour expansion)

expand_do_stmts doFlavour (stmt@(L loc (BindStmt xbsrn pat e)): lstmts)
  | SyntaxExprRn bind_op <- xbsrn_bindOp xbsrn
  , fail_op              <- xbsrn_failOp xbsrn
-- See  Note [Expanding HsDo with XXExprGhcRn] Equation (2) below
-- the pattern binding pat can fail
--      stmts ~~> stmt'    f = \case pat -> stmts';
--                                   _   -> fail "Pattern match failure .."
--    -------------------------------------------------------
--       pat <- e ; stmts   ~~> (>>=) e f
  = do expand_stmts_expr <- expand_do_stmts doFlavour lstmts
       failable_expr <- mk_failable_expr doFlavour pat (genPopErrCtxtExpr expand_stmts_expr) fail_op
       let expansion = genHsExpApps bind_op  -- (>>=)
                       [ genPopErrCtxtExpr e
                       , failable_expr ]
       return $ L loc (mkExpandedStmt stmt doFlavour expansion)
  | otherwise
  = pprPanic "expand_do_stmts: The impossible happened, missing bind operator from renamer" (text "stmt" <+> ppr  stmt)

expand_do_stmts doFlavour (stmt@(L loc (BodyStmt _ e (SyntaxExprRn then_op) _)) : lstmts) =
-- See Note [BodyStmt] in Language.Haskell.Syntax.Expr
-- See  Note [Expanding HsDo with XXExprGhcRn] Equation (1) below
--              stmts ~~> stmts'
--    ----------------------------------------------
--      e ; stmts ~~> (>>) e stmts'
  do expand_stmts_expr <- expand_do_stmts doFlavour lstmts
     let expansion = genHsExpApps then_op  -- (>>)
                     [ genPopErrCtxtExpr e
                     , genPopErrCtxtExpr $ expand_stmts_expr ]
     return $ L loc (mkExpandedStmt stmt doFlavour expansion)

expand_do_stmts doFlavour
       ((L loc (RecStmt { recS_stmts = L stmts_loc rec_stmts
                        , recS_later_ids = later_ids  -- forward referenced local ids
                        , recS_rec_ids = local_ids     -- ids referenced outside of the rec block
                        , recS_bind_fn = SyntaxExprRn bind_fun   -- the (>>=) expr
                        , recS_mfix_fn = SyntaxExprRn mfix_fun   -- the `mfix` expr
                        , recS_ret_fn  = SyntaxExprRn return_fun -- the `return` expr
                                                          -- use it explicitly
                                                          -- at the end of expanded rec block
                        }))
         : lstmts) =
-- See Note [Typing a RecStmt] in Language.Haskell.Syntax.Expr
-- See  Note [Expanding HsDo with XXExprGhcRn] Equation (4) and (6) below
--                                   stmts ~~> stmts'
--    -------------------------------------------------------------------------------------------
--      rec { later_ids, local_ids, rec_block } ; stmts
--                    ~~> (>>=) (mfix (\[ local_only_ids ++ later_ids ]
--                                           -> do { rec_stmts
--                                                 ; return (local_only_ids ++ later_ids) } ))
--                              (\ [ local_only_ids ++ later_ids ] -> stmts')
  do expand_stmts_expr <- expand_do_stmts doFlavour lstmts
     -- NB: No need to wrap the expansion with an ExpandedStmt
     -- as we want to flatten the rec block statements into its parent do block anyway
     return $ (mkHsApps (wrapGenSpan bind_fun)                                          -- (>>=)
                      [ (wrapGenSpan mfix_fun) `mkHsApp` mfix_expr                      -- (mfix (do block))
                      , genHsLamDoExp doFlavour [ mkBigLHsVarPatTup all_ids ]           --        (\ x ->
                                       expand_stmts_expr                                --  stmts')
                      ])
  where
    local_only_ids = local_ids \\ later_ids -- get unique local rec ids;
                                            -- local rec ids and later ids can overlap
    all_ids = local_only_ids ++ later_ids   -- put local ids before return ids

    return_stmt  :: ExprLStmt GhcRn
    return_stmt  = wrapGenSpan $ LastStmt noExtField
                                     (mkBigLHsTup (map nlHsVar all_ids) noExtField)
                                     Nothing
                                     (SyntaxExprRn return_fun)
    do_stmts     :: XRec GhcRn [ExprLStmt GhcRn]
    do_stmts     = L stmts_loc $ rec_stmts ++ [return_stmt]
    do_block     :: LHsExpr GhcRn
    do_block     = L loc $ HsDo noExtField doFlavour do_stmts
    mfix_expr    :: LHsExpr GhcRn
    mfix_expr    = genHsLamDoExp doFlavour [ wrapGenSpan (LazyPat noExtField $ mkBigLHsVarPatTup all_ids) ]
                                          $ do_block
                             -- NB: LazyPat because we do not want to eagerly evaluate the pattern
                             -- and potentially loop forever

expand_do_stmts _ stmts = pprPanic "expand_do_stmts: impossible happened" $ (ppr stmts)

-- checks the pattern `pat` for irrefutability which decides if we need to wrap it with a fail block
mk_failable_expr :: HsDoFlavour -> LPat GhcRn -> LHsExpr GhcRn -> FailOperator GhcRn -> TcM (LHsExpr GhcRn)
mk_failable_expr doFlav lpat expr@(L _exprloc _) fail_op =
  do { is_strict <- xoptM LangExt.Strict
     ; hscEnv <- getTopEnv
     ; rdrEnv <- getGlobalRdrEnv
     ; comps <- getCompleteMatchesTcM
     ; let irrf_pat = isIrrefutableHsPat is_strict (irrefutableConLikeRn hscEnv rdrEnv comps) lpat
     ; traceTc "mk_failable_expr" (vcat [ text "pat:" <+> ppr lpat
                                        , text "isIrrefutable:" <+> ppr irrf_pat
                                        ])
     ; if irrf_pat -- don't wrap with fail block if
                   -- the pattern is irrefutable
       then return $ genHsLamDoExp doFlav [lpat] expr
       else wrapGenSpan <$> mk_fail_block doFlav lpat expr fail_op
     }

-- | Makes the fail block with a given fail_op
-- mk_fail_block pat rhs fail builds
-- \x. case x of {pat -> rhs; _ -> fail "Pattern match failure..."}
mk_fail_block :: HsDoFlavour
              -> LPat GhcRn -> LHsExpr GhcRn -> FailOperator GhcRn -> TcM (HsExpr GhcRn)
mk_fail_block doFlav pat@(L ploc _) e (Just (SyntaxExprRn fail_op)) =
  do  dflags <- getDynFlags
      return $ HsLam noAnn LamCases $ mkMatchGroup (doExpansionOrigin doFlav) -- \
                (wrapGenSpan [ genHsCaseAltDoExp doFlav pat e                 --  pat -> expr
                             , fail_alt_case dflags pat fail_op               --  _   -> fail "fail pattern"
                             ])
        where
          fail_alt_case :: DynFlags -> LPat GhcRn -> HsExpr GhcRn -> LMatch GhcRn (LHsExpr GhcRn)
          fail_alt_case dflags pat fail_op = genHsCaseAltDoExp doFlav (L ploc $ WildPat noExtField) $
                                             wrapGenSpan (fail_op_expr dflags pat fail_op)

          fail_op_expr :: DynFlags -> LPat GhcRn -> HsExpr GhcRn -> HsExpr GhcRn
          fail_op_expr dflags pat fail_op
            = genHsApp fail_op (mk_fail_msg_expr dflags pat)

          mk_fail_msg_expr :: DynFlags -> LPat GhcRn -> LHsExpr GhcRn
          mk_fail_msg_expr dflags pat
            = nlHsLit $ mkHsString $ showPpr dflags $
              text "Pattern match failure in" <+> pprHsDoFlavour (DoExpr Nothing)
                   <+> text "at" <+> ppr (getLocA pat)


mk_fail_block _ _ _ _ = pprPanic "mk_fail_block: impossible happened" empty


{- Note [Expanding HsDo with XXExprGhcRn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We expand `do`-blocks before typechecking it, by re-using the existing `XXExprGhcRns` and `RebindableSyntax` machinery.
This is very similar to:
  1. Expansions done in `GHC.Rename.Expr.rnHsIf` for expanding `HsIf`; and
  2. `desugarRecordUpd` in `GHC.Tc.Gen.Expr.tcExpr` for expanding `RecordUpd`
See Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr

To disabmiguate desugaring (`HsExpr GhcTc -> Core.Expr`) we use the phrase expansion
(`HsExpr GhcRn -> HsExpr GhcRn`)

This expansion is done right before typechecking and after renaming
See Part 2. of Note [Doing XXExprGhcRn in the Renamer vs Typechecker] in `GHC.Rename.Expr`

Historical note START
---------------------
In previous versions of GHC, the `do`-notation wasn't expanded before typechecking,
instead the typechecker would operate directly on the original.
Why? because it ensured that type error messages were explained in terms of
what the programmer has written. In practice, however, this didn't work very well:

* Attempts to typecheck the original source code turned out to be buggy, and virtually impossible
  to fix (#14963, #15598, #21206 and others)

* The typechecker expected the `>>=` operator to have a type that matches
  `m _ -> (_ -> m _) -> m _` for some `m`. With `RebindableSyntax` or
  `QualifiedDo` the `>>=` operator might not have the
  standard type. It might have a type like

      (>>=) :: Wombat m => m a1 a2 b -> (b -> m a2 a3 c) -> m a1 a3 c

  Typechecking the term `(>>=) e1 (\x -> e2)` deals with all of this automatically.

* With `ImpredicativeTypes` the programmer will expect Quick Look to instantiate
  the quantifiers impredicatively (#18324). Again, that happens automatically if
  you typecheck the expanded expression.

Historical note END
-------------------

Do Expansions Equationally
--------------------------
We have the following schema for expanding `do`-statements.
They capture the essence of statement expansions as implemented in `expand_do_stmts`

  DO【 _ 】 maps a sequence of do statements and recursively converts them into expressions

          (1) DO【 s; ss 】      = ‹ExpansionStmt s›((>>) s (‹PopErrCtxt›DO【 ss 】))

          (2) DO【 p <- e; ss 】 = if p is irrefutable
                                   then ‹ExpansionStmt (p <- e)›
                                          (>>=) s ((\ p -> ‹PopExprCtxt› DO【 ss 】))
                                   else ‹ExpansionStmt (p <- e)›
                                          (>>=) s ((\case p -> ‹PopExprCtxt› DO【 ss 】
                                                          _ -> fail "pattern p failure"))

          (3) DO【 let x = e; ss 】
                                 = ‹ExpansionStmt (let x = e)› (let x = e in (‹PopErrCtxt›DO【 ss 】))


          (4) DO【 rec ss; sss 】
                                 = (>>=) e (\vars -> ‹PopErrCtxt›DO【 sss 】))
                                           where (vars, e) = RECDO【 ss 】

          (5) DO【 s 】          = s

  RECDO【 _ 】 maps a sequence of recursively dependent monadic statements and converts it into an expression paired
              with the variables that the rec finds a fix point of.

          (6) RECDO【 ss 】     = (vars, mfix (\~vars -> (>>=) (DO【 ss 】) (return vars)))
                                  where vars are all the variables free in ss


For a concrete example, consider a `do`-block written by the user

    f = {l0} do {l1} {pl}p <- {l1'} e1
                {l2} g p
                {l3} return {l3'} p

The expanded version (performed by `expand_do_stmts`) looks like:

    f = {g1} (>>=) ({l1'} e1) (\ {pl}p ->
                   {g2} (>>) ({l2} g p)
                             ({l3} return p))

The {l1} etc are location/source span information stored in the AST by the parser,
{g1} are compiler generated source spans.


The 3 non-obvious points to consider are:
 1. Wrap the expression with a `fail` block if the pattern match is not irrefutable.
    See Part 1. below
 2. Generate appropriate warnings for discarded results in a body statement
    eg. say `do { .. ; (g p :: m Int) ; ... }`
    See Part 2. below
 3. Generating appropriate type error messages which blame the correct source spans
    See Part 3. below

Part 1. Expanding Patterns Bindings
-----------------------------------
If `p` is a failable pattern---checked by `GHC.Tc.Gen.Pat.isIrrefutableHsPatRnTcM`---
we need to wrap it with a `fail`-block. See Equation (2) above.

The expansion of the `do`-block

        do { Just p <- e1; e2 }

(ignoring the location information) will be

        (>>=) (e1)
              (\case                 -- anonymous continuation lambda
                 Just p -> e2
                 _      -> fail "failable pattern p at location")

The `fail`-block wrapping is done by `GHC.Tc.Gen.Do.mk_failable_expr`.

* Note the explicit call to `fail`, in the monad of the `do`-block.  Part of the specification
  of do-notation is that if the pattern match fails, we fail in the monad, *not* just crash
  at runtime.

* According to the language specification, when the pattern is irrefutable,
  we should not add the `fail` alternative. This is important because
  the occurrence of `fail` means that the typechecker will generate a `MonadFail` constraint,
  and irrefutable patterns shouldn't need a fail alternative.

* _Wrinkel 1_: Note that pattern synonyms count as refutable during type checking,
  (see `isIrrefutableHsPat`). They will hence generate a
  `MonadFail` constraint and they will always be wrapped in a `fail`able-block.

  Consider a patten synonym declaration (testcase T24552):

             pattern MyJust :: a -> Maybe a
             pattern MyJust x <- Just x where MyJust = Just

  and a `do`-block with the following bind and return statement

             do { MyJust x <- [MyNothing, MyJust ()]
                ; return x }

  The `do`-expansion will generate the expansion

            (>>=) ([MyNothing, MyJust ()])
                  (\case MyJust x -> return x                     -- (1)
                         _        -> fail "failable pattern .. "  -- (2)
                  )

  This code (specifically the `match` spanning lines (1) and (2)) is a compiler generated code;
  the associated `Origin` in tagged `Generated`
  The alternative statements will thus be ignored by the pattern match check (c.f. `isMatchContextPmChecked`).
  This ensures we do not generate spurious redundant-pattern-match warnings due to the line (2) above.
  See Note [Generated code and pattern-match checking]
  See Note [Long-distance information in matchWrapper]

* _Wrinkle 2_: The call to `fail` will give rise to a `MonadFail` constraint. What `CtOrigin` do we
  attach to that constraint?  When the `MonadFail` constraint can't be solved, it'll show up in error
  messages and it needs to be a good location.  Ideally, it should identify the
  pattern `p`.  Hence, we wrap the `fail` alternative expression with a `ExpandedPat`
  that tags the fail expression with the failable pattern. (See testcase MonadFailErrors.hs)

Part 2. Generate warnings for discarded body statement results
--------------------------------------------------------------
If the `do`-blocks' body statement is an expression that returns a
value that is not of type `()`, we need to warn the user about discarded
the value when `-Wunused-binds` flag is turned on. (See testcase T3263-2.hs)

For example the `do`-block

    do { e1;  e2 } -- where, e1 :: m Int

expands to

    (>>) e1 e2

* If `e1` returns a non-() value we want to emit a warning, telling the user that they
  are discarding the value returned by e1. This is done by `HsToCore.dsExpr` in the `HsApp`
  with a call to `HsToCore.warnUnusedBindValue`.

* The decision to trigger the warning is: if the function is a compiler generated `(>>)`,
  and its first argument `e1` has a non-() type

Part 3. Blaming Offending Source Code and Generating Appropriate Error Messages
-------------------------------------------------------------------------------
To ensure we correctly track source of the offending user written source code,
in this case the `do`-statement, we need to keep track of
which source statement's expansion the typechecker is currently typechecking.
For this purpose we use the `XXExprGhcRn.ExpansionRn`.
It stores the original statement (with location) and the expanded expression

  A. Expanding Body Statements
  -----------------------------
  For example, the `do`-block

      do { e1;  e2; e3 }

  expands (ignoring the location info) to

      ‹ExpandedThingRn do { e1; e2; e3 }›                        -- Original Do Expression
                                                                 -- Expanded Do Expression
          (‹ExpandedThingRn e1›                                  -- Original Statement
               ({(>>) e1}                                        -- Expanded Expression
                  ‹PopErrCtxt› (‹ExpandedThingRn e2›
                         ({(>>) e2}
                            ‹PopErrCtxt› (‹ExpandedThingRn e3› {e3})))))

  * Whenever the typechecker steps through an `ExpandedThingRn`,
    we push the original statement in the error context, set the error location to the
    location of the statement, and then typecheck the expanded expression.
    This is similar to vanilla `XXExprGhcRn` and rebindable syntax
    See Note [Rebindable syntax and XXExprGhcRn] in `GHC.Hs.Expr`.

  * Recall, that when a source function argument fails to typecheck,
    we print an error message like "In the second argument of the function f..".
    However, `(>>)` is generated thus, we don't want to display that to the user; it would be confusing.
    But also, we do not want to completely ignore it as we do want to keep the error blame carets
    as precise as possible, and not just blame the complete `do`-block.
    Thus, when we typecheck the application `(>>) e1`, we push the "In the stmt of do block e1" with
    the source location of `e1` in the error context stack as we walk inside an `ExpandedThingRn`.
    See also Note [splitHsApps].

  * After the expanded expression of a `do`-statement is typechecked
    and before moving to the next statement of the `do`-block, we need to first pop the top
    of the error context stack which contains the error message for
    the previous statement: eg. "In the stmt of a do block: e1".
    This is explicitly encoded in the expansion expression using
    the `XXExprGhcRn.PopErrCtxt`. Whenever `GHC.Tc.Gen.Expr.tcExpr` (via `GHC.Tc.Gen.tcXExpr`)
    sees a `PopErrCtxt` it calls `GHC.Tc.Utils.Monad.popErrCtxt` to pop of the top of error context stack.
    See ‹PopErrCtxt› in the example above.
    Without this popping business for error context stack,
    if there is a type error in `e2`, we would get a spurious and confusing error message
    which mentions "In the stmt of a do block e1" along with the message
    "In the stmt of a do block e2".

  B. Expanding Bind Statements
  -----------------------------
  A `do`-block with a bind statement:

      do { p <- e1; e2 }

  expands (ignoring the location information) to

     ‹ExpandedThingRn do{ p <- e1; e2 }›                                      -- Original Do Expression
                                                                              --
         (‹ExpandedThingRn (p <- e1)›                                         -- Original Statement
                        (((>>=) e1)                                           -- Expanded Expression
                           ‹PopErrCtxt› ((\ p -> ‹ExpandedThingRn (e2)› e2)))
         )


  However, the expansion lambda `(\p -> e2)` is special as it is generated from a `do`-stmt expansion
  and if a type checker error occurs in the pattern `p` (which is source generated), we need to say
  "in a pattern binding in a do block" and not "in the pattern of a lambda" (cf. Typeable1.hs).
  We hence use a tag `GenReason` in `Ghc.Tc.Origin`. When typechecking a `HsLam` in `Tc.Gen.Expr.tcExpr`
  the `match_ctxt` is set to a `StmtCtxt` if `GenOrigin` is a `DoExpansionOrigin`.
-}


-- | Wrap a located expression with a `PopErrCtxt`
mkPopErrCtxtExpr :: HsExpr GhcRn -> HsExpr GhcRn
mkPopErrCtxtExpr a = XExpr (PopErrCtxt a)

genPopErrCtxtExpr :: LHsExpr GhcRn -> LHsExpr GhcRn
genPopErrCtxtExpr (L loc a) = L loc (mkPopErrCtxtExpr a)

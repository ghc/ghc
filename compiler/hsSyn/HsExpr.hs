{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- | Abstract Haskell syntax for expressions.
module HsExpr where

#include "HsVersions.h"

-- friends:
import GhcPrelude

import HsDecls
import HsPat
import HsLit
import PlaceHolder ( NameOrRdrName )
import HsExtension
import HsTypes
import HsBinds

-- others:
import TcEvidence
import CoreSyn
import DynFlags ( gopt, GeneralFlag(Opt_PrintExplicitCoercions) )
import Name
import NameSet
import RdrName  ( GlobalRdrEnv )
import BasicTypes
import ConLike
import SrcLoc
import Util
import Outputable
import FastString
import Type
import TcType (TcType)
import {-# SOURCE #-} TcRnTypes (TcLclEnv)

-- libraries:
import Data.Data hiding (Fixity(..))
import qualified Data.Data as Data (Fixity(..))
import Data.Maybe (isNothing)

import GHCi.RemoteTypes ( ForeignRef )
import qualified Language.Haskell.TH as TH (Q)

{-
************************************************************************
*                                                                      *
\subsection{Expressions proper}
*                                                                      *
************************************************************************
-}

-- * Expressions proper

-- | Located Haskell Expression
type LHsExpr p = Located (HsExpr p)
  -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
  --   in a list

  -- For details on above see note [Api annotations] in ApiAnnotation

-------------------------
-- | Post-Type checking Expression
--
-- PostTcExpr is an evidence expression attached to the syntax tree by the
-- type checker (c.f. postTcType).
type PostTcExpr  = HsExpr GhcTc

-- | Post-Type checking Table
--
-- We use a PostTcTable where there are a bunch of pieces of evidence, more
-- than is convenient to keep individually.
type PostTcTable = [(Name, PostTcExpr)]

-------------------------
-- | Syntax Expression
--
-- SyntaxExpr is like 'PostTcExpr', but it's filled in a little earlier,
-- by the renamer.  It's used for rebindable syntax.
--
-- E.g. @(>>=)@ is filled in before the renamer by the appropriate 'Name' for
--      @(>>=)@, and then instantiated by the type checker with its type args
--      etc
--
-- This should desugar to
--
-- > syn_res_wrap $ syn_expr (syn_arg_wraps[0] arg0)
-- >                         (syn_arg_wraps[1] arg1) ...
--
-- where the actual arguments come from elsewhere in the AST.
-- This could be defined using @GhcPass p@ and such, but it's
-- harder to get it all to work out that way. ('noSyntaxExpr' is hard to
-- write, for example.)
data SyntaxExpr p = SyntaxExpr { syn_expr      :: HsExpr p
                               , syn_arg_wraps :: [HsWrapper]
                               , syn_res_wrap  :: HsWrapper }

-- | This is used for rebindable-syntax pieces that are too polymorphic
-- for tcSyntaxOp (trS_fmap and the mzip in ParStmt)
noExpr :: HsExpr (GhcPass p)
noExpr = HsLit noExt (HsString (SourceText  "noExpr") (fsLit "noExpr"))

noSyntaxExpr :: SyntaxExpr (GhcPass p)
                              -- Before renaming, and sometimes after,
                              -- (if the syntax slot makes no sense)
noSyntaxExpr = SyntaxExpr { syn_expr      = HsLit noExt (HsString NoSourceText
                                                        (fsLit "noSyntaxExpr"))
                          , syn_arg_wraps = []
                          , syn_res_wrap  = WpHole }

-- | Make a 'SyntaxExpr (HsExpr _)', missing its HsWrappers.
mkSyntaxExpr :: HsExpr (GhcPass p) -> SyntaxExpr (GhcPass p)
mkSyntaxExpr expr = SyntaxExpr { syn_expr      = expr
                               , syn_arg_wraps = []
                               , syn_res_wrap  = WpHole }

-- | Make a 'SyntaxExpr Name' (the "rn" is because this is used in the
-- renamer), missing its HsWrappers.
mkRnSyntaxExpr :: Name -> SyntaxExpr GhcRn
mkRnSyntaxExpr name = mkSyntaxExpr $ HsVar noExt $ noLoc name
  -- don't care about filling in syn_arg_wraps because we're clearly
  -- not past the typechecker

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (SyntaxExpr p) where
  ppr (SyntaxExpr { syn_expr      = expr
                  , syn_arg_wraps = arg_wraps
                  , syn_res_wrap  = res_wrap })
    = sdocWithDynFlags $ \ dflags ->
      getPprStyle $ \s ->
      if debugStyle s || gopt Opt_PrintExplicitCoercions dflags
      then ppr expr <> braces (pprWithCommas ppr arg_wraps)
                    <> braces (ppr res_wrap)
      else ppr expr

-- | Command Syntax Table (for Arrow syntax)
type CmdSyntaxTable p = [(Name, HsExpr p)]
-- See Note [CmdSyntaxTable]

{-
Note [CmdSyntaxtable]
~~~~~~~~~~~~~~~~~~~~~
Used only for arrow-syntax stuff (HsCmdTop), the CmdSyntaxTable keeps
track of the methods needed for a Cmd.

* Before the renamer, this list is an empty list

* After the renamer, it takes the form @[(std_name, HsVar actual_name)]@
  For example, for the 'arr' method
   * normal case:            (GHC.Control.Arrow.arr, HsVar GHC.Control.Arrow.arr)
   * with rebindable syntax: (GHC.Control.Arrow.arr, arr_22)
             where @arr_22@ is whatever 'arr' is in scope

* After the type checker, it takes the form [(std_name, <expression>)]
  where <expression> is the evidence for the method.  This evidence is
  instantiated with the class, but is still polymorphic in everything
  else.  For example, in the case of 'arr', the evidence has type
         forall b c. (b->c) -> a b c
  where 'a' is the ambient type of the arrow.  This polymorphism is
  important because the desugarer uses the same evidence at multiple
  different types.

This is Less Cool than what we normally do for rebindable syntax, which is to
make fully-instantiated piece of evidence at every use site.  The Cmd way
is Less Cool because
  * The renamer has to predict which methods are needed.
    See the tedious RnExpr.methodNamesCmd.

  * The desugarer has to know the polymorphic type of the instantiated
    method. This is checked by Inst.tcSyntaxName, but is less flexible
    than the rest of rebindable syntax, where the type is less
    pre-ordained.  (And this flexibility is useful; for example we can
    typecheck do-notation with (>>=) :: m1 a -> (a -> m2 b) -> m2 b.)
-}

-- | An unbound variable; used for treating
-- out-of-scope variables as expression holes
--
-- Either "x", "y"     Plain OutOfScope
-- or     "_", "_x"    A TrueExprHole
--
-- Both forms indicate an out-of-scope variable,  but the latter
-- indicates that the user /expects/ it to be out of scope, and
-- just wants GHC to report its type
data UnboundVar
  = OutOfScope OccName GlobalRdrEnv  -- ^ An (unqualified) out-of-scope
                                     -- variable, together with the GlobalRdrEnv
                                     -- with respect to which it is unbound

                                     -- See Note [OutOfScope and GlobalRdrEnv]

  | TrueExprHole OccName             -- ^ A "true" expression hole (_ or _x)

  deriving Data

instance Outputable UnboundVar where
    ppr (OutOfScope occ _) = text "OutOfScope" <> parens (ppr occ)
    ppr (TrueExprHole occ) = text "ExprHole"   <> parens (ppr occ)

unboundVarOcc :: UnboundVar -> OccName
unboundVarOcc (OutOfScope occ _) = occ
unboundVarOcc (TrueExprHole occ) = occ

{-
Note [OutOfScope and GlobalRdrEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To understand why we bundle a GlobalRdrEnv with an out-of-scope variable,
consider the following module:

    module A where

    foo :: ()
    foo = bar

    bat :: [Double]
    bat = [1.2, 3.4]

    $(return [])

    bar = ()
    bad = False

When A is compiled, the renamer determines that `bar` is not in scope in the
declaration of `foo` (since `bar` is declared in the following inter-splice
group).  Once it has finished typechecking the entire module, the typechecker
then generates the associated error message, which specifies both the type of
`bar` and a list of possible in-scope alternatives:

    A.hs:6:7: error:
        • Variable not in scope: bar :: ()
        • ‘bar’ (line 13) is not in scope before the splice on line 11
          Perhaps you meant ‘bat’ (line 9)

When it calls RnEnv.unknownNameSuggestions to identify these alternatives, the
typechecker must provide a GlobalRdrEnv.  If it provided the current one, which
contains top-level declarations for the entire module, the error message would
incorrectly suggest the out-of-scope `bar` and `bad` as possible alternatives
for `bar` (see Trac #11680).  Instead, the typechecker must use the same
GlobalRdrEnv the renamer used when it determined that `bar` is out-of-scope.

To obtain this GlobalRdrEnv, can the typechecker simply use the out-of-scope
`bar`'s location to either reconstruct it (from the current GlobalRdrEnv) or to
look it up in some global store?  Unfortunately, no.  The problem is that
location information is not always sufficient for this task.  This is most
apparent when dealing with the TH function addTopDecls, which adds its
declarations to the FOLLOWING inter-splice group.  Consider these declarations:

    ex9 = cat               -- cat is NOT in scope here

    $(do -------------------------------------------------------------
        ds <- [d| f = cab   -- cat and cap are both in scope here
                  cat = ()
                |]
        addTopDecls ds
        [d| g = cab         -- only cap is in scope here
            cap = True
          |])

    ex10 = cat              -- cat is NOT in scope here

    $(return []) -----------------------------------------------------

    ex11 = cat              -- cat is in scope

Here, both occurrences of `cab` are out-of-scope, and so the typechecker needs
the GlobalRdrEnvs which were used when they were renamed.  These GlobalRdrEnvs
are different (`cat` is present only in the GlobalRdrEnv for f's `cab'), but the
locations of the two `cab`s are the same (they are both created in the same
splice).  Thus, we must include some additional information with each `cab` to
allow the typechecker to obtain the correct GlobalRdrEnv.  Clearly, the simplest
information to use is the GlobalRdrEnv itself.
-}

-- | A Haskell expression.
data HsExpr p
  = HsVar     (XVar p)
              (Located (IdP p)) -- ^ Variable

                             -- See Note [Located RdrNames]

  | HsUnboundVar (XUnboundVar p)
                 UnboundVar  -- ^ Unbound variable; also used for "holes"
                             --   (_ or _x).
                             -- Turned from HsVar to HsUnboundVar by the
                             --   renamer, when it finds an out-of-scope
                             --   variable or hole.
                             -- Turned into HsVar by type checker, to support
                             --   deferred type errors.

  | HsConLikeOut (XConLikeOut p)
                 ConLike     -- ^ After typechecker only; must be different
                             -- HsVar for pretty printing

  | HsRecFld  (XRecFld p)
              (AmbiguousFieldOcc p) -- ^ Variable pointing to record selector
                                    -- Not in use after typechecking

  | HsOverLabel (XOverLabel p)
                (Maybe (IdP p)) FastString
     -- ^ Overloaded label (Note [Overloaded labels] in GHC.OverloadedLabels)
     --   @Just id@ means @RebindableSyntax@ is in use, and gives the id of the
     --   in-scope 'fromLabel'.
     --   NB: Not in use after typechecking

  | HsIPVar   (XIPVar p)
              HsIPName   -- ^ Implicit parameter (not in use after typechecking)
  | HsOverLit (XOverLitE p)
              (HsOverLit p)  -- ^ Overloaded literals

  | HsLit     (XLitE p)
              (HsLit p)      -- ^ Simple (non-overloaded) literals

  | HsLam     (XLam p)
              (MatchGroup p (LHsExpr p))
                       -- ^ Lambda abstraction. Currently always a single match
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --       'ApiAnnotation.AnnRarrow',

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsLamCase (XLamCase p) (MatchGroup p (LHsExpr p)) -- ^ Lambda-case
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --           'ApiAnnotation.AnnCase','ApiAnnotation.AnnOpen',
       --           'ApiAnnotation.AnnClose'

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsApp     (XApp p) (LHsExpr p) (LHsExpr p) -- ^ Application

  | HsAppType (XAppTypeE p) (LHsExpr p) (LHsWcType (NoGhcTc p))  -- ^ Visible type application
       --
       -- Explicit type argument; e.g  f @Int x y
       -- NB: Has wildcards, but no implicit quantification
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt',

  -- | Operator applications:
  -- NB Bracketed ops such as (+) come out as Vars.

  -- NB We need an expr for the operator in an OpApp/Section since
  -- the typechecker may need to apply the operator to a few types.

  | OpApp       (XOpApp p)
                (LHsExpr p)       -- left operand
                (LHsExpr p)       -- operator
                (LHsExpr p)       -- right operand

  -- | Negation operator. Contains the negated expression and the name
  -- of 'negate'
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnMinus'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | NegApp      (XNegApp p)
                (LHsExpr p)
                (SyntaxExpr p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
  --             'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsPar       (XPar p)
                (LHsExpr p)  -- ^ Parenthesised expr; see Note [Parens in HsSyn]

  | SectionL    (XSectionL p)
                (LHsExpr p)    -- operand; see Note [Sections in HsSyn]
                (LHsExpr p)    -- operator
  | SectionR    (XSectionR p)
                (LHsExpr p)    -- operator; see Note [Sections in HsSyn]
                (LHsExpr p)    -- operand

  -- | Used for explicit tuples and sections thereof
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitTuple
        (XExplicitTuple p)
        [LHsTupArg p]
        Boxity

  -- | Used for unboxed sum types
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(#'@,
  --          'ApiAnnotation.AnnVbar', 'ApiAnnotation.AnnClose' @'#)'@,
  --
  --  There will be multiple 'ApiAnnotation.AnnVbar', (1 - alternative) before
  --  the expression, (arity - alternative) after it
  | ExplicitSum
          (XExplicitSum p)
          ConTag --  Alternative (one-based)
          Arity  --  Sum arity
          (LHsExpr p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnCase',
  --       'ApiAnnotation.AnnOf','ApiAnnotation.AnnOpen' @'{'@,
  --       'ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCase      (XCase p)
                (LHsExpr p)
                (MatchGroup p (LHsExpr p))

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf',
  --       'ApiAnnotation.AnnSemi',
  --       'ApiAnnotation.AnnThen','ApiAnnotation.AnnSemi',
  --       'ApiAnnotation.AnnElse',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsIf        (XIf p)
                (Maybe (SyntaxExpr p)) -- cond function
                                        -- Nothing => use the built-in 'if'
                                        -- See Note [Rebindable if]
                (LHsExpr p)    --  predicate
                (LHsExpr p)    --  then part
                (LHsExpr p)    --  else part

  -- | Multi-way if
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf'
  --       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsMultiIf   (XMultiIf p) [LGRHS p (LHsExpr p)]

  -- | let(rec)
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet',
  --       'ApiAnnotation.AnnOpen' @'{'@,
  --       'ApiAnnotation.AnnClose' @'}'@,'ApiAnnotation.AnnIn'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsLet       (XLet p)
                (LHsLocalBinds p)
                (LHsExpr  p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDo',
  --             'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnSemi',
  --             'ApiAnnotation.AnnVbar',
  --             'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsDo        (XDo p)                  -- Type of the whole expression
                (HsStmtContext Name)     -- The parameterisation is unimportant
                                         -- because in this context we never use
                                         -- the PatGuard or ParStmt variant
                (Located [ExprLStmt p]) -- "do":one or more stmts

  -- | Syntactic list: [a,b,c,...]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
  --              'ApiAnnotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitList
                (XExplicitList p)  -- Gives type of components of list
                (Maybe (SyntaxExpr p))
                                   -- For OverloadedLists, the fromListN witness
                [LHsExpr p]

  -- | Record construction
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
  --         'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecordCon
      { rcon_ext      :: XRecordCon p
      , rcon_con_name :: Located (IdP p)    -- The constructor name;
                                            --  not used after type checking
      , rcon_flds     :: HsRecordBinds p }  -- The fields

  -- | Record update
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
  --         'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecordUpd
      { rupd_ext  :: XRecordUpd p
      , rupd_expr :: LHsExpr p
      , rupd_flds :: [LHsRecUpdField p]
      }
  -- For a type family, the arg types are of the *instance* tycon,
  -- not the family tycon

  -- | Expression with an explicit type signature. @e :: type@
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExprWithTySig
                (XExprWithTySig p)

                (LHsExpr p)
                (LHsSigWcType (NoGhcTc p))

  -- | Arithmetic sequence
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
  --              'ApiAnnotation.AnnComma','ApiAnnotation.AnnDotdot',
  --              'ApiAnnotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ArithSeq
                (XArithSeq p)
                (Maybe (SyntaxExpr p))
                                  -- For OverloadedLists, the fromList witness
                (ArithSeqInfo p)

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsSCC       (XSCC p)
                SourceText            -- Note [Pragma source text] in BasicTypes
                StringLiteral         -- "set cost centre" SCC pragma
                (LHsExpr p)           -- expr whose cost is to be measured

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{-\# CORE'@,
  --             'ApiAnnotation.AnnVal', 'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCoreAnn   (XCoreAnn p)
                SourceText            -- Note [Pragma source text] in BasicTypes
                StringLiteral         -- hdaume: core annotation
                (LHsExpr p)

  -----------------------------------------------------------
  -- MetaHaskell Extensions

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnOpenE','ApiAnnotation.AnnOpenEQ',
  --         'ApiAnnotation.AnnClose','ApiAnnotation.AnnCloseQ'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsBracket    (XBracket p) (HsBracket p)

    -- See Note [Pending Splices]
  | HsRnBracketOut
      (XRnBracketOut p)
      (HsBracket GhcRn)    -- Output of the renamer is the *original* renamed
                           -- expression, plus
      [PendingRnSplice]    -- _renamed_ splices to be type checked

  | HsTcBracketOut
      (XTcBracketOut p)
      (HsBracket GhcRn)    -- Output of the type checker is the *original*
                           -- renamed expression, plus
      [PendingTcSplice]    -- _typechecked_ splices to be
                           -- pasted back in by the desugarer

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsSpliceE  (XSpliceE p) (HsSplice p)

  -----------------------------------------------------------
  -- Arrow notation extension

  -- | @proc@ notation for Arrows
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnProc',
  --          'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsProc      (XProc p)
                (LPat p)               -- arrow abstraction, proc
                (LHsCmdTop p)          -- body of the abstraction
                                       -- always has an empty stack

  ---------------------------------------
  -- static pointers extension
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnStatic',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsStatic (XStatic p) -- Free variables of the body
             (LHsExpr p)        -- Body

  ---------------------------------------
  -- The following are commands, not expressions proper
  -- They are only used in the parsing stage and are removed
  --    immediately in parser.RdrHsSyn.checkCommand

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.Annlarrowtail',
  --          'ApiAnnotation.Annrarrowtail','ApiAnnotation.AnnLarrowtail',
  --          'ApiAnnotation.AnnRarrowtail'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsArrApp             -- Arrow tail, or arrow application (f -< arg)
        (XArrApp p)     -- type of the arrow expressions f,
                        -- of the form a t t', where arg :: t
        (LHsExpr p)     -- arrow expression, f
        (LHsExpr p)     -- input expression, arg
        HsArrAppType    -- higher-order (-<<) or first-order (-<)
        Bool            -- True => right-to-left (f -< arg)
                        -- False => left-to-right (arg >- f)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpenB' @'(|'@,
  --         'ApiAnnotation.AnnCloseB' @'|)'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsArrForm            -- Command formation,  (| e cmd1 .. cmdn |)
        (XArrForm p)
        (LHsExpr p)      -- the operator
                         -- after type-checking, a type abstraction to be
                         -- applied to the type of the local environment tuple
        (Maybe Fixity)   -- fixity (filled in by the renamer), for forms that
                         -- were converted from OpApp's by the renamer
        [LHsCmdTop p]    -- argument commands

  ---------------------------------------
  -- Haskell program coverage (Hpc) Support

  | HsTick
     (XTick p)
     (Tickish (IdP p))
     (LHsExpr p)                       -- sub-expression

  | HsBinTick
     (XBinTick p)
     Int                                -- module-local tick number for True
     Int                                -- module-local tick number for False
     (LHsExpr p)                        -- sub-expression

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --       'ApiAnnotation.AnnOpen' @'{-\# GENERATED'@,
  --       'ApiAnnotation.AnnVal','ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnColon','ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnMinus',
  --       'ApiAnnotation.AnnVal','ApiAnnotation.AnnColon',
  --       'ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsTickPragma                      -- A pragma introduced tick
     (XTickPragma p)
     SourceText                       -- Note [Pragma source text] in BasicTypes
     (StringLiteral,(Int,Int),(Int,Int))
                                      -- external span for this tick
     ((SourceText,SourceText),(SourceText,SourceText))
        -- Source text for the four integers used in the span.
        -- See note [Pragma source text] in BasicTypes
     (LHsExpr p)

  ---------------------------------------
  -- These constructors only appear temporarily in the parser.
  -- The renamer translates them into the Right Thing.

  | EWildPat (XEWildPat p)        -- wildcard

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | EAsPat      (XEAsPat p)
                (Located (IdP p)) -- as pattern
                (LHsExpr p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | EViewPat    (XEViewPat p)
                (LHsExpr p) -- view pattern
                (LHsExpr p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ELazyPat    (XELazyPat p) (LHsExpr p) -- ~ pattern


  ---------------------------------------
  -- Finally, HsWrap appears only in typechecker output
  -- The contained Expr is *NOT* itself an HsWrap.
  -- See Note [Detecting forced eta expansion] in DsExpr. This invariant
  -- is maintained by HsUtils.mkHsWrap.

  |  HsWrap     (XWrap p)
                HsWrapper    -- TRANSLATION
                (HsExpr p)

  | XExpr       (XXExpr p) -- Note [Trees that Grow] extension constructor


-- | Extra data fields for a 'RecordCon', added by the type checker
data RecordConTc = RecordConTc
      { rcon_con_like :: ConLike      -- The data constructor or pattern synonym
      , rcon_con_expr :: PostTcExpr   -- Instantiated constructor function
      }

-- | Extra data fields for a 'RecordUpd', added by the type checker
data RecordUpdTc = RecordUpdTc
      { rupd_cons :: [ConLike]
                -- Filled in by the type checker to the
                -- _non-empty_ list of DataCons that have
                -- all the upd'd fields

      , rupd_in_tys  :: [Type] -- Argument types of *input* record type
      , rupd_out_tys :: [Type] --             and  *output* record type
                               -- The original type can be reconstructed
                               -- with conLikeResTy
      , rupd_wrap :: HsWrapper -- See note [Record Update HsWrapper]
      } deriving Data

-- ---------------------------------------------------------------------

type instance XVar           (GhcPass _) = NoExt
type instance XUnboundVar    (GhcPass _) = NoExt
type instance XConLikeOut    (GhcPass _) = NoExt
type instance XRecFld        (GhcPass _) = NoExt
type instance XOverLabel     (GhcPass _) = NoExt
type instance XIPVar         (GhcPass _) = NoExt
type instance XOverLitE      (GhcPass _) = NoExt
type instance XLitE          (GhcPass _) = NoExt
type instance XLam           (GhcPass _) = NoExt
type instance XLamCase       (GhcPass _) = NoExt
type instance XApp           (GhcPass _) = NoExt

type instance XAppTypeE      (GhcPass _) = NoExt

type instance XOpApp         GhcPs = NoExt
type instance XOpApp         GhcRn = Fixity
type instance XOpApp         GhcTc = Fixity

type instance XNegApp        (GhcPass _) = NoExt
type instance XPar           (GhcPass _) = NoExt
type instance XSectionL      (GhcPass _) = NoExt
type instance XSectionR      (GhcPass _) = NoExt
type instance XExplicitTuple (GhcPass _) = NoExt

type instance XExplicitSum   GhcPs = NoExt
type instance XExplicitSum   GhcRn = NoExt
type instance XExplicitSum   GhcTc = [Type]

type instance XCase          (GhcPass _) = NoExt
type instance XIf            (GhcPass _) = NoExt

type instance XMultiIf       GhcPs = NoExt
type instance XMultiIf       GhcRn = NoExt
type instance XMultiIf       GhcTc = Type

type instance XLet           (GhcPass _) = NoExt

type instance XDo            GhcPs = NoExt
type instance XDo            GhcRn = NoExt
type instance XDo            GhcTc = Type

type instance XExplicitList  GhcPs = NoExt
type instance XExplicitList  GhcRn = NoExt
type instance XExplicitList  GhcTc = Type

type instance XRecordCon     GhcPs = NoExt
type instance XRecordCon     GhcRn = NoExt
type instance XRecordCon     GhcTc = RecordConTc

type instance XRecordUpd     GhcPs = NoExt
type instance XRecordUpd     GhcRn = NoExt
type instance XRecordUpd     GhcTc = RecordUpdTc

type instance XExprWithTySig (GhcPass _) = NoExt

type instance XArithSeq      GhcPs = NoExt
type instance XArithSeq      GhcRn = NoExt
type instance XArithSeq      GhcTc = PostTcExpr

type instance XSCC           (GhcPass _) = NoExt
type instance XCoreAnn       (GhcPass _) = NoExt
type instance XBracket       (GhcPass _) = NoExt

type instance XRnBracketOut  (GhcPass _) = NoExt
type instance XTcBracketOut  (GhcPass _) = NoExt

type instance XSpliceE       (GhcPass _) = NoExt
type instance XProc          (GhcPass _) = NoExt

type instance XStatic        GhcPs = NoExt
type instance XStatic        GhcRn = NameSet
type instance XStatic        GhcTc = NameSet

type instance XArrApp        GhcPs = NoExt
type instance XArrApp        GhcRn = NoExt
type instance XArrApp        GhcTc = Type

type instance XArrForm       (GhcPass _) = NoExt
type instance XTick          (GhcPass _) = NoExt
type instance XBinTick       (GhcPass _) = NoExt
type instance XTickPragma    (GhcPass _) = NoExt
type instance XEWildPat      (GhcPass _) = NoExt
type instance XEAsPat        (GhcPass _) = NoExt
type instance XEViewPat      (GhcPass _) = NoExt
type instance XELazyPat      (GhcPass _) = NoExt
type instance XWrap          (GhcPass _) = NoExt
type instance XXExpr         (GhcPass _) = NoExt

-- ---------------------------------------------------------------------

-- | Located Haskell Tuple Argument
--
-- 'HsTupArg' is used for tuple sections
-- @(,a,)@ is represented by
-- @ExplicitTuple [Missing ty1, Present a, Missing ty3]@
-- Which in turn stands for @(\x:ty1 \y:ty2. (x,a,y))@
type LHsTupArg id = Located (HsTupArg id)
-- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma'

-- For details on above see note [Api annotations] in ApiAnnotation

-- | Haskell Tuple Argument
data HsTupArg id
  = Present (XPresent id) (LHsExpr id)     -- ^ The argument
  | Missing (XMissing id)    -- ^ The argument is missing, but this is its type
  | XTupArg (XXTupArg id)    -- ^ Note [Trees that Grow] extension point

type instance XPresent         (GhcPass _) = NoExt

type instance XMissing         GhcPs = NoExt
type instance XMissing         GhcRn = NoExt
type instance XMissing         GhcTc = Type

type instance XXTupArg         (GhcPass _) = NoExt

tupArgPresent :: LHsTupArg id -> Bool
tupArgPresent (L _ (Present {})) = True
tupArgPresent (L _ (Missing {})) = False
tupArgPresent (L _ (XTupArg {})) = False

{-
Note [Parens in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~
HsPar (and ParPat in patterns, HsParTy in types) is used as follows

  * HsPar is required; the pretty printer does not add parens.

  * HsPars are respected when rearranging operator fixities.
    So   a * (b + c)  means what it says (where the parens are an HsPar)

  * For ParPat and HsParTy the pretty printer does add parens but this should be
    a no-op for ParsedSource, based on the pretty printer round trip feature
    introduced in
    https://phabricator.haskell.org/rGHC499e43824bda967546ebf95ee33ec1f84a114a7c

  * ParPat and HsParTy are pretty printed as '( .. )' regardless of whether or
    not they are strictly necessary. This should be addressed when #13238 is
    completed, to be treated the same as HsPar.


Note [Sections in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~
Sections should always appear wrapped in an HsPar, thus
         HsPar (SectionR ...)
The parser parses sections in a wider variety of situations
(See Note [Parsing sections]), but the renamer checks for those
parens.  This invariant makes pretty-printing easier; we don't need
a special case for adding the parens round sections.

Note [Rebindable if]
~~~~~~~~~~~~~~~~~~~~
The rebindable syntax for 'if' is a bit special, because when
rebindable syntax is *off* we do not want to treat
   (if c then t else e)
as if it was an application (ifThenElse c t e).  Why not?
Because we allow an 'if' to return *unboxed* results, thus
  if blah then 3# else 4#
whereas that would not be possible using a all to a polymorphic function
(because you can't call a polymorphic function at an unboxed type).

So we use Nothing to mean "use the old built-in typing rule".

Note [Record Update HsWrapper]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a wrapper in RecordUpd which is used for the *required*
constraints for pattern synonyms. This wrapper is created in the
typechecking and is then directly used in the desugaring without
modification.

For example, if we have the record pattern synonym P,
  pattern P :: (Show a) => a -> Maybe a
  pattern P{x} = Just x

  foo = (Just True) { x = False }
then `foo` desugars to something like
  foo = case Just True of
          P x -> P False
hence we need to provide the correct dictionaries to P's matcher on
the RHS so that we can build the expression.

Note [Located RdrNames]
~~~~~~~~~~~~~~~~~~~~~~~
A number of syntax elements have seemingly redundant locations attached to them.
This is deliberate, to allow transformations making use of the API Annotations
to easily correlate a Located Name in the RenamedSource with a Located RdrName
in the ParsedSource.

There are unfortunately enough differences between the ParsedSource and the
RenamedSource that the API Annotations cannot be used directly with
RenamedSource, so this allows a simple mapping to be used based on the location.
-}

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsExpr p) where
    ppr expr = pprExpr expr

-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper;
-- the underscore versions do not
pprLExpr :: (OutputableBndrId (GhcPass p)) => LHsExpr (GhcPass p) -> SDoc
pprLExpr (L _ e) = pprExpr e

pprExpr :: (OutputableBndrId (GhcPass p)) => HsExpr (GhcPass p) -> SDoc
pprExpr e | isAtomicHsExpr e || isQuietHsExpr e =            ppr_expr e
          | otherwise                           = pprDeeper (ppr_expr e)

isQuietHsExpr :: HsExpr id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsExpr (HsPar {})        = True
-- applications don't display anything themselves
isQuietHsExpr (HsApp {})        = True
isQuietHsExpr (HsAppType {})    = True
isQuietHsExpr (OpApp {})        = True
isQuietHsExpr _ = False

pprBinds :: (OutputableBndrId (GhcPass idL), OutputableBndrId (GhcPass idR))
         => HsLocalBindsLR (GhcPass idL) (GhcPass idR) -> SDoc
pprBinds b = pprDeeper (ppr b)

-----------------------
ppr_lexpr :: (OutputableBndrId (GhcPass p)) => LHsExpr (GhcPass p) -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)

ppr_expr :: forall p. (OutputableBndrId (GhcPass p))
         => HsExpr (GhcPass p) -> SDoc
ppr_expr (HsVar _ (L _ v))  = pprPrefixOcc v
ppr_expr (HsUnboundVar _ uv)= pprPrefixOcc (unboundVarOcc uv)
ppr_expr (HsConLikeOut _ c) = pprPrefixOcc c
ppr_expr (HsIPVar _ v)      = ppr v
ppr_expr (HsOverLabel _ _ l)= char '#' <> ppr l
ppr_expr (HsLit _ lit)      = ppr lit
ppr_expr (HsOverLit _ lit)  = ppr lit
ppr_expr (HsPar _ e)        = parens (ppr_lexpr e)

ppr_expr (HsCoreAnn _ stc (StringLiteral sta s) e)
  = vcat [pprWithSourceText stc (text "{-# CORE")
          <+> pprWithSourceText sta (doubleQuotes $ ftext s) <+> text "#-}"
         , ppr_lexpr e]

ppr_expr e@(HsApp {})        = ppr_apps e []
ppr_expr e@(HsAppType {})    = ppr_apps e []

ppr_expr (OpApp _ e1 op e2)
  | Just pp_op <- should_print_infix (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly

  where
    should_print_infix (HsVar _ (L _ v)) = Just (pprInfixOcc v)
    should_print_infix (HsConLikeOut _ c)= Just (pprInfixOcc (conLikeName c))
    should_print_infix (HsRecFld _ f)    = Just (pprInfixOcc f)
    should_print_infix (HsUnboundVar _ h@TrueExprHole{})
                                       = Just (pprInfixOcc (unboundVarOcc h))
    should_print_infix (EWildPat _)    = Just (text "`_`")
    should_print_infix (HsWrap _ _ e)  = should_print_infix e
    should_print_infix _               = Nothing

    pp_e1 = pprDebugParendExpr opPrec e1   -- In debug mode, add parens
    pp_e2 = pprDebugParendExpr opPrec e2   -- to make precedence clear

    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])

    pp_infixly pp_op
      = hang pp_e1 2 (sep [pp_op, nest 2 pp_e2])

ppr_expr (NegApp _ e _) = char '-' <+> pprDebugParendExpr appPrec e

ppr_expr (SectionL _ expr op)
  = case unLoc op of
      HsVar _ (L _ v)  -> pp_infixly v
      HsConLikeOut _ c -> pp_infixly (conLikeName c)
      HsUnboundVar _ h@TrueExprHole{}
                       -> pp_infixly (unboundVarOcc h)
      _                -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr opPrec expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
                       4 (hsep [pp_expr, text "x_ )"])

    pp_infixly :: forall a. (OutputableBndr a) => a -> SDoc
    pp_infixly v = (sep [pp_expr, pprInfixOcc v])

ppr_expr (SectionR _ op expr)
  = case unLoc op of
      HsVar _ (L _ v)  -> pp_infixly v
      HsConLikeOut _ c -> pp_infixly (conLikeName c)
      HsUnboundVar _ h@TrueExprHole{}
                       -> pp_infixly (unboundVarOcc h)
      _                -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr opPrec expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, text "x_"])
                       4 (pp_expr <> rparen)

    pp_infixly :: forall a. (OutputableBndr a) => a -> SDoc
    pp_infixly v = sep [pprInfixOcc v, pp_expr]

ppr_expr (ExplicitTuple _ exprs boxity)
  = tupleParens (boxityTupleSort boxity) (fcat (ppr_tup_args $ map unLoc exprs))
  where
    ppr_tup_args []               = []
    ppr_tup_args (Present _ e : es) = (ppr_lexpr e <> punc es) : ppr_tup_args es
    ppr_tup_args (Missing _   : es) = punc es : ppr_tup_args es
    ppr_tup_args (XTupArg x   : es) = (ppr x <> punc es) : ppr_tup_args es

    punc (Present {} : _) = comma <> space
    punc (Missing {} : _) = comma
    punc (XTupArg {} : _) = comma <> space
    punc []               = empty

ppr_expr (ExplicitSum _ alt arity expr)
  = text "(#" <+> ppr_bars (alt - 1) <+> ppr expr <+> ppr_bars (arity - alt) <+> text "#)"
  where
    ppr_bars n = hsep (replicate n (char '|'))

ppr_expr (HsLam _ matches)
  = pprMatches matches

ppr_expr (HsLamCase _ matches)
  = sep [ sep [text "\\case"],
          nest 2 (pprMatches matches) ]

ppr_expr (HsCase _ expr matches@(MG { mg_alts = L _ [_] }))
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches matches) <+> char '}']
ppr_expr (HsCase _ expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of")],
          nest 2 (pprMatches matches) ]

ppr_expr (HsIf _ _ e1 e2 e3)
  = sep [hsep [text "if", nest 2 (ppr e1), ptext (sLit "then")],
         nest 4 (ppr e2),
         text "else",
         nest 4 (ppr e3)]

ppr_expr (HsMultiIf _ alts)
  = hang (text "if") 3  (vcat (map ppr_alt alts))
  where ppr_alt (L _ (GRHS _ guards expr)) =
          hang vbar 2 (ppr_one one_alt)
          where
            ppr_one [] = panic "ppr_exp HsMultiIf"
            ppr_one (h:t) = hang h 2 (sep t)
            one_alt = [ interpp'SP guards
                      , text "->" <+> pprDeeper (ppr expr) ]
        ppr_alt (L _ (XGRHS x)) = ppr x

-- special case: let ... in let ...
ppr_expr (HsLet _ (L _ binds) expr@(L _ (HsLet _ _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lexpr expr]

ppr_expr (HsLet _ (L _ binds) expr)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr expr)]

ppr_expr (HsDo _ do_or_list_comp (L _ stmts)) = pprDo do_or_list_comp stmts

ppr_expr (ExplicitList _ _ exprs)
  = brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (RecordCon { rcon_con_name = con_id, rcon_flds = rbinds })
  = hang (ppr con_id) 2 (ppr rbinds)

ppr_expr (RecordUpd { rupd_expr = L _ aexp, rupd_flds = rbinds })
  = hang (ppr aexp) 2 (braces (fsep (punctuate comma (map ppr rbinds))))

ppr_expr (ExprWithTySig _ expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)

ppr_expr (ArithSeq _ _ info) = brackets (ppr info)

ppr_expr (EWildPat _)     = char '_'
ppr_expr (ELazyPat _ e)   = char '~' <> ppr e
ppr_expr (EAsPat _ (L _ v) e) = pprPrefixOcc v <> char '@' <> ppr e
ppr_expr (EViewPat _ p e) = ppr p <+> text "->" <+> ppr e

ppr_expr (HsSCC _ st (StringLiteral stl lbl) expr)
  = sep [ pprWithSourceText st (text "{-# SCC")
         -- no doublequotes if stl empty, for the case where the SCC was written
         -- without quotes.
          <+> pprWithSourceText stl (ftext lbl) <+> text "#-}",
          ppr expr ]

ppr_expr (HsWrap _ co_fn e)
  = pprHsWrapper co_fn (\parens -> if parens then pprExpr e
                                             else pprExpr e)

ppr_expr (HsSpliceE _ s)         = pprSplice s
ppr_expr (HsBracket _ b)         = pprHsBracket b
ppr_expr (HsRnBracketOut _ e []) = ppr e
ppr_expr (HsRnBracketOut _ e ps) = ppr e $$ text "pending(rn)" <+> ppr ps
ppr_expr (HsTcBracketOut _ e []) = ppr e
ppr_expr (HsTcBracketOut _ e ps) = ppr e $$ text "pending(tc)" <+> ppr ps

ppr_expr (HsProc _ pat (L _ (HsCmdTop _ cmd)))
  = hsep [text "proc", ppr pat, ptext (sLit "->"), ppr cmd]
ppr_expr (HsProc _ pat (L _ (XCmdTop x)))
  = hsep [text "proc", ppr pat, ptext (sLit "->"), ppr x]

ppr_expr (HsStatic _ e)
  = hsep [text "static", ppr e]

ppr_expr (HsTick _ tickish exp)
  = pprTicks (ppr exp) $
    ppr tickish <+> ppr_lexpr exp
ppr_expr (HsBinTick _ tickIdTrue tickIdFalse exp)
  = pprTicks (ppr exp) $
    hcat [text "bintick<",
          ppr tickIdTrue,
          text ",",
          ppr tickIdFalse,
          text ">(",
          ppr exp, text ")"]
ppr_expr (HsTickPragma _ _ externalSrcLoc _ exp)
  = pprTicks (ppr exp) $
    hcat [text "tickpragma<",
          pprExternalSrcLoc externalSrcLoc,
          text ">(",
          ppr exp,
          text ")"]

ppr_expr (HsArrApp _ arrow arg HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_expr (HsArrApp _ arrow arg HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_expr (HsArrApp _ arrow arg HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_expr (HsArrApp _ arrow arg HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_expr (HsArrForm _ (L _ (HsVar _ (L _ v))) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc v, pprCmdArg (unLoc arg2)]]
ppr_expr (HsArrForm _ (L _ (HsConLikeOut _ c)) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc (conLikeName c), pprCmdArg (unLoc arg2)]]
ppr_expr (HsArrForm _ op _ args)
  = hang (text "(|" <+> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <+> text "|)")
ppr_expr (HsRecFld _ f) = ppr f
ppr_expr (XExpr x) = ppr x

ppr_apps :: (OutputableBndrId (GhcPass p))
         => HsExpr (GhcPass p)
         -> [Either (LHsExpr (GhcPass p)) (LHsWcType (NoGhcTc (GhcPass p)))]
         -> SDoc
ppr_apps (HsApp _ (L _ fun) arg)        args
  = ppr_apps fun (Left arg : args)
ppr_apps (HsAppType _ (L _ fun) arg)    args
  = ppr_apps fun (Right arg : args)
ppr_apps fun args = hang (ppr_expr fun) 2 (sep (map pp args))
  where
    pp (Left arg)                             = ppr arg
    -- pp (Right (LHsWcTypeX (HsWC { hswc_body = L _ arg })))
    --   = char '@' <> pprHsType arg
    pp (Right arg)
      = char '@' <> ppr arg

pprExternalSrcLoc :: (StringLiteral,(Int,Int),(Int,Int)) -> SDoc
pprExternalSrcLoc (StringLiteral _ src,(n1,n2),(n3,n4))
  = ppr (src,(n1,n2),(n3,n4))

{-
HsSyn records exactly where the user put parens, with HsPar.
So generally speaking we print without adding any parens.
However, some code is internally generated, and in some places
parens are absolutely required; so for these places we use
pprParendLExpr (but don't print double parens of course).

For operator applications we don't add parens, because the operator
fixities should do the job, except in debug mode (-dppr-debug) so we
can see the structure of the parse tree.
-}

pprDebugParendExpr :: (OutputableBndrId (GhcPass p))
                   => PprPrec -> LHsExpr (GhcPass p) -> SDoc
pprDebugParendExpr p expr
  = getPprStyle (\sty ->
    if debugStyle sty then pprParendLExpr p expr
                      else pprLExpr      expr)

pprParendLExpr :: (OutputableBndrId (GhcPass p))
               => PprPrec -> LHsExpr (GhcPass p) -> SDoc
pprParendLExpr p (L _ e) = pprParendExpr p e

pprParendExpr :: (OutputableBndrId (GhcPass p))
              => PprPrec -> HsExpr (GhcPass p) -> SDoc
pprParendExpr p expr
  | hsExprNeedsParens p expr = parens (pprExpr expr)
  | otherwise                = pprExpr expr
        -- Using pprLExpr makes sure that we go 'deeper'
        -- I think that is usually (always?) right

-- | @'hsExprNeedsParens' p e@ returns 'True' if the expression @e@ needs
-- parentheses under precedence @p@.
hsExprNeedsParens :: PprPrec -> HsExpr p -> Bool
hsExprNeedsParens p = go
  where
    go (HsVar{})                      = False
    go (HsUnboundVar{})               = False
    go (HsConLikeOut{})               = False
    go (HsIPVar{})                    = False
    go (HsOverLabel{})                = False
    go (HsLit _ l)                    = hsLitNeedsParens p l
    go (HsOverLit _ ol)               = hsOverLitNeedsParens p ol
    go (HsPar{})                      = False
    go (HsCoreAnn _ _ _ (L _ e))      = go e
    go (HsApp{})                      = p >= appPrec
    go (HsAppType {})                 = p >= appPrec
    go (OpApp{})                      = p >= opPrec
    go (NegApp{})                     = p > topPrec
    go (SectionL{})                   = True
    go (SectionR{})                   = True
    go (ExplicitTuple{})              = False
    go (ExplicitSum{})                = False
    go (HsLam{})                      = p > topPrec
    go (HsLamCase{})                  = p > topPrec
    go (HsCase{})                     = p > topPrec
    go (HsIf{})                       = p > topPrec
    go (HsMultiIf{})                  = p > topPrec
    go (HsLet{})                      = p > topPrec
    go (HsDo _ sc _)
      | isComprehensionContext sc     = False
      | otherwise                     = p > topPrec
    go (ExplicitList{})               = False
    go (RecordUpd{})                  = False
    go (ExprWithTySig{})              = p >= sigPrec
    go (ArithSeq{})                   = False
    go (EWildPat{})                   = False
    go (ELazyPat{})                   = False
    go (EAsPat{})                     = False
    go (EViewPat{})                   = True
    go (HsSCC{})                      = p >= appPrec
    go (HsWrap _ _ e)                 = go e
    go (HsSpliceE{})                  = False
    go (HsBracket{})                  = False
    go (HsRnBracketOut{})             = False
    go (HsTcBracketOut{})             = False
    go (HsProc{})                     = p > topPrec
    go (HsStatic{})                   = p >= appPrec
    go (HsTick _ _ (L _ e))           = go e
    go (HsBinTick _ _ _ (L _ e))      = go e
    go (HsTickPragma _ _ _ _ (L _ e)) = go e
    go (HsArrApp{})                   = True
    go (HsArrForm{})                  = True
    go (RecordCon{})                  = False
    go (HsRecFld{})                   = False
    go (XExpr{})                      = True

-- | @'parenthesizeHsExpr' p e@ checks if @'hsExprNeedsParens' p e@ is true,
-- and if so, surrounds @e@ with an 'HsPar'. Otherwise, it simply returns @e@.
parenthesizeHsExpr :: PprPrec -> LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
parenthesizeHsExpr p le@(L loc e)
  | hsExprNeedsParens p e = L loc (HsPar NoExt le)
  | otherwise             = le

isAtomicHsExpr :: HsExpr id -> Bool
-- True of a single token
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsConLikeOut {}) = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsUnboundVar {}) = True
isAtomicHsExpr (HsWrap _ _ e)    = isAtomicHsExpr e
isAtomicHsExpr (HsPar _ e)       = isAtomicHsExpr (unLoc e)
isAtomicHsExpr (HsRecFld{})      = True
isAtomicHsExpr _                 = False

{-
************************************************************************
*                                                                      *
\subsection{Commands (in arrow abstractions)}
*                                                                      *
************************************************************************

We re-use HsExpr to represent these.
-}

-- | Located Haskell Command (for arrow syntax)
type LHsCmd id = Located (HsCmd id)

-- | Haskell Command (e.g. a "statement" in an Arrow proc block)
data HsCmd id
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.Annlarrowtail',
  --          'ApiAnnotation.Annrarrowtail','ApiAnnotation.AnnLarrowtail',
  --          'ApiAnnotation.AnnRarrowtail'

  -- For details on above see note [Api annotations] in ApiAnnotation
  = HsCmdArrApp          -- Arrow tail, or arrow application (f -< arg)
        (XCmdArrApp id)  -- type of the arrow expressions f,
                         -- of the form a t t', where arg :: t
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpenB' @'(|'@,
  --         'ApiAnnotation.AnnCloseB' @'|)'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCmdArrForm         -- Command formation,  (| e cmd1 .. cmdn |)
        (XCmdArrForm id)
        (LHsExpr id)     -- The operator.
                         -- After type-checking, a type abstraction to be
                         -- applied to the type of the local environment tuple
        LexicalFixity    -- Whether the operator appeared prefix or infix when
                         -- parsed.
        (Maybe Fixity)   -- fixity (filled in by the renamer), for forms that
                         -- were converted from OpApp's by the renamer
        [LHsCmdTop id]   -- argument commands

  | HsCmdApp    (XCmdApp id)
                (LHsCmd id)
                (LHsExpr id)

  | HsCmdLam    (XCmdLam id)
                (MatchGroup id (LHsCmd id))     -- kappa
       -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --       'ApiAnnotation.AnnRarrow',

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdPar    (XCmdPar id)
                (LHsCmd id)                     -- parenthesised command
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --             'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdCase   (XCmdCase id)
                (LHsExpr id)
                (MatchGroup id (LHsCmd id))     -- bodies are HsCmd's
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnCase',
    --       'ApiAnnotation.AnnOf','ApiAnnotation.AnnOpen' @'{'@,
    --       'ApiAnnotation.AnnClose' @'}'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdIf     (XCmdIf id)
                (Maybe (SyntaxExpr id))         -- cond function
                (LHsExpr id)                    -- predicate
                (LHsCmd id)                     -- then part
                (LHsCmd id)                     -- else part
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf',
    --       'ApiAnnotation.AnnSemi',
    --       'ApiAnnotation.AnnThen','ApiAnnotation.AnnSemi',
    --       'ApiAnnotation.AnnElse',

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdLet    (XCmdLet id)
                (LHsLocalBinds id)      -- let(rec)
                (LHsCmd  id)
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet',
    --       'ApiAnnotation.AnnOpen' @'{'@,
    --       'ApiAnnotation.AnnClose' @'}'@,'ApiAnnotation.AnnIn'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdDo     (XCmdDo id)                     -- Type of the whole expression
                (Located [CmdLStmt id])
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDo',
    --             'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnSemi',
    --             'ApiAnnotation.AnnVbar',
    --             'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdWrap   (XCmdWrap id)
                HsWrapper
                (HsCmd id)     -- If   cmd :: arg1 --> res
                               --      wrap :: arg1 "->" arg2
                               -- Then (HsCmdWrap wrap cmd) :: arg2 --> res
  | XCmd        (XXCmd id)     -- Note [Trees that Grow] extension point

type instance XCmdArrApp  GhcPs = NoExt
type instance XCmdArrApp  GhcRn = NoExt
type instance XCmdArrApp  GhcTc = Type

type instance XCmdArrForm (GhcPass _) = NoExt
type instance XCmdApp     (GhcPass _) = NoExt
type instance XCmdLam     (GhcPass _) = NoExt
type instance XCmdPar     (GhcPass _) = NoExt
type instance XCmdCase    (GhcPass _) = NoExt
type instance XCmdIf      (GhcPass _) = NoExt
type instance XCmdLet     (GhcPass _) = NoExt

type instance XCmdDo      GhcPs = NoExt
type instance XCmdDo      GhcRn = NoExt
type instance XCmdDo      GhcTc = Type

type instance XCmdWrap    (GhcPass _) = NoExt
type instance XXCmd       (GhcPass _) = NoExt

-- | Haskell Array Application Type
data HsArrAppType = HsHigherOrderApp | HsFirstOrderApp
  deriving Data


{- | Top-level command, introducing a new arrow.
This may occur inside a proc (where the stack is empty) or as an
argument of a command-forming operator.
-}

-- | Located Haskell Top-level Command
type LHsCmdTop p = Located (HsCmdTop p)

-- | Haskell Top-level Command
data HsCmdTop p
  = HsCmdTop (XCmdTop p)
             (LHsCmd p)
  | XCmdTop (XXCmdTop p)        -- Note [Trees that Grow] extension point

data CmdTopTc
  = CmdTopTc Type    -- Nested tuple of inputs on the command's stack
             Type    -- return type of the command
             (CmdSyntaxTable GhcTc) -- See Note [CmdSyntaxTable]

type instance XCmdTop  GhcPs = NoExt
type instance XCmdTop  GhcRn = CmdSyntaxTable GhcRn -- See Note [CmdSyntaxTable]
type instance XCmdTop  GhcTc = CmdTopTc

type instance XXCmdTop (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsCmd p) where
    ppr cmd = pprCmd cmd

-----------------------
-- pprCmd and pprLCmd call pprDeeper;
-- the underscore versions do not
pprLCmd :: (OutputableBndrId (GhcPass p)) => LHsCmd (GhcPass p) -> SDoc
pprLCmd (L _ c) = pprCmd c

pprCmd :: (OutputableBndrId (GhcPass p)) => HsCmd (GhcPass p) -> SDoc
pprCmd c | isQuietHsCmd c =            ppr_cmd c
         | otherwise      = pprDeeper (ppr_cmd c)

isQuietHsCmd :: HsCmd id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsCmd (HsCmdPar {}) = True
-- applications don't display anything themselves
isQuietHsCmd (HsCmdApp {}) = True
isQuietHsCmd _ = False

-----------------------
ppr_lcmd :: (OutputableBndrId (GhcPass p)) => LHsCmd (GhcPass p) -> SDoc
ppr_lcmd c = ppr_cmd (unLoc c)

ppr_cmd :: forall p. (OutputableBndrId (GhcPass p)) => HsCmd (GhcPass p) -> SDoc
ppr_cmd (HsCmdPar _ c) = parens (ppr_lcmd c)

ppr_cmd (HsCmdApp _ c e)
  = let (fun, args) = collect_args c [e] in
    hang (ppr_lcmd fun) 2 (sep (map ppr args))
  where
    collect_args (L _ (HsCmdApp _ fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)

ppr_cmd (HsCmdLam _ matches)
  = pprMatches matches

ppr_cmd (HsCmdCase _ expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of")],
          nest 2 (pprMatches matches) ]

ppr_cmd (HsCmdIf _ _ e ct ce)
  = sep [hsep [text "if", nest 2 (ppr e), ptext (sLit "then")],
         nest 4 (ppr ct),
         text "else",
         nest 4 (ppr ce)]

-- special case: let ... in let ...
ppr_cmd (HsCmdLet _ (L _ binds) cmd@(L _ (HsCmdLet {})))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lcmd cmd]

ppr_cmd (HsCmdLet _ (L _ binds) cmd)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr cmd)]

ppr_cmd (HsCmdDo _ (L _ stmts))  = pprDo ArrowExpr stmts

ppr_cmd (HsCmdWrap _ w cmd)
  = pprHsWrapper w (\_ -> parens (ppr_cmd cmd))
ppr_cmd (HsCmdArrApp _ arrow arg HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp _ arrow arg HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_cmd (HsCmdArrApp _ arrow arg HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp _ arrow arg HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_cmd (HsCmdArrForm _ (L _ (HsVar _ (L _ v))) _ (Just _) [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc v
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm _ (L _ (HsVar _ (L _ v))) Infix _    [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc v
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm _ (L _ (HsConLikeOut _ c)) _ (Just _) [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc (conLikeName c)
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm _ (L _ (HsConLikeOut _ c)) Infix _    [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc (conLikeName c)
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm _ op _ _ args)
  = hang (text "(|" <> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <> text "|)")
ppr_cmd (XCmd x) = ppr x

pprCmdArg :: (OutputableBndrId (GhcPass p)) => HsCmdTop (GhcPass p) -> SDoc
pprCmdArg (HsCmdTop _ cmd)
  = ppr_lcmd cmd
pprCmdArg (XCmdTop x) = ppr x

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsCmdTop p) where
    ppr = pprCmdArg

{-
************************************************************************
*                                                                      *
\subsection{Record binds}
*                                                                      *
************************************************************************
-}

-- | Haskell Record Bindings
type HsRecordBinds p = HsRecFields p (LHsExpr p)

{-
************************************************************************
*                                                                      *
\subsection{@Match@, @GRHSs@, and @GRHS@ datatypes}
*                                                                      *
************************************************************************

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
-}

data MatchGroup p body
  = MG { mg_ext     :: XMG p body -- Posr typechecker, types of args and result
       , mg_alts    :: Located [LMatch p body]  -- The alternatives
       , mg_origin  :: Origin }
     -- The type is the type of the entire group
     --      t1 -> ... -> tn -> tr
     -- where there are n patterns
  | XMatchGroup (XXMatchGroup p body)

data MatchGroupTc
  = MatchGroupTc
       { mg_arg_tys :: [Type]  -- Types of the arguments, t1..tn
       , mg_res_ty  :: Type    -- Type of the result, tr
       } deriving Data

type instance XMG         GhcPs b = NoExt
type instance XMG         GhcRn b = NoExt
type instance XMG         GhcTc b = MatchGroupTc

type instance XXMatchGroup (GhcPass _) b = NoExt

-- | Located Match
type LMatch id body = Located (Match id body)
-- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when in a
--   list

-- For details on above see note [Api annotations] in ApiAnnotation
data Match p body
  = Match {
        m_ext :: XCMatch p body,
        m_ctxt :: HsMatchContext (NameOrRdrName (IdP p)),
          -- See note [m_ctxt in Match]
        m_pats :: [LPat p], -- The patterns
        m_grhss :: (GRHSs p body)
  }
  | XMatch (XXMatch p body)

type instance XCMatch (GhcPass _) b = NoExt
type instance XXMatch (GhcPass _) b = NoExt

instance (idR ~ GhcPass pr, OutputableBndrId idR, Outputable body)
            => Outputable (Match idR body) where
  ppr = pprMatch

{-
Note [m_ctxt in Match]
~~~~~~~~~~~~~~~~~~~~~~

A Match can occur in a number of contexts, such as a FunBind, HsCase, HsLam and
so on.

In order to simplify tooling processing and pretty print output, the provenance
is captured in an HsMatchContext.

This is particularly important for the API Annotations for a multi-equation
FunBind.

The parser initially creates a FunBind with a single Match in it for
every function definition it sees.

These are then grouped together by getMonoBind into a single FunBind,
where all the Matches are combined.

In the process, all the original FunBind fun_id's bar one are
discarded, including the locations.

This causes a problem for source to source conversions via API
Annotations, so the original fun_ids and infix flags are preserved in
the Match, when it originates from a FunBind.

Example infix function definition requiring individual API Annotations

    (&&&  ) [] [] =  []
    xs    &&&   [] =  xs
    (  &&&  ) [] ys =  ys



-}


isInfixMatch :: Match id body -> Bool
isInfixMatch match = case m_ctxt match of
  FunRhs {mc_fixity = Infix} -> True
  _                          -> False

isEmptyMatchGroup :: MatchGroup id body -> Bool
isEmptyMatchGroup (MG { mg_alts = ms }) = null $ unLoc ms
isEmptyMatchGroup (XMatchGroup{}) = panic "isEmptyMatchGroup"

-- | Is there only one RHS in this list of matches?
isSingletonMatchGroup :: [LMatch id body] -> Bool
isSingletonMatchGroup matches
  | [L _ match] <- matches
  , Match { m_grhss = GRHSs { grhssGRHSs = [_] } } <- match
  = True
  | otherwise
  = False

matchGroupArity :: MatchGroup id body -> Arity
-- Precondition: MatchGroup is non-empty
-- This is called before type checking, when mg_arg_tys is not set
matchGroupArity (MG { mg_alts = alts })
  | L _ (alt1:_) <- alts = length (hsLMatchPats alt1)
  | otherwise        = panic "matchGroupArity"
matchGroupArity (XMatchGroup{}) = panic "matchGroupArity"

hsLMatchPats :: LMatch id body -> [LPat id]
hsLMatchPats (L _ (Match { m_pats = pats })) = pats
hsLMatchPats (L _ (XMatch _)) = panic "hsLMatchPats"

-- | Guarded Right-Hand Sides
--
-- GRHSs are used both for pattern bindings and for Matches
--
--  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVbar',
--        'ApiAnnotation.AnnEqual','ApiAnnotation.AnnWhere',
--        'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'
--        'ApiAnnotation.AnnRarrow','ApiAnnotation.AnnSemi'

-- For details on above see note [Api annotations] in ApiAnnotation
data GRHSs p body
  = GRHSs {
      grhssExt :: XCGRHSs p body,
      grhssGRHSs :: [LGRHS p body],      -- ^ Guarded RHSs
      grhssLocalBinds :: LHsLocalBinds p -- ^ The where clause
    }
  | XGRHSs (XXGRHSs p body)

type instance XCGRHSs (GhcPass _) b = NoExt
type instance XXGRHSs (GhcPass _) b = NoExt

-- | Located Guarded Right-Hand Side
type LGRHS id body = Located (GRHS id body)

-- | Guarded Right Hand Side.
data GRHS p body = GRHS (XCGRHS p body)
                        [GuardLStmt p] -- Guards
                        body           -- Right hand side
                  | XGRHS (XXGRHS p body)

type instance XCGRHS (GhcPass _) b = NoExt
type instance XXGRHS (GhcPass _) b = NoExt

-- We know the list must have at least one @Match@ in it.

pprMatches :: (OutputableBndrId (GhcPass idR), Outputable body)
           => MatchGroup (GhcPass idR) body -> SDoc
pprMatches MG { mg_alts = matches }
    = vcat (map pprMatch (map unLoc (unLoc matches)))
      -- Don't print the type; it's only a place-holder before typechecking
pprMatches (XMatchGroup x) = ppr x

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprFunBind :: (OutputableBndrId (GhcPass idR), Outputable body)
           => MatchGroup (GhcPass idR) body -> SDoc
pprFunBind matches = pprMatches matches

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprPatBind :: forall bndr p body. (OutputableBndrId (GhcPass bndr),
                                   OutputableBndrId (GhcPass p),
                                   Outputable body)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) body -> SDoc
pprPatBind pat (grhss)
 = sep [ppr pat,
       nest 2 (pprGRHSs (PatBindRhs :: HsMatchContext (IdP (GhcPass p))) grhss)]

pprMatch :: (OutputableBndrId (GhcPass idR), Outputable body)
         => Match (GhcPass idR) body -> SDoc
pprMatch match
  = sep [ sep (herald : map (nest 2 . pprParendLPat appPrec) other_pats)
        , nest 2 (pprGRHSs ctxt (m_grhss match)) ]
  where
    ctxt = m_ctxt match
    (herald, other_pats)
        = case ctxt of
            FunRhs {mc_fun=L _ fun, mc_fixity=fixity, mc_strictness=strictness}
                | strictness == SrcStrict -> ASSERT(null $ m_pats match)
                                             (char '!'<>pprPrefixOcc fun, m_pats match)
                        -- a strict variable binding
                | fixity == Prefix -> (pprPrefixOcc fun, m_pats match)
                        -- f x y z = e
                        -- Not pprBndr; the AbsBinds will
                        -- have printed the signature

                | null pats2 -> (pp_infix, [])
                        -- x &&& y = e

                | otherwise -> (parens pp_infix, pats2)
                        -- (x &&& y) z = e
                where
                  pp_infix = pprParendLPat opPrec pat1
                         <+> pprInfixOcc fun
                         <+> pprParendLPat opPrec pat2

            LambdaExpr -> (char '\\', m_pats match)

            _  -> if null (m_pats match)
                     then (empty, [])
                     else ASSERT2( null pats1, ppr ctxt $$ ppr pat1 $$ ppr pats1 )
                          (ppr pat1, [])        -- No parens around the single pat

    (pat1:pats1) = m_pats match
    (pat2:pats2) = pats1

pprGRHSs :: (OutputableBndrId (GhcPass idR), Outputable body)
         => HsMatchContext idL -> GRHSs (GhcPass idR) body -> SDoc
pprGRHSs ctxt (GRHSs _ grhss (L _ binds))
  = vcat (map (pprGRHS ctxt . unLoc) grhss)
  -- Print the "where" even if the contents of the binds is empty. Only
  -- EmptyLocalBinds means no "where" keyword
 $$ ppUnless (eqEmptyLocalBinds binds)
      (text "where" $$ nest 4 (pprBinds binds))
pprGRHSs _ (XGRHSs x) = ppr x

pprGRHS :: (OutputableBndrId (GhcPass idR), Outputable body)
        => HsMatchContext idL -> GRHS (GhcPass idR) body -> SDoc
pprGRHS ctxt (GRHS _ [] body)
 =  pp_rhs ctxt body

pprGRHS ctxt (GRHS _ guards body)
 = sep [vbar <+> interpp'SP guards, pp_rhs ctxt body]

pprGRHS _ (XGRHS x) = ppr x

pp_rhs :: Outputable body => HsMatchContext idL -> body -> SDoc
pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)

{-
************************************************************************
*                                                                      *
\subsection{Do stmts and list comprehensions}
*                                                                      *
************************************************************************
-}

-- | Located @do@ block Statement
type LStmt id body = Located (StmtLR id id body)

-- | Located Statement with separate Left and Right id's
type LStmtLR idL idR body = Located (StmtLR idL idR body)

-- | @do@ block Statement
type Stmt id body = StmtLR id id body

-- | Command Located Statement
type CmdLStmt   id = LStmt id (LHsCmd  id)

-- | Command Statement
type CmdStmt    id = Stmt  id (LHsCmd  id)

-- | Expression Located Statement
type ExprLStmt  id = LStmt id (LHsExpr id)

-- | Expression Statement
type ExprStmt   id = Stmt  id (LHsExpr id)

-- | Guard Located Statement
type GuardLStmt id = LStmt id (LHsExpr id)

-- | Guard Statement
type GuardStmt  id = Stmt  id (LHsExpr id)

-- | Ghci Located Statement
type GhciLStmt  id = LStmt id (LHsExpr id)

-- | Ghci Statement
type GhciStmt   id = Stmt  id (LHsExpr id)

-- The SyntaxExprs in here are used *only* for do-notation and monad
-- comprehensions, which have rebindable syntax. Otherwise they are unused.
-- | API Annotations when in qualifier lists or guards
--  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVbar',
--         'ApiAnnotation.AnnComma','ApiAnnotation.AnnThen',
--         'ApiAnnotation.AnnBy','ApiAnnotation.AnnBy',
--         'ApiAnnotation.AnnGroup','ApiAnnotation.AnnUsing'

-- For details on above see note [Api annotations] in ApiAnnotation
data StmtLR idL idR body -- body should always be (LHs**** idR)
  = LastStmt  -- Always the last Stmt in ListComp, MonadComp,
              -- and (after the renamer, see RnExpr.checkLastStmt) DoExpr, MDoExpr
              -- Not used for GhciStmtCtxt, PatGuard, which scope over other stuff
          (XLastStmt idL idR body)
          body
          Bool               -- True <=> return was stripped by ApplicativeDo
          (SyntaxExpr idR)   -- The return operator
            -- The return operator is used only for MonadComp
            -- For ListComp we use the baked-in 'return'
            -- For DoExpr, MDoExpr, we don't apply a 'return' at all
            -- See Note [Monad Comprehensions]
            -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | BindStmt (XBindStmt idL idR body) -- Post typechecking,
                                -- result type of the function passed to bind;
                                -- that is, S in (>>=) :: Q -> (R -> S) -> T
             (LPat idL)
             body
             (SyntaxExpr idR) -- The (>>=) operator; see Note [The type of bind in Stmts]
             (SyntaxExpr idR) -- The fail operator
             -- The fail operator is noSyntaxExpr
             -- if the pattern match can't fail

  -- | 'ApplicativeStmt' represents an applicative expression built with
  -- '<$>' and '<*>'.  It is generated by the renamer, and is desugared into the
  -- appropriate applicative expression by the desugarer, but it is intended
  -- to be invisible in error messages.
  --
  -- For full details, see Note [ApplicativeDo] in RnExpr
  --
  | ApplicativeStmt
             (XApplicativeStmt idL idR body) -- Post typecheck, Type of the body
             [ ( SyntaxExpr idR
               , ApplicativeArg idL) ]
                      -- [(<$>, e1), (<*>, e2), ..., (<*>, en)]
             (Maybe (SyntaxExpr idR))  -- 'join', if necessary

  | BodyStmt (XBodyStmt idL idR body) -- Post typecheck, element type
                                      -- of the RHS (used for arrows)
             body              -- See Note [BodyStmt]
             (SyntaxExpr idR)  -- The (>>) operator
             (SyntaxExpr idR)  -- The `guard` operator; used only in MonadComp
                               -- See notes [Monad Comprehensions]

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet'
  --          'ApiAnnotation.AnnOpen' @'{'@,'ApiAnnotation.AnnClose' @'}'@,

  -- For details on above see note [Api annotations] in ApiAnnotation
  | LetStmt  (XLetStmt idL idR body) (LHsLocalBindsLR idL idR)

  -- ParStmts only occur in a list/monad comprehension
  | ParStmt  (XParStmt idL idR body)    -- Post typecheck,
                                        -- S in (>>=) :: Q -> (R -> S) -> T
             [ParStmtBlock idL idR]
             (HsExpr idR)               -- Polymorphic `mzip` for monad comprehensions
             (SyntaxExpr idR)           -- The `>>=` operator
                                        -- See notes [Monad Comprehensions]
            -- After renaming, the ids are the binders
            -- bound by the stmts and used after themp

  | TransStmt {
      trS_ext   :: XTransStmt idL idR body, -- Post typecheck,
                                            -- R in (>>=) :: Q -> (R -> S) -> T
      trS_form  :: TransForm,
      trS_stmts :: [ExprLStmt idL],   -- Stmts to the *left* of the 'group'
                                      -- which generates the tuples to be grouped

      trS_bndrs :: [(IdP idR, IdP idR)], -- See Note [TransStmt binder map]

      trS_using :: LHsExpr idR,
      trS_by :: Maybe (LHsExpr idR),  -- "by e" (optional)
        -- Invariant: if trS_form = GroupBy, then grp_by = Just e

      trS_ret :: SyntaxExpr idR,      -- The monomorphic 'return' function for
                                      -- the inner monad comprehensions
      trS_bind :: SyntaxExpr idR,     -- The '(>>=)' operator
      trS_fmap :: HsExpr idR          -- The polymorphic 'fmap' function for desugaring
                                      -- Only for 'group' forms
                                      -- Just a simple HsExpr, because it's
                                      -- too polymorphic for tcSyntaxOp
    }                                 -- See Note [Monad Comprehensions]

  -- Recursive statement (see Note [How RecStmt works] below)
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRec'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecStmt
     { recS_ext :: XRecStmt idL idR body
     , recS_stmts :: [LStmtLR idL idR body]

        -- The next two fields are only valid after renaming
     , recS_later_ids :: [IdP idR]
                         -- The ids are a subset of the variables bound by the
                         -- stmts that are used in stmts that follow the RecStmt

     , recS_rec_ids :: [IdP idR]
                         -- Ditto, but these variables are the "recursive" ones,
                         -- that are used before they are bound in the stmts of
                         -- the RecStmt.
        -- An Id can be in both groups
        -- Both sets of Ids are (now) treated monomorphically
        -- See Note [How RecStmt works] for why they are separate

        -- Rebindable syntax
     , recS_bind_fn :: SyntaxExpr idR -- The bind function
     , recS_ret_fn  :: SyntaxExpr idR -- The return function
     , recS_mfix_fn :: SyntaxExpr idR -- The mfix function
      }
  | XStmtLR (XXStmtLR idL idR body)

-- Extra fields available post typechecking for RecStmt.
data RecStmtTc =
  RecStmtTc
     { recS_bind_ty :: Type       -- S in (>>=) :: Q -> (R -> S) -> T
     , recS_later_rets :: [PostTcExpr] -- (only used in the arrow version)
     , recS_rec_rets :: [PostTcExpr] -- These expressions correspond 1-to-1
                                  -- with recS_later_ids and recS_rec_ids,
                                  -- and are the expressions that should be
                                  -- returned by the recursion.
                                  -- They may not quite be the Ids themselves,
                                  -- because the Id may be *polymorphic*, but
                                  -- the returned thing has to be *monomorphic*,
                                  -- so they may be type applications

      , recS_ret_ty :: Type        -- The type of
                                   -- do { stmts; return (a,b,c) }
                                   -- With rebindable syntax the type might not
                                   -- be quite as simple as (m (tya, tyb, tyc)).
      }


type instance XLastStmt        (GhcPass _) (GhcPass _) b = NoExt

type instance XBindStmt        (GhcPass _) GhcPs b = NoExt
type instance XBindStmt        (GhcPass _) GhcRn b = NoExt
type instance XBindStmt        (GhcPass _) GhcTc b = Type

type instance XApplicativeStmt (GhcPass _) GhcPs b = NoExt
type instance XApplicativeStmt (GhcPass _) GhcRn b = NoExt
type instance XApplicativeStmt (GhcPass _) GhcTc b = Type

type instance XBodyStmt        (GhcPass _) GhcPs b = NoExt
type instance XBodyStmt        (GhcPass _) GhcRn b = NoExt
type instance XBodyStmt        (GhcPass _) GhcTc b = Type

type instance XLetStmt         (GhcPass _) (GhcPass _) b = NoExt

type instance XParStmt         (GhcPass _) GhcPs b = NoExt
type instance XParStmt         (GhcPass _) GhcRn b = NoExt
type instance XParStmt         (GhcPass _) GhcTc b = Type

type instance XTransStmt       (GhcPass _) GhcPs b = NoExt
type instance XTransStmt       (GhcPass _) GhcRn b = NoExt
type instance XTransStmt       (GhcPass _) GhcTc b = Type

type instance XRecStmt         (GhcPass _) GhcPs b = NoExt
type instance XRecStmt         (GhcPass _) GhcRn b = NoExt
type instance XRecStmt         (GhcPass _) GhcTc b = RecStmtTc

type instance XXStmtLR         (GhcPass _) (GhcPass _) b = NoExt

data TransForm   -- The 'f' below is the 'using' function, 'e' is the by function
  = ThenForm     -- then f               or    then f by e             (depending on trS_by)
  | GroupForm    -- then group using f   or    then group by e using f (depending on trS_by)
  deriving Data

-- | Parenthesised Statement Block
data ParStmtBlock idL idR
  = ParStmtBlock
        (XParStmtBlock idL idR)
        [ExprLStmt idL]
        [IdP idR]          -- The variables to be returned
        (SyntaxExpr idR)   -- The return operator
  | XParStmtBlock (XXParStmtBlock idL idR)

type instance XParStmtBlock  (GhcPass pL) (GhcPass pR) = NoExt
type instance XXParStmtBlock (GhcPass pL) (GhcPass pR) = NoExt

-- | Applicative Argument
data ApplicativeArg idL
  = ApplicativeArgOne      -- A single statement (BindStmt or BodyStmt)
      (XApplicativeArgOne idL)
      (LPat idL)           -- WildPat if it was a BodyStmt (see below)
      (LHsExpr idL)
      Bool                 -- True <=> was a BodyStmt
                           -- False <=> was a BindStmt
                           -- See Note [Applicative BodyStmt]

  | ApplicativeArgMany     -- do { stmts; return vars }
      (XApplicativeArgMany idL)
      [ExprLStmt idL]      -- stmts
      (HsExpr idL)         -- return (v1,..,vn), or just (v1,..,vn)
      (LPat idL)           -- (v1,...,vn)
  | XApplicativeArg (XXApplicativeArg idL)

type instance XApplicativeArgOne  (GhcPass _) = NoExt
type instance XApplicativeArgMany (GhcPass _) = NoExt
type instance XXApplicativeArg    (GhcPass _) = NoExt

{-
Note [The type of bind in Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some Stmts, notably BindStmt, keep the (>>=) bind operator.
We do NOT assume that it has type
    (>>=) :: m a -> (a -> m b) -> m b
In some cases (see Trac #303, #1537) it might have a more
exotic type, such as
    (>>=) :: m i j a -> (a -> m j k b) -> m i k b
So we must be careful not to make assumptions about the type.
In particular, the monad may not be uniform throughout.

Note [TransStmt binder map]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The [(idR,idR)] in a TransStmt behaves as follows:

  * Before renaming: []

  * After renaming:
          [ (x27,x27), ..., (z35,z35) ]
    These are the variables
       bound by the stmts to the left of the 'group'
       and used either in the 'by' clause,
                or     in the stmts following the 'group'
    Each item is a pair of identical variables.

  * After typechecking:
          [ (x27:Int, x27:[Int]), ..., (z35:Bool, z35:[Bool]) ]
    Each pair has the same unique, but different *types*.

Note [BodyStmt]
~~~~~~~~~~~~~~~
BodyStmts are a bit tricky, because what they mean
depends on the context.  Consider the following contexts:

        A do expression of type (m res_ty)
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E any_ty:   do { ....; E; ... }
                E :: m any_ty
          Translation: E >> ...

        A list comprehensions of type [elt_ty]
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E Bool:   [ .. | .... E ]
                        [ .. | ..., E, ... ]
                        [ .. | .... | ..., E | ... ]
                E :: Bool
          Translation: if E then fail else ...

        A guard list, guarding a RHS of type rhs_ty
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E BooParStmtBlockl:   f x | ..., E, ... = ...rhs...
                E :: Bool
          Translation: if E then fail else ...

        A monad comprehension of type (m res_ty)
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E Bool:   [ .. | .... E ]
                E :: Bool
          Translation: guard E >> ...

Array comprehensions are handled like list comprehensions.

Note [How RecStmt works]
~~~~~~~~~~~~~~~~~~~~~~~~
Example:
   HsDo [ BindStmt x ex

        , RecStmt { recS_rec_ids   = [a, c]
                  , recS_stmts     = [ BindStmt b (return (a,c))
                                     , LetStmt a = ...b...
                                     , BindStmt c ec ]
                  , recS_later_ids = [a, b]

        , return (a b) ]

Here, the RecStmt binds a,b,c; but
  - Only a,b are used in the stmts *following* the RecStmt,
  - Only a,c are used in the stmts *inside* the RecStmt
        *before* their bindings

Why do we need *both* rec_ids and later_ids?  For monads they could be
combined into a single set of variables, but not for arrows.  That
follows from the types of the respective feedback operators:

        mfix :: MonadFix m => (a -> m a) -> m a
        loop :: ArrowLoop a => a (b,d) (c,d) -> a b c

* For mfix, the 'a' covers the union of the later_ids and the rec_ids
* For 'loop', 'c' is the later_ids and 'd' is the rec_ids

Note [Typing a RecStmt]
~~~~~~~~~~~~~~~~~~~~~~~
A (RecStmt stmts) types as if you had written

  (v1,..,vn, _, ..., _) <- mfix (\~(_, ..., _, r1, ..., rm) ->
                                 do { stmts
                                    ; return (v1,..vn, r1, ..., rm) })

where v1..vn are the later_ids
      r1..rm are the rec_ids

Note [Monad Comprehensions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Monad comprehensions require separate functions like 'return' and
'>>=' for desugaring. These functions are stored in the statements
used in monad comprehensions. For example, the 'return' of the 'LastStmt'
expression is used to lift the body of the monad comprehension:

  [ body | stmts ]
   =>
  stmts >>= \bndrs -> return body

In transform and grouping statements ('then ..' and 'then group ..') the
'return' function is required for nested monad comprehensions, for example:

  [ body | stmts, then f, rest ]
   =>
  f [ env | stmts ] >>= \bndrs -> [ body | rest ]

BodyStmts require the 'Control.Monad.guard' function for boolean
expressions:

  [ body | exp, stmts ]
   =>
  guard exp >> [ body | stmts ]

Parallel statements require the 'Control.Monad.Zip.mzip' function:

  [ body | stmts1 | stmts2 | .. ]
   =>
  mzip stmts1 (mzip stmts2 (..)) >>= \(bndrs1, (bndrs2, ..)) -> return body

In any other context than 'MonadComp', the fields for most of these
'SyntaxExpr's stay bottom.


Note [Applicative BodyStmt]

(#12143) For the purposes of ApplicativeDo, we treat any BodyStmt
as if it was a BindStmt with a wildcard pattern.  For example,

  do
    x <- A
    B
    return x

is transformed as if it were

  do
    x <- A
    _ <- B
    return x

so it transforms to

  (\(x,_) -> x) <$> A <*> B

But we have to remember when we treat a BodyStmt like a BindStmt,
because in error messages we want to emit the original syntax the user
wrote, not our internal representation.  So ApplicativeArgOne has a
Bool flag that is True when the original statement was a BodyStmt, so
that we can pretty-print it correctly.
-}

instance (Outputable (StmtLR idL idL (LHsExpr idL)),
          Outputable (XXParStmtBlock idL idR))
        => Outputable (ParStmtBlock idL idR) where
  ppr (ParStmtBlock _ stmts _ _) = interpp'SP stmts
  ppr (XParStmtBlock x)          = ppr x

instance (idL ~ GhcPass pl,idR ~ GhcPass pr,
          OutputableBndrId idL, OutputableBndrId idR,
          Outputable body)
         => Outputable (StmtLR idL idR body) where
    ppr stmt = pprStmt stmt

pprStmt :: forall idL idR body . (OutputableBndrId (GhcPass idL),
                                  OutputableBndrId (GhcPass idR),
                                  Outputable body)
        => (StmtLR (GhcPass idL) (GhcPass idR) body) -> SDoc
pprStmt (LastStmt _ expr ret_stripped _)
  = whenPprDebug (text "[last]") <+>
       (if ret_stripped then text "return" else empty) <+>
       ppr expr
pprStmt (BindStmt _ pat expr _ _) = hsep [ppr pat, larrow, ppr expr]
pprStmt (LetStmt _ (L _ binds))   = hsep [text "let", pprBinds binds]
pprStmt (BodyStmt _ expr _ _)     = ppr expr
pprStmt (ParStmt _ stmtss _ _)   = sep (punctuate (text " | ") (map ppr stmtss))

pprStmt (TransStmt { trS_stmts = stmts, trS_by = by
                   , trS_using = using, trS_form = form })
  = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])

pprStmt (RecStmt { recS_stmts = segment, recS_rec_ids = rec_ids
                 , recS_later_ids = later_ids })
  = text "rec" <+>
    vcat [ ppr_do_stmts segment
         , whenPprDebug (vcat [ text "rec_ids=" <> ppr rec_ids
                            , text "later_ids=" <> ppr later_ids])]

pprStmt (ApplicativeStmt _ args mb_join)
  = getPprStyle $ \style ->
      if userStyle style
         then pp_for_user
         else pp_debug
  where
  -- make all the Applicative stuff invisible in error messages by
  -- flattening the whole ApplicativeStmt nest back to a sequence
  -- of statements.
   pp_for_user = vcat $ concatMap flattenArg args

   -- ppr directly rather than transforming here, because we need to
   -- inject a "return" which is hard when we're polymorphic in the id
   -- type.
   flattenStmt :: ExprLStmt (GhcPass idL) -> [SDoc]
   flattenStmt (L _ (ApplicativeStmt _ args _)) = concatMap flattenArg args
   flattenStmt stmt = [ppr stmt]

   flattenArg :: forall a . (a, ApplicativeArg (GhcPass idL)) -> [SDoc]
   flattenArg (_, ApplicativeArgOne _ pat expr isBody)
     | isBody =  -- See Note [Applicative BodyStmt]
     [ppr (BodyStmt (panic "pprStmt") expr noSyntaxExpr noSyntaxExpr
             :: ExprStmt (GhcPass idL))]
     | otherwise =
     [ppr (BindStmt (panic "pprStmt") pat expr noSyntaxExpr noSyntaxExpr
             :: ExprStmt (GhcPass idL))]
   flattenArg (_, ApplicativeArgMany _ stmts _ _) =
     concatMap flattenStmt stmts
   flattenArg (_, XApplicativeArg _) = panic "flattenArg"

   pp_debug =
     let
         ap_expr = sep (punctuate (text " |") (map pp_arg args))
     in
       if isNothing mb_join
          then ap_expr
          else text "join" <+> parens ap_expr

   pp_arg :: (a, ApplicativeArg (GhcPass idL)) -> SDoc
   pp_arg (_, ApplicativeArgOne _ pat expr isBody)
     | isBody =  -- See Note [Applicative BodyStmt]
     ppr (BodyStmt (panic "pprStmt") expr noSyntaxExpr noSyntaxExpr
            :: ExprStmt (GhcPass idL))
     | otherwise =
     ppr (BindStmt (panic "pprStmt") pat expr noSyntaxExpr noSyntaxExpr
            :: ExprStmt (GhcPass idL))
   pp_arg (_, ApplicativeArgMany _ stmts return pat) =
     ppr pat <+>
     text "<-" <+>
     ppr (HsDo (panic "pprStmt") DoExpr (noLoc
               (stmts ++
                   [noLoc (LastStmt noExt (noLoc return) False noSyntaxExpr)])))
   pp_arg (_, XApplicativeArg x) = ppr x

pprStmt (XStmtLR x) = ppr x

pprTransformStmt :: (OutputableBndrId (GhcPass p))
                 => [IdP (GhcPass p)] -> LHsExpr (GhcPass p)
                 -> Maybe (LHsExpr (GhcPass p)) -> SDoc
pprTransformStmt bndrs using by
  = sep [ text "then" <+> whenPprDebug (braces (ppr bndrs))
        , nest 2 (ppr using)
        , nest 2 (pprBy by)]

pprTransStmt :: Outputable body => Maybe body -> body -> TransForm -> SDoc
pprTransStmt by using ThenForm
  = sep [ text "then", nest 2 (ppr using), nest 2 (pprBy by)]
pprTransStmt by using GroupForm
  = sep [ text "then group", nest 2 (pprBy by), nest 2 (ptext (sLit "using") <+> ppr using)]

pprBy :: Outputable body => Maybe body -> SDoc
pprBy Nothing  = empty
pprBy (Just e) = text "by" <+> ppr e

pprDo :: (OutputableBndrId (GhcPass p), Outputable body)
      => HsStmtContext any -> [LStmt (GhcPass p) body] -> SDoc
pprDo DoExpr        stmts = text "do"  <+> ppr_do_stmts stmts
pprDo GhciStmtCtxt  stmts = text "do"  <+> ppr_do_stmts stmts
pprDo ArrowExpr     stmts = text "do"  <+> ppr_do_stmts stmts
pprDo MDoExpr       stmts = text "mdo" <+> ppr_do_stmts stmts
pprDo ListComp      stmts = brackets    $ pprComp stmts
pprDo MonadComp     stmts = brackets    $ pprComp stmts
pprDo _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

ppr_do_stmts :: (OutputableBndrId (GhcPass idL), OutputableBndrId (GhcPass idR),
                 Outputable body)
             => [LStmtLR (GhcPass idL) (GhcPass idR) body] -> SDoc
-- Print a bunch of do stmts
ppr_do_stmts stmts = pprDeeperList vcat (map ppr stmts)

pprComp :: (OutputableBndrId (GhcPass p), Outputable body)
        => [LStmt (GhcPass p) body] -> SDoc
pprComp quals     -- Prints:  body | qual1, ..., qualn
  | Just (initStmts, L _ (LastStmt _ body _ _)) <- snocView quals
  = if null initStmts
       -- If there are no statements in a list comprehension besides the last
       -- one, we simply treat it like a normal list. This does arise
       -- occasionally in code that GHC generates, e.g., in implementations of
       -- 'range' for derived 'Ix' instances for product datatypes with exactly
       -- one constructor (e.g., see Trac #12583).
       then ppr body
       else hang (ppr body <+> vbar) 2 (pprQuals initStmts)
  | otherwise
  = pprPanic "pprComp" (pprQuals quals)

pprQuals :: (OutputableBndrId (GhcPass p), Outputable body)
         => [LStmt (GhcPass p) body] -> SDoc
-- Show list comprehension qualifiers separated by commas
pprQuals quals = interpp'SP quals

{-
************************************************************************
*                                                                      *
                Template Haskell quotation brackets
*                                                                      *
************************************************************************
-}

-- | Haskell Splice
data HsSplice id
   = HsTypedSplice       --  $$z  or $$(f 4)
        (XTypedSplice id)
        SpliceDecoration -- Whether $$( ) variant found, for pretty printing
        (IdP id)         -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsUntypedSplice     --  $z  or $(f 4)
        (XUntypedSplice id)
        SpliceDecoration -- Whether $( ) variant found, for pretty printing
        (IdP id)         -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsQuasiQuote        -- See Note [Quasi-quote overview] in TcSplice
        (XQuasiQuote id)
        (IdP id)         -- Splice point
        (IdP id)         -- Quoter
        SrcSpan          -- The span of the enclosed string
        FastString       -- The enclosed string

   -- AZ:TODO: use XSplice instead of HsSpliced
   | HsSpliced  -- See Note [Delaying modFinalizers in untyped splices] in
                -- RnSplice.
                -- This is the result of splicing a splice. It is produced by
                -- the renamer and consumed by the typechecker. It lives only
                -- between the two.
        (XSpliced id)
        ThModFinalizers     -- TH finalizers produced by the splice.
        (HsSplicedThing id) -- The result of splicing
   | HsSplicedT
      DelayedSplice
   | XSplice (XXSplice id)  -- Note [Trees that Grow] extension point

type instance XTypedSplice   (GhcPass _) = NoExt
type instance XUntypedSplice (GhcPass _) = NoExt
type instance XQuasiQuote    (GhcPass _) = NoExt
type instance XSpliced       (GhcPass _) = NoExt
type instance XXSplice       (GhcPass _) = NoExt

-- | A splice can appear with various decorations wrapped around it. This data
-- type captures explicitly how it was originally written, for use in the pretty
-- printer.
data SpliceDecoration
  = HasParens -- ^ $( splice ) or $$( splice )
  | HasDollar -- ^ $splice or $$splice
  | NoParens  -- ^ bare splice
  deriving (Data, Eq, Show)

instance Outputable SpliceDecoration where
  ppr x = text $ show x


isTypedSplice :: HsSplice id -> Bool
isTypedSplice (HsTypedSplice {}) = True
isTypedSplice _                  = False   -- Quasi-quotes are untyped splices

-- | Finalizers produced by a splice with
-- 'Language.Haskell.TH.Syntax.addModFinalizer'
--
-- See Note [Delaying modFinalizers in untyped splices] in RnSplice. For how
-- this is used.
--
newtype ThModFinalizers = ThModFinalizers [ForeignRef (TH.Q ())]

-- A Data instance which ignores the argument of 'ThModFinalizers'.
instance Data ThModFinalizers where
  gunfold _ z _ = z $ ThModFinalizers []
  toConstr  a   = mkConstr (dataTypeOf a) "ThModFinalizers" [] Data.Prefix
  dataTypeOf a  = mkDataType "HsExpr.ThModFinalizers" [toConstr a]

-- See Note [Running typed splices in the zonker]
-- These are the arguments that are passed to `TcSplice.runTopSplice`
data DelayedSplice =
  DelayedSplice
    TcLclEnv          -- The local environment to run the splice in
    (LHsExpr GhcRn)   -- The original renamed expression
    TcType            -- The result type of running the splice, unzonked
    (LHsExpr GhcTcId) -- The typechecked expression to run and splice in the result

-- A Data instance which ignores the argument of 'DelayedSplice'.
instance Data DelayedSplice where
  gunfold _ _ _ = panic "DelayedSplice"
  toConstr  a   = mkConstr (dataTypeOf a) "DelayedSplice" [] Data.Prefix
  dataTypeOf a  = mkDataType "HsExpr.DelayedSplice" [toConstr a]

-- | Haskell Spliced Thing
--
-- Values that can result from running a splice.
data HsSplicedThing id
    = HsSplicedExpr (HsExpr id) -- ^ Haskell Spliced Expression
    | HsSplicedTy   (HsType id) -- ^ Haskell Spliced Type
    | HsSplicedPat  (Pat id)    -- ^ Haskell Spliced Pattern


-- See Note [Pending Splices]
type SplicePointName = Name

-- | Pending Renamer Splice
data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LHsExpr GhcRn)

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice
  deriving Data

-- | Pending Type-checker Splice
data PendingTcSplice
  = PendingTcSplice SplicePointName (LHsExpr GhcTc)

{-
Note [Pending Splices]
~~~~~~~~~~~~~~~~~~~~~~
When we rename an untyped bracket, we name and lift out all the nested
splices, so that when the typechecker hits the bracket, it can
typecheck those nested splices without having to walk over the untyped
bracket code.  So for example
    [| f $(g x) |]
looks like

    HsBracket (HsApp (HsVar "f") (HsSpliceE _ (g x)))

which the renamer rewrites to

    HsRnBracketOut (HsApp (HsVar f) (HsSpliceE sn (g x)))
                   [PendingRnSplice UntypedExpSplice sn (g x)]

* The 'sn' is the Name of the splice point, the SplicePointName

* The PendingRnExpSplice gives the splice that splice-point name maps to;
  and the typechecker can now conveniently find these sub-expressions

* The other copy of the splice, in the second argument of HsSpliceE
                                in the renamed first arg of HsRnBracketOut
  is used only for pretty printing

There are four varieties of pending splices generated by the renamer,
distinguished by their UntypedSpliceFlavour

 * Pending expression splices (UntypedExpSplice), e.g.,
       [|$(f x) + 2|]

   UntypedExpSplice is also used for
     * quasi-quotes, where the pending expression expands to
          $(quoter "...blah...")
       (see RnSplice.makePending, HsQuasiQuote case)

     * cross-stage lifting, where the pending expression expands to
          $(lift x)
       (see RnSplice.checkCrossStageLifting)

 * Pending pattern splices (UntypedPatSplice), e.g.,
       [| \$(f x) -> x |]

 * Pending type splices (UntypedTypeSplice), e.g.,
       [| f :: $(g x) |]

 * Pending declaration (UntypedDeclSplice), e.g.,
       [| let $(f x) in ... |]

There is a fifth variety of pending splice, which is generated by the type
checker:

  * Pending *typed* expression splices, (PendingTcSplice), e.g.,
        [||1 + $$(f 2)||]

It would be possible to eliminate HsRnBracketOut and use HsBracketOut for the
output of the renamer. However, when pretty printing the output of the renamer,
e.g., in a type error message, we *do not* want to print out the pending
splices. In contrast, when pretty printing the output of the type checker, we
*do* want to print the pending splices. So splitting them up seems to make
sense, although I hate to add another constructor to HsExpr.
-}

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (HsSplicedThing p) where
  ppr (HsSplicedExpr e) = ppr_expr e
  ppr (HsSplicedTy   t) = ppr t
  ppr (HsSplicedPat  p) = ppr p

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsSplice p) where
  ppr s = pprSplice s

pprPendingSplice :: (OutputableBndrId (GhcPass p))
                 => SplicePointName -> LHsExpr (GhcPass p) -> SDoc
pprPendingSplice n e = angleBrackets (ppr n <> comma <+> ppr e)

pprSpliceDecl ::  (OutputableBndrId (GhcPass p))
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc
pprSpliceDecl e@HsQuasiQuote{} _ = pprSplice e
pprSpliceDecl e ExplicitSplice   = text "$(" <> ppr_splice_decl e <> text ")"
pprSpliceDecl e ImplicitSplice   = ppr_splice_decl e

ppr_splice_decl :: (OutputableBndrId (GhcPass p))
                => HsSplice (GhcPass p) -> SDoc
ppr_splice_decl (HsUntypedSplice _ _ n e) = ppr_splice empty n e empty
ppr_splice_decl e = pprSplice e

pprSplice :: (OutputableBndrId (GhcPass p)) => HsSplice (GhcPass p) -> SDoc
pprSplice (HsTypedSplice _ HasParens  n e)
  = ppr_splice (text "$$(") n e (text ")")
pprSplice (HsTypedSplice _ HasDollar n e)
  = ppr_splice (text "$$") n e empty
pprSplice (HsTypedSplice _ NoParens n e)
  = ppr_splice empty n e empty
pprSplice (HsUntypedSplice _ HasParens  n e)
  = ppr_splice (text "$(") n e (text ")")
pprSplice (HsUntypedSplice _ HasDollar n e)
  = ppr_splice (text "$")  n e empty
pprSplice (HsUntypedSplice _ NoParens n e)
  = ppr_splice empty  n e empty
pprSplice (HsQuasiQuote _ n q _ s)      = ppr_quasi n q s
pprSplice (HsSpliced _ _ thing)         = ppr thing
pprSplice (HsSplicedT {})               = text "Unevaluated typed splice"
pprSplice (XSplice x)                   = ppr x

ppr_quasi :: OutputableBndr p => p -> p -> FastString -> SDoc
ppr_quasi n quoter quote = whenPprDebug (brackets (ppr n)) <>
                           char '[' <> ppr quoter <> vbar <>
                           ppr quote <> text "|]"

ppr_splice :: (OutputableBndrId (GhcPass p))
           => SDoc -> (IdP (GhcPass p)) -> LHsExpr (GhcPass p) -> SDoc -> SDoc
ppr_splice herald n e trail
    = herald <> whenPprDebug (brackets (ppr n)) <> ppr e <> trail

-- | Haskell Bracket
data HsBracket p
  = ExpBr  (XExpBr p)   (LHsExpr p)    -- [|  expr  |]
  | PatBr  (XPatBr p)   (LPat p)      -- [p| pat   |]
  | DecBrL (XDecBrL p)  [LHsDecl p]   -- [d| decls |]; result of parser
  | DecBrG (XDecBrG p)  (HsGroup p)   -- [d| decls |]; result of renamer
  | TypBr  (XTypBr p)   (LHsType p)   -- [t| type  |]
  | VarBr  (XVarBr p)   Bool (IdP p)  -- True: 'x, False: ''T
                                -- (The Bool flag is used only in pprHsBracket)
  | TExpBr (XTExpBr p) (LHsExpr p)    -- [||  expr  ||]
  | XBracket (XXBracket p)            -- Note [Trees that Grow] extension point

type instance XExpBr      (GhcPass _) = NoExt
type instance XPatBr      (GhcPass _) = NoExt
type instance XDecBrL     (GhcPass _) = NoExt
type instance XDecBrG     (GhcPass _) = NoExt
type instance XTypBr      (GhcPass _) = NoExt
type instance XVarBr      (GhcPass _) = NoExt
type instance XTExpBr     (GhcPass _) = NoExt
type instance XXBracket   (GhcPass _) = NoExt

isTypedBracket :: HsBracket id -> Bool
isTypedBracket (TExpBr {}) = True
isTypedBracket _           = False

instance (p ~ GhcPass pass, OutputableBndrId p)
          => Outputable (HsBracket p) where
  ppr = pprHsBracket


pprHsBracket :: (OutputableBndrId (GhcPass p)) => HsBracket (GhcPass p) -> SDoc
pprHsBracket (ExpBr _ e)   = thBrackets empty (ppr e)
pprHsBracket (PatBr _ p)   = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBrG _ gp) = thBrackets (char 'd') (ppr gp)
pprHsBracket (DecBrL _ ds) = thBrackets (char 'd') (vcat (map ppr ds))
pprHsBracket (TypBr _ t)   = thBrackets (char 't') (ppr t)
pprHsBracket (VarBr _ True n)
  = char '\'' <> pprPrefixOcc n
pprHsBracket (VarBr _ False n)
  = text "''" <> pprPrefixOcc n
pprHsBracket (TExpBr _ e)  = thTyBrackets (ppr e)
pprHsBracket (XBracket e)  = ppr e

thBrackets :: SDoc -> SDoc -> SDoc
thBrackets pp_kind pp_body = char '[' <> pp_kind <> vbar <+>
                             pp_body <+> text "|]"

thTyBrackets :: SDoc -> SDoc
thTyBrackets pp_body = text "[||" <+> pp_body <+> ptext (sLit "||]")

instance Outputable PendingRnSplice where
  ppr (PendingRnSplice _ n e) = pprPendingSplice n e

instance Outputable PendingTcSplice where
  ppr (PendingTcSplice n e) = pprPendingSplice n e

{-
************************************************************************
*                                                                      *
\subsection{Enumerations and list comprehensions}
*                                                                      *
************************************************************************
-}

-- | Arithmetic Sequence Information
data ArithSeqInfo id
  = From            (LHsExpr id)
  | FromThen        (LHsExpr id)
                    (LHsExpr id)
  | FromTo          (LHsExpr id)
                    (LHsExpr id)
  | FromThenTo      (LHsExpr id)
                    (LHsExpr id)
                    (LHsExpr id)
-- AZ: Sould ArithSeqInfo have a TTG extension?

instance (p ~ GhcPass pass, OutputableBndrId p)
         => Outputable (ArithSeqInfo p) where
    ppr (From e1)             = hcat [ppr e1, pp_dotdot]
    ppr (FromThen e1 e2)      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot]
    ppr (FromTo e1 e3)        = hcat [ppr e1, pp_dotdot, ppr e3]
    ppr (FromThenTo e1 e2 e3)
      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot, ppr e3]

pp_dotdot :: SDoc
pp_dotdot = text " .. "

{-
************************************************************************
*                                                                      *
\subsection{HsMatchCtxt}
*                                                                      *
************************************************************************
-}

-- | Haskell Match Context
--
-- Context of a pattern match. This is more subtle than it would seem. See Note
-- [Varieties of pattern matches].
data HsMatchContext id -- Not an extensible tag
  = FunRhs { mc_fun        :: Located id    -- ^ function binder of @f@
           , mc_fixity     :: LexicalFixity -- ^ fixing of @f@
           , mc_strictness :: SrcStrictness -- ^ was @f@ banged?
                                            -- See Note [FunBind vs PatBind]
           }
                                -- ^A pattern matching on an argument of a
                                -- function binding
  | LambdaExpr                  -- ^Patterns of a lambda
  | CaseAlt                     -- ^Patterns and guards on a case alternative
  | IfAlt                       -- ^Guards of a multi-way if alternative
  | ProcExpr                    -- ^Patterns of a proc
  | PatBindRhs                  -- ^A pattern binding  eg [y] <- e = e
  | PatBindGuards               -- ^Guards of pattern bindings, e.g.,
                                --    (Just b) | Just _ <- x = e
                                --             | otherwise   = e'

  | RecUpd                      -- ^Record update [used only in DsExpr to
                                --    tell matchWrapper what sort of
                                --    runtime error message to generate]

  | StmtCtxt (HsStmtContext id) -- ^Pattern of a do-stmt, list comprehension,
                                -- pattern guard, etc

  | ThPatSplice            -- ^A Template Haskell pattern splice
  | ThPatQuote             -- ^A Template Haskell pattern quotation [p| (a,b) |]
  | PatSyn                 -- ^A pattern synonym declaration
  deriving Functor
deriving instance (Data id) => Data (HsMatchContext id)

instance OutputableBndr id => Outputable (HsMatchContext id) where
  ppr m@(FunRhs{})          = text "FunRhs" <+> ppr (mc_fun m) <+> ppr (mc_fixity m)
  ppr LambdaExpr            = text "LambdaExpr"
  ppr CaseAlt               = text "CaseAlt"
  ppr IfAlt                 = text "IfAlt"
  ppr ProcExpr              = text "ProcExpr"
  ppr PatBindRhs            = text "PatBindRhs"
  ppr PatBindGuards         = text "PatBindGuards"
  ppr RecUpd                = text "RecUpd"
  ppr (StmtCtxt _)          = text "StmtCtxt _"
  ppr ThPatSplice           = text "ThPatSplice"
  ppr ThPatQuote            = text "ThPatQuote"
  ppr PatSyn                = text "PatSyn"

isPatSynCtxt :: HsMatchContext id -> Bool
isPatSynCtxt ctxt =
  case ctxt of
    PatSyn -> True
    _      -> False

-- | Haskell Statement Context. It expects to be parameterised with one of
-- 'RdrName', 'Name' or 'Id'
data HsStmtContext id
  = ListComp
  | MonadComp

  | DoExpr                           -- ^do { ... }
  | MDoExpr                          -- ^mdo { ... }  ie recursive do-expression
  | ArrowExpr                        -- ^do-notation in an arrow-command context

  | GhciStmtCtxt                     -- ^A command-line Stmt in GHCi pat <- rhs
  | PatGuard (HsMatchContext id)     -- ^Pattern guard for specified thing
  | ParStmtCtxt (HsStmtContext id)   -- ^A branch of a parallel stmt
  | TransStmtCtxt (HsStmtContext id) -- ^A branch of a transform stmt
  deriving Functor
deriving instance (Data id) => Data (HsStmtContext id)

isComprehensionContext :: HsStmtContext id -> Bool
-- Uses comprehension syntax [ e | quals ]
isComprehensionContext ListComp          = True
isComprehensionContext MonadComp         = True
isComprehensionContext (ParStmtCtxt c)   = isComprehensionContext c
isComprehensionContext (TransStmtCtxt c) = isComprehensionContext c
isComprehensionContext _ = False

-- | Should pattern match failure in a 'HsStmtContext' be desugared using
-- 'MonadFail'?
isMonadFailStmtContext :: HsStmtContext id -> Bool
isMonadFailStmtContext MonadComp            = True
isMonadFailStmtContext DoExpr               = True
isMonadFailStmtContext MDoExpr              = True
isMonadFailStmtContext GhciStmtCtxt         = True
isMonadFailStmtContext (ParStmtCtxt ctxt)   = isMonadFailStmtContext ctxt
isMonadFailStmtContext (TransStmtCtxt ctxt) = isMonadFailStmtContext ctxt
isMonadFailStmtContext _ = False -- ListComp, PatGuard, ArrowExpr

isMonadCompContext :: HsStmtContext id -> Bool
isMonadCompContext MonadComp = True
isMonadCompContext _         = False

matchSeparator :: HsMatchContext id -> SDoc
matchSeparator (FunRhs {})   = text "="
matchSeparator CaseAlt       = text "->"
matchSeparator IfAlt         = text "->"
matchSeparator LambdaExpr    = text "->"
matchSeparator ProcExpr      = text "->"
matchSeparator PatBindRhs    = text "="
matchSeparator PatBindGuards = text "="
matchSeparator (StmtCtxt _)  = text "<-"
matchSeparator RecUpd        = text "=" -- This can be printed by the pattern
                                       -- match checker trace
matchSeparator ThPatSplice  = panic "unused"
matchSeparator ThPatQuote   = panic "unused"
matchSeparator PatSyn       = panic "unused"

pprMatchContext :: (Outputable (NameOrRdrName id),Outputable id)
                => HsMatchContext id -> SDoc
pprMatchContext ctxt
  | want_an ctxt = text "an" <+> pprMatchContextNoun ctxt
  | otherwise    = text "a"  <+> pprMatchContextNoun ctxt
  where
    want_an (FunRhs {}) = True  -- Use "an" in front
    want_an ProcExpr    = True
    want_an _           = False

pprMatchContextNoun :: (Outputable (NameOrRdrName id),Outputable id)
                    => HsMatchContext id -> SDoc
pprMatchContextNoun (FunRhs {mc_fun=L _ fun})
                                    = text "equation for"
                                      <+> quotes (ppr fun)
pprMatchContextNoun CaseAlt         = text "case alternative"
pprMatchContextNoun IfAlt           = text "multi-way if alternative"
pprMatchContextNoun RecUpd          = text "record-update construct"
pprMatchContextNoun ThPatSplice     = text "Template Haskell pattern splice"
pprMatchContextNoun ThPatQuote      = text "Template Haskell pattern quotation"
pprMatchContextNoun PatBindRhs      = text "pattern binding"
pprMatchContextNoun PatBindGuards   = text "pattern binding guards"
pprMatchContextNoun LambdaExpr      = text "lambda abstraction"
pprMatchContextNoun ProcExpr        = text "arrow abstraction"
pprMatchContextNoun (StmtCtxt ctxt) = text "pattern binding in"
                                      $$ pprAStmtContext ctxt
pprMatchContextNoun PatSyn          = text "pattern synonym declaration"

-----------------
pprAStmtContext, pprStmtContext :: (Outputable id,
                                    Outputable (NameOrRdrName id))
                                => HsStmtContext id -> SDoc
pprAStmtContext ctxt = article <+> pprStmtContext ctxt
  where
    pp_an = text "an"
    pp_a  = text "a"
    article = case ctxt of
                  MDoExpr       -> pp_an
                  GhciStmtCtxt  -> pp_an
                  _             -> pp_a


-----------------
pprStmtContext GhciStmtCtxt    = text "interactive GHCi command"
pprStmtContext DoExpr          = text "'do' block"
pprStmtContext MDoExpr         = text "'mdo' block"
pprStmtContext ArrowExpr       = text "'do' block in an arrow command"
pprStmtContext ListComp        = text "list comprehension"
pprStmtContext MonadComp       = text "monad comprehension"
pprStmtContext (PatGuard ctxt) = text "pattern guard for" $$ pprMatchContext ctxt

-- Drop the inner contexts when reporting errors, else we get
--     Unexpected transform statement
--     in a transformed branch of
--          transformed branch of
--          transformed branch of monad comprehension
pprStmtContext (ParStmtCtxt c) =
  ifPprDebug (sep [text "parallel branch of", pprAStmtContext c])
             (pprStmtContext c)
pprStmtContext (TransStmtCtxt c) =
  ifPprDebug (sep [text "transformed branch of", pprAStmtContext c])
             (pprStmtContext c)

instance (Outputable p, Outputable (NameOrRdrName p))
      => Outputable (HsStmtContext p) where
    ppr = pprStmtContext

-- Used to generate the string for a *runtime* error message
matchContextErrString :: Outputable id
                      => HsMatchContext id -> SDoc
matchContextErrString (FunRhs{mc_fun=L _ fun})   = text "function" <+> ppr fun
matchContextErrString CaseAlt                    = text "case"
matchContextErrString IfAlt                      = text "multi-way if"
matchContextErrString PatBindRhs                 = text "pattern binding"
matchContextErrString PatBindGuards              = text "pattern binding guards"
matchContextErrString RecUpd                     = text "record update"
matchContextErrString LambdaExpr                 = text "lambda"
matchContextErrString ProcExpr                   = text "proc"
matchContextErrString ThPatSplice                = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString ThPatQuote                 = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString PatSyn                     = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString (StmtCtxt (ParStmtCtxt c))   = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (TransStmtCtxt c)) = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (PatGuard _))      = text "pattern guard"
matchContextErrString (StmtCtxt GhciStmtCtxt)      = text "interactive GHCi command"
matchContextErrString (StmtCtxt DoExpr)            = text "'do' block"
matchContextErrString (StmtCtxt ArrowExpr)         = text "'do' block"
matchContextErrString (StmtCtxt MDoExpr)           = text "'mdo' block"
matchContextErrString (StmtCtxt ListComp)          = text "list comprehension"
matchContextErrString (StmtCtxt MonadComp)         = text "monad comprehension"

pprMatchInCtxt :: (OutputableBndrId (GhcPass idR),
                   -- TODO:AZ these constraints do not make sense
                 Outputable (NameOrRdrName (NameOrRdrName (IdP (GhcPass idR)))),
                 Outputable body)
               => Match (GhcPass idR) body -> SDoc
pprMatchInCtxt match  = hang (text "In" <+> pprMatchContext (m_ctxt match)
                                        <> colon)
                             4 (pprMatch match)

pprStmtInCtxt :: (OutputableBndrId (GhcPass idL),
                  OutputableBndrId (GhcPass idR),
                  Outputable body)
              => HsStmtContext (IdP (GhcPass idL))
              -> StmtLR (GhcPass idL) (GhcPass idR) body
              -> SDoc
pprStmtInCtxt ctxt (LastStmt _ e _ _)
  | isComprehensionContext ctxt      -- For [ e | .. ], do not mutter about "stmts"
  = hang (text "In the expression:") 2 (ppr e)

pprStmtInCtxt ctxt stmt
  = hang (text "In a stmt of" <+> pprAStmtContext ctxt <> colon)
       2 (ppr_stmt stmt)
  where
    -- For Group and Transform Stmts, don't print the nested stmts!
    ppr_stmt (TransStmt { trS_by = by, trS_using = using
                        , trS_form = form }) = pprTransStmt by using form
    ppr_stmt stmt = pprStmt stmt

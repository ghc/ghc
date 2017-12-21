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

noPostTcExpr :: PostTcExpr
noPostTcExpr = HsLit (HsString noSourceText (fsLit "noPostTcExpr"))

noPostTcTable :: PostTcTable
noPostTcTable = []

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
-- This could be defined using @PostRn@ and @PostTc@ and such, but it's
-- harder to get it all to work out that way. ('noSyntaxExpr' is hard to
-- write, for example.)
data SyntaxExpr p = SyntaxExpr { syn_expr      :: HsExpr p
                               , syn_arg_wraps :: [HsWrapper]
                               , syn_res_wrap  :: HsWrapper }
deriving instance (DataId p) => Data (SyntaxExpr p)

-- | This is used for rebindable-syntax pieces that are too polymorphic
-- for tcSyntaxOp (trS_fmap and the mzip in ParStmt)
noExpr :: SourceTextX p => HsExpr p
noExpr = HsLit (HsString (sourceText  "noExpr") (fsLit "noExpr"))

noSyntaxExpr :: SourceTextX p => SyntaxExpr p
                              -- Before renaming, and sometimes after,
                              -- (if the syntax slot makes no sense)
noSyntaxExpr = SyntaxExpr { syn_expr      = HsLit (HsString noSourceText
                                                        (fsLit "noSyntaxExpr"))
                          , syn_arg_wraps = []
                          , syn_res_wrap  = WpHole }

-- | Make a 'SyntaxExpr Name' (the "rn" is because this is used in the
-- renamer), missing its HsWrappers.
mkRnSyntaxExpr :: Name -> SyntaxExpr GhcRn
mkRnSyntaxExpr name = SyntaxExpr { syn_expr      = HsVar $ noLoc name
                                 , syn_arg_wraps = []
                                 , syn_res_wrap  = WpHole }
  -- don't care about filling in syn_arg_wraps because we're clearly
  -- not past the typechecker

instance (SourceTextX p, OutputableBndrId p) => Outputable (SyntaxExpr p) where
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

-- | An unbound variable; used for treating out-of-scope variables as
-- expression holes
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
  = HsVar     (Located (IdP p)) -- ^ Variable

                             -- See Note [Located RdrNames]

  | HsUnboundVar UnboundVar  -- ^ Unbound variable; also used for "holes"
                             --   (_ or _x).
                             -- Turned from HsVar to HsUnboundVar by the
                             --   renamer, when it finds an out-of-scope
                             --   variable or hole.
                             -- Turned into HsVar by type checker, to support
                             --   deferred type errors.

  | HsConLikeOut ConLike     -- ^ After typechecker only; must be different
                             -- HsVar for pretty printing

  | HsRecFld (AmbiguousFieldOcc p) -- ^ Variable pointing to record selector
                                    -- Not in use after typechecking

  | HsOverLabel (Maybe (IdP p)) FastString
     -- ^ Overloaded label (Note [Overloaded labels] in GHC.OverloadedLabels)
     --   @Just id@ means @RebindableSyntax@ is in use, and gives the id of the
     --   in-scope 'fromLabel'.
     --   NB: Not in use after typechecking

  | HsIPVar   HsIPName       -- ^ Implicit parameter (not in use after typechecking)
  | HsOverLit (HsOverLit p)  -- ^ Overloaded literals

  | HsLit     (HsLit p)      -- ^ Simple (non-overloaded) literals

  | HsLam     (MatchGroup p (LHsExpr p))
                       -- ^ Lambda abstraction. Currently always a single match
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --       'ApiAnnotation.AnnRarrow',

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsLamCase (MatchGroup p (LHsExpr p)) -- ^ Lambda-case
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --           'ApiAnnotation.AnnCase','ApiAnnotation.AnnOpen',
       --           'ApiAnnotation.AnnClose'

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsApp     (LHsExpr p) (LHsExpr p) -- ^ Application

  | HsAppType (LHsExpr p) (LHsWcType p) -- ^ Visible type application
       --
       -- Explicit type argument; e.g  f @Int x y
       -- NB: Has wildcards, but no implicit quantification
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt',

  -- TODO:AZ: Sort out Name
  | HsAppTypeOut (LHsExpr p) (LHsWcType GhcRn) -- just for pretty-printing


  -- | Operator applications:
  -- NB Bracketed ops such as (+) come out as Vars.

  -- NB We need an expr for the operator in an OpApp/Section since
  -- the typechecker may need to apply the operator to a few types.

  | OpApp       (LHsExpr p)       -- left operand
                (LHsExpr p)       -- operator
                (PostRn p Fixity) -- Renamer adds fixity; bottom until then
                (LHsExpr p)       -- right operand

  -- | Negation operator. Contains the negated expression and the name
  -- of 'negate'
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnMinus'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | NegApp      (LHsExpr p)
                (SyntaxExpr p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
  --             'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsPar       (LHsExpr p)  -- ^ Parenthesised expr; see Note [Parens in HsSyn]

  | SectionL    (LHsExpr p)    -- operand; see Note [Sections in HsSyn]
                (LHsExpr p)    -- operator
  | SectionR    (LHsExpr p)    -- operator; see Note [Sections in HsSyn]
                (LHsExpr p)    -- operand

  -- | Used for explicit tuples and sections thereof
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitTuple
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
          ConTag --  Alternative (one-based)
          Arity  --  Sum arity
          (LHsExpr p)
          (PostTc p [Type])   -- the type arguments

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnCase',
  --       'ApiAnnotation.AnnOf','ApiAnnotation.AnnOpen' @'{'@,
  --       'ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCase      (LHsExpr p)
                (MatchGroup p (LHsExpr p))

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf',
  --       'ApiAnnotation.AnnSemi',
  --       'ApiAnnotation.AnnThen','ApiAnnotation.AnnSemi',
  --       'ApiAnnotation.AnnElse',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsIf        (Maybe (SyntaxExpr p)) -- cond function
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
  | HsMultiIf   (PostTc p Type) [LGRHS p (LHsExpr p)]

  -- | let(rec)
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet',
  --       'ApiAnnotation.AnnOpen' @'{'@,
  --       'ApiAnnotation.AnnClose' @'}'@,'ApiAnnotation.AnnIn'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsLet       (LHsLocalBinds p)
                (LHsExpr  p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDo',
  --             'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnSemi',
  --             'ApiAnnotation.AnnVbar',
  --             'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsDo        (HsStmtContext Name)     -- The parameterisation is unimportant
                                         -- because in this context we never use
                                         -- the PatGuard or ParStmt variant
                (Located [ExprLStmt p]) -- "do":one or more stmts
                (PostTc p Type)         -- Type of the whole expression

  -- | Syntactic list: [a,b,c,...]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
  --              'ApiAnnotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitList
                (PostTc p Type)        -- Gives type of components of list
                (Maybe (SyntaxExpr p))
                                   -- For OverloadedLists, the fromListN witness
                [LHsExpr p]

  -- | Syntactic parallel array: [:e1, ..., en:]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
  --              'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnComma',
  --              'ApiAnnotation.AnnVbar'
  --              'ApiAnnotation.AnnClose' @':]'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitPArr
                (PostTc p Type)   -- type of elements of the parallel array
                [LHsExpr p]

  -- | Record construction
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
  --         'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecordCon
      { rcon_con_name :: Located (IdP p)    -- The constructor name;
                                            --  not used after type checking
      , rcon_con_like :: PostTc p ConLike
                                      -- The data constructor or pattern synonym
      , rcon_con_expr :: PostTcExpr         -- Instantiated constructor function
      , rcon_flds     :: HsRecordBinds p }  -- The fields

  -- | Record update
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
  --         'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecordUpd
      { rupd_expr :: LHsExpr p
      , rupd_flds :: [LHsRecUpdField p]
      , rupd_cons :: PostTc p [ConLike]
                -- Filled in by the type checker to the
                -- _non-empty_ list of DataCons that have
                -- all the upd'd fields

      , rupd_in_tys  :: PostTc p [Type] -- Argument types of *input* record type
      , rupd_out_tys :: PostTc p [Type] --             and  *output* record type
                                       -- The original type can be reconstructed
                                       -- with conLikeResTy
      , rupd_wrap :: PostTc p HsWrapper  -- See note [Record Update HsWrapper]
      }
  -- For a type family, the arg types are of the *instance* tycon,
  -- not the family tycon

  -- | Expression with an explicit type signature. @e :: type@
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExprWithTySig
                (LHsExpr p)
                (LHsSigWcType p)

  | ExprWithTySigOut              -- Post typechecking
                (LHsExpr p)
                (LHsSigWcType GhcRn)  -- Retain the signature,
                                     -- as HsSigType Name, for
                                     -- round-tripping purposes

  -- | Arithmetic sequence
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
  --              'ApiAnnotation.AnnComma','ApiAnnotation.AnnDotdot',
  --              'ApiAnnotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ArithSeq
                PostTcExpr
                (Maybe (SyntaxExpr p))
                                  -- For OverloadedLists, the fromList witness
                (ArithSeqInfo p)

  -- | Arithmetic sequence for parallel array
  --
  -- > [:e1..e2:] or [:e1, e2..e3:]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
  --              'ApiAnnotation.AnnComma','ApiAnnotation.AnnDotdot',
  --              'ApiAnnotation.AnnVbar',
  --              'ApiAnnotation.AnnClose' @':]'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | PArrSeq
                PostTcExpr
                (ArithSeqInfo p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{-\# SCC'@,
  --             'ApiAnnotation.AnnVal' or 'ApiAnnotation.AnnValStr',
  --              'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsSCC       SourceText            -- Note [Pragma source text] in BasicTypes
                StringLiteral         -- "set cost centre" SCC pragma
                (LHsExpr p)           -- expr whose cost is to be measured

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{-\# CORE'@,
  --             'ApiAnnotation.AnnVal', 'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCoreAnn   SourceText            -- Note [Pragma source text] in BasicTypes
                StringLiteral         -- hdaume: core annotation
                (LHsExpr p)

  -----------------------------------------------------------
  -- MetaHaskell Extensions

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnOpenE','ApiAnnotation.AnnOpenEQ',
  --         'ApiAnnotation.AnnClose','ApiAnnotation.AnnCloseQ'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsBracket    (HsBracket p)

    -- See Note [Pending Splices]
  | HsRnBracketOut
      (HsBracket GhcRn)    -- Output of the renamer is the *original* renamed
                           -- expression, plus
      [PendingRnSplice]    -- _renamed_ splices to be type checked

  | HsTcBracketOut
      (HsBracket GhcRn)    -- Output of the type checker is the *original*
                           -- renamed expression, plus
      [PendingTcSplice]    -- _typechecked_ splices to be
                           -- pasted back in by the desugarer

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsSpliceE  (HsSplice p)

  -----------------------------------------------------------
  -- Arrow notation extension

  -- | @proc@ notation for Arrows
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnProc',
  --          'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsProc      (LPat p)               -- arrow abstraction, proc
                (LHsCmdTop p)          -- body of the abstraction
                                       -- always has an empty stack

  ---------------------------------------
  -- static pointers extension
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnStatic',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsStatic (PostRn p NameSet) -- Free variables of the body
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
        (LHsExpr p)     -- arrow expression, f
        (LHsExpr p)     -- input expression, arg
        (PostTc p Type) -- type of the arrow expressions f,
                        -- of the form a t t', where arg :: t
        HsArrAppType    -- higher-order (-<<) or first-order (-<)
        Bool            -- True => right-to-left (f -< arg)
                        -- False => left-to-right (arg >- f)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpenB' @'(|'@,
  --         'ApiAnnotation.AnnCloseB' @'|)'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsArrForm            -- Command formation,  (| e cmd1 .. cmdn |)
        (LHsExpr p)      -- the operator
                         -- after type-checking, a type abstraction to be
                         -- applied to the type of the local environment tuple
        (Maybe Fixity)   -- fixity (filled in by the renamer), for forms that
                         -- were converted from OpApp's by the renamer
        [LHsCmdTop p]    -- argument commands

  ---------------------------------------
  -- Haskell program coverage (Hpc) Support

  | HsTick
     (Tickish (IdP p))
     (LHsExpr p)                       -- sub-expression

  | HsBinTick
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

  | EWildPat                 -- wildcard

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | EAsPat      (Located (IdP p)) -- as pattern
                (LHsExpr p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | EViewPat    (LHsExpr p) -- view pattern
                (LHsExpr p)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ELazyPat    (LHsExpr p) -- ~ pattern


  ---------------------------------------
  -- Finally, HsWrap appears only in typechecker output
  -- The contained Expr is *NOT* itself an HsWrap.
  -- See Note [Detecting forced eta expansion] in DsExpr. This invariant
  -- is maintained by HsUtils.mkHsWrap.

  |  HsWrap     HsWrapper    -- TRANSLATION
                (HsExpr p)

deriving instance (DataId p) => Data (HsExpr p)

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
  = Present (LHsExpr id)     -- ^ The argument
  | Missing (PostTc id Type) -- ^ The argument is missing, but this is its type
deriving instance (DataId id) => Data (HsTupArg id)

tupArgPresent :: LHsTupArg id -> Bool
tupArgPresent (L _ (Present {})) = True
tupArgPresent (L _ (Missing {})) = False

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

instance (SourceTextX p, OutputableBndrId p) => Outputable (HsExpr p) where
    ppr expr = pprExpr expr

-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper;
-- the underscore versions do not
pprLExpr :: (SourceTextX p, OutputableBndrId p) => LHsExpr p -> SDoc
pprLExpr (L _ e) = pprExpr e

pprExpr :: (SourceTextX p, OutputableBndrId p) => HsExpr p -> SDoc
pprExpr e | isAtomicHsExpr e || isQuietHsExpr e =            ppr_expr e
          | otherwise                           = pprDeeper (ppr_expr e)

isQuietHsExpr :: HsExpr id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsExpr (HsPar _)          = True
-- applications don't display anything themselves
isQuietHsExpr (HsApp _ _)        = True
isQuietHsExpr (HsAppType _ _)    = True
isQuietHsExpr (HsAppTypeOut _ _) = True
isQuietHsExpr (OpApp _ _ _ _)    = True
isQuietHsExpr _ = False

pprBinds :: (SourceTextX idL, SourceTextX idR,
             OutputableBndrId idL, OutputableBndrId idR)
         => HsLocalBindsLR idL idR -> SDoc
pprBinds b = pprDeeper (ppr b)

-----------------------
ppr_lexpr :: (SourceTextX p, OutputableBndrId p) => LHsExpr p -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)

ppr_expr :: forall p. (SourceTextX p, OutputableBndrId p) => HsExpr p -> SDoc
ppr_expr (HsVar (L _ v))  = pprPrefixOcc v
ppr_expr (HsUnboundVar uv)= pprPrefixOcc (unboundVarOcc uv)
ppr_expr (HsConLikeOut c) = pprPrefixOcc c
ppr_expr (HsIPVar v)      = ppr v
ppr_expr (HsOverLabel _ l)= char '#' <> ppr l
ppr_expr (HsLit lit)      = ppr lit
ppr_expr (HsOverLit lit)  = ppr lit
ppr_expr (HsPar e)        = parens (ppr_lexpr e)

ppr_expr (HsCoreAnn stc (StringLiteral sta s) e)
  = vcat [pprWithSourceText stc (text "{-# CORE")
          <+> pprWithSourceText sta (doubleQuotes $ ftext s) <+> text "#-}"
         , ppr_lexpr e]

ppr_expr e@(HsApp {})        = ppr_apps e []
ppr_expr e@(HsAppType {})    = ppr_apps e []
ppr_expr e@(HsAppTypeOut {}) = ppr_apps e []

ppr_expr (OpApp e1 op _ e2)
  | Just pp_op <- should_print_infix (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly

  where
    should_print_infix (HsVar (L _ v)) = Just (pprInfixOcc v)
    should_print_infix (HsConLikeOut c)= Just (pprInfixOcc (conLikeName c))
    should_print_infix (HsRecFld f)    = Just (pprInfixOcc f)
    should_print_infix (HsUnboundVar h@TrueExprHole{})
                                       = Just (pprInfixOcc (unboundVarOcc h))
    should_print_infix EWildPat        = Just (text "`_`")
    should_print_infix (HsWrap _ e)    = should_print_infix e
    should_print_infix _               = Nothing

    pp_e1 = pprDebugParendExpr e1   -- In debug mode, add parens
    pp_e2 = pprDebugParendExpr e2   -- to make precedence clear

    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])

    pp_infixly pp_op
      = hang pp_e1 2 (sep [pp_op, nest 2 pp_e2])

ppr_expr (NegApp e _) = char '-' <+> pprDebugParendExpr e

ppr_expr (SectionL expr op)
  = case unLoc op of
      HsVar (L _ v)  -> pp_infixly v
      HsConLikeOut c -> pp_infixly (conLikeName c)
      HsUnboundVar h@TrueExprHole{}
                     -> pp_infixly (unboundVarOcc h)
      _              -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
                       4 (hsep [pp_expr, text "x_ )"])
    pp_infixly v = (sep [pp_expr, pprInfixOcc v])

ppr_expr (SectionR op expr)
  = case unLoc op of
      HsVar (L _ v)  -> pp_infixly v
      HsConLikeOut c -> pp_infixly (conLikeName c)
      HsUnboundVar h@TrueExprHole{}
                     -> pp_infixly (unboundVarOcc h)
      _              -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, text "x_"])
                       4 (pp_expr <> rparen)
    pp_infixly v = sep [pprInfixOcc v, pp_expr]

ppr_expr (ExplicitTuple exprs boxity)
  = tupleParens (boxityTupleSort boxity) (fcat (ppr_tup_args $ map unLoc exprs))
  where
    ppr_tup_args []               = []
    ppr_tup_args (Present e : es) = (ppr_lexpr e <> punc es) : ppr_tup_args es
    ppr_tup_args (Missing _ : es) = punc es : ppr_tup_args es

    punc (Present {} : _) = comma <> space
    punc (Missing {} : _) = comma
    punc []               = empty

ppr_expr (ExplicitSum alt arity expr _)
  = text "(#" <+> ppr_bars (alt - 1) <+> ppr expr <+> ppr_bars (arity - alt) <+> text "#)"
  where
    ppr_bars n = hsep (replicate n (char '|'))

ppr_expr (HsLam matches)
  = pprMatches matches

ppr_expr (HsLamCase matches)
  = sep [ sep [text "\\case"],
          nest 2 (pprMatches matches) ]

ppr_expr (HsCase expr matches@(MG { mg_alts = L _ [_] }))
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches matches) <+> char '}']
ppr_expr (HsCase expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of")],
          nest 2 (pprMatches matches) ]

ppr_expr (HsIf _ e1 e2 e3)
  = sep [hsep [text "if", nest 2 (ppr e1), ptext (sLit "then")],
         nest 4 (ppr e2),
         text "else",
         nest 4 (ppr e3)]

ppr_expr (HsMultiIf _ alts)
  = hang (text "if") 3  (vcat (map ppr_alt alts))
  where ppr_alt (L _ (GRHS guards expr)) =
          hang vbar 2 (ppr_one one_alt)
          where
            ppr_one [] = panic "ppr_exp HsMultiIf"
            ppr_one (h:t) = hang h 2 (sep t)
            one_alt = [ interpp'SP guards
                      , text "->" <+> pprDeeper (ppr expr) ]

-- special case: let ... in let ...
ppr_expr (HsLet (L _ binds) expr@(L _ (HsLet _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lexpr expr]

ppr_expr (HsLet (L _ binds) expr)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr expr)]

ppr_expr (HsDo do_or_list_comp (L _ stmts) _) = pprDo do_or_list_comp stmts

ppr_expr (ExplicitList _ _ exprs)
  = brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (ExplicitPArr _ exprs)
  = paBrackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (RecordCon { rcon_con_name = con_id, rcon_flds = rbinds })
  = hang (ppr con_id) 2 (ppr rbinds)

ppr_expr (RecordUpd { rupd_expr = L _ aexp, rupd_flds = rbinds })
  = hang (ppr aexp) 2 (braces (fsep (punctuate comma (map ppr rbinds))))

ppr_expr (ExprWithTySig expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)
ppr_expr (ExprWithTySigOut expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)

ppr_expr (ArithSeq _ _ info) = brackets (ppr info)
ppr_expr (PArrSeq  _ info) = paBrackets (ppr info)

ppr_expr EWildPat       = char '_'
ppr_expr (ELazyPat e)   = char '~' <> ppr e
ppr_expr (EAsPat v e)   = ppr v <> char '@' <> ppr e
ppr_expr (EViewPat p e) = ppr p <+> text "->" <+> ppr e

ppr_expr (HsSCC st (StringLiteral stl lbl) expr)
  = sep [ pprWithSourceText st (text "{-# SCC")
         -- no doublequotes if stl empty, for the case where the SCC was written
         -- without quotes.
          <+> pprWithSourceText stl (ftext lbl) <+> text "#-}",
          ppr expr ]

ppr_expr (HsWrap co_fn e)
  = pprHsWrapper co_fn (\parens -> if parens then pprExpr e
                                             else pprExpr e)

ppr_expr (HsSpliceE s)         = pprSplice s
ppr_expr (HsBracket b)         = pprHsBracket b
ppr_expr (HsRnBracketOut e []) = ppr e
ppr_expr (HsRnBracketOut e ps) = ppr e $$ text "pending(rn)" <+> ppr ps
ppr_expr (HsTcBracketOut e []) = ppr e
ppr_expr (HsTcBracketOut e ps) = ppr e $$ text "pending(tc)" <+> ppr ps

ppr_expr (HsProc pat (L _ (HsCmdTop cmd _ _ _)))
  = hsep [text "proc", ppr pat, ptext (sLit "->"), ppr cmd]

ppr_expr (HsStatic _ e)
  = hsep [text "static", ppr e]

ppr_expr (HsTick tickish exp)
  = pprTicks (ppr exp) $
    ppr tickish <+> ppr_lexpr exp
ppr_expr (HsBinTick tickIdTrue tickIdFalse exp)
  = pprTicks (ppr exp) $
    hcat [text "bintick<",
          ppr tickIdTrue,
          text ",",
          ppr tickIdFalse,
          text ">(",
          ppr exp, text ")"]
ppr_expr (HsTickPragma _ externalSrcLoc _ exp)
  = pprTicks (ppr exp) $
    hcat [text "tickpragma<",
          pprExternalSrcLoc externalSrcLoc,
          text ">(",
          ppr exp,
          text ")"]

ppr_expr (HsArrApp arrow arg _ HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg _ HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_expr (HsArrApp arrow arg _ HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg _ HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_expr (HsArrForm (L _ (HsVar (L _ v))) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc v, pprCmdArg (unLoc arg2)]]
ppr_expr (HsArrForm (L _ (HsConLikeOut c)) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc (conLikeName c), pprCmdArg (unLoc arg2)]]
ppr_expr (HsArrForm op _ args)
  = hang (text "(|" <+> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <+> text "|)")
ppr_expr (HsRecFld f) = ppr f

-- We must tiresomely make the "id" parameter to the LHsWcType existential
-- because it's different in the HsAppType case and the HsAppTypeOut case
-- | Located Haskell Wildcard Type Expression
data LHsWcTypeX = forall p. (SourceTextX p, OutputableBndrId p)
                       => LHsWcTypeX (LHsWcType p)

ppr_apps :: (SourceTextX p, OutputableBndrId p) => HsExpr p
         -> [Either (LHsExpr p) LHsWcTypeX]
         -> SDoc
ppr_apps (HsApp (L _ fun) arg)        args
  = ppr_apps fun (Left arg : args)
ppr_apps (HsAppType (L _ fun) arg)    args
  = ppr_apps fun (Right (LHsWcTypeX arg) : args)
ppr_apps (HsAppTypeOut (L _ fun) arg) args
  = ppr_apps fun (Right (LHsWcTypeX arg) : args)
ppr_apps fun args = hang (ppr_expr fun) 2 (sep (map pp args))
  where
    pp (Left arg)                             = ppr arg
    pp (Right (LHsWcTypeX (HsWC { hswc_body = L _ arg })))
      = char '@' <> pprHsType arg

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

pprDebugParendExpr :: (SourceTextX p, OutputableBndrId p) => LHsExpr p -> SDoc
pprDebugParendExpr expr
  = getPprStyle (\sty ->
    if debugStyle sty then pprParendLExpr expr
                      else pprLExpr      expr)

pprParendLExpr :: (SourceTextX p, OutputableBndrId p) => LHsExpr p -> SDoc
pprParendLExpr (L _ e) = pprParendExpr e

pprParendExpr :: (SourceTextX p, OutputableBndrId p) => HsExpr p -> SDoc
pprParendExpr expr
  | hsExprNeedsParens expr = parens (pprExpr expr)
  | otherwise              = pprExpr expr
        -- Using pprLExpr makes sure that we go 'deeper'
        -- I think that is usually (always?) right

hsExprNeedsParens :: HsExpr id -> Bool
-- True of expressions for which '(e)' and 'e'
-- mean the same thing
hsExprNeedsParens (ArithSeq {})       = False
hsExprNeedsParens (PArrSeq {})        = False
hsExprNeedsParens (HsLit {})          = False
hsExprNeedsParens (HsOverLit {})      = False
hsExprNeedsParens (HsVar {})          = False
hsExprNeedsParens (HsUnboundVar {})   = False
hsExprNeedsParens (HsConLikeOut {})   = False
hsExprNeedsParens (HsIPVar {})        = False
hsExprNeedsParens (HsOverLabel {})    = False
hsExprNeedsParens (ExplicitTuple {})  = False
hsExprNeedsParens (ExplicitList {})   = False
hsExprNeedsParens (ExplicitPArr {})   = False
hsExprNeedsParens (HsPar {})          = False
hsExprNeedsParens (HsBracket {})      = False
hsExprNeedsParens (HsRnBracketOut {}) = False
hsExprNeedsParens (HsTcBracketOut {}) = False
hsExprNeedsParens (HsDo sc _ _)
       | isListCompExpr sc            = False
hsExprNeedsParens (HsRecFld{})        = False
hsExprNeedsParens (RecordCon{})       = False
hsExprNeedsParens (HsSpliceE{})       = False
hsExprNeedsParens (RecordUpd{})       = False
hsExprNeedsParens (HsWrap _ e)        = hsExprNeedsParens e
hsExprNeedsParens _ = True


isAtomicHsExpr :: HsExpr id -> Bool
-- True of a single token
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsConLikeOut {}) = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsUnboundVar {}) = True
isAtomicHsExpr (HsWrap _ e)      = isAtomicHsExpr e
isAtomicHsExpr (HsPar e)         = isAtomicHsExpr (unLoc e)
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
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        (PostTc id Type) -- type of the arrow expressions f,
                         -- of the form a t t', where arg :: t
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpenB' @'(|'@,
  --         'ApiAnnotation.AnnCloseB' @'|)'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCmdArrForm         -- Command formation,  (| e cmd1 .. cmdn |)
        (LHsExpr id)     -- The operator.
                         -- After type-checking, a type abstraction to be
                         -- applied to the type of the local environment tuple
        LexicalFixity    -- Whether the operator appeared prefix or infix when
                         -- parsed.
        (Maybe Fixity)   -- fixity (filled in by the renamer), for forms that
                         -- were converted from OpApp's by the renamer
        [LHsCmdTop id]   -- argument commands

  | HsCmdApp    (LHsCmd id)
                (LHsExpr id)

  | HsCmdLam    (MatchGroup id (LHsCmd id))     -- kappa
       -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --       'ApiAnnotation.AnnRarrow',

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdPar    (LHsCmd id)                     -- parenthesised command
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --             'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdCase   (LHsExpr id)
                (MatchGroup id (LHsCmd id))     -- bodies are HsCmd's
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnCase',
    --       'ApiAnnotation.AnnOf','ApiAnnotation.AnnOpen' @'{'@,
    --       'ApiAnnotation.AnnClose' @'}'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdIf     (Maybe (SyntaxExpr id))         -- cond function
                (LHsExpr id)                    -- predicate
                (LHsCmd id)                     -- then part
                (LHsCmd id)                     -- else part
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf',
    --       'ApiAnnotation.AnnSemi',
    --       'ApiAnnotation.AnnThen','ApiAnnotation.AnnSemi',
    --       'ApiAnnotation.AnnElse',

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdLet    (LHsLocalBinds id)      -- let(rec)
                (LHsCmd  id)
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet',
    --       'ApiAnnotation.AnnOpen' @'{'@,
    --       'ApiAnnotation.AnnClose' @'}'@,'ApiAnnotation.AnnIn'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdDo     (Located [CmdLStmt id])
                (PostTc id Type)                -- Type of the whole expression
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDo',
    --             'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnSemi',
    --             'ApiAnnotation.AnnVbar',
    --             'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdWrap   HsWrapper
                (HsCmd id)     -- If   cmd :: arg1 --> res
                               --      wrap :: arg1 "->" arg2
                               -- Then (HsCmdWrap wrap cmd) :: arg2 --> res
deriving instance (DataId id) => Data (HsCmd id)

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
  = HsCmdTop (LHsCmd p)
             (PostTc p Type)    -- Nested tuple of inputs on the command's stack
             (PostTc p Type)    -- return type of the command
             (CmdSyntaxTable p) -- See Note [CmdSyntaxTable]
deriving instance (DataId p) => Data (HsCmdTop p)

instance (SourceTextX p, OutputableBndrId p) => Outputable (HsCmd p) where
    ppr cmd = pprCmd cmd

-----------------------
-- pprCmd and pprLCmd call pprDeeper;
-- the underscore versions do not
pprLCmd :: (SourceTextX p, OutputableBndrId p) => LHsCmd p -> SDoc
pprLCmd (L _ c) = pprCmd c

pprCmd :: (SourceTextX p, OutputableBndrId p) => HsCmd p -> SDoc
pprCmd c | isQuietHsCmd c =            ppr_cmd c
         | otherwise      = pprDeeper (ppr_cmd c)

isQuietHsCmd :: HsCmd id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsCmd (HsCmdPar _) = True
-- applications don't display anything themselves
isQuietHsCmd (HsCmdApp _ _) = True
isQuietHsCmd _ = False

-----------------------
ppr_lcmd :: (SourceTextX p, OutputableBndrId p) => LHsCmd p -> SDoc
ppr_lcmd c = ppr_cmd (unLoc c)

ppr_cmd :: forall p. (SourceTextX p, OutputableBndrId p) => HsCmd p -> SDoc
ppr_cmd (HsCmdPar c) = parens (ppr_lcmd c)

ppr_cmd (HsCmdApp c e)
  = let (fun, args) = collect_args c [e] in
    hang (ppr_lcmd fun) 2 (sep (map ppr args))
  where
    collect_args (L _ (HsCmdApp fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)

ppr_cmd (HsCmdLam matches)
  = pprMatches matches

ppr_cmd (HsCmdCase expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of")],
          nest 2 (pprMatches matches) ]

ppr_cmd (HsCmdIf _ e ct ce)
  = sep [hsep [text "if", nest 2 (ppr e), ptext (sLit "then")],
         nest 4 (ppr ct),
         text "else",
         nest 4 (ppr ce)]

-- special case: let ... in let ...
ppr_cmd (HsCmdLet (L _ binds) cmd@(L _ (HsCmdLet _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lcmd cmd]

ppr_cmd (HsCmdLet (L _ binds) cmd)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr cmd)]

ppr_cmd (HsCmdDo (L _ stmts) _)  = pprDo ArrowExpr stmts

ppr_cmd (HsCmdWrap w cmd)
  = pprHsWrapper w (\_ -> parens (ppr_cmd cmd))
ppr_cmd (HsCmdArrApp arrow arg _ HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp arrow arg _ HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_cmd (HsCmdArrApp arrow arg _ HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp arrow arg _ HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_cmd (HsCmdArrForm (L _ (HsVar (L _ v))) _ (Just _) [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc v
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm (L _ (HsVar (L _ v))) Infix _    [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc v
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm (L _ (HsConLikeOut c)) _ (Just _) [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc (conLikeName c)
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm (L _ (HsConLikeOut c)) Infix _    [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc (conLikeName c)
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm op _ _ args)
  = hang (text "(|" <> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <> text "|)")

pprCmdArg :: (SourceTextX p, OutputableBndrId p) => HsCmdTop p -> SDoc
pprCmdArg (HsCmdTop cmd _ _ _)
  = ppr_lcmd cmd

instance (SourceTextX p, OutputableBndrId p) => Outputable (HsCmdTop p) where
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
  = MG { mg_alts    :: Located [LMatch p body]  -- The alternatives
       , mg_arg_tys :: [PostTc p Type]  -- Types of the arguments, t1..tn
       , mg_res_ty  :: PostTc p Type    -- Type of the result, tr
       , mg_origin  :: Origin }
     -- The type is the type of the entire group
     --      t1 -> ... -> tn -> tr
     -- where there are n patterns
deriving instance (Data body,DataId p) => Data (MatchGroup p body)

-- | Located Match
type LMatch id body = Located (Match id body)
-- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when in a
--   list

-- For details on above see note [Api annotations] in ApiAnnotation
data Match p body
  = Match {
        m_ctxt :: HsMatchContext (NameOrRdrName (IdP p)),
          -- See note [m_ctxt in Match]
        m_pats :: [LPat p], -- The patterns
        m_grhss :: (GRHSs p body)
  }
deriving instance (Data body,DataId p) => Data (Match p body)

instance (SourceTextX idR, OutputableBndrId idR, Outputable body)
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

hsLMatchPats :: LMatch id body -> [LPat id]
hsLMatchPats (L _ (Match { m_pats = pats })) = pats

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
      grhssGRHSs :: [LGRHS p body],      -- ^ Guarded RHSs
      grhssLocalBinds :: LHsLocalBinds p -- ^ The where clause
    }
deriving instance (Data body,DataId p) => Data (GRHSs p body)

-- | Located Guarded Right-Hand Side
type LGRHS id body = Located (GRHS id body)

-- | Guarded Right Hand Side.
data GRHS id body = GRHS [GuardLStmt id] -- Guards
                         body            -- Right hand side
deriving instance (Data body,DataId id) => Data (GRHS id body)

-- We know the list must have at least one @Match@ in it.

pprMatches :: (SourceTextX idR, OutputableBndrId idR, Outputable body)
           => MatchGroup idR body -> SDoc
pprMatches MG { mg_alts = matches }
    = vcat (map pprMatch (map unLoc (unLoc matches)))
      -- Don't print the type; it's only a place-holder before typechecking

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprFunBind :: (SourceTextX idR, OutputableBndrId idR, Outputable body)
           => MatchGroup idR body -> SDoc
pprFunBind matches = pprMatches matches

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprPatBind :: forall bndr p body. (SourceTextX p, SourceTextX bndr,
                                   OutputableBndrId bndr,
                                   OutputableBndrId p,
                                   Outputable body)
           => LPat bndr -> GRHSs p body -> SDoc
pprPatBind pat (grhss)
 = sep [ppr pat, nest 2 (pprGRHSs (PatBindRhs :: HsMatchContext (IdP p)) grhss)]

pprMatch :: (SourceTextX idR, OutputableBndrId idR, Outputable body)
         => Match idR body -> SDoc
pprMatch match
  = sep [ sep (herald : map (nest 2 . pprParendLPat) other_pats)
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
                  pp_infix = pprParendLPat pat1 <+> pprInfixOcc fun <+> pprParendLPat pat2

            LambdaExpr -> (char '\\', m_pats match)

            _  -> ASSERT2( null pats1, ppr ctxt $$ ppr pat1 $$ ppr pats1 )
                  (ppr pat1, [])        -- No parens around the single pat

    (pat1:pats1) = m_pats match
    (pat2:pats2) = pats1

pprGRHSs :: (SourceTextX idR, OutputableBndrId idR, Outputable body)
         => HsMatchContext idL -> GRHSs idR body -> SDoc
pprGRHSs ctxt (GRHSs grhss (L _ binds))
  = vcat (map (pprGRHS ctxt . unLoc) grhss)
  -- Print the "where" even if the contents of the binds is empty. Only
  -- EmptyLocalBinds means no "where" keyword
 $$ ppUnless (eqEmptyLocalBinds binds)
      (text "where" $$ nest 4 (pprBinds binds))

pprGRHS :: (SourceTextX idR, OutputableBndrId idR, Outputable body)
        => HsMatchContext idL -> GRHS idR body -> SDoc
pprGRHS ctxt (GRHS [] body)
 =  pp_rhs ctxt body

pprGRHS ctxt (GRHS guards body)
 = sep [vbar <+> interpp'SP guards, pp_rhs ctxt body]

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
  = LastStmt  -- Always the last Stmt in ListComp, MonadComp, PArrComp,
              -- and (after the renamer) DoExpr, MDoExpr
              -- Not used for GhciStmtCtxt, PatGuard, which scope over other stuff
          body
          Bool               -- True <=> return was stripped by ApplicativeDo
          (SyntaxExpr idR)   -- The return operator, used only for
                             -- MonadComp For ListComp, PArrComp, we
                             -- use the baked-in 'return' For DoExpr,
                             -- MDoExpr, we don't apply a 'return' at
                             -- all See Note [Monad Comprehensions] |
                             -- - 'ApiAnnotation.AnnKeywordId' :
                             -- 'ApiAnnotation.AnnLarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | BindStmt (LPat idL)
             body
             (SyntaxExpr idR) -- The (>>=) operator; see Note [The type of bind in Stmts]
             (SyntaxExpr idR) -- The fail operator
             -- The fail operator is noSyntaxExpr
             -- if the pattern match can't fail

             (PostTc idR Type)  -- result type of the function passed to bind;
                                -- that is, S in (>>=) :: Q -> (R -> S) -> T

  -- | 'ApplicativeStmt' represents an applicative expression built with
  -- <$> and <*>.  It is generated by the renamer, and is desugared into the
  -- appropriate applicative expression by the desugarer, but it is intended
  -- to be invisible in error messages.
  --
  -- For full details, see Note [ApplicativeDo] in RnExpr
  --
  | ApplicativeStmt
             [ ( SyntaxExpr idR
               , ApplicativeArg idL idR) ]
                      -- [(<$>, e1), (<*>, e2), ..., (<*>, en)]
             (Maybe (SyntaxExpr idR))  -- 'join', if necessary
             (PostTc idR Type)     -- Type of the body

  | BodyStmt body              -- See Note [BodyStmt]
             (SyntaxExpr idR)  -- The (>>) operator
             (SyntaxExpr idR)  -- The `guard` operator; used only in MonadComp
                               -- See notes [Monad Comprehensions]
             (PostTc idR Type) -- Element type of the RHS (used for arrows)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet'
  --          'ApiAnnotation.AnnOpen' @'{'@,'ApiAnnotation.AnnClose' @'}'@,

  -- For details on above see note [Api annotations] in ApiAnnotation
  | LetStmt  (LHsLocalBindsLR idL idR)

  -- ParStmts only occur in a list/monad comprehension
  | ParStmt  [ParStmtBlock idL idR]
             (HsExpr idR)               -- Polymorphic `mzip` for monad comprehensions
             (SyntaxExpr idR)           -- The `>>=` operator
                                        -- See notes [Monad Comprehensions]
             (PostTc idR Type)          -- S in (>>=) :: Q -> (R -> S) -> T
            -- After renaming, the ids are the binders
            -- bound by the stmts and used after themp

  | TransStmt {
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
      trS_bind_arg_ty :: PostTc idR Type,  -- R in (>>=) :: Q -> (R -> S) -> T
      trS_fmap :: HsExpr idR          -- The polymorphic 'fmap' function for desugaring
                                      -- Only for 'group' forms
                                      -- Just a simple HsExpr, because it's
                                      -- too polymorphic for tcSyntaxOp
    }                                 -- See Note [Monad Comprehensions]

  -- Recursive statement (see Note [How RecStmt works] below)
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRec'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecStmt
     { recS_stmts :: [LStmtLR idL idR body]

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
     , recS_bind_ty :: PostTc idR Type  -- S in (>>=) :: Q -> (R -> S) -> T

        -- These fields are only valid after typechecking
     , recS_later_rets :: [PostTcExpr] -- (only used in the arrow version)
     , recS_rec_rets :: [PostTcExpr] -- These expressions correspond 1-to-1
                                     -- with recS_later_ids and recS_rec_ids,
                                     -- and are the expressions that should be
                                     -- returned by the recursion.
                                     -- They may not quite be the Ids themselves,
                                     -- because the Id may be *polymorphic*, but
                                     -- the returned thing has to be *monomorphic*,
                                     -- so they may be type applications

      , recS_ret_ty :: PostTc idR Type -- The type of
                                       -- do { stmts; return (a,b,c) }
                                   -- With rebindable syntax the type might not
                                   -- be quite as simple as (m (tya, tyb, tyc)).
      }
deriving instance (Data body, DataId idL, DataId idR)
  => Data (StmtLR idL idR body)

data TransForm   -- The 'f' below is the 'using' function, 'e' is the by function
  = ThenForm     -- then f               or    then f by e             (depending on trS_by)
  | GroupForm    -- then group using f   or    then group by e using f (depending on trS_by)
  deriving Data

-- | Parenthesised Statement Block
data ParStmtBlock idL idR
  = ParStmtBlock
        [ExprLStmt idL]
        [IdP idR]          -- The variables to be returned
        (SyntaxExpr idR)   -- The return operator
deriving instance (DataId idL, DataId idR) => Data (ParStmtBlock idL idR)

-- | Applicative Argument
data ApplicativeArg idL idR
  = ApplicativeArgOne      -- A single statement (BindStmt or BodyStmt)
      (LPat idL)           -- WildPat if it was a BodyStmt (see below)
      (LHsExpr idL)
      Bool                 -- True <=> was a BodyStmt
                           -- False <=> was a BindStmt
                           -- See Note [Applicative BodyStmt]

  | ApplicativeArgMany     -- do { stmts; return vars }
      [ExprLStmt idL]      -- stmts
      (HsExpr idL)         -- return (v1,..,vn), or just (v1,..,vn)
      (LPat idL)           -- (v1,...,vn)

deriving instance (DataId idL, DataId idR) => Data (ApplicativeArg idL idR)

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

instance (SourceTextX idL, OutputableBndrId idL)
       => Outputable (ParStmtBlock idL idR) where
  ppr (ParStmtBlock stmts _ _) = interpp'SP stmts

instance (SourceTextX idL, SourceTextX idR,
          OutputableBndrId idL, OutputableBndrId idR, Outputable body)
         => Outputable (StmtLR idL idR body) where
    ppr stmt = pprStmt stmt

pprStmt :: forall idL idR body . (SourceTextX idL, SourceTextX idR,
                                  OutputableBndrId idL, OutputableBndrId idR,
                                  Outputable body)
        => (StmtLR idL idR body) -> SDoc
pprStmt (LastStmt expr ret_stripped _)
  = whenPprDebug (text "[last]") <+>
       (if ret_stripped then text "return" else empty) <+>
       ppr expr
pprStmt (BindStmt pat expr _ _ _) = hsep [ppr pat, larrow, ppr expr]
pprStmt (LetStmt (L _ binds))     = hsep [text "let", pprBinds binds]
pprStmt (BodyStmt expr _ _ _)     = ppr expr
pprStmt (ParStmt stmtss _ _ _)    = sep (punctuate (text " | ") (map ppr stmtss))

pprStmt (TransStmt { trS_stmts = stmts, trS_by = by
                   , trS_using = using, trS_form = form })
  = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])

pprStmt (RecStmt { recS_stmts = segment, recS_rec_ids = rec_ids
                 , recS_later_ids = later_ids })
  = text "rec" <+>
    vcat [ ppr_do_stmts segment
         , whenPprDebug (vcat [ text "rec_ids=" <> ppr rec_ids
                            , text "later_ids=" <> ppr later_ids])]

pprStmt (ApplicativeStmt args mb_join _)
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
   flattenStmt :: ExprLStmt idL -> [SDoc]
   flattenStmt (L _ (ApplicativeStmt args _ _)) = concatMap flattenArg args
   flattenStmt stmt = [ppr stmt]

   flattenArg (_, ApplicativeArgOne pat expr isBody)
     | isBody =  -- See Note [Applicative BodyStmt]
     [ppr (BodyStmt expr noSyntaxExpr noSyntaxExpr (panic "pprStmt")
             :: ExprStmt idL)]
     | otherwise =
     [ppr (BindStmt pat expr noSyntaxExpr noSyntaxExpr (panic "pprStmt")
             :: ExprStmt idL)]
   flattenArg (_, ApplicativeArgMany stmts _ _) =
     concatMap flattenStmt stmts

   pp_debug =
     let
         ap_expr = sep (punctuate (text " |") (map pp_arg args))
     in
       if isNothing mb_join
          then ap_expr
          else text "join" <+> parens ap_expr

   pp_arg (_, ApplicativeArgOne pat expr isBody)
     | isBody =  -- See Note [Applicative BodyStmt]
     ppr (BodyStmt expr noSyntaxExpr noSyntaxExpr (panic "pprStmt")
            :: ExprStmt idL)
     | otherwise =
     ppr (BindStmt pat expr noSyntaxExpr noSyntaxExpr (panic "pprStmt")
            :: ExprStmt idL)
   pp_arg (_, ApplicativeArgMany stmts return pat) =
     ppr pat <+>
     text "<-" <+>
     ppr (HsDo DoExpr (noLoc
                (stmts ++ [noLoc (LastStmt (noLoc return) False noSyntaxExpr)]))
           (error "pprStmt"))

pprTransformStmt :: (SourceTextX p, OutputableBndrId p)
                 => [IdP p] -> LHsExpr p -> Maybe (LHsExpr p) -> SDoc
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

pprDo :: (SourceTextX p, OutputableBndrId p, Outputable body)
      => HsStmtContext any -> [LStmt p body] -> SDoc
pprDo DoExpr        stmts = text "do"  <+> ppr_do_stmts stmts
pprDo GhciStmtCtxt  stmts = text "do"  <+> ppr_do_stmts stmts
pprDo ArrowExpr     stmts = text "do"  <+> ppr_do_stmts stmts
pprDo MDoExpr       stmts = text "mdo" <+> ppr_do_stmts stmts
pprDo ListComp      stmts = brackets    $ pprComp stmts
pprDo PArrComp      stmts = paBrackets  $ pprComp stmts
pprDo MonadComp     stmts = brackets    $ pprComp stmts
pprDo _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

ppr_do_stmts :: (SourceTextX idL, SourceTextX idR,
                 OutputableBndrId idL, OutputableBndrId idR, Outputable body)
             => [LStmtLR idL idR body] -> SDoc
-- Print a bunch of do stmts
ppr_do_stmts stmts = pprDeeperList vcat (map ppr stmts)

pprComp :: (SourceTextX p, OutputableBndrId p, Outputable body)
        => [LStmt p body] -> SDoc
pprComp quals     -- Prints:  body | qual1, ..., qualn
  | Just (initStmts, L _ (LastStmt body _ _)) <- snocView quals
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

pprQuals :: (SourceTextX p, OutputableBndrId p, Outputable body)
         => [LStmt p body] -> SDoc
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
        SpliceDecoration -- Whether $$( ) variant found, for pretty printing
        (IdP id)         -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsUntypedSplice     --  $z  or $(f 4)
        SpliceDecoration -- Whether $( ) variant found, for pretty printing
        (IdP id)         -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsQuasiQuote        -- See Note [Quasi-quote overview] in TcSplice
        (IdP id)         -- Splice point
        (IdP id)         -- Quoter
        SrcSpan          -- The span of the enclosed string
        FastString       -- The enclosed string

   | HsSpliced  -- See Note [Delaying modFinalizers in untyped splices] in
                -- RnSplice.
                -- This is the result of splicing a splice. It is produced by
                -- the renamer and consumed by the typechecker. It lives only
                -- between the two.
        ThModFinalizers     -- TH finalizers produced by the splice.
        (HsSplicedThing id) -- The result of splicing
  deriving Typeable
deriving instance (DataId id) => Data (HsSplice id)

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

-- | Haskell Spliced Thing
--
-- Values that can result from running a splice.
data HsSplicedThing id
    = HsSplicedExpr (HsExpr id) -- ^ Haskell Spliced Expression
    | HsSplicedTy   (HsType id) -- ^ Haskell Spliced Type
    | HsSplicedPat  (Pat id)    -- ^ Haskell Spliced Pattern
  deriving Typeable

deriving instance (DataId id) => Data (HsSplicedThing id)

-- See Note [Pending Splices]
type SplicePointName = Name

-- | Pending Renamer Splice
data PendingRnSplice
  -- AZ:TODO: The hard-coded GhcRn feels wrong. How to force the PostRn?
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LHsExpr GhcRn)
  deriving Data

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice
  deriving Data

-- | Pending Type-checker Splice
data PendingTcSplice
  -- AZ:TODO: The hard-coded GhcTc feels wrong. How to force the PostTc?
  = PendingTcSplice SplicePointName (LHsExpr GhcTc)
  deriving Data


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

instance (SourceTextX p, OutputableBndrId p)
       => Outputable (HsSplicedThing p) where
  ppr (HsSplicedExpr e) = ppr_expr e
  ppr (HsSplicedTy   t) = ppr t
  ppr (HsSplicedPat  p) = ppr p

instance (SourceTextX p, OutputableBndrId p) => Outputable (HsSplice p) where
  ppr s = pprSplice s

pprPendingSplice :: (SourceTextX p, OutputableBndrId p)
                 => SplicePointName -> LHsExpr p -> SDoc
pprPendingSplice n e = angleBrackets (ppr n <> comma <+> ppr e)

pprSpliceDecl ::  (SourceTextX p, OutputableBndrId p)
          => HsSplice p -> SpliceExplicitFlag -> SDoc
pprSpliceDecl e@HsQuasiQuote{} _ = pprSplice e
pprSpliceDecl e ExplicitSplice   = text "$(" <> ppr_splice_decl e <> text ")"
pprSpliceDecl e ImplicitSplice   = ppr_splice_decl e

ppr_splice_decl :: (SourceTextX p, OutputableBndrId p) => HsSplice p -> SDoc
ppr_splice_decl (HsUntypedSplice _ n e) = ppr_splice empty n e empty
ppr_splice_decl e = pprSplice e

pprSplice :: (SourceTextX p, OutputableBndrId p) => HsSplice p -> SDoc
pprSplice (HsTypedSplice HasParens  n e)
  = ppr_splice (text "$$(") n e (text ")")
pprSplice (HsTypedSplice HasDollar n e)
  = ppr_splice (text "$$") n e empty
pprSplice (HsTypedSplice NoParens n e)
  = ppr_splice empty n e empty
pprSplice (HsUntypedSplice HasParens  n e)
  = ppr_splice (text "$(") n e (text ")")
pprSplice (HsUntypedSplice HasDollar n e)
  = ppr_splice (text "$")  n e empty
pprSplice (HsUntypedSplice NoParens n e)
  = ppr_splice empty  n e empty
pprSplice (HsQuasiQuote n q _ s)      = ppr_quasi n q s
pprSplice (HsSpliced _ thing)         = ppr thing

ppr_quasi :: OutputableBndr p => p -> p -> FastString -> SDoc
ppr_quasi n quoter quote = whenPprDebug (brackets (ppr n)) <>
                           char '[' <> ppr quoter <> vbar <>
                           ppr quote <> text "|]"

ppr_splice :: (SourceTextX p, OutputableBndrId p)
           => SDoc -> (IdP p) -> LHsExpr p -> SDoc -> SDoc
ppr_splice herald n e trail
    = herald <> whenPprDebug (brackets (ppr n)) <> ppr e <> trail

-- | Haskell Bracket
data HsBracket p = ExpBr (LHsExpr p)    -- [|  expr  |]
                  | PatBr (LPat p)      -- [p| pat   |]
                  | DecBrL [LHsDecl p]  -- [d| decls |]; result of parser
                  | DecBrG (HsGroup p)  -- [d| decls |]; result of renamer
                  | TypBr (LHsType p)   -- [t| type  |]
                  | VarBr Bool (IdP p)  -- True: 'x, False: ''T
                                 -- (The Bool flag is used only in pprHsBracket)
                  | TExpBr (LHsExpr p)  -- [||  expr  ||]
deriving instance (DataId p) => Data (HsBracket p)

isTypedBracket :: HsBracket id -> Bool
isTypedBracket (TExpBr {}) = True
isTypedBracket _           = False

instance (SourceTextX p, OutputableBndrId p) => Outputable (HsBracket p) where
  ppr = pprHsBracket


pprHsBracket :: (SourceTextX p, OutputableBndrId p) => HsBracket p -> SDoc
pprHsBracket (ExpBr e)   = thBrackets empty (ppr e)
pprHsBracket (PatBr p)   = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBrG gp) = thBrackets (char 'd') (ppr gp)
pprHsBracket (DecBrL ds) = thBrackets (char 'd') (vcat (map ppr ds))
pprHsBracket (TypBr t)   = thBrackets (char 't') (ppr t)
pprHsBracket (VarBr True n)
  = char '\'' <> pprPrefixOcc n
pprHsBracket (VarBr False n)
  = text "''" <> pprPrefixOcc n
pprHsBracket (TExpBr e)  = thTyBrackets (ppr e)

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
deriving instance (DataId id) => Data (ArithSeqInfo id)

instance (SourceTextX p, OutputableBndrId p)
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
  | PArrComp                         -- ^Parallel array comprehension

  | DoExpr                           -- ^do { ... }
  | MDoExpr                          -- ^mdo { ... }  ie recursive do-expression
  | ArrowExpr                        -- ^do-notation in an arrow-command context

  | GhciStmtCtxt                     -- ^A command-line Stmt in GHCi pat <- rhs
  | PatGuard (HsMatchContext id)     -- ^Pattern guard for specified thing
  | ParStmtCtxt (HsStmtContext id)   -- ^A branch of a parallel stmt
  | TransStmtCtxt (HsStmtContext id) -- ^A branch of a transform stmt
  deriving Functor
deriving instance (Data id) => Data (HsStmtContext id)

isListCompExpr :: HsStmtContext id -> Bool
-- Uses syntax [ e | quals ]
isListCompExpr ListComp          = True
isListCompExpr PArrComp          = True
isListCompExpr MonadComp         = True
isListCompExpr (ParStmtCtxt c)   = isListCompExpr c
isListCompExpr (TransStmtCtxt c) = isListCompExpr c
isListCompExpr _ = False

-- | Should pattern match failure in a 'HsStmtContext' be desugared using
-- 'MonadFail'?
isMonadFailStmtContext :: HsStmtContext id -> Bool
isMonadFailStmtContext MonadComp            = True
isMonadFailStmtContext DoExpr               = True
isMonadFailStmtContext MDoExpr              = True
isMonadFailStmtContext GhciStmtCtxt         = True
isMonadFailStmtContext (ParStmtCtxt ctxt)   = isMonadFailStmtContext ctxt
isMonadFailStmtContext (TransStmtCtxt ctxt) = isMonadFailStmtContext ctxt
isMonadFailStmtContext _ = False -- ListComp, PArrComp, PatGuard, ArrowExpr

matchSeparator :: HsMatchContext id -> SDoc
matchSeparator (FunRhs {})  = text "="
matchSeparator CaseAlt      = text "->"
matchSeparator IfAlt        = text "->"
matchSeparator LambdaExpr   = text "->"
matchSeparator ProcExpr     = text "->"
matchSeparator PatBindRhs   = text "="
matchSeparator (StmtCtxt _) = text "<-"
matchSeparator RecUpd       = text "=" -- This can be printed by the pattern
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
pprMatchContextNoun LambdaExpr      = text "lambda abstraction"
pprMatchContextNoun ProcExpr        = text "arrow abstraction"
pprMatchContextNoun (StmtCtxt ctxt) = text "pattern binding in"
                                      $$ pprStmtContext ctxt
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
                  PArrComp      -> pp_an
                  GhciStmtCtxt  -> pp_an
                  _             -> pp_a


-----------------
pprStmtContext GhciStmtCtxt    = text "interactive GHCi command"
pprStmtContext DoExpr          = text "'do' block"
pprStmtContext MDoExpr         = text "'mdo' block"
pprStmtContext ArrowExpr       = text "'do' block in an arrow command"
pprStmtContext ListComp        = text "list comprehension"
pprStmtContext MonadComp       = text "monad comprehension"
pprStmtContext PArrComp        = text "array comprehension"
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
matchContextErrString (StmtCtxt PArrComp)          = text "array comprehension"

pprMatchInCtxt :: (SourceTextX idR, OutputableBndrId idR,
                   -- TODO:AZ these constraints do not make sense
                   Outputable (NameOrRdrName (NameOrRdrName (IdP idR))),
                   Outputable body)
               => Match idR body -> SDoc
pprMatchInCtxt match  = hang (text "In" <+> pprMatchContext (m_ctxt match)
                                        <> colon)
                             4 (pprMatch match)

pprStmtInCtxt :: (SourceTextX idL, SourceTextX idR,
                  OutputableBndrId idL, OutputableBndrId idR,
                  Outputable body)
               => HsStmtContext (IdP idL) -> StmtLR idL idR body -> SDoc
pprStmtInCtxt ctxt (LastStmt e _ _)
  | isListCompExpr ctxt      -- For [ e | .. ], do not mutter about "stmts"
  = hang (text "In the expression:") 2 (ppr e)

pprStmtInCtxt ctxt stmt
  = hang (text "In a stmt of" <+> pprAStmtContext ctxt <> colon)
       2 (ppr_stmt stmt)
  where
    -- For Group and Transform Stmts, don't print the nested stmts!
    ppr_stmt (TransStmt { trS_by = by, trS_using = using
                        , trS_form = form }) = pprTransStmt by using form
    ppr_stmt stmt = pprStmt stmt

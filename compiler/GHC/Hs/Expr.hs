{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | Abstract Haskell syntax for expressions.
module GHC.Hs.Expr where

#include "HsVersions.h"

-- friends:
import GHC.Prelude

import GHC.Hs.Decls
import GHC.Hs.Pat
import GHC.Hs.Lit
import GHC.Hs.Extension
import GHC.Hs.Type
import GHC.Hs.Binds
import GHC.Parser.Annotation

-- others:
import GHC.Tc.Types.Evidence
import GHC.Core
import GHC.Types.Id( Id )
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Core.ConLike
import GHC.Unit.Module (ModuleName)
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import GHC.Core.Type
import GHC.Builtin.Types (mkTupleStr)
import GHC.Tc.Utils.TcType (TcType)
import {-# SOURCE #-} GHC.Tc.Types (TcLclEnv)

-- libraries:
import Data.Data hiding (Fixity(..))
import qualified Data.Data as Data (Fixity(..))
import qualified Data.Kind
import Data.Maybe (isJust)

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
type LHsExpr p = XRec p (HsExpr p)
-- type LHsExpr p = LocatedA (HsExpr p)
                       -- AZ: old one
  -- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnComma' when
  --   in a list

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation

type instance Anno (HsExpr (GhcPass p)) = SrcSpanAnnA


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
{- Note [NoSyntaxExpr]
~~~~~~~~~~~~~~~~~~~~~~
Syntax expressions can be missing (NoSyntaxExprRn or NoSyntaxExprTc)
for several reasons:

 1. As described in Note [Rebindable if]

 2. In order to suppress "not in scope: xyz" messages when a bit of
    rebindable syntax does not apply. For example, when using an irrefutable
    pattern in a BindStmt, we don't need a `fail` operator.

 3. Rebindable syntax might just not make sense. For example, a BodyStmt
    contains the syntax for `guard`, but that's used only in monad comprehensions.
    If we had more of a whiz-bang type system, we might be able to rule this
    case out statically.
-}

-- | Syntax Expression
--
-- SyntaxExpr is represents the function used in interpreting rebindable
-- syntax. In the parser, we have no information to supply; in the renamer,
-- we have the name of the function (but see
-- Note [Monad fail : Rebindable syntax, overloaded strings] for a wrinkle)
-- and in the type-checker we have a more elaborate structure 'SyntaxExprTc'.
--
-- In some contexts, rebindable syntax is not implemented, and so we have
-- constructors to represent that possibility in both the renamer and
-- typechecker instantiations.
--
-- E.g. @(>>=)@ is filled in before the renamer by the appropriate 'Name' for
--      @(>>=)@, and then instantiated by the type checker with its type args
--      etc
type family SyntaxExpr p

-- Defining SyntaxExpr in two stages allows for better type inference, because
-- we can declare SyntaxExprGhc to be injective (and closed). Without injectivity,
-- noSyntaxExpr would be ambiguous.
type instance SyntaxExpr (GhcPass p) = SyntaxExprGhc p

type family SyntaxExprGhc (p :: Pass) = (r :: Data.Kind.Type) | r -> p where
  SyntaxExprGhc 'Parsed      = NoExtField
  SyntaxExprGhc 'Renamed     = SyntaxExprRn
  SyntaxExprGhc 'Typechecked = SyntaxExprTc

-- | The function to use in rebindable syntax. See Note [NoSyntaxExpr].
data SyntaxExprRn = SyntaxExprRn (HsExpr GhcRn)
    -- Why is the payload not just a Name?
    -- See Note [Monad fail : Rebindable syntax, overloaded strings] in "GHC.Rename.Expr"
                  | NoSyntaxExprRn

-- | An expression with wrappers, used for rebindable syntax
--
-- This should desugar to
--
-- > syn_res_wrap $ syn_expr (syn_arg_wraps[0] arg0)
-- >                         (syn_arg_wraps[1] arg1) ...
--
-- where the actual arguments come from elsewhere in the AST.
data SyntaxExprTc = SyntaxExprTc { syn_expr      :: HsExpr GhcTc
                                 , syn_arg_wraps :: [HsWrapper]
                                 , syn_res_wrap  :: HsWrapper }
                  | NoSyntaxExprTc  -- See Note [NoSyntaxExpr]

-- | This is used for rebindable-syntax pieces that are too polymorphic
-- for tcSyntaxOp (trS_fmap and the mzip in ParStmt)
noExpr :: HsExpr (GhcPass p)
noExpr = HsLit noComments (HsString (SourceText  "noExpr") (fsLit "noExpr"))

noSyntaxExpr :: forall p. IsPass p => SyntaxExpr (GhcPass p)
                              -- Before renaming, and sometimes after
                              -- See Note [NoSyntaxExpr]
noSyntaxExpr = case ghcPass @p of
  GhcPs -> noExtField
  GhcRn -> NoSyntaxExprRn
  GhcTc -> NoSyntaxExprTc

-- | Make a 'SyntaxExpr GhcRn' from an expression
-- Used only in getMonadFailOp.
-- See Note [Monad fail : Rebindable syntax, overloaded strings] in "GHC.Rename.Expr"
mkSyntaxExpr :: HsExpr GhcRn -> SyntaxExprRn
mkSyntaxExpr = SyntaxExprRn

-- | Make a 'SyntaxExpr' from a 'Name' (the "rn" is because this is used in the
-- renamer).
mkRnSyntaxExpr :: Name -> SyntaxExprRn
mkRnSyntaxExpr name = SyntaxExprRn $ HsVar noExtField $ noLocA name

instance Outputable SyntaxExprRn where
  ppr (SyntaxExprRn expr) = ppr expr
  ppr NoSyntaxExprRn      = text "<no syntax expr>"

instance Outputable SyntaxExprTc where
  ppr (SyntaxExprTc { syn_expr      = expr
                    , syn_arg_wraps = arg_wraps
                    , syn_res_wrap  = res_wrap })
    = sdocOption sdocPrintExplicitCoercions $ \print_co ->
      getPprDebug $ \debug ->
      if debug || print_co
      then ppr expr <> braces (pprWithCommas ppr arg_wraps)
                    <> braces (ppr res_wrap)
      else ppr expr

  ppr NoSyntaxExprTc = text "<no syntax expr>"

-- | Command Syntax Table (for Arrow syntax)
type CmdSyntaxTable p = [(Name, HsExpr p)]
-- See Note [CmdSyntaxTable]

{-
Note [CmdSyntaxTable]
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
    See the tedious GHC.Rename.Expr.methodNamesCmd.

  * The desugarer has to know the polymorphic type of the instantiated
    method. This is checked by Inst.tcSyntaxName, but is less flexible
    than the rest of rebindable syntax, where the type is less
    pre-ordained.  (And this flexibility is useful; for example we can
    typecheck do-notation with (>>=) :: m1 a -> (a -> m2 b) -> m2 b.)
-}

-- | A Haskell expression.
data HsExpr p
  = HsVar     (XVar p)
              (LIdP p) -- ^ Variable
              -- (LocatedN (IdP p)) -- ^ Variable
                       -- AZ: old one

                             -- See Note [Located RdrNames]

  | HsUnboundVar (XUnboundVar p)
                 OccName     -- ^ Unbound variable; also used for "holes"
                             --   (_ or _x).
                             -- Turned from HsVar to HsUnboundVar by the
                             --   renamer, when it finds an out-of-scope
                             --   variable or hole.
                             -- The (XUnboundVar p) field becomes Id
                             --   after typechecking

  | HsConLikeOut (XConLikeOut p)
                 ConLike     -- ^ After typechecker only; must be different
                             -- HsVar for pretty printing

  | HsRecFld  (XRecFld p)
              (AmbiguousFieldOcc p) -- ^ Variable pointing to record selector
              -- The parser produces HsVars
              -- The renamer renames record-field selectors to HsRecFld
              -- The typechecker preserves HsRecFld

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
       -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnLam',
       --       'GHC.Parser.Annotation.AnnRarrow',

       -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsLamCase (XLamCase p) (MatchGroup p (LHsExpr p)) -- ^ Lambda-case
       --
       -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnLam',
       --           'GHC.Parser.Annotation.AnnCase','GHC.Parser.Annotation.AnnOpen',
       --           'GHC.Parser.Annotation.AnnClose'

       -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsApp     (XApp p) (LHsExpr p) (LHsExpr p) -- ^ Application

  | HsAppType (XAppTypeE p) -- After typechecking: the type argument
              (LHsExpr p)
              (LHsWcType (NoGhcTc p))  -- ^ Visible type application
       --
       -- Explicit type argument; e.g  f @Int x y
       -- NB: Has wildcards, but no implicit quantification
       --
       -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnAt',

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
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnMinus'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | NegApp      (XNegApp p)
                (LHsExpr p)
                (SyntaxExpr p)

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'('@,
  --             'GHC.Parser.Annotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
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
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
  --         'GHC.Parser.Annotation.AnnClose'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  -- Note [ExplicitTuple]
  | ExplicitTuple
        (XExplicitTuple p)
        [HsTupArg p]
        Boxity

  -- | Used for unboxed sum types
  --
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'(#'@,
  --          'GHC.Parser.Annotation.AnnVbar', 'GHC.Parser.Annotation.AnnClose' @'#)'@,
  --
  --  There will be multiple 'GHC.Parser.Annotation.AnnVbar', (1 - alternative) before
  --  the expression, (arity - alternative) after it
  | ExplicitSum
          (XExplicitSum p)
          ConTag --  Alternative (one-based)
          Arity  --  Sum arity
          (LHsExpr p)

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnCase',
  --       'GHC.Parser.Annotation.AnnOf','GHC.Parser.Annotation.AnnOpen' @'{'@,
  --       'GHC.Parser.Annotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsCase      (XCase p)
                (LHsExpr p)
                (MatchGroup p (LHsExpr p))

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnIf',
  --       'GHC.Parser.Annotation.AnnSemi',
  --       'GHC.Parser.Annotation.AnnThen','GHC.Parser.Annotation.AnnSemi',
  --       'GHC.Parser.Annotation.AnnElse',

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsIf        (XIf p)        -- GhcPs: this is a Bool; False <=> do not use
                               --  rebindable syntax
                (LHsExpr p)    --  predicate
                (LHsExpr p)    --  then part
                (LHsExpr p)    --  else part

  -- | Multi-way if
  --
  -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnIf'
  --       'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnClose',

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsMultiIf   (XMultiIf p) [LGRHS p (LHsExpr p)]

  -- | let(rec)
  --
  -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnLet',
  --       'GHC.Parser.Annotation.AnnOpen' @'{'@,
  --       'GHC.Parser.Annotation.AnnClose' @'}'@,'GHC.Parser.Annotation.AnnIn'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsLet       (XLet p)
                (HsLocalBinds p)
                (LHsExpr  p)

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDo',
  --             'GHC.Parser.Annotation.AnnOpen', 'GHC.Parser.Annotation.AnnSemi',
  --             'GHC.Parser.Annotation.AnnVbar',
  --             'GHC.Parser.Annotation.AnnClose'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsDo        (XDo p)                  -- Type of the whole expression
                (HsStmtContext GhcRn)    -- The parameterisation is unimportant
                                         -- because in this context we never use
                                         -- the PatGuard or ParStmt variant
                (XRec p [ExprLStmt p])   -- "do":one or more stmts
                -- (LocatedL [ExprLStmt p]) -- "do":one or more stmts
                       -- AZ: old one

  -- | Syntactic list: [a,b,c,...]
  --
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'['@,
  --              'GHC.Parser.Annotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  -- See Note [Empty lists]
  | ExplicitList
                (XExplicitList p)  -- Gives type of components of list
                (Maybe (SyntaxExpr p))
                                   -- For OverloadedLists, the fromListN witness
                [LHsExpr p]

  -- | Record construction
  --
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'{'@,
  --         'GHC.Parser.Annotation.AnnDotdot','GHC.Parser.Annotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | RecordCon
      { rcon_ext      :: XRecordCon p
      , rcon_con_name :: LIdP p             -- The constructor name;
      -- , rcon_con_name :: LocatedN (IdP p)   -- The constructor name;
                       -- AZ: old one
                                            --  not used after type checking
      , rcon_flds     :: HsRecordBinds p }  -- The fields

  -- | Record update
  --
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'{'@,
  --         'GHC.Parser.Annotation.AnnDotdot','GHC.Parser.Annotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | RecordUpd
      { rupd_ext  :: XRecordUpd p
      , rupd_expr :: LHsExpr p
      , rupd_flds :: [LHsRecUpdField p]
      }
  -- For a type family, the arg types are of the *instance* tycon,
  -- not the family tycon

  -- | Expression with an explicit type signature. @e :: type@
  --
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | ExprWithTySig
                (XExprWithTySig p)

                (LHsExpr p)
                (LHsSigWcType (NoGhcTc p))

  -- | Arithmetic sequence
  --
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'['@,
  --              'GHC.Parser.Annotation.AnnComma','GHC.Parser.Annotation.AnnDotdot',
  --              'GHC.Parser.Annotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | ArithSeq
                (XArithSeq p)
                (Maybe (SyntaxExpr p))
                                  -- For OverloadedLists, the fromList witness
                (ArithSeqInfo p)

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  -----------------------------------------------------------
  -- MetaHaskell Extensions

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
  --         'GHC.Parser.Annotation.AnnOpenE','GHC.Parser.Annotation.AnnOpenEQ',
  --         'GHC.Parser.Annotation.AnnClose','GHC.Parser.Annotation.AnnCloseQ'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsBracket    (XBracket p) (HsBracket p)

    -- See Note [Pending Splices]
  | HsRnBracketOut
      (XRnBracketOut p)
      (HsBracket GhcRn)    -- Output of the renamer is the *original* renamed
                           -- expression, plus
      [PendingRnSplice]    -- _renamed_ splices to be type checked

  | HsTcBracketOut
      (XTcBracketOut p)
      (Maybe QuoteWrapper) -- The wrapper to apply type and dictionary argument
                           -- to the quote.
      (HsBracket GhcRn)    -- Output of the type checker is the *original*
                           -- renamed expression, plus
      [PendingTcSplice]    -- _typechecked_ splices to be
                           -- pasted back in by the desugarer

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
  --         'GHC.Parser.Annotation.AnnClose'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsSpliceE  (XSpliceE p) (HsSplice p)

  -----------------------------------------------------------
  -- Arrow notation extension

  -- | @proc@ notation for Arrows
  --
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnProc',
  --          'GHC.Parser.Annotation.AnnRarrow'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsProc      (XProc p)
                (LPat p)               -- arrow abstraction, proc
                (LHsCmdTop p)          -- body of the abstraction
                                       -- always has an empty stack

  ---------------------------------------
  -- static pointers extension
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnStatic',

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | HsStatic (XStatic p) -- Free variables of the body
             (LHsExpr p)        -- Body

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

  ---------------------------------------
  -- Expressions annotated with pragmas, written as {-# ... #-}
  | HsPragE (XPragE p) (HsPragE p) (LHsExpr p)

  | XExpr       !(XXExpr p)
  -- Note [Trees that Grow] extension constructor for the
  -- general idea, and Note [Rebindable syntax and HsExpansion]
  -- for an example of how we use it.

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

      , rupd_in_tys  :: [Type]  -- Argument types of *input* record type
      , rupd_out_tys :: [Type]  --             and  *output* record type
                -- For a data family, these are the type args of the
                -- /representation/ type constructor

      , rupd_wrap :: HsWrapper  -- See note [Record Update HsWrapper]
      }

-- | HsWrap appears only in typechecker output
-- Invariant: The contained Expr is *NOT* itself an HsWrap.
-- See Note [Detecting forced eta expansion] in "GHC.HsToCore.Expr".
-- This invariant is maintained by 'GHC.Hs.Utils.mkHsWrap'.
-- hs_syn is something like HsExpr or HsCmd
data HsWrap hs_syn = HsWrap HsWrapper      -- the wrapper
                            (hs_syn GhcTc) -- the thing that is wrapped

deriving instance (Data (hs_syn GhcTc), Typeable hs_syn) => Data (HsWrap hs_syn)

-- ---------------------------------------------------------------------

-- API Annotations types

data ApiAnnHsCase = ApiAnnHsCase
      { hsCaseAnnCase :: RealSrcSpan
      , hsCaseAnnOf   :: RealSrcSpan
      , hsCaseAnnsRest :: [AddApiAnn]
      } deriving Data

data ApiAnnUnboundVar = ApiAnnUnboundVar
     { hsUnboundBackquotes :: (RealSrcSpan, RealSrcSpan)
     , hsUnboundHole       :: RealSrcSpan
     } deriving Data

-- ---------------------------------------------------------------------

type instance XVar           (GhcPass _) = NoExtField

type instance XUnboundVar    GhcPs = ApiAnn' ApiAnnUnboundVar
type instance XUnboundVar    GhcRn = NoExtField
type instance XUnboundVar    GhcTc = Id

type instance XConLikeOut    (GhcPass _) = NoExtField
type instance XRecFld        (GhcPass _) = NoExtField
type instance XOverLabel     (GhcPass _) = ApiAnnCO
type instance XIPVar         (GhcPass _) = ApiAnnCO
type instance XOverLitE      (GhcPass _) = ApiAnnCO
type instance XLitE          (GhcPass _) = ApiAnnCO

type instance XLam           (GhcPass _) = NoExtField

type instance XLamCase       (GhcPass _) = ApiAnn
type instance XApp           (GhcPass _) = ApiAnnCO

type instance XAppTypeE      GhcPs = ApiAnn
type instance XAppTypeE      GhcRn = NoExtField
type instance XAppTypeE      GhcTc = Type

type instance XOpApp         GhcPs = ApiAnn
type instance XOpApp         GhcRn = Fixity
type instance XOpApp         GhcTc = Fixity

type instance XNegApp        GhcPs = ApiAnn
type instance XNegApp        GhcRn = NoExtField
type instance XNegApp        GhcTc = NoExtField

type instance XPar           (GhcPass _) = ApiAnn' AnnParen
type instance XSectionL      (GhcPass _) = ApiAnnCO
type instance XSectionR      (GhcPass _) = ApiAnnCO

type instance XExplicitTuple GhcPs = ApiAnn
type instance XExplicitTuple GhcRn = NoExtField
type instance XExplicitTuple GhcTc = NoExtField

type instance XExplicitSum   GhcPs = ApiAnn' AnnExplicitSum
type instance XExplicitSum   GhcRn = NoExtField
type instance XExplicitSum   GhcTc = [Type]

type instance XCase          GhcPs = ApiAnn' ApiAnnHsCase
type instance XCase          GhcRn = NoExtField
type instance XCase          GhcTc = NoExtField

type instance XIf            GhcPs = ApiAnn
type instance XIf            GhcRn = NoExtField
type instance XIf            GhcTc = NoExtField

type instance XMultiIf       GhcPs = ApiAnn
type instance XMultiIf       GhcRn = NoExtField
type instance XMultiIf       GhcTc = Type

type instance XLet           GhcPs = ApiAnn
type instance XLet           GhcRn = NoExtField
type instance XLet           GhcTc = NoExtField

type instance XDo            GhcPs = ApiAnn' AnnList
type instance XDo            GhcRn = NoExtField
type instance XDo            GhcTc = Type

type instance XExplicitList  GhcPs = ApiAnn' AnnList
type instance XExplicitList  GhcRn = NoExtField
type instance XExplicitList  GhcTc = Type

type instance XRecordCon     GhcPs = ApiAnn
type instance XRecordCon     GhcRn = NoExtField
type instance XRecordCon     GhcTc = RecordConTc

type instance XRecordUpd     GhcPs = ApiAnn
type instance XRecordUpd     GhcRn = NoExtField
type instance XRecordUpd     GhcTc = RecordUpdTc

type instance XExprWithTySig GhcPs = ApiAnn
type instance XExprWithTySig GhcRn = NoExtField
type instance XExprWithTySig GhcTc = NoExtField

type instance XArithSeq      GhcPs = ApiAnn
type instance XArithSeq      GhcRn = NoExtField
type instance XArithSeq      GhcTc = PostTcExpr

type instance XBracket       (GhcPass _) = ApiAnn

type instance XRnBracketOut  (GhcPass _) = NoExtField
type instance XTcBracketOut  (GhcPass _) = NoExtField

type instance XSpliceE       (GhcPass _) = ApiAnnCO
type instance XProc          (GhcPass _) = ApiAnn

type instance XStatic        GhcPs = ApiAnn
type instance XStatic        GhcRn = NameSet
type instance XStatic        GhcTc = NameSet

type instance XTick          (GhcPass _) = NoExtField
type instance XBinTick       (GhcPass _) = NoExtField

type instance XPragE         (GhcPass _) = NoExtField

type instance XXExpr         GhcPs       = NoExtCon

-- See Note [Rebindable syntax and HsExpansion] below
type instance XXExpr         GhcRn       = HsExpansion (HsExpr GhcRn)
                                                       (HsExpr GhcRn)
type instance XXExpr         GhcTc       = XXExprGhcTc


                -- (XRec p [ExprLStmt p])   -- "do":one or more stmts
                -- (LocatedL [ExprLStmt p]) -- "do":one or more stmts
type instance Anno [LocatedA ((StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (HsExpr (GhcPass pr)))))] = SrcSpanAnnL
type instance Anno [LocatedA ((StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (HsCmd (GhcPass pr)))))] = SrcSpanAnnL

data XXExprGhcTc
  = WrapExpr {-# UNPACK #-} !(HsWrap HsExpr)
  | ExpansionExpr {-# UNPACK #-} !(HsExpansion (HsExpr GhcRn) (HsExpr GhcTc))

data AnnExplicitSum
  = AnnExplicitSum {
      aesOpen       :: RealSrcSpan,
      aesBarsBefore :: [RealSrcSpan],
      aesBarsAfter  :: [RealSrcSpan],
      aesClose      :: RealSrcSpan
      } deriving Data

{-
Note [Rebindable syntax and HsExpansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We implement rebindable syntax (RS) support by performing a desugaring
in the renamer. We transform GhcPs expressions affected by RS into the
appropriate desugared form, but **annotated with the original expression**.

Let us consider a piece of code like:

    {-# LANGUAGE RebindableSyntax #-}
    ifThenElse :: Char -> () -> () -> ()
    ifThenElse _ _ _ = ()
    x = if 'a' then () else True

The parsed AST for the RHS of x would look something like (slightly simplified):

    L locif (HsIf (L loca 'a') (L loctrue ()) (L locfalse True))

Upon seeing such an AST with RS on, we could transform it into a
mere function call, as per the RS rules, equivalent to the
following function application:

    ifThenElse 'a' () True

which doesn't typecheck. But GHC would report an error about
not being able to match the third argument's type (Bool) with the
expected type: (), in the expression _as desugared_, i.e in
the aforementioned function application. But the user never
wrote a function application! This would be pretty bad.

To remedy this, instead of transforming the original HsIf
node into mere applications of 'ifThenElse', we keep the
original 'if' expression around too, using the TTG
XExpr extension point to allow GHC to construct an
'HsExpansion' value that will keep track of the original
expression in its first field, and the desugared one in the
second field. The resulting renamed AST would look like:

    L locif (XExpr
      (HsExpanded
        (HsIf (L loca 'a')
              (L loctrue ())
              (L locfalse True)
        )
        (App (L generatedSrcSpan
                (App (L generatedSrcSpan
                        (App (L generatedSrcSpan (Var ifThenElse))
                             (L loca 'a')
                        )
                     )
                     (L loctrue ())
                )
             )
             (L locfalse True)
        )
      )
    )

When comes the time to typecheck the program, we end up calling
tcMonoExpr on the AST above. If this expression gives rise to
a type error, then it will appear in a context line and GHC
will pretty-print it using the 'Outputable (HsExpansion a b)'
instance defined below, which *only prints the original
expression*. This is the gist of the idea, but is not quite
enough to recover the error messages that we had with the
SyntaxExpr-based, typechecking/desugaring-to-core time
implementation of rebindable syntax. The key idea is to decorate
some elements of the desugared expression so as to be able to
give them a special treatment when typechecking the desugared
expression, to print a different context line or skip one
altogether.

Whenever we 'setSrcSpan' a 'generatedSrcSpan', we update a field in
TcLclEnv called 'tcl_in_gen_code', setting it to True, which indicates that we
entered generated code, i.e code fabricated by the compiler when rebinding some
syntax. If someone tries to push some error context line while that field is set
to True, the pushing won't actually happen and the context line is just dropped.
Once we 'setSrcSpan' a real span (for an expression that was in the original
source code), we set 'tcl_in_gen_code' back to False, indicating that we
"emerged from the generated code tunnel", and that the expressions we will be
processing are relevant to report in context lines again.

You might wonder why we store a RealSrcSpan in addition to a Bool in
the TcLclEnv: could we not store a Maybe RealSrcSpan? The problem is
that we still generate constraints when processing generated code,
and a CtLoc must contain a RealSrcSpan -- otherwise, error messages
might appear without source locations. So we keep the RealSrcSpan of
the last location spotted that wasn't generated; it's as good as
we're going to get in generated code. Once we get to sub-trees that
are not generated, then we update the RealSrcSpan appropriately, and
set the tcl_in_gen_code Bool to False.

---

A general recipe to follow this approach for new constructs could go as follows:

- Remove any GhcRn-time SyntaxExpr extensions to the relevant constructor for your
  construct, in HsExpr or related syntax data types.
- At renaming-time:
    - take your original node of interest (HsIf above)
    - rename its subexpressions (condition, true branch, false branch above)
    - construct the suitable "rebound"-and-renamed result (ifThenElse call
      above), where the 'SrcSpan' attached to any _fabricated node_ (the
      HsVar/HsApp nodes, above) is set to 'generatedSrcSpan'
    - take both the original node and that rebound-and-renamed result and wrap
      them in an XExpr: XExpr (HsExpanded <original node> <desugared>)
 - At typechecking-time:
    - remove any logic that was previously dealing with your rebindable
      construct, typically involving [tc]SyntaxOp, SyntaxExpr and friends.
    - the XExpr (HsExpanded ... ...) case in tcExpr already makes sure that we
      typecheck the desugared expression while reporting the original one in
      errors

-}

-- See Note [Rebindable syntax and HsExpansion] just above.
data HsExpansion a b
  = HsExpanded a b
  deriving Data

-- | Build a "wrapped" 'HsExpansion' out of an extension constructor,
--   and the two components of the expansion: original and desugared
--   expressions.
--
--   See Note [Rebindable Syntax and HsExpansion] above for more details.
mkExpanded
  :: (HsExpansion a b -> b) -- ^ XExpr, XCmd, ...
  -> a                      -- ^ source expression ('GhcPs')
  -> b                      -- ^ "desugared" expression
                            --   ('GhcRn')
  -> b                      -- ^ suitably wrapped
                            --   'HsExpansion'
mkExpanded xwrap a b = xwrap (HsExpanded a b)

-- | Just print the original expression (the @a@).
instance (Outputable a, Outputable b) => Outputable (HsExpansion a b) where
  ppr (HsExpanded a b) = ifPprDebug (vcat [ppr a, ppr b]) (ppr a)

-- ---------------------------------------------------------------------

-- | A pragma, written as {-# ... #-}, that may appear within an expression.
data HsPragE p
  = HsPragSCC   (XSCC p)
                SourceText            -- Note [Pragma source text] in GHC.Types.SourceText
                StringLiteral         -- "set cost centre" SCC pragma

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
  --       'GHC.Parser.Annotation.AnnOpen' @'{-\# GENERATED'@,
  --       'GHC.Parser.Annotation.AnnVal','GHC.Parser.Annotation.AnnVal',
  --       'GHC.Parser.Annotation.AnnColon','GHC.Parser.Annotation.AnnVal',
  --       'GHC.Parser.Annotation.AnnMinus',
  --       'GHC.Parser.Annotation.AnnVal','GHC.Parser.Annotation.AnnColon',
  --       'GHC.Parser.Annotation.AnnVal',
  --       'GHC.Parser.Annotation.AnnClose' @'\#-}'@

  | XHsPragE !(XXPragE p)

type instance XSCC           (GhcPass _) = ApiAnn' AnnPragma
type instance XXPragE        (GhcPass _) = NoExtCon

-- data ApiAnnPragmaTick = ApiAnnPragmaTick
--       { aprt_open      :: AddApiAnn
--       , aprt_close     :: AddApiAnn
--       , aprt_rest      :: [AddApiAnn]
--       } deriving Data

-- | Haskell Tuple Argument
--
-- 'HsTupArg' is used for tuple sections
-- @(,a,)@ is represented by
-- @ExplicitTuple [Missing ty1, Present a, Missing ty3]@
-- Which in turn stands for @(\x:ty1 \y:ty2. (x,a,y))@
data HsTupArg id
  = Present (XPresent id) (LHsExpr id)     -- ^ The argument
  | Missing (XMissing id)    -- ^ The argument is missing, but this is its type
  | XTupArg !(XXTupArg id)   -- ^ Note [Trees that Grow] extension point

type instance XPresent         (GhcPass _) = ApiAnn

type instance XMissing         GhcPs = ApiAnn' RealSrcSpan
type instance XMissing         GhcRn = NoExtField
type instance XMissing         GhcTc = Scaled Type

type instance XXTupArg         (GhcPass _) = NoExtCon

tupArgPresent :: HsTupArg (GhcPass p) -> Bool
tupArgPresent (Present {}) = True
tupArgPresent (Missing {}) = False

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

So we use NoSyntaxExpr to mean "use the old built-in typing rule".

A further complication is that, in the `deriving` code, we never want
to use rebindable syntax. So, even in GhcPs, we want to denote whether
to use rebindable syntax or not. This is done via the type instance
for XIf GhcPs.

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

Note [ExplicitTuple]
~~~~~~~~~~~~~~~~~~~~
An ExplicitTuple is never just a data constructor like (,,,).
That is, the `[LHsTupArg p]` argument of `ExplicitTuple` has at least
one `Present` member (and is thus never empty).

A tuple data constructor like () or (,,,) is parsed as an `HsVar`, not an
`ExplicitTuple`, and stays that way. This is important for two reasons:

  1. We don't need -XTupleSections for (,,,)
  2. The type variables in (,,,) can be instantiated with visible type application.
     That is,

       (,,)     :: forall a b c. a -> b -> c -> (a,b,c)
       (True,,) :: forall {b} {c}. b -> c -> (Bool,b,c)

     Note that the tuple section has *inferred* arguments, while the data
     constructor has *specified* ones.
     (See Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
     for background.)

Sadly, the grammar for this is actually ambiguous, and it's only thanks to the
preference of a shift in a shift/reduce conflict that the parser works as this
Note details. Search for a reference to this Note in GHC.Parser for further
explanation.

Note [Empty lists]
~~~~~~~~~~~~~~~~~~
An empty list could be considered either a data constructor (stored with
HsVar) or an ExplicitList. This Note describes how empty lists flow through the
various phases and why.

Parsing
-------
An empty list is parsed by the sysdcon nonterminal. It thus comes to life via
HsVar nilDataCon (defined in GHC.Builtin.Types). A freshly-parsed (HsExpr GhcPs) empty list
is never a ExplicitList.

Renaming
--------
If -XOverloadedLists is enabled, we must type-check the empty list as if it
were a call to fromListN. (This is true regardless of the setting of
-XRebindableSyntax.) This is very easy if the empty list is an ExplicitList,
but an annoying special case if it's an HsVar. So the renamer changes a
HsVar nilDataCon to an ExplicitList [], but only if -XOverloadedLists is on.
(Why not always? Read on, dear friend.) This happens in the HsVar case of rnExpr.

Type-checking
-------------
We want to accept an expression like [] @Int. To do this, we must infer that
[] :: forall a. [a]. This is easy if [] is a HsVar with the right DataCon inside.
However, the type-checking for explicit lists works differently: [x,y,z] is never
polymorphic. Instead, we unify the types of x, y, and z together, and use the
unified type as the argument to the cons and nil constructors. Thus, treating
[] as an empty ExplicitList in the type-checker would prevent [] @Int from working.

However, if -XOverloadedLists is on, then [] @Int really shouldn't be allowed:
it's just like fromListN 0 [] @Int. Since
  fromListN :: forall list. IsList list => Int -> [Item list] -> list
that expression really should be rejected. Thus, the renamer's behaviour is
exactly what we want: treat [] as a datacon when -XNoOverloadedLists, and as
an empty ExplicitList when -XOverloadedLists.

See also #13680, which requested [] @Int to work.
-}

instance (OutputableBndrId p) => Outputable (HsExpr (GhcPass p)) where
    ppr expr = pprExpr expr

-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper;
-- the underscore versions do not
pprLExpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc
pprLExpr (L _ e) = pprExpr e

pprExpr :: (OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc
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

pprBinds :: (OutputableBndrId idL, OutputableBndrId idR)
         => HsLocalBindsLR (GhcPass idL) (GhcPass idR) -> SDoc
pprBinds b = pprDeeper (ppr b)

-----------------------
ppr_lexpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)

ppr_expr :: forall p. (OutputableBndrId p)
         => HsExpr (GhcPass p) -> SDoc
ppr_expr (HsVar _ (L _ v))   = pprPrefixOcc v
ppr_expr (HsUnboundVar _ uv) = pprPrefixOcc uv
ppr_expr (HsConLikeOut _ c)  = pprPrefixOcc c
ppr_expr (HsRecFld _ f)      = pprPrefixOcc f
ppr_expr (HsIPVar _ v)       = ppr v
ppr_expr (HsOverLabel _ _ l) = char '#' <> ppr l
ppr_expr (HsLit _ lit)       = ppr lit
ppr_expr (HsOverLit _ lit)   = ppr lit
ppr_expr (HsPar _ e)         = parens (ppr_lexpr e)

ppr_expr (HsPragE _ prag e) = sep [ppr prag, ppr_lexpr e]

ppr_expr e@(HsApp {})        = ppr_apps e []
ppr_expr e@(HsAppType {})    = ppr_apps e []

ppr_expr (OpApp _ e1 op e2)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly

  where
    pp_e1 = pprDebugParendExpr opPrec e1   -- In debug mode, add parens
    pp_e2 = pprDebugParendExpr opPrec e2   -- to make precedence clear

    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])

    pp_infixly pp_op
      = hang pp_e1 2 (sep [pp_op, nest 2 pp_e2])

ppr_expr (NegApp _ e _) = char '-' <+> pprDebugParendExpr appPrec e

ppr_expr (SectionL _ expr op)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly
  where
    pp_expr = pprDebugParendExpr opPrec expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
                       4 (hsep [pp_expr, text "x_ )"])

    pp_infixly v = (sep [pp_expr, v])

ppr_expr (SectionR _ op expr)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly
  where
    pp_expr = pprDebugParendExpr opPrec expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, text "x_"])
                       4 (pp_expr <> rparen)

    pp_infixly v = sep [v, pp_expr]

ppr_expr (ExplicitTuple _ exprs boxity)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `Solo x`, not `(x)`
  | [Present _ expr] <- exprs
  , Boxed <- boxity
  = hsep [text (mkTupleStr Boxed 1), ppr expr]
  | otherwise
  = tupleParens (boxityTupleSort boxity) (fcat (ppr_tup_args exprs))
  where
    ppr_tup_args []               = []
    ppr_tup_args (Present _ e : es) = (ppr_lexpr e <> punc es) : ppr_tup_args es
    ppr_tup_args (Missing _   : es) = punc es : ppr_tup_args es

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

ppr_expr (HsCase _ expr matches@(MG { mg_alts = L _ alts }))
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of")],
          pp_alts ]
  where
    pp_alts | null alts = text "{}"
            | otherwise = nest 2 (pprMatches matches)

ppr_expr (HsIf _ e1 e2 e3)
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
ppr_expr (HsLet _ binds expr@(L _ (HsLet _ _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lexpr expr]

ppr_expr (HsLet _ binds expr)
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

ppr_expr (HsSpliceE _ s)         = pprSplice s
ppr_expr (HsBracket _ b)         = pprHsBracket b
ppr_expr (HsRnBracketOut _ e []) = ppr e
ppr_expr (HsRnBracketOut _ e ps) = ppr e $$ text "pending(rn)" <+> ppr ps
ppr_expr (HsTcBracketOut _ _wrap e []) = ppr e
ppr_expr (HsTcBracketOut _ _wrap e ps) = ppr e $$ text "pending(tc)" <+> pprIfTc @p (ppr ps)

ppr_expr (HsProc _ pat (L _ (HsCmdTop _ cmd)))
  = hsep [text "proc", ppr pat, ptext (sLit "->"), ppr cmd]

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

ppr_expr (XExpr x) = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
  GhcPs -> ppr x
#endif
  GhcRn -> ppr x
  GhcTc -> case x of
    WrapExpr (HsWrap co_fn e) -> pprHsWrapper co_fn
      (\parens -> if parens then pprExpr e else pprExpr e)
    ExpansionExpr e -> ppr e -- e is an HsExpansion, we print the original
                             -- expression (LHsExpr GhcPs), not the
                             -- desugared one (LHsExpr GhcT).

ppr_infix_expr :: forall p. (OutputableBndrId p) => HsExpr (GhcPass p) -> Maybe SDoc
ppr_infix_expr (HsVar _ (L _ v))    = Just (pprInfixOcc v)
ppr_infix_expr (HsConLikeOut _ c)   = Just (pprInfixOcc (conLikeName c))
ppr_infix_expr (HsRecFld _ f)       = Just (pprInfixOcc f)
ppr_infix_expr (HsUnboundVar _ occ) = Just (pprInfixOcc occ)
ppr_infix_expr (XExpr x)            = case (ghcPass @p, x) of
#if __GLASGOW_HASKELL__ <= 810
  (GhcPs, _)                              -> Nothing
#endif
  (GhcRn, HsExpanded a _)                 -> ppr_infix_expr a
  (GhcTc, WrapExpr (HsWrap _ e))          -> ppr_infix_expr e
  (GhcTc, ExpansionExpr (HsExpanded a _)) -> ppr_infix_expr a
ppr_infix_expr _ = Nothing

ppr_apps :: (OutputableBndrId p)
         => HsExpr (GhcPass p)
         -> [Either (LHsExpr (GhcPass p)) (LHsWcType (NoGhcTc (GhcPass p)))]
         -> SDoc
ppr_apps (HsApp _ (L _ fun) arg)        args
  = ppr_apps fun (Left arg : args)
ppr_apps (HsAppType _ (L _ fun) arg)    args
  = ppr_apps fun (Right arg : args)
ppr_apps fun args = hang (ppr_expr fun) 2 (fsep (map pp args))
  where
    pp (Left arg)                             = ppr arg
    -- pp (Right (LHsWcTypeX (HsWC { hswc_body = L _ arg })))
    --   = char '@' <> pprHsType arg
    pp (Right arg)
      = text "@" <> ppr arg

pprExternalSrcLoc :: (StringLiteral,(Int,Int),(Int,Int)) -> SDoc
pprExternalSrcLoc (StringLiteral _ src _,(n1,n2),(n3,n4))
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

pprDebugParendExpr :: (OutputableBndrId p)
                   => PprPrec -> LHsExpr (GhcPass p) -> SDoc
pprDebugParendExpr p expr
  = getPprDebug $ \case
      True  -> pprParendLExpr p expr
      False -> pprLExpr         expr

pprParendLExpr :: (OutputableBndrId p)
               => PprPrec -> LHsExpr (GhcPass p) -> SDoc
pprParendLExpr p (L _ e) = pprParendExpr p e

pprParendExpr :: (OutputableBndrId p)
              => PprPrec -> HsExpr (GhcPass p) -> SDoc
pprParendExpr p expr
  | hsExprNeedsParens p expr = parens (pprExpr expr)
  | otherwise                = pprExpr expr
        -- Using pprLExpr makes sure that we go 'deeper'
        -- I think that is usually (always?) right

-- | @'hsExprNeedsParens' p e@ returns 'True' if the expression @e@ needs
-- parentheses under precedence @p@.
hsExprNeedsParens :: forall p. IsPass p => PprPrec -> HsExpr (GhcPass p) -> Bool
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
    go (HsApp{})                      = p >= appPrec
    go (HsAppType {})                 = p >= appPrec
    go (OpApp{})                      = p >= opPrec
    go (NegApp{})                     = p > topPrec
    go (SectionL{})                   = True
    go (SectionR{})                   = True
    -- Special-case unary boxed tuple applications so that they are
    -- parenthesized as `Identity (Solo x)`, not `Identity Solo x` (#18612)
    -- See Note [One-tuples] in GHC.Builtin.Types
    go (ExplicitTuple _ [Present{}] Boxed)
                                      = p >= appPrec
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
    go (HsPragE{})                    = p >= appPrec
    go (HsSpliceE{})                  = False
    go (HsBracket{})                  = False
    go (HsRnBracketOut{})             = False
    go (HsTcBracketOut{})             = False
    go (HsProc{})                     = p > topPrec
    go (HsStatic{})                   = p >= appPrec
    go (HsTick _ _ (L _ e))           = go e
    go (HsBinTick _ _ _ (L _ e))      = go e
    go (RecordCon{})                  = False
    go (HsRecFld{})                   = False
    go (XExpr x)
      | GhcTc <- ghcPass @p
      = case x of
          WrapExpr      (HsWrap _ e)     -> go e
          ExpansionExpr (HsExpanded a _) -> hsExprNeedsParens p a
      | GhcRn <- ghcPass @p
      = case x of HsExpanded a _ -> hsExprNeedsParens p a
#if __GLASGOW_HASKELL__ <= 900
      | otherwise
      = True
#endif


-- | @'parenthesizeHsExpr' p e@ checks if @'hsExprNeedsParens' p e@ is true,
-- and if so, surrounds @e@ with an 'HsPar'. Otherwise, it simply returns @e@.
parenthesizeHsExpr :: IsPass p => PprPrec -> LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
parenthesizeHsExpr p le@(L loc e)
  | hsExprNeedsParens p e = L loc (HsPar noAnn le)
  | otherwise             = le

stripParensLHsExpr :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
stripParensLHsExpr (L _ (HsPar _ e)) = stripParensLHsExpr e
stripParensLHsExpr e = e

stripParensHsExpr :: HsExpr (GhcPass p) -> HsExpr (GhcPass p)
stripParensHsExpr (HsPar _ (L _ e)) = stripParensHsExpr e
stripParensHsExpr e = e

isAtomicHsExpr :: forall p. IsPass p => HsExpr (GhcPass p) -> Bool
-- True of a single token
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsConLikeOut {}) = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsUnboundVar {}) = True
isAtomicHsExpr (HsRecFld{})      = True
isAtomicHsExpr (XExpr x)
  | GhcTc <- ghcPass @p          = case x of
      WrapExpr      (HsWrap _ e)     -> isAtomicHsExpr e
      ExpansionExpr (HsExpanded a _) -> isAtomicHsExpr a
  | GhcRn <- ghcPass @p          = case x of
      HsExpanded a _         -> isAtomicHsExpr a
isAtomicHsExpr _                 = False

instance Outputable (HsPragE (GhcPass p)) where
  ppr (HsPragSCC _ st (StringLiteral stl lbl _)) =
    pprWithSourceText st (text "{-# SCC")
     -- no doublequotes if stl empty, for the case where the SCC was written
     -- without quotes.
    <+> pprWithSourceText stl (ftext lbl) <+> text "#-}"

{-
************************************************************************
*                                                                      *
\subsection{Commands (in arrow abstractions)}
*                                                                      *
************************************************************************

We re-use HsExpr to represent these.
-}

-- | Located Haskell Command (for arrow syntax)
type LHsCmd id = XRec id (HsCmd id)
-- type LHsCmd id = LocatedA (HsCmd id)
                       -- AZ: old one
type instance Anno (HsCmd (GhcPass p)) = SrcSpanAnnA

-- | Haskell Command (e.g. a "statement" in an Arrow proc block)
data HsCmd id
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.Annlarrowtail',
  --          'GHC.Parser.Annotation.Annrarrowtail','GHC.Parser.Annotation.AnnLarrowtail',
  --          'GHC.Parser.Annotation.AnnRarrowtail'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  = HsCmdArrApp          -- Arrow tail, or arrow application (f -< arg)
        (XCmdArrApp id)  -- type of the arrow expressions f,
                         -- of the form a t t', where arg :: t
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpenB' @'(|'@,
  --         'GHC.Parser.Annotation.AnnCloseB' @'|)'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
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
       -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnLam',
       --       'GHC.Parser.Annotation.AnnRarrow',

       -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsCmdPar    (XCmdPar id)
                (LHsCmd id)                     -- parenthesised command
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'('@,
    --             'GHC.Parser.Annotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsCmdCase   (XCmdCase id)
                (LHsExpr id)
                (MatchGroup id (LHsCmd id))     -- bodies are HsCmd's
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnCase',
    --       'GHC.Parser.Annotation.AnnOf','GHC.Parser.Annotation.AnnOpen' @'{'@,
    --       'GHC.Parser.Annotation.AnnClose' @'}'@

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsCmdLamCase (XCmdLamCase id)
                 (MatchGroup id (LHsCmd id))    -- bodies are HsCmd's
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnLam',
    --       'GHC.Parser.Annotation.AnnCase','GHC.Parser.Annotation.AnnOpen' @'{'@,
    --       'GHC.Parser.Annotation.AnnClose' @'}'@

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsCmdIf     (XCmdIf id)
                (SyntaxExpr id)         -- cond function
                (LHsExpr id)            -- predicate
                (LHsCmd id)             -- then part
                (LHsCmd id)             -- else part
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnIf',
    --       'GHC.Parser.Annotation.AnnSemi',
    --       'GHC.Parser.Annotation.AnnThen','GHC.Parser.Annotation.AnnSemi',
    --       'GHC.Parser.Annotation.AnnElse',

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsCmdLet    (XCmdLet id)
                (HsLocalBinds id)      -- let(rec)
                (LHsCmd  id)
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnLet',
    --       'GHC.Parser.Annotation.AnnOpen' @'{'@,
    --       'GHC.Parser.Annotation.AnnClose' @'}'@,'GHC.Parser.Annotation.AnnIn'

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | HsCmdDo     (XCmdDo id)                     -- Type of the whole expression
                (XRec id [CmdLStmt id])
                -- (LocatedL [CmdLStmt id])
                       -- AZ: old one
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDo',
    --             'GHC.Parser.Annotation.AnnOpen', 'GHC.Parser.Annotation.AnnSemi',
    --             'GHC.Parser.Annotation.AnnVbar',
    --             'GHC.Parser.Annotation.AnnClose'

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | XCmd        !(XXCmd id)     -- Note [Trees that Grow] extension point

type instance XCmdArrApp  GhcPs = ApiAnn' AddApiAnn
type instance XCmdArrApp  GhcRn = NoExtField
type instance XCmdArrApp  GhcTc = Type

type instance XCmdArrForm GhcPs = ApiAnn' AnnList
type instance XCmdArrForm GhcRn = NoExtField
type instance XCmdArrForm GhcTc = NoExtField

type instance XCmdApp     (GhcPass _) = ApiAnnCO
type instance XCmdLam     (GhcPass _) = NoExtField
type instance XCmdPar     (GhcPass _) = ApiAnn' AnnParen

type instance XCmdCase    GhcPs = ApiAnn' ApiAnnHsCase
type instance XCmdCase    GhcRn = NoExtField
type instance XCmdCase    GhcTc = NoExtField

type instance XCmdLamCase (GhcPass _) = ApiAnn

type instance XCmdIf      GhcPs = ApiAnn
type instance XCmdIf      GhcRn = NoExtField
type instance XCmdIf      GhcTc = NoExtField

type instance XCmdLet     GhcPs = ApiAnn
type instance XCmdLet     GhcRn = NoExtField
type instance XCmdLet     GhcTc = NoExtField

type instance XCmdDo      GhcPs = ApiAnn' AnnList
type instance XCmdDo      GhcRn = NoExtField
type instance XCmdDo      GhcTc = Type

type instance XCmdWrap    (GhcPass _) = NoExtField

type instance XXCmd       GhcPs = NoExtCon
type instance XXCmd       GhcRn = NoExtCon
type instance XXCmd       GhcTc = HsWrap HsCmd

                -- (XRec id [CmdLStmt id])
                -- -- (LocatedL [CmdLStmt id])
type instance Anno [LocatedA (StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (HsCmd (GhcPass pr))))]
  = SrcSpanAnnL

    -- If   cmd :: arg1 --> res
    --      wrap :: arg1 "->" arg2
    -- Then (XCmd (HsWrap wrap cmd)) :: arg2 --> res

-- | Haskell Array Application Type
data HsArrAppType = HsHigherOrderApp | HsFirstOrderApp
  deriving Data


{- | Top-level command, introducing a new arrow.
This may occur inside a proc (where the stack is empty) or as an
argument of a command-forming operator.
-}

-- | Located Haskell Top-level Command
type LHsCmdTop p = XRec p (HsCmdTop p)
type instance Anno (HsCmdTop (GhcPass p)) = SrcSpan

-- | Haskell Top-level Command
data HsCmdTop p
  = HsCmdTop (XCmdTop p)
             (LHsCmd p)
  | XCmdTop !(XXCmdTop p)        -- Note [Trees that Grow] extension point

data CmdTopTc
  = CmdTopTc Type    -- Nested tuple of inputs on the command's stack
             Type    -- return type of the command
             (CmdSyntaxTable GhcTc) -- See Note [CmdSyntaxTable]

type instance XCmdTop  GhcPs = NoExtField
type instance XCmdTop  GhcRn = CmdSyntaxTable GhcRn -- See Note [CmdSyntaxTable]
type instance XCmdTop  GhcTc = CmdTopTc

type instance XXCmdTop (GhcPass _) = NoExtCon

instance (OutputableBndrId p) => Outputable (HsCmd (GhcPass p)) where
    ppr cmd = pprCmd cmd

-----------------------
-- pprCmd and pprLCmd call pprDeeper;
-- the underscore versions do not
pprLCmd :: (OutputableBndrId p) => LHsCmd (GhcPass p) -> SDoc
pprLCmd (L _ c) = pprCmd c

pprCmd :: (OutputableBndrId p) => HsCmd (GhcPass p) -> SDoc
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
ppr_lcmd :: (OutputableBndrId p) => LHsCmd (GhcPass p) -> SDoc
ppr_lcmd c = ppr_cmd (unLoc c)

ppr_cmd :: forall p. (OutputableBndrId p
                 -- Anno (StmtLR (GhcPass p) (GhcPass p) body) ~ SrcSpanAnnA
                     ) => HsCmd (GhcPass p) -> SDoc
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

ppr_cmd (HsCmdLamCase _ matches)
  = sep [ text "\\case", nest 2 (pprMatches matches) ]

ppr_cmd (HsCmdIf _ _ e ct ce)
  = sep [hsep [text "if", nest 2 (ppr e), ptext (sLit "then")],
         nest 4 (ppr ct),
         text "else",
         nest 4 (ppr ce)]

-- special case: let ... in let ...
ppr_cmd (HsCmdLet _ binds cmd@(L _ (HsCmdLet {})))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lcmd cmd]

ppr_cmd (HsCmdLet _ binds cmd)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr cmd)]

ppr_cmd (HsCmdDo _ (L _ stmts))  = pprDo ArrowExpr stmts

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
  = hang (text "(|" <+> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <+> text "|)")
ppr_cmd (XCmd x) = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
  GhcPs -> ppr x
  GhcRn -> ppr x
#endif
  GhcTc -> case x of
    HsWrap w cmd -> pprHsWrapper w (\_ -> parens (ppr_cmd cmd))

pprCmdArg :: (OutputableBndrId p) => HsCmdTop (GhcPass p) -> SDoc
pprCmdArg (HsCmdTop _ cmd)
  = ppr_lcmd cmd

instance (OutputableBndrId p) => Outputable (HsCmdTop (GhcPass p)) where
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
  = MG { mg_ext     :: XMG p body -- Post-typechecker, types of args and result
       , mg_alts    :: XRec p [LMatch p body]  -- The alternatives
       -- , mg_alts    :: LocatedL [LMatch p body]  -- The alternatives
                       -- AZ: old one
       --                -- TODO:AZ: need mg_alts be located? put the
       --                -- info into XMG instead?  Need list offset
       --                -- though, so maybe not.  And AnnSortKey
       , mg_origin  :: Origin }
     -- The type is the type of the entire group
     --      t1 -> ... -> tn -> tr
     -- where there are n patterns
  | XMatchGroup !(XXMatchGroup p body)

data MatchGroupTc
  = MatchGroupTc
       { mg_arg_tys :: [Scaled Type]  -- Types of the arguments, t1..tn
       , mg_res_ty  :: Type    -- Type of the result, tr
       } deriving Data

type instance XMG         GhcPs b = NoExtField
type instance XMG         GhcRn b = NoExtField
type instance XMG         GhcTc b = MatchGroupTc

type instance XXMatchGroup (GhcPass _) b = NoExtCon

type instance Anno [LocatedA (Match (GhcPass p) (LocatedA (HsExpr (GhcPass p))))] = SrcSpanAnnL
type instance Anno [LocatedA (Match (GhcPass p) (LocatedA (HsCmd  (GhcPass p))))] = SrcSpanAnnL

-- | Located Match
type LMatch id body = XRec id (Match id body)
-- type LMatch id body = LocatedA (Match id body)
-- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnSemi' when in a
--   list

-- For details on above see note [Api annotations] in GHC.Parser.Annotation

type instance Anno (Match (GhcPass p) (LocatedA (HsExpr (GhcPass p)))) = SrcSpanAnnA
type instance Anno (Match (GhcPass p) (LocatedA (HsCmd  (GhcPass p)))) = SrcSpanAnnA

data Match p body
  = Match {
        m_ext :: XCMatch p body,
        m_ctxt :: HsMatchContext (NoGhcTc p),
          -- See note [m_ctxt in Match]
        m_pats :: [LPat p], -- The patterns
        m_grhss :: (GRHSs p body)
  }
  | XMatch !(XXMatch p body)

type instance XCMatch (GhcPass _) b = ApiAnn
type instance XXMatch (GhcPass _) b = NoExtCon

instance (OutputableBndrId pr, Outputable body)
            => Outputable (Match (GhcPass pr) body) where
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

isEmptyMatchGroup :: MatchGroup (GhcPass p) body -> Bool
isEmptyMatchGroup (MG { mg_alts = ms }) = null $ unLoc ms

-- | Is there only one RHS in this list of matches?
isSingletonMatchGroup :: [LMatch (GhcPass p) body] -> Bool
isSingletonMatchGroup matches
  | [L _ match] <- matches
  , Match { m_grhss = GRHSs { grhssGRHSs = [_] } } <- match
  = True
  | otherwise
  = False

matchGroupArity :: MatchGroup (GhcPass id) body -> Arity
-- Precondition: MatchGroup is non-empty
-- This is called before type checking, when mg_arg_tys is not set
matchGroupArity (MG { mg_alts = alts })
  | L _ (alt1:_) <- alts = length (hsLMatchPats alt1)
  | otherwise        = panic "matchGroupArity"

hsLMatchPats :: LMatch (GhcPass id) body -> [LPat (GhcPass id)]
hsLMatchPats (L _ (Match { m_pats = pats })) = pats

-- | Guarded Right-Hand Sides
--
-- GRHSs are used both for pattern bindings and for Matches
--
--  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnVbar',
--        'GHC.Parser.Annotation.AnnEqual','GHC.Parser.Annotation.AnnWhere',
--        'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnClose'
--        'GHC.Parser.Annotation.AnnRarrow','GHC.Parser.Annotation.AnnSemi'

-- For details on above see note [Api annotations] in GHC.Parser.Annotation
data GRHSs p body
  = GRHSs {
      grhssExt :: XCGRHSs p body,
      grhssGRHSs :: [LGRHS p body],      -- ^ Guarded RHSs
      grhssLocalBinds :: HsLocalBinds p -- ^ The where clause
    }
  | XGRHSs !(XXGRHSs p body)

type instance XCGRHSs (GhcPass _) _ = NoExtField

type instance XXGRHSs (GhcPass _) _ = NoExtCon

-- | Located Guarded Right-Hand Side
type LGRHS id body = XRec id (GRHS id body)
-- type LGRHS id body = Located (GRHS id body)
type instance Anno (GRHS (GhcPass p) (LocatedA (HsExpr (GhcPass p)))) = SrcSpan
type instance Anno (GRHS (GhcPass p) (LocatedA (HsCmd  (GhcPass p)))) = SrcSpan


-- | Guarded Right Hand Side.
data GRHS p body = GRHS (XCGRHS p body)
                        [GuardLStmt p] -- Guards
                        body           -- Right hand side
                  | XGRHS !(XXGRHS p body)

type instance XCGRHS (GhcPass _) _ = ApiAnn' GrhsAnn
                                   -- Location of matchSeparator
                                   -- TODO:AZ does this belong on the GRHS, or GRHSs?

type instance XXGRHS (GhcPass _) _ = NoExtCon

data GrhsAnn
  = GrhsAnn {
      ga_vbar :: Maybe RealSrcSpan, -- TODO:AZ do we need this?
      ga_sep  :: AddApiAnn -- ^ Match separator location
      } deriving (Data)

-- We know the list must have at least one @Match@ in it.

pprMatches :: (OutputableBndrId idR, Outputable body)
           => MatchGroup (GhcPass idR) body -> SDoc
pprMatches MG { mg_alts = matches }
    = vcat (map pprMatch (map unLoc (unLoc matches)))
      -- Don't print the type; it's only a place-holder before typechecking

-- Exported to GHC.Hs.Binds, which can't see the defn of HsMatchContext
-- pprFunBind :: (OutputableBndrId idR, Outputable body)
--            => MatchGroup (GhcPass idR) body -> SDoc
pprFunBind :: (OutputableBndrId idR)
           => MatchGroup (GhcPass idR) (LHsExpr (GhcPass idR)) -> SDoc
pprFunBind matches = pprMatches matches

-- Exported to GHC.Hs.Binds, which can't see the defn of HsMatchContext
-- pprPatBind :: forall bndr p body. (OutputableBndrId bndr,
--                                    OutputableBndrId p,
--                                    Outputable body)
--            => LPat (GhcPass bndr) -> GRHSs (GhcPass p) body -> SDoc
pprPatBind :: forall bndr p . (OutputableBndrId bndr,
                               OutputableBndrId p)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) (LHsExpr (GhcPass p)) -> SDoc
pprPatBind pat grhss
 = sep [ppr pat,
       nest 2 (pprGRHSs (PatBindRhs :: HsMatchContext (GhcPass p)) grhss)]

pprMatch :: (OutputableBndrId idR, Outputable body)
         => Match (GhcPass idR) body -> SDoc
pprMatch (Match { m_pats = pats, m_ctxt = ctxt, m_grhss = grhss })
  = sep [ sep (herald : map (nest 2 . pprParendLPat appPrec) other_pats)
        , nest 2 (pprGRHSs ctxt grhss) ]
  where
    (herald, other_pats)
        = case ctxt of
            FunRhs {mc_fun=L _ fun, mc_fixity=fixity, mc_strictness=strictness}
                | SrcStrict <- strictness
                -> ASSERT(null pats)     -- A strict variable binding
                   (char '!'<>pprPrefixOcc fun, pats)

                | Prefix <- fixity
                -> (pprPrefixOcc fun, pats) -- f x y z = e
                                            -- Not pprBndr; the AbsBinds will
                                            -- have printed the signature
                | otherwise
                -> case pats of
                     (p1:p2:rest)
                        | null rest -> (pp_infix, [])           -- x &&& y = e
                        | otherwise -> (parens pp_infix, rest)  -- (x &&& y) z = e
                        where
                          pp_infix = pprParendLPat opPrec p1
                                     <+> pprInfixOcc fun
                                     <+> pprParendLPat opPrec p2
                     _ -> pprPanic "pprMatch" (ppr ctxt $$ ppr pats)

            LambdaExpr -> (char '\\', pats)

            _ -> case pats of
                   []    -> (empty, [])
                   [pat] -> (ppr pat, [])  -- No parens around the single pat in a case
                   _     -> pprPanic "pprMatch" (ppr ctxt $$ ppr pats)

pprGRHSs :: (OutputableBndrId idR, Outputable body)
         => HsMatchContext passL -> GRHSs (GhcPass idR) body -> SDoc
pprGRHSs ctxt (GRHSs _ grhss binds)
  = vcat (map (pprGRHS ctxt . unLoc) grhss)
  -- Print the "where" even if the contents of the binds is empty. Only
  -- EmptyLocalBinds means no "where" keyword
 $$ ppUnless (eqEmptyLocalBinds binds)
      (text "where" $$ nest 4 (pprBinds binds))

pprGRHS :: (OutputableBndrId idR, Outputable body)
        => HsMatchContext passL -> GRHS (GhcPass idR) body -> SDoc
pprGRHS ctxt (GRHS _ [] body)
 =  pp_rhs ctxt body

pprGRHS ctxt (GRHS _ guards body)
 = sep [vbar <+> interpp'SP guards, pp_rhs ctxt body]

pp_rhs :: Outputable body => HsMatchContext passL -> body -> SDoc
pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)

instance Outputable GrhsAnn where
  ppr (GrhsAnn v s) = text "GrhsAnn" <+> ppr v <+> ppr s

{-
************************************************************************
*                                                                      *
\subsection{Do stmts and list comprehensions}
*                                                                      *
************************************************************************
-}

-- | Located @do@ block Statement
type LStmt id body = XRec id (StmtLR id id body)
-- type LStmt id body = LocatedA (StmtLR id id body)
                       -- AZ: old one
type instance Anno (StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (HsExpr (GhcPass pr)))) = SrcSpanAnnA
type instance Anno (StmtLR (GhcPass pl) (GhcPass pr) (LocatedA (HsCmd  (GhcPass pr)))) = SrcSpanAnnA

-- | Located Statement with separate Left and Right id's
type LStmtLR idL idR body = XRec idL (StmtLR idL idR body)
-- type LStmtLR idL idR body = LocatedA (StmtLR idL idR body)

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
--  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnVbar',
--         'GHC.Parser.Annotation.AnnComma','GHC.Parser.Annotation.AnnThen',
--         'GHC.Parser.Annotation.AnnBy','GHC.Parser.Annotation.AnnBy',
--         'GHC.Parser.Annotation.AnnGroup','GHC.Parser.Annotation.AnnUsing'

-- For details on above see note [Api annotations] in GHC.Parser.Annotation
data StmtLR idL idR body -- body should always be (LHs**** idR)
  = LastStmt  -- Always the last Stmt in ListComp, MonadComp,
              -- and (after the renamer, see GHC.Rename.Expr.checkLastStmt) DoExpr, MDoExpr
              -- Not used for GhciStmtCtxt, PatGuard, which scope over other stuff
          (XLastStmt idL idR body)
          body
          (Maybe Bool)  -- Whether return was stripped
            -- Just True <=> return with a dollar was stripped by ApplicativeDo
            -- Just False <=> return without a dollar was stripped by ApplicativeDo
            -- Nothing <=> Nothing was stripped
          (SyntaxExpr idR)   -- The return operator
            -- The return operator is used only for MonadComp
            -- For ListComp we use the baked-in 'return'
            -- For DoExpr, MDoExpr, we don't apply a 'return' at all
            -- See Note [Monad Comprehensions]
            -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnLarrow'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | BindStmt (XBindStmt idL idR body)
             -- ^ Post renaming has optional fail and bind / (>>=) operator.
             -- Post typechecking, also has multiplicity of the argument
             -- and the result type of the function passed to bind;
             -- that is, (P, S) in (>>=) :: Q -> (R # P -> S) -> T
             -- See Note [The type of bind in Stmts]
             (LPat idL)
             body

  -- | 'ApplicativeStmt' represents an applicative expression built with
  -- '<$>' and '<*>'.  It is generated by the renamer, and is desugared into the
  -- appropriate applicative expression by the desugarer, but it is intended
  -- to be invisible in error messages.
  --
  -- For full details, see Note [ApplicativeDo] in "GHC.Rename.Expr"
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

  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnLet'
  --          'GHC.Parser.Annotation.AnnOpen' @'{'@,'GHC.Parser.Annotation.AnnClose' @'}'@,

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | LetStmt  (XLetStmt idL idR body) (HsLocalBindsLR idL idR)

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
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnRec'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | RecStmt
     { recS_ext :: XRecStmt idL idR body
     , recS_stmts :: LocatedL [LStmtLR idL idR body]

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
  | XStmtLR !(XXStmtLR idL idR body)

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


type instance XLastStmt        (GhcPass _) (GhcPass _) b = NoExtField

type instance XBindStmt        (GhcPass _) GhcPs b = ApiAnn
type instance XBindStmt        (GhcPass _) GhcRn b = XBindStmtRn
type instance XBindStmt        (GhcPass _) GhcTc b = XBindStmtTc

data XBindStmtRn = XBindStmtRn
  { xbsrn_bindOp :: SyntaxExpr GhcRn
  , xbsrn_failOp :: FailOperator GhcRn
  }

data XBindStmtTc = XBindStmtTc
  { xbstc_bindOp :: SyntaxExpr GhcTc
  , xbstc_boundResultType :: Type -- If (>>=) :: Q -> (R -> S) -> T, this is S
  , xbstc_boundResultMult :: Mult -- If (>>=) :: Q -> (R -> S) -> T, this is S
  , xbstc_failOp :: FailOperator GhcTc
  }

type instance XApplicativeStmt (GhcPass _) GhcPs b = NoExtField
type instance XApplicativeStmt (GhcPass _) GhcRn b = NoExtField
type instance XApplicativeStmt (GhcPass _) GhcTc b = Type

type instance XBodyStmt        (GhcPass _) GhcPs b = NoExtField
type instance XBodyStmt        (GhcPass _) GhcRn b = NoExtField
type instance XBodyStmt        (GhcPass _) GhcTc b = Type

type instance XLetStmt         (GhcPass _) (GhcPass _) b = ApiAnn

type instance XParStmt         (GhcPass _) GhcPs b = NoExtField
type instance XParStmt         (GhcPass _) GhcRn b = NoExtField
type instance XParStmt         (GhcPass _) GhcTc b = Type

type instance XTransStmt       (GhcPass _) GhcPs b = ApiAnn
type instance XTransStmt       (GhcPass _) GhcRn b = NoExtField
type instance XTransStmt       (GhcPass _) GhcTc b = Type

type instance XRecStmt         (GhcPass _) GhcPs b = ApiAnn' AnnList
type instance XRecStmt         (GhcPass _) GhcRn b = NoExtField
type instance XRecStmt         (GhcPass _) GhcTc b = RecStmtTc

type instance XXStmtLR         (GhcPass _) (GhcPass _) b = NoExtCon

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
  | XParStmtBlock !(XXParStmtBlock idL idR)

type instance XParStmtBlock  (GhcPass pL) (GhcPass pR) = NoExtField
type instance XXParStmtBlock (GhcPass pL) (GhcPass pR) = NoExtCon

-- | The fail operator
--
-- This is used for `.. <-` "bind statments" in do notation, including
-- non-monadic "binds" in applicative.
--
-- The fail operator is 'Just expr' if it potentially fail monadically. if the
-- pattern match cannot fail, or shouldn't fail monadically (regular incomplete
-- pattern exception), it is 'Nothing'.
--
-- See Note [Monad fail : Rebindable syntax, overloaded strings] for the type of
-- expression in the 'Just' case, and why it is so.
--
-- See Note [Failing pattern matches in Stmts] for which contexts for
-- '@BindStmt@'s should use the monadic fail and which shouldn't.
type FailOperator id = Maybe (SyntaxExpr id)

-- | Applicative Argument
data ApplicativeArg idL
  = ApplicativeArgOne      -- A single statement (BindStmt or BodyStmt)
    { xarg_app_arg_one  :: XApplicativeArgOne idL
      -- ^ The fail operator, after renaming
      --
      -- The fail operator is needed if this is a BindStmt
      -- where the pattern can fail. E.g.:
      -- (Just a) <- stmt
      -- The fail operator will be invoked if the pattern
      -- match fails.
      -- It is also used for guards in MonadComprehensions.
      -- The fail operator is Nothing
      -- if the pattern match can't fail
    , app_arg_pattern   :: LPat idL -- WildPat if it was a BodyStmt (see below)
    , arg_expr          :: LHsExpr idL
    , is_body_stmt      :: Bool
      -- ^ True <=> was a BodyStmt,
      -- False <=> was a BindStmt.
      -- See Note [Applicative BodyStmt]
    }
  | ApplicativeArgMany     -- do { stmts; return vars }
    { xarg_app_arg_many :: XApplicativeArgMany idL
    , app_stmts         :: [ExprLStmt idL] -- stmts
    , final_expr        :: HsExpr idL    -- return (v1,..,vn), or just (v1,..,vn)
    , bv_pattern        :: LPat idL      -- (v1,...,vn)
    , stmt_context      :: HsStmtContext GhcRn  -- context of the do expression
                                                -- used in pprArg
    }
  | XApplicativeArg !(XXApplicativeArg idL)

type instance XApplicativeArgOne GhcPs = NoExtField
type instance XApplicativeArgOne GhcRn = FailOperator GhcRn
type instance XApplicativeArgOne GhcTc = FailOperator GhcTc

type instance XApplicativeArgMany (GhcPass _) = NoExtField
type instance XXApplicativeArg    (GhcPass _) = NoExtCon

{-
Note [The type of bind in Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some Stmts, notably BindStmt, keep the (>>=) bind operator.
We do NOT assume that it has type
    (>>=) :: m a -> (a -> m b) -> m b
In some cases (see #303, #1537) it might have a more
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

instance (Outputable (StmtLR (GhcPass idL) (GhcPass idL) (LHsExpr (GhcPass idL))),
          Outputable (XXParStmtBlock (GhcPass idL) (GhcPass idR)))
        => Outputable (ParStmtBlock (GhcPass idL) (GhcPass idR)) where
  ppr (ParStmtBlock _ stmts _ _) = interpp'SP stmts

instance (OutputableBndrId pl, OutputableBndrId pr,
                 Anno (StmtLR (GhcPass pl) (GhcPass pr) body) ~ SrcSpanAnnA,
          Outputable body)
         => Outputable (StmtLR (GhcPass pl) (GhcPass pr) body) where
    ppr stmt = pprStmt stmt

pprStmt :: forall idL idR body . (OutputableBndrId idL,
                                  OutputableBndrId idR,
                 Anno (StmtLR (GhcPass idL) (GhcPass idR) body) ~ SrcSpanAnnA,
                                  Outputable body)
        => (StmtLR (GhcPass idL) (GhcPass idR) body) -> SDoc
pprStmt (LastStmt _ expr m_dollar_stripped _)
  = whenPprDebug (text "[last]") <+>
      (case m_dollar_stripped of
        Just True -> text "return $"
        Just False -> text "return"
        Nothing -> empty) <+>
      ppr expr
pprStmt (BindStmt _ pat expr)  = pprBindStmt pat expr
pprStmt (LetStmt _ binds)      = hsep [text "let", pprBinds binds]
pprStmt (BodyStmt _ expr _ _)  = ppr expr
pprStmt (ParStmt _ stmtss _ _) = sep (punctuate (text " | ") (map ppr stmtss))

pprStmt (TransStmt { trS_stmts = stmts, trS_by = by
                   , trS_using = using, trS_form = form })
  = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])

pprStmt (RecStmt { recS_stmts = segment, recS_rec_ids = rec_ids
                 , recS_later_ids = later_ids })
  = text "rec" <+>
    vcat [ ppr_do_stmts (unLoc segment)
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
     | isBody =  [ppr expr] -- See Note [Applicative BodyStmt]
     | otherwise = [pprBindStmt pat expr]
   flattenArg (_, ApplicativeArgMany _ stmts _ _ _) =
     concatMap flattenStmt stmts

   pp_debug =
     let
         ap_expr = sep (punctuate (text " |") (map pp_arg args))
     in
       whenPprDebug (if isJust mb_join then text "[join]" else empty) <+>
       (if lengthAtLeast args 2 then parens else id) ap_expr

   pp_arg :: (a, ApplicativeArg (GhcPass idL)) -> SDoc
   pp_arg (_, applicativeArg) = ppr applicativeArg

pprBindStmt :: (Outputable pat, Outputable expr) => pat -> expr -> SDoc
pprBindStmt pat expr = hsep [ppr pat, larrow, ppr expr]

instance (OutputableBndrId idL)
      => Outputable (ApplicativeArg (GhcPass idL)) where
  ppr = pprArg

pprArg :: forall idL . (OutputableBndrId idL) => ApplicativeArg (GhcPass idL) -> SDoc
pprArg (ApplicativeArgOne _ pat expr isBody)
  | isBody = ppr expr -- See Note [Applicative BodyStmt]
  | otherwise = pprBindStmt pat expr
pprArg (ApplicativeArgMany _ stmts return pat ctxt) =
     ppr pat <+>
     text "<-" <+>
     pprDo ctxt (stmts ++
                   [noLocA (LastStmt noExtField (noLocA return) Nothing noSyntaxExpr)])

pprTransformStmt :: (OutputableBndrId p)
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

pprDo :: (OutputableBndrId p, Outputable body,
                 Anno (StmtLR (GhcPass p) (GhcPass p) body) ~ SrcSpanAnnA
         )
      => HsStmtContext any -> [LStmt (GhcPass p) body] -> SDoc
pprDo (DoExpr m)    stmts =
  ppr_module_name_prefix m <> text "do"  <+> ppr_do_stmts stmts
pprDo GhciStmtCtxt  stmts = text "do"  <+> ppr_do_stmts stmts
pprDo ArrowExpr     stmts = text "do"  <+> ppr_do_stmts stmts
pprDo (MDoExpr m)   stmts =
  ppr_module_name_prefix m <> text "mdo"  <+> ppr_do_stmts stmts
pprDo ListComp      stmts = brackets    $ pprComp stmts
pprDo MonadComp     stmts = brackets    $ pprComp stmts
pprDo _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

ppr_module_name_prefix :: Maybe ModuleName -> SDoc
ppr_module_name_prefix = \case
  Nothing -> empty
  Just module_name -> ppr module_name <> char '.'

ppr_do_stmts :: (OutputableBndrId idL, OutputableBndrId idR,
                 Anno (StmtLR (GhcPass idL) (GhcPass idR) body) ~ SrcSpanAnnA,
                 Outputable body)
             => [LStmtLR (GhcPass idL) (GhcPass idR) body] -> SDoc
-- Print a bunch of do stmts
ppr_do_stmts stmts = pprDeeperList vcat (map ppr stmts)

pprComp :: (OutputableBndrId p, Outputable body,
                 Anno (StmtLR (GhcPass p) (GhcPass p) body) ~ SrcSpanAnnA)
        => [LStmt (GhcPass p) body] -> SDoc
pprComp quals     -- Prints:  body | qual1, ..., qualn
  | Just (initStmts, L _ (LastStmt _ body _ _)) <- snocView quals
  = if null initStmts
       -- If there are no statements in a list comprehension besides the last
       -- one, we simply treat it like a normal list. This does arise
       -- occasionally in code that GHC generates, e.g., in implementations of
       -- 'range' for derived 'Ix' instances for product datatypes with exactly
       -- one constructor (e.g., see #12583).
       then ppr body
       else hang (ppr body <+> vbar) 2 (pprQuals initStmts)
  | otherwise
  = pprPanic "pprComp" (pprQuals quals)

pprQuals :: (OutputableBndrId p, Outputable body,
                 Anno (StmtLR (GhcPass p) (GhcPass p) body) ~ SrcSpanAnnA)
         => [LStmt (GhcPass p) body] -> SDoc
-- pprQuals :: (OutputableBndrId p)
--          => [LStmt (GhcPass p) (LocatedA (HsExpr (GhcPass p)))] -> SDoc
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

   | HsQuasiQuote        -- See Note [Quasi-quote overview] in GHC.Tc.Gen.Splice
        (XQuasiQuote id)
        (IdP id)         -- Splice point
        (IdP id)         -- Quoter
        SrcSpan          -- The span of the enclosed string
        FastString       -- The enclosed string

   -- AZ:TODO: use XSplice instead of HsSpliced
   | HsSpliced  -- See Note [Delaying modFinalizers in untyped splices] in
                -- GHC.Rename.Splice.
                -- This is the result of splicing a splice. It is produced by
                -- the renamer and consumed by the typechecker. It lives only
                -- between the two.
        (XSpliced id)
        ThModFinalizers     -- TH finalizers produced by the splice.
        (HsSplicedThing id) -- The result of splicing
   | XSplice !(XXSplice id) -- Note [Trees that Grow] extension point

newtype HsSplicedT = HsSplicedT DelayedSplice deriving (Data)

type instance XTypedSplice   (GhcPass _) = ApiAnn
type instance XUntypedSplice (GhcPass _) = ApiAnn
type instance XQuasiQuote    (GhcPass _) = NoExtField
type instance XSpliced       (GhcPass _) = NoExtField
type instance XXSplice       GhcPs       = NoExtCon
type instance XXSplice       GhcRn       = NoExtCon
type instance XXSplice       GhcTc       = HsSplicedT

-- | A splice can appear with various decorations wrapped around it. This data
-- type captures explicitly how it was originally written, for use in the pretty
-- printer.
data SpliceDecoration
  = DollarSplice  -- ^ $splice or $$splice
  | BareSplice    -- ^ bare splice
  deriving (Data, Eq, Show)

instance Outputable SpliceDecoration where
  ppr x = text $ show x


isTypedSplice :: HsSplice id -> Bool
isTypedSplice (HsTypedSplice {}) = True
isTypedSplice _                  = False   -- Quasi-quotes are untyped splices

-- | Finalizers produced by a splice with
-- 'Language.Haskell.TH.Syntax.addModFinalizer'
--
-- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice. For how
-- this is used.
--
newtype ThModFinalizers = ThModFinalizers [ForeignRef (TH.Q ())]

-- A Data instance which ignores the argument of 'ThModFinalizers'.
instance Data ThModFinalizers where
  gunfold _ z _ = z $ ThModFinalizers []
  toConstr  a   = mkConstr (dataTypeOf a) "ThModFinalizers" [] Data.Prefix
  dataTypeOf a  = mkDataType "HsExpr.ThModFinalizers" [toConstr a]

-- See Note [Running typed splices in the zonker]
-- These are the arguments that are passed to `GHC.Tc.Gen.Splice.runTopSplice`
data DelayedSplice =
  DelayedSplice
    TcLclEnv          -- The local environment to run the splice in
    (LHsExpr GhcRn)   -- The original renamed expression
    TcType            -- The result type of running the splice, unzonked
    (LHsExpr GhcTc)   -- The typechecked expression to run and splice in the result

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
       (see GHC.Rename.Splice.makePending, HsQuasiQuote case)

     * cross-stage lifting, where the pending expression expands to
          $(lift x)
       (see GHC.Rename.Splice.checkCrossStageLifting)

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

instance OutputableBndrId p
       => Outputable (HsSplicedThing (GhcPass p)) where
  ppr (HsSplicedExpr e) = ppr_expr e
  ppr (HsSplicedTy   t) = ppr t
  ppr (HsSplicedPat  p) = ppr p

instance (OutputableBndrId p) => Outputable (HsSplice (GhcPass p)) where
  ppr s = pprSplice s

pprPendingSplice :: (OutputableBndrId p)
                 => SplicePointName -> LHsExpr (GhcPass p) -> SDoc
pprPendingSplice n e = angleBrackets (ppr n <> comma <+> ppr (stripParensLHsExpr e))

pprSpliceDecl ::  (OutputableBndrId p)
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc
pprSpliceDecl e@HsQuasiQuote{} _ = pprSplice e
pprSpliceDecl e ExplicitSplice   = text "$" <> ppr_splice_decl e
pprSpliceDecl e ImplicitSplice   = ppr_splice_decl e

ppr_splice_decl :: (OutputableBndrId p)
                => HsSplice (GhcPass p) -> SDoc
ppr_splice_decl (HsUntypedSplice _ _ n e) = ppr_splice empty n e empty
ppr_splice_decl e = pprSplice e

pprSplice :: forall p. (OutputableBndrId p) => HsSplice (GhcPass p) -> SDoc
pprSplice (HsTypedSplice _ DollarSplice n e)
  = ppr_splice (text "$$") n e empty
pprSplice (HsTypedSplice _ BareSplice _ _ )
  = panic "Bare typed splice"  -- impossible
pprSplice (HsUntypedSplice _ DollarSplice n e)
  = ppr_splice (text "$")  n e empty
pprSplice (HsUntypedSplice _ BareSplice n e)
  = ppr_splice empty  n e empty
pprSplice (HsQuasiQuote _ n q _ s)      = ppr_quasi n q s
pprSplice (HsSpliced _ _ thing)         = ppr thing
pprSplice (XSplice x)                   = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
                                            GhcPs -> noExtCon x
                                            GhcRn -> noExtCon x
#endif
                                            GhcTc -> case x of
                                                       HsSplicedT _ -> text "Unevaluated typed splice"

ppr_quasi :: OutputableBndr p => p -> p -> FastString -> SDoc
ppr_quasi n quoter quote = whenPprDebug (brackets (ppr n)) <>
                           char '[' <> ppr quoter <> vbar <>
                           ppr quote <> text "|]"

ppr_splice :: (OutputableBndrId p)
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
  | VarBr  (XVarBr p)   Bool (LocatedN (IdP p))
                                -- True: 'x, False: ''T
                                -- (The Bool flag is used only in pprHsBracket)
  | TExpBr (XTExpBr p) (LHsExpr p)    -- [||  expr  ||]
  | XBracket !(XXBracket p)           -- Note [Trees that Grow] extension point

type instance XExpBr      (GhcPass _) = NoExtField
type instance XPatBr      (GhcPass _) = NoExtField
type instance XDecBrL     (GhcPass _) = NoExtField
type instance XDecBrG     (GhcPass _) = NoExtField
type instance XTypBr      (GhcPass _) = NoExtField
type instance XVarBr      (GhcPass _) = NoExtField
type instance XTExpBr     (GhcPass _) = NoExtField
type instance XXBracket   (GhcPass _) = NoExtCon

isTypedBracket :: HsBracket id -> Bool
isTypedBracket (TExpBr {}) = True
isTypedBracket _           = False

instance OutputableBndrId p
          => Outputable (HsBracket (GhcPass p)) where
  ppr = pprHsBracket


pprHsBracket :: (OutputableBndrId p) => HsBracket (GhcPass p) -> SDoc
pprHsBracket (ExpBr _ e)   = thBrackets empty (ppr e)
pprHsBracket (PatBr _ p)   = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBrG _ gp) = thBrackets (char 'd') (ppr gp)
pprHsBracket (DecBrL _ ds) = thBrackets (char 'd') (vcat (map ppr ds))
pprHsBracket (TypBr _ t)   = thBrackets (char 't') (ppr t)
pprHsBracket (VarBr _ True n)
  = char '\'' <> pprPrefixOcc (unLoc n)
pprHsBracket (VarBr _ False n)
  = text "''" <> pprPrefixOcc (unLoc n)
pprHsBracket (TExpBr _ e)  = thTyBrackets (ppr e)

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
-- AZ: Should ArithSeqInfo have a TTG extension?

instance OutputableBndrId p
         => Outputable (ArithSeqInfo (GhcPass p)) where
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
data HsMatchContext p
  = FunRhs { mc_fun        :: LIdP p    -- ^ function binder of @f@
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

  | RecUpd                      -- ^Record update [used only in GHC.HsToCore.Expr to
                                --    tell matchWrapper what sort of
                                --    runtime error message to generate]

  | StmtCtxt (HsStmtContext p)  -- ^Pattern of a do-stmt, list comprehension,
                                -- pattern guard, etc

  | ThPatSplice            -- ^A Template Haskell pattern splice
  | ThPatQuote             -- ^A Template Haskell pattern quotation [p| (a,b) |]
  | PatSyn                 -- ^A pattern synonym declaration

instance OutputableBndrId p => Outputable (HsMatchContext (GhcPass p)) where
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

isPatSynCtxt :: HsMatchContext p -> Bool
isPatSynCtxt ctxt =
  case ctxt of
    PatSyn -> True
    _      -> False

-- | Haskell Statement Context.
data HsStmtContext p
  = ListComp
  | MonadComp

  | DoExpr (Maybe ModuleName)        -- ^[ModuleName.]do { ... }
  | MDoExpr (Maybe ModuleName)       -- ^[ModuleName.]mdo { ... }  ie recursive do-expression
  | ArrowExpr                        -- ^do-notation in an arrow-command context

  | GhciStmtCtxt                     -- ^A command-line Stmt in GHCi pat <- rhs
  | PatGuard (HsMatchContext p)      -- ^Pattern guard for specified thing
  | ParStmtCtxt (HsStmtContext p)    -- ^A branch of a parallel stmt
  | TransStmtCtxt (HsStmtContext p)  -- ^A branch of a transform stmt

qualifiedDoModuleName_maybe :: HsStmtContext p -> Maybe ModuleName
qualifiedDoModuleName_maybe ctxt = case ctxt of
  DoExpr m -> m
  MDoExpr m -> m
  _ -> Nothing

isComprehensionContext :: HsStmtContext id -> Bool
-- Uses comprehension syntax [ e | quals ]
isComprehensionContext ListComp          = True
isComprehensionContext MonadComp         = True
isComprehensionContext (ParStmtCtxt c)   = isComprehensionContext c
isComprehensionContext (TransStmtCtxt c) = isComprehensionContext c
isComprehensionContext _ = False

-- | Is this a monadic context?
isMonadStmtContext :: HsStmtContext id -> Bool
isMonadStmtContext MonadComp            = True
isMonadStmtContext DoExpr{}             = True
isMonadStmtContext MDoExpr{}            = True
isMonadStmtContext GhciStmtCtxt         = True
isMonadStmtContext (ParStmtCtxt ctxt)   = isMonadStmtContext ctxt
isMonadStmtContext (TransStmtCtxt ctxt) = isMonadStmtContext ctxt
isMonadStmtContext _ = False -- ListComp, PatGuard, ArrowExpr

isMonadCompContext :: HsStmtContext id -> Bool
isMonadCompContext MonadComp = True
isMonadCompContext _         = False

matchSeparator :: HsMatchContext p -> SDoc
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

pprMatchContext :: (Outputable (IdP p), UnXRec p)
                => HsMatchContext p -> SDoc
pprMatchContext ctxt
  | want_an ctxt = text "an" <+> pprMatchContextNoun ctxt
  | otherwise    = text "a"  <+> pprMatchContextNoun ctxt
  where
    want_an (FunRhs {}) = True  -- Use "an" in front
    want_an ProcExpr    = True
    want_an _           = False

pprMatchContextNoun :: forall p. (Outputable (IdP p), UnXRec p)
                    => HsMatchContext p -> SDoc
pprMatchContextNoun (FunRhs {mc_fun=fun})
                                    = text "equation for"
                                      <+> quotes (ppr (unXRec @p fun))
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
pprAStmtContext, pprStmtContext :: (Outputable (IdP p), UnXRec p)
                                => HsStmtContext p -> SDoc
pprAStmtContext ctxt = article <+> pprStmtContext ctxt
  where
    pp_an = text "an"
    pp_a  = text "a"
    article = case ctxt of
                  MDoExpr Nothing -> pp_an
                  GhciStmtCtxt  -> pp_an
                  _             -> pp_a


-----------------
pprStmtContext GhciStmtCtxt    = text "interactive GHCi command"
pprStmtContext (DoExpr m)      = prependQualified m (text "'do' block")
pprStmtContext (MDoExpr m)     = prependQualified m (text "'mdo' block")
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

prependQualified :: Maybe ModuleName -> SDoc -> SDoc
prependQualified Nothing  t = t
prependQualified (Just _) t = text "qualified" <+> t

instance OutputableBndrId p
      => Outputable (HsStmtContext (GhcPass p)) where
    ppr = pprStmtContext

-- Used to generate the string for a *runtime* error message
matchContextErrString :: OutputableBndrId p
                      => HsMatchContext (GhcPass p) -> SDoc
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
matchContextErrString (StmtCtxt (DoExpr m))        = prependQualified m (text "'do' block")
matchContextErrString (StmtCtxt ArrowExpr)         = text "'do' block"
matchContextErrString (StmtCtxt (MDoExpr m))       = prependQualified m (text "'mdo' block")
matchContextErrString (StmtCtxt ListComp)          = text "list comprehension"
matchContextErrString (StmtCtxt MonadComp)         = text "monad comprehension"

pprMatchInCtxt :: (OutputableBndrId idR, Outputable body)
               => Match (GhcPass idR) body -> SDoc
pprMatchInCtxt match  = hang (text "In" <+> pprMatchContext (m_ctxt match)
                                        <> colon)
                             4 (pprMatch match)

pprStmtInCtxt :: (OutputableBndrId idL,
                  OutputableBndrId idR,
                  Outputable body,
                 Anno (StmtLR (GhcPass idL) (GhcPass idR) body) ~ SrcSpanAnnA)
              => HsStmtContext (GhcPass idL)
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

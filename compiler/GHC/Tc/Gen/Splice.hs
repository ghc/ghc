{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}

#if __GLASGOW_HASKELL__ < 914
-- In GHC 9.14, GHC.Desugar will be removed from base in favour of
-- ghc-internal's GHC.Internal.Desugar. However, because of bootstrapping
-- concerns, we will only depend on ghc-internal when the boot compiler is
-- certain to have it.
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Template Haskell splices
module GHC.Tc.Gen.Splice(
     tcTypedSplice, tcTypedBracket, tcUntypedBracket,
     runAnnotation, getUntypedSpliceBody,

     runMetaE, runMetaP, runMetaT, runMetaD, runQuasi,
     tcTopSpliceExpr, lookupThName_maybe,
     defaultRunMeta, runMeta', runRemoteModFinalizers,
     finishTH, runTopSplice
      ) where

import GHC.Prelude

import GHC.Driver.Errors
import GHC.Driver.Plugins
import GHC.Driver.Main
import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Driver.Hooks
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Finder

import GHC.Hs

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Gen.Expr
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Env
import GHC.Tc.Types.Origin
import GHC.Tc.Types.LclEnv
import GHC.Tc.Types.Evidence
import GHC.Tc.Zonk.Type
import GHC.Tc.Zonk.TcType
import GHC.Tc.Solver
import GHC.Tc.Utils.TcMType
import GHC.Tc.Gen.HsType
import GHC.Tc.Instance.Family
import GHC.Tc.Utils.Instantiate

import GHC.Core.Multiplicity
import GHC.Core.Coercion( etaExpandCoAxBranch )
import GHC.Core.Type as Type
import GHC.Core.TyCo.Rep as TyCoRep
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv as InstEnv

import GHC.Builtin.Names.TH
import GHC.Builtin.Names
import GHC.Builtin.Types

import GHC.ThToHs
import GHC.HsToCore.Docs
import GHC.HsToCore.Expr
import GHC.HsToCore.Monad
import GHC.IfaceToCore
import GHC.Iface.Load

import GHCi.Message
import GHCi.RemoteTypes
import GHC.Runtime.Interpreter

import GHC.Rename.Splice( traceSplice, SpliceInfo(..))
import GHC.Rename.Expr
import GHC.Rename.Env
import GHC.Rename.Fixity ( lookupFixityRn_help )
import GHC.Rename.HsType

import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.PatSyn
import GHC.Core.ConLike
import GHC.Core.DataCon as DataCon

import GHC.Types.SrcLoc
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence as OccName
import GHC.Types.Var
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Unique
import GHC.Types.Var.Set
import GHC.Types.Meta
import GHC.Types.Basic hiding( SuccessFlag(..) )
import GHC.Types.Error
import GHC.Types.Fixity as Hs
import GHC.Types.Annotations
import GHC.Types.Name
import GHC.Types.Unique.Map
import GHC.Serialized

import GHC.Unit.Finder
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Deps

import GHC.Utils.Misc
import GHC.Utils.Panic as Panic
import GHC.Utils.Lexeme
import GHC.Utils.Outputable
import GHC.Utils.Logger
import GHC.Utils.Exception (throwIO, ErrorCall(..))

import GHC.Utils.TmpFs ( newTempName, TempFileLifetime(..) )

import GHC.Data.FastString
import GHC.Data.Maybe( MaybeErr(..) )
import qualified GHC.Data.EnumSet as EnumSet

-- THSyntax gives access to internal functions and data types
import qualified GHC.Internal.TH.Syntax as TH
import qualified GHC.Internal.TH.Ppr    as TH

#if defined(HAVE_INTERNAL_INTERPRETER)
import Unsafe.Coerce    ( unsafeCoerce )
import GHC.Desugar ( AnnotationWrapper(..) )
#endif

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic  ( fromDynamic, toDyn )
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Typeable ( typeOf, Typeable, TypeRep, typeRep )
import Data.Data (Data)
import Data.Proxy    ( Proxy (..) )
import Data.IORef
import GHC.Parser.HaddockLex (lexHsDoc)
import GHC.Parser (parseIdentifier)
import GHC.Rename.Doc (rnHsDoc)



{-
Note [Template Haskell state diagram]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here are the ThStages, s, their corresponding level numbers
(the result of (thLevel s)), and their state transitions.
The top level of the program is stage Comp:

     Start here
         |
         V
      -----------     $      ------------   $
      |  Comp   | ---------> |  Splice  | -----|
      |   1     |            |    0     | <----|
      -----------            ------------
        ^     |                ^      |
      $ |     | [||]         $ |      | [||]
        |     v                |      v
   --------------          ----------------
   | Brack Comp |          | Brack Splice |
   |     2      |          |      1       |
   --------------          ----------------

* Normal top-level declarations start in state Comp
       (which has level 1).
  Annotations start in state Splice, since they are
       treated very like a splice (only without a '$')

* Code compiled in state Splice (and only such code)
  will be *run at compile time*, with the result replacing
  the splice

* The original paper used level -1 instead of 0, etc.

* The original paper did not allow a splice within a
  splice, but there is no reason not to. This is the
  $ transition in the top right.

Note [Template Haskell levels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Imported things are impLevel (= 0)

* However things at level 0 are not *necessarily* imported.
      eg  $( \b -> ... )   here b is bound at level 0

* In GHCi, variables bound by a previous command are treated
  as impLevel, because we have bytecode for them.

* Variables are bound at the "current level"

* The current level starts off at outerLevel (= 1)

* The level is decremented by splicing $(..)
               incremented by brackets [| |]
               incremented by name-quoting 'f

* When a variable is used, checkWellStaged compares
        bind:  binding level, and
        use:   current level at usage site

  Generally
        bind > use      Always error (bound later than used)
                        [| \x -> $(f x) |]

        bind = use      Always OK (bound same stage as used)
                        [| \x -> $(f [| x |]) |]

        bind < use      Inside brackets, it depends
                        Inside splice, OK
                        Inside neither, OK

  For (bind < use) inside brackets, there are three cases:
    - Imported things   OK      f = [| map |]
    - Top-level things  OK      g = [| f |]
    - Non-top-level     Only if there is a liftable instance
                                h = \(x:Int) -> [| x |]

  To track top-level-ness we use the ThBindEnv in TcLclEnv

  For example:
           f = ...
           g1 = $(map ...)         is OK
           g2 = $(f ...)           is not OK; because we haven't compiled f yet


Note [How top-level splices are handled]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Top-level splices (those not inside a [| .. |] quotation bracket) are handled
very straightforwardly:

  1. tcTopSpliceExpr: typecheck the body e of the splice $(e)

  2. runMetaT: desugar, compile, run it, and convert result back to
     GHC.Hs syntax RdrName (of the appropriate flavour, eg HsType RdrName,
     HsExpr RdrName etc)

  3. treat the result as if that's what you saw in the first place
     e.g for HsType, rename and kind-check
         for HsExpr, rename and type-check

     (The last step is different for decls, because they can *only* be
      top-level: we return the result of step 2.)

Note [Warnings for TH splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only produce warnings for TH splices when the user requests so
(-fenable-th-splice-warnings). There are multiple reasons:

  * It's not clear that the user that compiles a splice is the author of the code
    that produces the warning. Think of the situation where they just splice in
    code from a third-party library that produces incomplete pattern matches.
    In this scenario, the user isn't even able to fix that warning.
  * Gathering information for producing the warnings (pattern-match check
    warnings in particular) is costly. There's no point in doing so if the user
    is not interested in those warnings.

That's why we store Origin flags in the Haskell AST. The functions from ThToHs
take such a flag and depending on whether TH splice warnings were enabled or
not, we pass FromSource (if the user requests warnings) or Generated
(otherwise). This is implemented in getThSpliceOrigin.

For correct pattern-match warnings it's crucial that we annotate the Origin
consistently (#17270). In the future we could offer the Origin as part of the
TH AST. That would enable us to give quotes from the current module get
FromSource origin, and/or third library authors to tag certain parts of
generated code as FromSource to enable warnings.
That effort is tracked in #14838.

Note [The life cycle of a TH quotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When desugaring a bracket (aka quotation), we want to produce Core
code that, when run, will produce the TH syntax tree for the quotation.
To that end, we want to desugar /renamed/ but not /typechecked/ code;
the latter is cluttered with the typechecker's elaboration that should
not appear in the TH syntax tree. So in (HsExpr GhcTc) tree, we must
have a (HsExpr GhcRn) for the quotation itself.

As such, when typechecking both typed and untyped brackets,
we keep a /renamed/ bracket in the extension field.

The HsBracketTc, the GhcTc ext field for both typed and untyped
brackets, contains:
  - The renamed quote :: HsQuote GhcRn -- for the desugarer
  - [PendingTcSplice]
  - The type of the quote
  - Maybe QuoteWrapper

Note that HsBracketTc stores the untyped (HsQuote GhcRn) for both typed and
untyped brackets. They are treated uniformly by the desugarer, and we can
easily construct untyped brackets from typed ones (with ExpBr).

See Note [Desugaring of brackets].

------------
Typed quotes
------------
Here is the life cycle of a /typed/ quote [|| e ||], whose datacon is
  HsTypedBracket (XTypedBracket p) (LHsExpr p)

  In pass p   (XTypedBracket p)       (LHsExpr p)
  -------------------------------------------
  GhcPs       Annotations only        LHsExpr GhcPs
  GhcRn       Annotations only        LHsExpr GhcRn
  GhcTc       HsBracketTc             LHsExpr GhcTc: unused!

Note that in the GhcTc tree, the second field (HsExpr GhcTc)
is entirely unused; the desugarer uses the (HsExpr GhcRn) from the
first field.

--------------
Untyped quotes
--------------
Here is the life cycle of an /untyped/ quote, whose datacon is
   HsUntypedBracket (XUntypedBracket p) (HsQuote p)

Here HsQuote is a sum-type of expressions [| e |], patterns [| p |],
types [| t |] etc.

  In pass p   (XUntypedBracket p)          (HsQuote p)
  -------------------------------------------------------
  GhcPs   Annotations only                 HsQuote GhcPs
  GhcRn   Annotations, [PendingRnSplice]   HsQuote GhcRn
  GhcTc   HsBracketTc                      HsQuote GhcTc: unused!

The difficulty is: the typechecker does not typecheck the body of an
untyped quote, so how do we make a (HsQuote GhcTc) to put in the
second field?

Answer: we use the extension constructor of HsQuote, namely XQuote,
and make all the other constructors into DataConCantHappen.  That is,
the only non-bottom value of type (HsQuote GhcTc) is (XQuote noExtField).
Hence the instances

  type instance XExpBr GhcTc = DataConCantHappen
  ...etc...

See the related Note [How brackets and nested splices are handled]

Note [Typechecking Overloaded Quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The main function for typechecking untyped quotations is `tcUntypedBracket`.

Consider an expression quote, `[| e |]`, its type is `forall m . Quote m => m Exp`.
Note carefully that this is overloaded: its type is not `Q Exp` for some fixed Q.

When we typecheck it we therefore create a template of a metavariable
`m` applied to `Exp` and emit a constraint `Quote m`. All this is done
in the `brackTy` function.  `brackTy` also selects the correct
contents type for the quotation (Exp, Type, Decs etc).

The meta variable and the constraint evidence variable are
returned together in a `QuoteWrapper` and then passed along to two further places
during compilation:

1. Typechecking nested splices (immediately in tcPendingSplice)
2. Desugaring quotations (see GHC.HsToCore.Quote)

`tcPendingSplice` takes the `m` type variable as an argument and
checks each nested splice against this variable `m`. During this
process the variable `m` can either be fixed to a specific value or
further constrained by the nested splices.

Once we have checked all the nested splices, the quote type is checked against
the expected return type.

The process is very simple and like typechecking a list where the quotation is
like the container and the splices are the elements of the list which must have
a specific type.

After the typechecking process is completed, the evidence variable for `Quote m`
and the type `m` is stored in a `QuoteWrapper` which is passed through the pipeline
and used when desugaring quotations.

Typechecking typed quotations is a similar idea but the `QuoteWrapper` is stored
in the `PendingStuff` as the nested splices are gathered up in a different way
to untyped splices. Untyped splices are found in the renamer but typed splices are
not typechecked and extracted until during typechecking.

Note [Lifecycle of an untyped splice, and PendingRnSplice]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Untyped splices $(f x) and quasiquotes [p| stuff |] have the following
life cycle. Remember, quasi-quotes are very like splices; see Note [Quasi-quote overview]).

The type structure is

  data HsExpr p = ...
    | HsUntypedSplice (XUntypedSplice p) (HsUntypedSplice p)

  data HsUntypedSplice p
    = HsUntypedSpliceExpr (XUntypedSpliceExpr p) (LHsExpr p)
    | HsQuasiQuote (XQuasiQuote p) (IdP id) (XRec p FastString)

Remember that untyped splices can occur in expressions, patterns,
types, and declarations.  So we have a HsUntypedSplice data
constructor in all four of these types.

Untyped splices never occur in (HsExpr GhcTc), and similarly
patterns etc. So we have

   type instance XUntypedSplice GhcTc = DataConCantHappen

Top-level and nested splices are handled differently.

-------------------------------------
Nested untyped splices/quasiquotes
----------------------------------
When we rename an /untyped/ bracket, such as
     [| f $(g x) |]
we name and lift out all the nested splices, so that when the
typechecker hits the bracket, it can typecheck those nested splices
without having to walk over the untyped bracket code.  Our example
[| f $(g x) |] parses as

    HsUntypedBracket _
       (HsApp (HsVar "f")
              (HsUntypedSplice _ (HsUntypedSpliceExpr _ (g x :: LHsExpr GhcPs)))

RENAMER (rnUntypedBracket):

* Set the ThStage to (Brack s (RnPendingUntyped ps_var))

* Rename the body

* Nested splices (which must be untyped) are renamed (rnUntypedSplice),
  and the results accumulated in ps_var. Each gets a fresh
  SplicePointName, 'spn'

* The SplicePointName connects the `PendingRnSplice` with the particular point
  in the syntax tree where that expression should be spliced in.  That point
  in the tree is identified by `(HsUntypedSpliceNested spn)`.  It is used by
  the desugarer, so that we ultimately generate something like
       let spn = g x
       in App (Var "f") spn

The result is
    HsUntypedBracket
        [PendingRnSplice UntypedExpSplice spn (g x  :: LHsExpr GHcRn)]
        (HsApp (HsVar f) (HsUntypedSplice (HsUntypedSpliceNested spn)
                                          (HsUntypedSpliceExpr _ (g x :: LHsExpr GhcRn))))

Note that a nested splice, such as the `$(g x)` now appears twice:
  - In the PendingRnSplice: this is the version that will later be typechecked
  - In the HsUntypedSpliceExpr in the body of the bracket. This copy is used
    only for pretty printing.

NB: a single untyped bracket can contain many splices, each of a different
`UntypedSpliceFlavour`. For example

   [| let $e0 in (f :: $e1) $e2 (\ $e -> body ) |] + 1

Here $e0 is a declaration splice, $e1 is a type splice, $e2 is an
expression splice, and $e3 is a pattern splice.  The `PendingRnSplice`
keeps track of which is which through its `UntypedSpliceFlavour`
field.

TYPECHECKER (tcUntypedBracket): see also Note [Typechecking Overloaded Quotes]

* Typecheck the [PendingRnSplice] individually, to give [PendingTcSplice]
  So PendingTcSplice is used for both typed and untyped splices.

* Ignore the body of the bracket; just check that the context
  expects a bracket of that type (e.g. a [p| pat |] bracket should
  be in a context needing a (m Pat)

* Stash the whole lot inside a HsBracketTc

Result is:
    HsUntypedBracket
        (HsBracketTc { hsb_splices = [PendingTcSplice spn (g x  :: LHsExpr GHcTc)]
                     , hsb_quote = HsApp (HsVar f)
                                         (HsUntypedSplice (HsUntypedSpliceNested spn)
                                            (HsUntypedSpliceExpr _ (g x :: LHsExpr GhcRn)))
                     })
        (XQuote noExtField)

NB in the typechecker output, the original payload (which would now
have type (HsQuote GhcTc) is stubbed off with (XQuote noExtField). The payload
is now in the hsb_quote field of the HsBracketTc.


-------------------------------------
Top-level untyped splices/quasiquotes
-------------------------------------
A top-level splice (not inside a bracket) does not need a SpliceName,
nor does a top-level splice ever end up inside a PendingRnSplice;
hence HsUntypedSpliceTop does not have a SplicePointName field.

Example $(g x).  This is parsed as

  HsUntypedSplice _ (HsUntypedSpliceExpr _ ((g x) :: LHsExpr GhcPs))

Renamer: the renamer runs the splice, so the output of the renamer looks like

  HsUntypedSplice (HsUntypedSpliceTop fins (e2 :: LHsExpr GhcRn))
                  (HsUntypedSpliceExpr ((g x) :: LHsExpr GhcRn))

where 'e2' is the result of running (g x) to
             produce the syntax tree for 'e2'
      'fins' is a bunch of TH finalisers, to be run later.

Typechecker: the typechecker simply adds the finalisers, and
typechecks e2, discarding the HsUntypedSplice altogether.


Note [Lifecycle of an typed splice, and PendingTcSplice]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

----------------------
Nested, typed splices
----------------------
When we typecheck a /typed/ bracket, we lift nested splices out as
`PendingTcSplice`, very similar to Note [PendingRnSplice]. Again, the
splice needs a SplicePointName, for the desguarer to use to connect
the splice expression with the point in the syntax tree where it is
used.  Example:
     [||  f $$(g 2)||]

Parser: this is parsed as

    HsTypedBracket _ (HsApp (HsVar "f")
                            (HsTypedSplice _ (g 2 :: LHsExpr GhcPs)))

RENAMER (rnTypedSplice): the renamer adds a SplicePointName, spn:

    HsTypedBracket _ (HsApp (HsVar "f")
                            (HsTypedSplice spn (g x :: LHsExpr GhcRn)))

TYPECHECKER (tcTypedBracket):

* Set the ThStage to (Brack s (TcPending ps_var lie_var))

* Typecheck the body, and keep the elaborated result (despite never using it!)

* Nested splices (which must be typed) are typechecked by tcNestedSplice, and
  the results accumulated in ps_var; their constraints accumulate in lie_var

* Result is a HsTypedBracket (HsBracketTc rn_brack ty quote_wrapper pending_splices) tc_brack
  where rn_brack is the untyped renamed exp quote constructed from the typed renamed expression :: HsQuote GhcRn

Just like untyped brackets, dump the output into a HsBracketTc.

    HsTypedBracket
        (HsBracketTc { hsb_splices = [PendingTcSplice spn (g x  :: LHsExpr GHcTc)]
                     , hsb_quote = HsApp (HsVar f)
                                         (HsUntypedSplice (HsUntypedSpliceNested spn)
                                            (HsUntypedSpliceExpr _ (g x :: LHsExpr GhcRn)))
                     })
        (panic "should never be looked at")

NB: we never need to represent typed /nested/ splices in phase GhcTc.

There are only typed expression splices so `PendingTcSplice` doesn't have a
flavour field.


--------------------------------
Top-level, typed splices $$(f x)
--------------------------------
Typed splices are renamed and typechecked, but only actually run in
the zonker, after typechecking. See Note [Running typed splices in the zonker]

* Output of parser:
  HsTypedSplice _ (e :: HsExpr GhcPs)

* Output of renamer:
  HsTypedSplice (n :: SplicePointName) (e :: HsExpr GhcRn)

* Output of typechecker: (top-level splices only)
  HsTypedSplice (del_splice :: DelayedSplice) (e :: HsExpr GhcTc)
  where 'del_splice' is something the zonker can run to produce
           the syntax tree to splice in.
           See Note [Running typed splices in the zonker]

Note [Desugaring of brackets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In both cases, desugaring happens like this:
  * Hs*Bracket is desugared by GHC.HsToCore.Quote.dsBracket using the renamed
    expression held in `HsBracketTc` (`type instance X*Bracket GhcTc = HsBracketTc`). It

      a) Extends the ds_meta environment with the PendingSplices
         attached to the bracket

      b) Converts the quoted (HsExpr Name) to a CoreExpr that, when
         run, will produce a suitable TH expression/type/decl.  This
         is why we leave the *renamed* expression attached to the bracket:
         the quoted expression should not be decorated with all the goop
         added by the type checker

  * Each splice carries a unique Name, called a "splice point", thus
    ${n}(e).  The name is initialised to an (Unqual "splice") when the
    splice is created; the renamer gives it a unique.

  * When GHC.HsToCore.Quote (used to desugar the body of the bracket) comes across
    a splice, it looks up the splice's Name, n, in the ds_meta envt,
    to find an (HsExpr Id) that should be substituted for the splice;
    it just desugars it to get a CoreExpr (GHC.HsToCore.Quote.repSplice).

Example:
    Source:       f = [| Just $(g 3) |]
      The [| |] part is a HsUntypedBracket GhcPs

    Typechecked:  f = [| Just ${s7}(g 3) |]{s7 = g Int 3}
      The [| |] part is a HsUntypedBracket GhcTc, containing *renamed*
        (not typechecked) expression (see Note [The life cycle of a TH quotation])
      The "s7" is the "splice point"; the (g Int 3) part
        is a typechecked expression

    Desugared:    f = do { s7 <- g Int 3
                         ; return (ConE "Data.Maybe.Just" s7) }

-}

{-
************************************************************************
*                                                                      *
\subsection{Main interface + stubs for the non-GHCI case
*                                                                      *
************************************************************************
-}

-- None of these functions add constraints to the LIE

tcTypedBracket    :: HsExpr GhcRn -> LHsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
tcUntypedBracket  :: HsExpr GhcRn -> HsQuote GhcRn -> [PendingRnSplice] -> ExpRhoType
                  -> TcM (HsExpr GhcTc)
tcTypedSplice     :: Name -> LHsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)

getUntypedSpliceBody :: HsUntypedSpliceResult (HsExpr GhcRn) -> TcM (HsExpr GhcRn)
runAnnotation        :: CoreAnnTarget -> LHsExpr GhcRn -> TcM Annotation

{-
************************************************************************
*                                                                      *
\subsection{Quoting an expression}
*                                                                      *
************************************************************************
-}

-- See Note [How brackets and nested splices are handled]
tcTypedBracket rn_expr expr res_ty
  = addErrCtxt (quotationCtxtDoc expr) $
    do { cur_stage <- getStage
       ; ps_ref <- newMutVar []
       ; lie_var <- getConstraintVar   -- Any constraints arising from nested splices
                                       -- should get thrown into the constraint set
                                       -- from outside the bracket

       -- Make a new type variable for the type of the overall quote
       ; m_var <- mkTyVarTy <$> mkMetaTyVar
       -- Make sure the type variable satisfies Quote
       ; ev_var <- emitQuoteWanted m_var
       -- Bundle them together so they can be used in GHC.HsToCore.Quote for desugaring
       -- brackets.
       ; let wrapper = QuoteWrapper ev_var m_var
       -- Typecheck expr to make sure it is valid.
       -- The typechecked expression won't be used, so we just discard it
       --   (See Note [The life cycle of a TH quotation] in GHC.Hs.Expr)
       -- We'll typecheck it again when we splice it in somewhere
       ; (tc_expr, expr_ty) <- setStage (Brack cur_stage (TcPending ps_ref lie_var wrapper)) $
                                tcScalingUsage ManyTy $
                                -- Scale by Many, TH lifting is currently nonlinear (#18465)
                                tcInferRhoNC expr
                                -- NC for no context; tcBracket does that
       ; let rep = getRuntimeRep expr_ty
       ; meta_ty <- tcTExpTy m_var expr_ty
       ; ps' <- readMutVar ps_ref
       ; codeco <- tcLookupId unsafeCodeCoerceName
       ; bracket_ty <- mkAppTy m_var <$> tcMetaTy expTyConName
       ; let brack_tc = HsBracketTc { hsb_quote = ExpBr noExtField expr, hsb_ty = bracket_ty
                                    , hsb_wrap  = Just wrapper, hsb_splices = ps' }
             -- The tc_expr is stored here so that the expression can be used in HIE files.
             brack_expr = HsTypedBracket brack_tc tc_expr
       ; tcWrapResultO (Shouldn'tHappenOrigin "TH typed bracket expression")
                       rn_expr
                       (unLoc (mkHsApp (mkLHsWrap (applyQuoteWrapper wrapper)
                                                  (nlHsTyApp codeco [rep, expr_ty]))
                                      (noLocA brack_expr)))
                       meta_ty res_ty }

-- See Note [Typechecking Overloaded Quotes]
tcUntypedBracket rn_expr brack ps res_ty
  = do { traceTc "tc_bracket untyped" (ppr brack $$ ppr ps)

       -- Create the type m Exp for expression bracket, m Type for a type
       -- bracket and so on. The brack_info is a Maybe because the
       -- VarBracket ('a) isn't overloaded, but also shouldn't contain any
       -- splices.
       ; (brack_info, expected_type) <- brackTy brack

       -- Match the expected type with the type of all the internal
       -- splices. They might have further constrained types and if they do
       -- we want to reflect that in the overall type of the bracket.
       ; ps' <- case quoteWrapperTyVarTy <$> brack_info of
                  Just m_var -> mapM (tcPendingSplice m_var) ps
                  Nothing -> assert (null ps) $ return []

       -- Notice that we don't attempt to typecheck the body
       -- of the bracket, which is in brack.
       ; traceTc "tc_bracket done untyped" (ppr expected_type)

       -- Unify the overall type of the bracket with the expected result type
       ; tcWrapResultO BracketOrigin rn_expr
            (HsUntypedBracket (HsBracketTc { hsb_quote = brack, hsb_ty = expected_type
                                           , hsb_wrap = brack_info, hsb_splices = ps' })
                              (XQuote noExtField))
                -- (XQuote noExtField): see Note [The life cycle of a TH quotation] in GHC.Hs.Expr
            expected_type res_ty

       }

-- | A type variable with kind * -> * named "m"
mkMetaTyVar :: TcM TyVar
mkMetaTyVar =
  newNamedFlexiTyVar (fsLit "m") (mkVisFunTyMany liftedTypeKind liftedTypeKind)


-- | For a type 'm', emit the constraint 'Quote m'.
emitQuoteWanted :: Type -> TcM EvVar
emitQuoteWanted m_var =  do
        quote_con <- tcLookupTyCon quoteClassName
        emitWantedEvVar BracketOrigin $
          mkTyConApp quote_con [m_var]

---------------
-- | Compute the expected type of a quotation, and also the QuoteWrapper in
-- the case where it is an overloaded quotation. All quotation forms are
-- overloaded aprt from Variable quotations ('foo)
brackTy :: HsQuote GhcRn -> TcM (Maybe QuoteWrapper, Type)
brackTy b =
  let mkTy n = do
        -- New polymorphic type variable for the bracket
        m_var <- mkTyVarTy <$> mkMetaTyVar
        -- Emit a Quote constraint for the bracket
        ev_var <- emitQuoteWanted m_var
        -- Construct the final expected type of the quote, for example
        -- m Exp or m Type
        final_ty <- mkAppTy m_var <$> tcMetaTy n
        -- Return the evidence variable and metavariable to be used during
        -- desugaring.
        let wrapper = QuoteWrapper ev_var m_var
        return (Just wrapper, final_ty)
  in
  case b of
    (VarBr {}) -> (Nothing,) <$> tcMetaTy nameTyConName
                                           -- Result type is Var (not Quote-monadic)
    (ExpBr {})  -> mkTy expTyConName  -- Result type is m Exp
    (TypBr {})  -> mkTy typeTyConName -- Result type is m Type
    (DecBrG {}) -> mkTy decsTyConName -- Result type is m [Dec]
    (PatBr {})  -> mkTy patTyConName  -- Result type is m Pat
    (DecBrL {}) -> panic "tcBrackTy: Unexpected DecBrL"

---------------
-- | Typechecking a pending splice from a untyped bracket
tcPendingSplice :: TcType -- Metavariable for the expected overall type of the
                          -- quotation.
                -> PendingRnSplice
                -> TcM PendingTcSplice
tcPendingSplice m_var (PendingRnSplice flavour splice_name expr)
  -- See Note [Typechecking Overloaded Quotes]
  = do { meta_ty <- tcMetaTy meta_ty_name
         -- Expected type of splice, e.g. m Exp
       ; let expected_type = mkAppTy m_var meta_ty
       ; expr' <- tcScalingUsage ManyTy $ tcCheckPolyExpr expr expected_type
                  -- Scale by Many, TH lifting is currently nonlinear (#18465)
       ; return (PendingTcSplice splice_name expr') }
  where
     meta_ty_name = case flavour of
                       UntypedExpSplice  -> expTyConName
                       UntypedPatSplice  -> patTyConName
                       UntypedTypeSplice -> typeTyConName
                       UntypedDeclSplice -> decsTyConName

---------------
-- Takes a m and tau and returns the type m (TExp tau)
tcTExpTy :: TcType -> TcType -> TcM TcType
tcTExpTy m_ty exp_ty
  = do { unless (isTauTy exp_ty) $ addErr $
          TcRnTHError $ TypedTHError $ TypedTHWithPolyType exp_ty
       ; codeCon <- tcLookupTyCon codeTyConName
       ; let rep = getRuntimeRep exp_ty
       ; return (mkTyConApp codeCon [m_ty, rep, exp_ty]) }

quotationCtxtDoc :: LHsExpr GhcRn -> SDoc
quotationCtxtDoc br_body
  = hang (text "In the Template Haskell quotation")
         2 (thTyBrackets . ppr $ br_body)


  -- The whole of the rest of the file is the else-branch (ie stage2 only)


{-
************************************************************************
*                                                                      *
\subsection{Splicing an expression}
*                                                                      *
************************************************************************
-}

-- getUntypedSpliceBody: the renamer has expanded the splice.
-- Just run the finalizers that it produced, and return
-- the renamed expression
getUntypedSpliceBody (HsUntypedSpliceTop { utsplice_result_finalizers = mod_finalizers
                                         , utsplice_result = rn_expr })
  = do { addModFinalizersWithLclEnv mod_finalizers
       ; return rn_expr }
getUntypedSpliceBody (HsUntypedSpliceNested {})
  = panic "tcTopUntypedSplice: invalid nested splice"

tcTypedSplice splice_name expr res_ty
  = addErrCtxt (typedSpliceCtxtDoc splice_name expr) $
    setSrcSpan (getLocA expr)    $ do
    { stage <- getStage
    ; case stage of
          Splice {}            -> tcTopSplice expr res_ty
          Brack pop_stage pend -> tcNestedSplice pop_stage pend splice_name expr res_ty
          RunSplice _          ->
            -- See Note [RunSplice ThLevel] in "GHC.Tc.Types".
            pprPanic ("tcSpliceExpr: attempted to typecheck a splice when " ++
                      "running another splice") (pprTypedSplice (Just splice_name) expr)
          Comp                 -> tcTopSplice expr res_ty
    }

{- Note [Collecting modFinalizers in typed splices]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'qAddModFinalizer' of the @Quasi TcM@ instance adds finalizers in the local
environment (see Note [Delaying modFinalizers in untyped splices] in
GHC.Rename.Splice). Thus after executing the splice, we move the finalizers to the
finalizer list in the global environment and set them to use the current local
environment (with 'addModFinalizersWithLclEnv').

-}

------------------
tcTopSplice :: LHsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
tcTopSplice expr res_ty
  = do { -- Typecheck the expression,
         -- making sure it has type Q (T res_ty)
         res_ty <- expTypeToType res_ty
       ; q_type <- tcMetaTy qTyConName
       -- Top level splices must still be of type Q (TExp a)
       ; meta_exp_ty <- tcTExpTy q_type res_ty
       ; q_expr <- tcTopSpliceExpr Typed $
                   tcCheckMonoExpr expr meta_exp_ty
       ; lcl_env <- getLclEnv
       ; let delayed_splice
              = DelayedSplice lcl_env expr res_ty q_expr
       ; return (HsTypedSplice delayed_splice q_expr)

       }

-------------------
tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr GhcTc) -> TcM (LHsExpr GhcTc)
-- Note [How top-level splices are handled]
-- Type check an expression that is the body of a top-level splice
--   (the caller will compile and run it)
-- Note that set the level to Splice, regardless of the original level,
-- before typechecking the expression.  For example:
--      f x = $( ...$(g 3) ... )
-- The recursive call to tcCheckPolyExpr will simply expand the
-- inner escape before dealing with the outer one

tcTopSpliceExpr isTypedSplice tc_action
  = checkNoErrs $  -- checkNoErrs: must not try to run the thing
                   -- if the type checker fails!
    setStage (Splice isTypedSplice) $
    do {    -- Typecheck the expression
         (mb_expr', wanted) <- tryCaptureConstraints tc_action
             -- If tc_action fails (perhaps because of insoluble constraints)
             -- we want to capture and report those constraints, else we may
             -- just get a silent failure (#20179). Hence the 'try' part.

       ; const_binds <- simplifyTop wanted

       ; case mb_expr' of
            Nothing    -> failM   -- In this case simplifyTop should have
                                  -- reported some errors
            Just expr' -> return $ mkHsDictLet (EvBinds const_binds) expr' }

------------------
tcNestedSplice :: ThStage -> PendingStuff -> Name
                -> LHsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
    -- See Note [How brackets and nested splices are handled]
    -- A splice inside brackets
tcNestedSplice pop_stage (TcPending ps_var lie_var q@(QuoteWrapper _ m_var)) splice_name expr res_ty
  = do { res_ty <- expTypeToType res_ty
       ; let rep = getRuntimeRep res_ty
       ; meta_exp_ty <- tcTExpTy m_var res_ty
       ; expr' <- setStage pop_stage $
                  setConstraintVar lie_var $
                  tcCheckMonoExpr expr meta_exp_ty
       ; untype_code <- tcLookupId unTypeCodeName
       ; let expr'' = mkHsApp
                        (mkLHsWrap (applyQuoteWrapper q)
                          (nlHsTyApp untype_code [rep, res_ty])) expr'
       ; ps <- readMutVar ps_var
       ; writeMutVar ps_var (PendingTcSplice splice_name expr'' : ps)

       -- The returned expression is ignored; it's in the pending splices
       ; return stubNestedSplice }

tcNestedSplice _ _ splice_name _ _
  = pprPanic "tcNestedSplice: rename stage found" (ppr splice_name)


------------------
-- This is called in the zonker
-- See Note [Running typed splices in the zonker]
runTopSplice :: DelayedSplice -> TcM (HsExpr GhcTc)
runTopSplice (DelayedSplice lcl_env orig_expr res_ty q_expr)
  = restoreLclEnv lcl_env $
    do { zonked_ty <- liftZonkM $ zonkTcType res_ty
       ; zonked_q_expr <- zonkTopLExpr q_expr
        -- See Note [Collecting modFinalizers in typed splices].
       ; modfinalizers_ref <- newTcRef []
         -- Run the expression
       ; expr2 <- setStage (RunSplice modfinalizers_ref) $
                    runMetaE zonked_q_expr
       ; mod_finalizers <- readTcRef modfinalizers_ref
       ; addModFinalizersWithLclEnv $ ThModFinalizers mod_finalizers
       -- We use orig_expr here and not q_expr when tracing as a call to
       -- unsafeCodeCoerce is added to the original expression by the
       -- typechecker when typed quotes are type checked.
       ; traceSplice (SpliceInfo { spliceDescription = "expression"
                                 , spliceIsDecl      = False
                                 , spliceSource      = Just orig_expr
                                 , spliceGenerated   = ppr expr2 })
        -- Rename and typecheck the spliced-in expression,
        -- making sure it has type res_ty
        -- These steps should never fail; this is a *typed* splice
       ; (res, wcs) <-
            captureConstraints $
              addErrCtxt (spliceResultDoc zonked_q_expr) $ do
                { (exp3, _fvs) <- rnLExpr expr2
                ; tcCheckMonoExpr exp3 zonked_ty }
       ; ev <- simplifyTop wcs
       ; return $ unLoc (mkHsDictLet (EvBinds ev) res)
       }


{-
************************************************************************
*                                                                      *

*                                                                      *
************************************************************************
-}

typedSpliceCtxtDoc :: SplicePointName -> LHsExpr GhcRn -> SDoc
typedSpliceCtxtDoc n splice
  = hang (text "In the Template Haskell splice")
         2 (pprTypedSplice (Just n) splice)

spliceResultDoc :: LHsExpr GhcTc -> SDoc
spliceResultDoc expr
  = sep [ text "In the result of the splice:"
        , nest 2 (text "$$" <> ppr expr)
        , text "To see what the splice expanded to, use -ddump-splices"]

stubNestedSplice :: HsExpr GhcTc
-- Used when we need a (LHsExpr GhcTc) that we are never going
-- to look at.  We could use "panic" but that's confusing if we ever
-- do a debug-print.  The warning is because this should never happen
-- /except/ when doing debug prints.
stubNestedSplice = warnPprTrace True "stubNestedSplice" empty $
                   HsLit noExtField (mkHsString "stubNestedSplice")


{-
************************************************************************
*                                                                      *
        Annotations
*                                                                      *
************************************************************************
-}

runAnnotation target expr = do
    -- Find the classes we want instances for in order to call toAnnotationWrapper
    loc <- getSrcSpanM
    data_class <- tcLookupClass dataClassName
    to_annotation_wrapper_id <- tcLookupId toAnnotationWrapperName

    -- Check the instances we require live in another module (we want to execute it..)
    -- and check identifiers live in other modules using TH stage checks. tcSimplifyStagedExpr
    -- also resolves the LIE constraints to detect e.g. instance ambiguity
    zonked_wrapped_expr' <- zonkTopLExpr =<< tcTopSpliceExpr Untyped (
           do { (expr', expr_ty) <- tcInferRhoNC expr
                -- We manually wrap the typechecked expression in a call to toAnnotationWrapper
                -- By instantiating the call >here< it gets registered in the
                -- LIE consulted by tcTopSpliceExpr
                -- and hence ensures the appropriate dictionary is bound by const_binds
              ; wrapper <- instCall AnnOrigin [expr_ty] [mkClassPred data_class [expr_ty]]
              ; let loc' = noAnnSrcSpan loc
              ; let specialised_to_annotation_wrapper_expr
                      = L loc' (mkHsWrap wrapper
                                 (HsVar noExtField (L (noAnnSrcSpan loc) to_annotation_wrapper_id)))
              ; return (L loc' (HsApp noExtField
                                specialised_to_annotation_wrapper_expr expr'))
                                })

    -- Run the appropriately wrapped expression to get the value of
    -- the annotation and its dictionaries. The return value is of
    -- type AnnotationWrapper by construction, so this conversion is
    -- safe
    serialized <- runMetaAW zonked_wrapped_expr'
    return Annotation {
               ann_target = target,
               ann_value = serialized
           }

convertAnnotationWrapper :: ForeignHValue -> TcM Serialized
convertAnnotationWrapper fhv = do
  interp <- tcGetInterp
  case interpInstance interp of
    ExternalInterp {} -> runTH THAnnWrapper fhv
#if defined(HAVE_INTERNAL_INTERPRETER)
    InternalInterp    -> do
      annotation_wrapper <- liftIO $ wormhole interp fhv
      return $
        case unsafeCoerce annotation_wrapper of
           AnnotationWrapper value | let serialized = toSerialized serializeWithData value ->
               -- Got the value and dictionaries: build the serialized value and
               -- call it a day. We ensure that we seq the entire serialized value
               -- in order that any errors in the user-written code for the
               -- annotation are exposed at this point.  This is also why we are
               -- doing all this stuff inside the context of runMeta: it has the
               -- facilities to deal with user error in a meta-level expression
               seqSerialized serialized `seq` serialized

-- | Force the contents of the Serialized value so weknow it doesn't contain any bottoms
seqSerialized :: Serialized -> ()
seqSerialized (Serialized the_type bytes) = the_type `seq` bytes `seqList` ()

#endif

{-
************************************************************************
*                                                                      *
\subsection{Running an expression}
*                                                                      *
************************************************************************
-}

runQuasi :: TH.Q a -> TcM a
runQuasi act = TH.runQ act

runRemoteModFinalizers :: ThModFinalizers -> TcM ()
runRemoteModFinalizers (ThModFinalizers finRefs) = do
  let withForeignRefs [] f = f []
      withForeignRefs (x : xs) f = withForeignRef x $ \r ->
        withForeignRefs xs $ \rs -> f (r : rs)
  interp <- tcGetInterp

  case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
    InternalInterp -> do
      qs <- liftIO (withForeignRefs finRefs $ mapM localRef)
      runQuasi $ sequence_ qs
#endif

    ExternalInterp ext -> withExtInterp ext $ \inst -> do
      tcg <- getGblEnv
      th_state <- readTcRef (tcg_th_remote_state tcg)
      case th_state of
        Nothing -> return () -- TH was not started, nothing to do
        Just fhv -> do
          r <- liftIO $ withForeignRef fhv $ \st ->
            withForeignRefs finRefs $ \qrefs ->
              sendMessageDelayedResponse inst (RunModFinalizers st qrefs)
          () <- runRemoteTH inst []
          qr <- liftIO $ receiveDelayedResponse inst r
          checkQResult qr

runQResult
  :: (a -> String)
  -> (Origin -> SrcSpan -> a -> b)
  -> (ForeignHValue -> TcM a)
  -> SrcSpan
  -> ForeignHValue {- TH.Q a -}
  -> TcM b
runQResult show_th f runQ expr_span hval
  = do { th_result <- runQ hval
       ; th_origin <- getThSpliceOrigin
       ; traceTc "Got TH result:" (text (show_th th_result))
       ; return (f th_origin expr_span th_result) }


-----------------
runMeta :: (MetaHook TcM -> LHsExpr GhcTc -> TcM hs_syn)
        -> LHsExpr GhcTc
        -> TcM hs_syn
runMeta unwrap e = do
    hooks <- getHooks
    case runMetaHook hooks of
        Nothing -> unwrap defaultRunMeta e
        Just h  -> unwrap h e

defaultRunMeta :: MetaHook TcM
defaultRunMeta (MetaE r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToHsExpr runTHExp)
defaultRunMeta (MetaP r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToPat runTHPat)
defaultRunMeta (MetaT r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToHsType runTHType)
defaultRunMeta (MetaD r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToHsDecls runTHDec)
defaultRunMeta (MetaAW r)
  = fmap r . runMeta' False (const empty) (const $ fmap Right . convertAnnotationWrapper)
    -- We turn off showing the code in meta-level exceptions because doing so exposes
    -- the toAnnotationWrapper function that we slap around the user's code

----------------
runMetaAW :: LHsExpr GhcTc         -- Of type AnnotationWrapper
          -> TcM Serialized
runMetaAW = runMeta metaRequestAW

runMetaE :: LHsExpr GhcTc          -- Of type (Q Exp)
         -> TcM (LHsExpr GhcPs)
runMetaE = runMeta metaRequestE

runMetaP :: LHsExpr GhcTc          -- Of type (Q Pat)
         -> TcM (LPat GhcPs)
runMetaP = runMeta metaRequestP

runMetaT :: LHsExpr GhcTc          -- Of type (Q Type)
         -> TcM (LHsType GhcPs)
runMetaT = runMeta metaRequestT

runMetaD :: LHsExpr GhcTc          -- Of type Q [Dec]
         -> TcM [LHsDecl GhcPs]
runMetaD = runMeta metaRequestD

{- Note [Errors in desugaring a splice]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we do if there are errors when desugaring a splice? We should
abort. There are several cases to consider:

(a) The desugarer hits an unrecoverable error and fails in the monad.
(b) The desugarer hits a recoverable error, reports it, and continues.
(c) The desugarer reports a fatal warning (with -Werror), reports it, and continues.
(d) The desugarer reports a non-fatal warning, and continues.

Each case is tested in th/T19709[abcd].

General principle: we wish to report all messages from dealing with a splice
eagerly, as these messages arise during an earlier stage than type-checking
generally. It's also likely that a compile-time warning from spliced code
will be easier to understand then an error that arises from processing the
code the splice produces. (Rationale: the warning will be about the code the
user actually wrote, not what is generated.)

Case (a): We have no choice but to abort here, but we must make sure that
the messages are printed or logged before aborting. Logging them is annoying,
because we're in the type-checker, and the messages are DsMessages, from the
desugarer. So we report and then fail in the monad. This case is detected
by the fact that initDsTc returns Nothing.

Case (b): We detect this case by looking for errors in the messages returned
from initDsTc and aborting if we spot any (after printing, of course). Note
that initDsTc will return a Just ds_expr in this case, but we don't wish to
use the (likely very bogus) expression.

Case (c): This is functionally the same as (b), except that the expression
isn't bogus. We still don't wish to use it, as the user's request for -Werror
tells us not to.

Case (d): We report the warnings and then carry on with the expression.
This might result in warnings printed out of source order, but this is
appropriate, as the warnings from the splice arise from an earlier stage
of compilation.

Previously, we failed to abort in cases (b) and (c), leading to #19709.
-}

---------------
runMeta' :: Bool                 -- Whether code should be printed in the exception message
         -> (hs_syn -> SDoc)                                    -- how to print the code
         -> (SrcSpan -> ForeignHValue -> TcM (Either RunSpliceFailReason hs_syn)) -- How to run x
         -> LHsExpr GhcTc        -- Of type x; typically x = Q TH.Exp, or
                                 --    something like that
         -> TcM hs_syn           -- Of type t
runMeta' show_code ppr_hs run_and_convert expr
  = do  { traceTc "About to run" (ppr expr)
        ; recordThSpliceUse -- seems to be the best place to do this,
                            -- we catch all kinds of splices and annotations.

        -- Check that we've had no errors of any sort so far.
        -- For example, if we found an error in an earlier defn f, but
        -- recovered giving it type f :: forall a.a, it'd be very dodgy
        -- to carry on.  Mind you, the staging restrictions mean we won't
        -- actually run f, but it still seems wrong. And, more concretely,
        -- see #5358 for an example that fell over when trying to
        -- reify a function with an unlifted kind in it.  (These don't occur
        -- in type-correct programs.)
        ; failIfErrsM

        -- run plugins
        ; hsc_env <- getTopEnv
        ; expr' <- withPlugins (hsc_plugins hsc_env) spliceRunAction expr

        -- Desugar
        ; (ds_msgs, mb_ds_expr) <- initDsTc (dsLExpr expr')

        -- Print any messages (even warnings) eagerly: they might be helpful if anything
        -- goes wrong. See Note [Errors in desugaring a splice]. This happens in all
        -- cases.
        ; logger <- getLogger
        ; diag_opts <- initDiagOpts <$> getDynFlags
        ; print_config <- initDsMessageOpts <$> getDynFlags
        ; liftIO $ printMessages logger print_config diag_opts ds_msgs

        ; ds_expr <- case mb_ds_expr of
            Nothing      -> failM   -- Case (a) from Note [Errors in desugaring a splice]
            Just ds_expr ->  -- There still might be a fatal warning or recoverable
                             -- Cases (b) and (c) from Note [Errors in desugaring a splice]
              do { when (errorsOrFatalWarningsFound ds_msgs)
                     failM
                 ; return ds_expr }

        -- Compile and link it; might fail if linking fails
        ; src_span <- getSrcSpanM
        ; traceTc "About to run (desugared)" (ppr ds_expr)
        ; either_hval <- tryM $ liftIO $
                         GHC.Driver.Main.hscCompileCoreExpr hsc_env src_span ds_expr
        ; case either_hval of {
            Left exn   -> fail_with_exn SplicePhase_CompileAndLink exn ;
            Right (hval, needed_mods, needed_pkgs) -> do

        {       -- Coerce it to Q t, and run it

                -- Running might fail if it throws an exception of any kind (hence tryAllM)
                -- including, say, a pattern-match exception in the code we are running
                --
                -- We also do the TH -> HS syntax conversion inside the same
                -- exception-catching thing so that if there are any lurking
                -- exceptions in the data structure returned by hval, we'll
                -- encounter them inside the try
                --
                -- See Note [Exceptions in TH]
          let expr_span = getLocA expr
        ; recordThNeededRuntimeDeps needed_mods needed_pkgs
        ; either_tval <- tryAllM $
                         setSrcSpan expr_span $ -- Set the span so that qLocation can
                                                -- see where this splice is
             do { mb_result <- run_and_convert expr_span hval
                ; case mb_result of
                    Left err     -> failWithTc $
                      TcRnTHError $ THSpliceFailed $ RunSpliceFailure err
                    Right result -> do { traceTc "Got HsSyn result:" (ppr_hs result)
                                       ; return $! result } }

        ; case either_tval of
            Right v -> return v
            Left se -> case fromException se of
                         Just IOEnvFailure -> failM -- Error already in Tc monad
                         _ -> fail_with_exn SplicePhase_Run se -- Exception
        }}}
  where
    -- see Note [Concealed TH exceptions]
    fail_with_exn :: Exception e => SplicePhase -> e -> TcM a
    fail_with_exn phase exn = do
        exn_msg <- liftIO $ Panic.safeShowException exn
        failWithTc $ TcRnTHError $ THSpliceFailed $
          SpliceThrewException phase (toException exn) exn_msg expr show_code

{-
Note [Running typed splices in the zonker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See #15471 for the full discussion.

For many years typed splices were run immediately after they were type checked
however, this is too early as it means to zonk some type variables before
they can be unified with type variables in the surrounding context.

For example,

```
module A where

test_foo :: forall a . Q (TExp (a -> a))
test_foo = [|| id ||]

module B where

import A

qux = $$(test_foo)
```

We would expect `qux` to have inferred type `forall a . a -> a` but if
we run the splices too early the unified variables are zonked to `Any`. The
inferred type is the unusable `Any -> Any`.

To run the splice, we must compile `test_foo` all the way to byte code.
But at the moment when the type checker is looking at the splice, test_foo
has type `Q (TExp (alpha -> alpha))` and we
certainly can't compile code involving unification variables!

We could default `alpha` to `Any` but then we infer `qux :: Any -> Any`
which definitely is not what we want.  Moreover, if we had
  qux = [$$(test_foo), (\x -> x +1::Int)]
then `alpha` would have to be `Int`.

Conclusion: we must defer taking decisions about `alpha` until the
typechecker is done; and *then* we can run the splice.  It's fine to do it
later, because we know it'll produce type-correct code.

Deferring running the splice until later, in the zonker, means that the
unification variables propagate upwards from the splice into the surrounding
context and are unified correctly.

This is implemented by storing the arguments we need for running the splice
in a `DelayedSplice`. In the zonker, the arguments are passed to
`GHC.Tc.Gen.Splice.runTopSplice` and the expression inserted into the AST as normal.



Note [Exceptions in TH]
~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have something like this
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

  * The TcM monad is an instance of Quasi (see GHC.Tc.Gen.Splice), and it implements
    (qReport True s) by using addErr to add an error message to the bag of errors.
    The 'fail' in TcM raises an IOEnvFailure exception

 * 'qReport' forces the message to ensure any exception hidden in unevaluated
   thunk doesn't get into the bag of errors. Otherwise the following splice
   will trigger panic (#8987):
        $(fail undefined)
   See also Note [Concealed TH exceptions]

  * So, when running a splice, we catch all exceptions; then for
        - an IOEnvFailure exception, we assume the error is already
                in the error-bag (above)
        - other errors, we add an error to the bag
    and then fail

Note [Concealed TH exceptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When displaying the error message contained in an exception originated from TH
code, we need to make sure that the error message itself does not contain an
exception.  For example, when executing the following splice:

    $( error ("foo " ++ error "bar") )

the message for the outer exception is a thunk which will throw the inner
exception when evaluated.

For this reason, we display the message of a TH exception using the
'safeShowException' function, which recursively catches any exception thrown
when showing an error message.


To call runQ in the Tc monad, we need to make TcM an instance of Quasi:
-}

instance TH.Quasi TcM where
  qNewName s = do { u <- newUnique
                  ; let i = toInteger (getKey u)
                  ; return (TH.mkNameU s i) }

  -- 'msg' is forced to ensure exceptions don't escape,
  -- see Note [Exceptions in TH]
  qReport True msg  = seqList msg $ addErr        $ TcRnTHError $ ReportCustomQuasiError True  msg
  qReport False msg = seqList msg $ addDiagnostic $ TcRnTHError $ ReportCustomQuasiError False msg

  qLocation :: TcM TH.Loc
  qLocation = do { m <- getModule
                 ; l <- getSrcSpanM
                 ; r <- case l of
                        UnhelpfulSpan _ -> pprPanic "qLocation: Unhelpful location"
                                                    (ppr l)
                        RealSrcSpan s _ -> return s
                 ; return (TH.Loc { TH.loc_filename = unpackFS (srcSpanFile r)
                                  , TH.loc_module   = moduleNameString (moduleName m)
                                  , TH.loc_package  = unitString (moduleUnit m)
                                  , TH.loc_start = (srcSpanStartLine r, srcSpanStartCol r)
                                  , TH.loc_end = (srcSpanEndLine   r, srcSpanEndCol   r) }) }

  qLookupName       = lookupName
  qReify            = reify
  qReifyFixity nm   = lookupThName nm >>= reifyFixity
  qReifyType        = reifyTypeOfThing
  qReifyInstances   = reifyInstances
  qReifyRoles       = reifyRoles
  qReifyAnnotations = reifyAnnotations
  qReifyModule      = reifyModule
  qReifyConStrictness nm = do { nm' <- lookupThName nm
                              ; dc  <- tcLookupDataCon nm'
                              ; let bangs = dataConImplBangs dc
                              ; return (map reifyDecidedStrictness bangs) }

        -- For qRecover, discard error messages if
        -- the recovery action is chosen.  Otherwise
        -- we'll only fail higher up.
  qRecover recover main = tryTcDiscardingErrs recover main

  qGetPackageRoot = do
    dflags <- getDynFlags
    return $ fromMaybe "." (workingDirectory dflags)

  qAddDependentFile fp = do
    ref <- fmap tcg_dependent_files getGblEnv
    dep_files <- readTcRef ref
    writeTcRef ref (fp:dep_files)

  qAddTempFile suffix = do
    dflags <- getDynFlags
    logger <- getLogger
    tmpfs  <- hsc_tmpfs <$> getTopEnv
    liftIO $ newTempName logger tmpfs (tmpDir dflags) TFL_GhcSession suffix

  qAddTopDecls thds = do
      l <- getSrcSpanM
      th_origin <- getThSpliceOrigin
      let either_hval = convertToHsDecls th_origin l thds
      ds <- case either_hval of
              Left exn -> failWithTc $ TcRnTHError $ AddTopDeclsError $
                AddTopDeclsRunSpliceFailure exn
              Right ds -> return ds
      mapM_ (checkTopDecl . unLoc) ds
      th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
      updTcRef th_topdecls_var (\topds -> ds ++ topds)
    where
      checkTopDecl :: HsDecl GhcPs -> TcM ()
      checkTopDecl (ValD _ binds)
        = mapM_ bindName (collectHsBindBinders CollNoDictBinders binds)
      checkTopDecl (SigD _ _)
        = return ()
      checkTopDecl (AnnD _ _)
        = return ()
      checkTopDecl (ForD _ (ForeignImport { fd_name = L _ name }))
        = bindName name
      checkTopDecl d
        = addErr $ TcRnTHError $ AddTopDeclsError $ InvalidTopDecl d

      bindName :: RdrName -> TcM ()
      bindName (Exact n)
        = do { th_topnames_var <- fmap tcg_th_topnames getGblEnv
             ; updTcRef th_topnames_var (\ns -> extendNameSet ns n)
             }

      bindName name = addErr $ TcRnTHError $ THNameError $ NonExactName name

  qAddForeignFilePath lang fp = do
    var <- fmap tcg_th_foreign_files getGblEnv
    updTcRef var ((lang, fp) :)

  qAddModFinalizer fin = do
      r <- liftIO $ mkRemoteRef fin
      fref <- liftIO $ mkForeignRef r (freeRemoteRef r)
      addModFinalizerRef fref

  qAddCorePlugin plugin = do
      hsc_env <- getTopEnv
      let fc        = hsc_FC hsc_env
      let home_unit = hsc_home_unit hsc_env
      let dflags    = hsc_dflags hsc_env
      let fopts     = initFinderOpts dflags
      r <- liftIO $ findHomeModule fc fopts home_unit (mkModuleName plugin)
      let err = TcRnTHError $ AddInvalidCorePlugin plugin
      case r of
        Found {} -> addErr err
        FoundMultiple {} -> addErr err
        _ -> return ()
      th_coreplugins_var <- tcg_th_coreplugins <$> getGblEnv
      updTcRef th_coreplugins_var (plugin:)

  qGetQ :: forall a. Typeable a => TcM (Maybe a)
  qGetQ = do
      th_state_var <- fmap tcg_th_state getGblEnv
      th_state <- readTcRef th_state_var
      -- See #10596 for why we use a scoped type variable here.
      return (Map.lookup (typeRep (Proxy :: Proxy a)) th_state >>= fromDynamic)

  qPutQ x = do
      th_state_var <- fmap tcg_th_state getGblEnv
      updTcRef th_state_var (\m -> Map.insert (typeOf x) (toDyn x) m)

  qIsExtEnabled = xoptM

  qExtsEnabled =
    EnumSet.toList . extensionFlags . hsc_dflags <$> getTopEnv

  qPutDoc doc_loc s = do
    th_doc_var <- tcg_th_docs <$> getGblEnv
    resolved_doc_loc <- resolve_loc doc_loc
    is_local <- checkLocalName resolved_doc_loc
    unless is_local $ failWithTc $ TcRnTHError $ AddDocToNonLocalDefn doc_loc
    let ds = mkGeneratedHsDocString s
        hd = lexHsDoc parseIdentifier ds
    hd' <- rnHsDoc hd
    updTcRef th_doc_var (Map.insert resolved_doc_loc hd')
    where
      resolve_loc (TH.DeclDoc n) = DeclDoc <$> lookupThName n
      resolve_loc (TH.ArgDoc n i) = ArgDoc <$> lookupThName n <*> pure i
      resolve_loc (TH.InstDoc t) = InstDoc <$> fmap getName (lookupThInstName t)
      resolve_loc TH.ModuleDoc = pure ModuleDoc

      -- It doesn't make sense to add documentation to something not inside
      -- the current module. So check for it!
      checkLocalName (DeclDoc n) = nameIsLocalOrFrom <$> getModule <*> pure n
      checkLocalName (ArgDoc n _) = nameIsLocalOrFrom <$> getModule <*> pure n
      checkLocalName (InstDoc n) = nameIsLocalOrFrom <$> getModule <*> pure n
      checkLocalName ModuleDoc = pure True


  qGetDoc (TH.DeclDoc n) = lookupThName n >>= lookupDeclDoc
  qGetDoc (TH.InstDoc t) = lookupThInstName t >>= lookupDeclDoc
  qGetDoc (TH.ArgDoc n i) = lookupThName n >>= lookupArgDoc i
  qGetDoc TH.ModuleDoc = do
    df <- getDynFlags
    docs <- getGblEnv >>= extractDocs df
    return (renderHsDocString . hsDocString <$> (docs_mod_hdr =<< docs))

-- | Looks up documentation for a declaration in first the current module,
-- otherwise tries to find it in another module via 'hscGetModuleInterface'.
lookupDeclDoc :: Name -> TcM (Maybe String)
lookupDeclDoc nm = do
  df <- getDynFlags
  Docs{docs_decls} <- fmap (fromMaybe emptyDocs) $ getGblEnv >>= extractDocs df
  case lookupUniqMap docs_decls nm of
    Just doc -> pure $ Just (renderHsDocStrings $ map hsDocString doc)
    Nothing -> do
      -- Wasn't in the current module. Try searching other external ones!
      mIface <- getExternalModIface nm
      case mIface of
        Just iface
          | Just Docs{docs_decls = dmap} <- mi_docs iface ->
          pure $ renderHsDocStrings . map hsDocString <$> lookupUniqMap dmap nm
        _ -> pure Nothing

-- | Like 'lookupDeclDoc', looks up documentation for a function argument. If
-- it can't find any documentation for a function in this module, it tries to
-- find it in another module.
lookupArgDoc :: Int -> Name -> TcM (Maybe String)
lookupArgDoc i nm = do
  df <- getDynFlags
  Docs{docs_args = argDocs} <- fmap (fromMaybe emptyDocs) $ getGblEnv >>= extractDocs df
  case lookupUniqMap argDocs nm of
    Just m -> pure $ renderHsDocString . hsDocString <$> IntMap.lookup i m
    Nothing -> do
      mIface <- getExternalModIface nm
      case mIface of
        Just iface
          | Just Docs{docs_args = amap} <- mi_docs iface->
          pure $ renderHsDocString . hsDocString <$> (lookupUniqMap amap nm >>= IntMap.lookup i)
        _ -> pure Nothing

-- | Returns the module a Name belongs to, if it is isn't local.
getExternalModIface :: Name -> TcM (Maybe ModIface)
getExternalModIface nm = do
  isLocal <- nameIsLocalOrFrom <$> getModule <*> pure nm
  if isLocal
    then pure Nothing
    else case nameModule_maybe nm of
          Nothing -> pure Nothing
          Just modNm -> do
            hsc_env <- getTopEnv
            iface <- liftIO $ hscGetModuleInterface hsc_env modNm
            pure (Just iface)

-- | Find the GHC name of the first instance that matches the TH type
lookupThInstName :: TH.Type -> TcM Name
lookupThInstName th_type = do
  cls_name <- inst_cls_name th_type
  insts <- reifyInstances' cls_name (inst_arg_types th_type)
  case insts of   -- This expands any type synonyms
    Left  (_, (inst:_)) -> return $ getName inst
    Left  (_, [])       -> noMatches
    Right (_, (inst:_)) -> return $ getName inst
    Right (_, [])       -> noMatches
  where
    noMatches = failWithTc $
      TcRnTHError $ FailedToLookupThInstName th_type NoMatchesFound

    -- Get the name of the class for the instance we are documenting
    -- > inst_cls_name (Monad Maybe) == Monad
    -- > inst_cls_name C = C
    inst_cls_name :: TH.Type -> TcM TH.Name
    inst_cls_name (TH.AppT t _)              = inst_cls_name t
    inst_cls_name (TH.SigT n _)              = inst_cls_name n
    inst_cls_name (TH.VarT n)                = pure n
    inst_cls_name (TH.ConT n)                = pure n
    inst_cls_name (TH.PromotedT n)           = pure n
    inst_cls_name (TH.InfixT _ n _)          = pure n
    inst_cls_name (TH.UInfixT _ n _)         = pure n
    inst_cls_name (TH.PromotedInfixT _ n _)  = pure n
    inst_cls_name (TH.PromotedUInfixT _ n _) = pure n
    inst_cls_name (TH.ParensT t)             = inst_cls_name t

    inst_cls_name (TH.ForallT _ _ _)         = inst_cls_name_err
    inst_cls_name (TH.ForallVisT _ _)        = inst_cls_name_err
    inst_cls_name (TH.AppKindT _ _)          = inst_cls_name_err
    inst_cls_name (TH.TupleT _)              = inst_cls_name_err
    inst_cls_name (TH.UnboxedTupleT _)       = inst_cls_name_err
    inst_cls_name (TH.UnboxedSumT _)         = inst_cls_name_err
    inst_cls_name TH.ArrowT                  = inst_cls_name_err
    inst_cls_name TH.MulArrowT               = inst_cls_name_err
    inst_cls_name TH.EqualityT               = inst_cls_name_err
    inst_cls_name TH.ListT                   = inst_cls_name_err
    inst_cls_name (TH.PromotedTupleT _)      = inst_cls_name_err
    inst_cls_name TH.PromotedNilT            = inst_cls_name_err
    inst_cls_name TH.PromotedConsT           = inst_cls_name_err
    inst_cls_name TH.StarT                   = inst_cls_name_err
    inst_cls_name TH.ConstraintT             = inst_cls_name_err
    inst_cls_name (TH.LitT _)                = inst_cls_name_err
    inst_cls_name TH.WildCardT               = inst_cls_name_err
    inst_cls_name (TH.ImplicitParamT _ _)    = inst_cls_name_err

    inst_cls_name_err = failWithTc $
      TcRnTHError $ FailedToLookupThInstName th_type CouldNotDetermineInstance

    -- Basically does the opposite of 'mkThAppTs'
    -- > inst_arg_types (Monad Maybe) == [Maybe]
    -- > inst_arg_types C == []
    inst_arg_types :: TH.Type -> [TH.Type]
    inst_arg_types (TH.AppT _ args) =
      let go (TH.AppT t ts) = t:go ts
          go t = [t]
        in go args
    inst_arg_types _ = []

-- | Adds a mod finalizer reference to the local environment.
addModFinalizerRef :: ForeignRef (TH.Q ()) -> TcM ()
addModFinalizerRef finRef = do
    th_stage <- getStage
    case th_stage of
      RunSplice th_modfinalizers_var -> updTcRef th_modfinalizers_var (finRef :)
      -- This case happens only if a splice is executed and the caller does
      -- not set the 'ThStage' to 'RunSplice' to collect finalizers.
      -- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
      _ ->
        pprPanic "addModFinalizer was called when no finalizers were collected"
                 (ppr th_stage)

-- | Releases the external interpreter state.
finishTH :: TcM ()
finishTH = do
  hsc_env <- getTopEnv
  case interpInstance <$> hsc_interp hsc_env of
    Nothing                  -> pure ()
#if defined(HAVE_INTERNAL_INTERPRETER)
    Just InternalInterp      -> pure ()
#endif
    Just (ExternalInterp {}) -> do
      tcg <- getGblEnv
      writeTcRef (tcg_th_remote_state tcg) Nothing


runTHExp :: ForeignHValue -> TcM TH.Exp
runTHExp = runTH THExp

runTHPat :: ForeignHValue -> TcM TH.Pat
runTHPat = runTH THPat

runTHType :: ForeignHValue -> TcM TH.Type
runTHType = runTH THType

runTHDec :: ForeignHValue -> TcM [TH.Dec]
runTHDec = runTH THDec

runTH :: Binary a => THResultType -> ForeignHValue -> TcM a
runTH ty fhv = do
  interp <- tcGetInterp
  case interpInstance interp of
#if defined(HAVE_INTERNAL_INTERPRETER)
    InternalInterp -> do
       -- Run it in the local TcM
      hv <- liftIO $ wormhole interp fhv
      r <- runQuasi (unsafeCoerce hv :: TH.Q a)
      return r
#endif

    ExternalInterp ext -> withExtInterp ext $ \inst -> do
      -- Run it on the server.  For an overview of how TH works with
      -- Remote GHCi, see Note [Remote Template Haskell] in
      -- libraries/ghci/GHCi/TH.hs.
      rstate <- getTHState inst
      loc <- TH.qLocation
      -- run a remote TH request
      r <- liftIO $
        withForeignRef rstate $ \state_hv ->
        withForeignRef fhv $ \q_hv ->
          sendMessageDelayedResponse inst (RunTH state_hv q_hv ty (Just loc))
      -- respond to requests from the interpreter
      runRemoteTH inst []
      -- get the final result
      qr <- liftIO $ receiveDelayedResponse inst r
      bs <- checkQResult qr
      return $! runGet get (LB.fromStrict bs)


-- | communicate with a remotely-running TH computation until it finishes.
-- See Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs.
runRemoteTH
  :: ExtInterpInstance d
  -> [Messages TcRnMessage]   --  saved from nested calls to qRecover
  -> TcM ()
runRemoteTH inst recovers = do
  THMsg msg <- liftIO $ receiveTHMessage inst
  case msg of
    RunTHDone -> return ()
    StartRecover -> do -- Note [TH recover with -fexternal-interpreter]
      v <- getErrsVar
      msgs <- readTcRef v
      writeTcRef v emptyMessages
      runRemoteTH inst (msgs : recovers)
    EndRecover caught_error -> do
      let (prev_msgs, rest) = case recovers of
             [] -> panic "EndRecover"
             a : b -> (a,b)
      v <- getErrsVar
      warn_msgs <- getWarningMessages <$> readTcRef v
      -- keep the warnings only if there were no errors
      writeTcRef v $ if caught_error
        then prev_msgs
        else mkMessages warn_msgs `unionMessages` prev_msgs
      runRemoteTH inst rest
    _other -> do
      r <- handleTHMessage msg
      liftIO $ sendAnyValue inst r
      runRemoteTH inst recovers

-- | Check a QResult
checkQResult :: QResult a -> TcM a
checkQResult qr =
  case qr of
    QDone a -> return a
    QException str -> liftIO $ throwIO (ErrorCall str)
    QFail str -> fail str

{- Note [TH recover with -fexternal-interpreter]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recover is slightly tricky to implement.

The meaning of "recover a b" is
 - Do a
   - If it finished with no errors, then keep the warnings it generated
   - If it failed, discard any messages it generated, and do b

Note that "failed" here can mean either
  (1) threw an exception (failTc)
  (2) generated an error message (addErrTcM)

The messages are managed by GHC in the TcM monad, whereas the
exception-handling is done in the ghc-iserv process, so we have to
coordinate between the two.

On the server:
  - emit a StartRecover message
  - run "a; FailIfErrs" inside a try
  - emit an (EndRecover x) message, where x = True if "a; FailIfErrs" failed
  - if "a; FailIfErrs" failed, run "b"

Back in GHC, when we receive:

  FailIfErrrs
    failTc if there are any error messages (= failIfErrsM)
  StartRecover
    save the current messages and start with an empty set.
  EndRecover caught_error
    Restore the previous messages,
    and merge in the new messages if caught_error is false.
-}

-- | Retrieve (or create, if it hasn't been created already), the
-- remote TH state.  The TH state is a remote reference to an IORef
-- QState living on the server, and we have to pass this to each RunTH
-- call we make.
--
-- The TH state is stored in tcg_th_remote_state in the TcGblEnv.
--
getTHState :: ExtInterpInstance d -> TcM (ForeignRef (IORef QState))
getTHState inst = do
  th_state_var <- tcg_th_remote_state <$> getGblEnv
  liftIO $ do
    th_state <- readIORef th_state_var
    case th_state of
      Just rhv -> return rhv
      Nothing -> do
        rref <- sendMessage inst StartTH
        fhv <- mkForeignRef rref (freeReallyRemoteRef inst rref)
        writeIORef th_state_var (Just fhv)
        return fhv

wrapTHResult :: TcM a -> TcM (THResult a)
wrapTHResult tcm = do
  e <- tryM tcm   -- only catch 'fail', treat everything else as catastrophic
  case e of
    Left e -> return (THException (show e))
    Right a -> return (THComplete a)

handleTHMessage :: THMessage a -> TcM a
handleTHMessage msg = case msg of
  NewName a -> wrapTHResult $ TH.qNewName a
  Report b str -> wrapTHResult $ TH.qReport b str
  LookupName b str -> wrapTHResult $ TH.qLookupName b str
  Reify n -> wrapTHResult $ TH.qReify n
  ReifyFixity n -> wrapTHResult $ TH.qReifyFixity n
  ReifyType n -> wrapTHResult $ TH.qReifyType n
  ReifyInstances n ts -> wrapTHResult $ TH.qReifyInstances n ts
  ReifyRoles n -> wrapTHResult $ TH.qReifyRoles n
  ReifyAnnotations lookup tyrep ->
    wrapTHResult $ (map B.pack <$> getAnnotationsByTypeRep lookup tyrep)
  ReifyModule m -> wrapTHResult $ TH.qReifyModule m
  ReifyConStrictness nm -> wrapTHResult $ TH.qReifyConStrictness nm
  GetPackageRoot -> wrapTHResult $ TH.qGetPackageRoot
  AddDependentFile f -> wrapTHResult $ TH.qAddDependentFile f
  AddTempFile s -> wrapTHResult $ TH.qAddTempFile s
  AddModFinalizer r -> do
    interp <- hscInterp <$> getTopEnv
    wrapTHResult $ liftIO (mkFinalizedHValue interp r) >>= addModFinalizerRef
  AddCorePlugin str -> wrapTHResult $ TH.qAddCorePlugin str
  AddTopDecls decs -> wrapTHResult $ TH.qAddTopDecls decs
  AddForeignFilePath lang str -> wrapTHResult $ TH.qAddForeignFilePath lang str
  IsExtEnabled ext -> wrapTHResult $ TH.qIsExtEnabled ext
  ExtsEnabled -> wrapTHResult $ TH.qExtsEnabled
  PutDoc l s -> wrapTHResult $ TH.qPutDoc l s
  GetDoc l -> wrapTHResult $ TH.qGetDoc l
  FailIfErrs -> wrapTHResult failIfErrsM
  _ -> panic ("handleTHMessage: unexpected message " ++ show msg)

getAnnotationsByTypeRep :: TH.AnnLookup -> TypeRep -> TcM [[Word8]]
getAnnotationsByTypeRep th_name tyrep
  = do { name <- lookupThAnnLookup th_name
       ; topEnv <- getTopEnv
       ; epsHptAnns <- liftIO $ prepareAnnotations topEnv Nothing
       ; tcg <- getGblEnv
       ; let selectedEpsHptAnns = findAnnsByTypeRep epsHptAnns name tyrep
       ; let selectedTcgAnns = findAnnsByTypeRep (tcg_ann_env tcg) name tyrep
       ; return (selectedEpsHptAnns ++ selectedTcgAnns) }

{-
************************************************************************
*                                                                      *
            Instance Testing
*                                                                      *
************************************************************************
-}

reifyInstances :: TH.Name -> [TH.Type] -> TcM [TH.Dec]
reifyInstances th_nm th_tys
  = do { insts <- reifyInstances' th_nm th_tys
       ; case insts of
           Left (cls, cls_insts) ->
             reifyClassInstances cls cls_insts
           Right (tc, fam_insts) ->
             reifyFamilyInstances tc fam_insts }

reifyInstances' :: TH.Name
                -> [TH.Type]
                -> TcM (Either (Class, [ClsInst]) (TyCon, [FamInst]))
                -- ^ Returns 'Left' in the case that the instances were found to
                -- be class instances, or 'Right' if they are family instances.
reifyInstances' th_nm th_tys
   = addErrCtxt (text "In the argument of reifyInstances:"
                 <+> ppr_th th_nm <+> sep (map ppr_th th_tys)) $
     do { loc <- getSrcSpanM
        ; th_origin <- getThSpliceOrigin
        ; rdr_ty <- cvt th_origin loc (mkThAppTs (TH.ConT th_nm) th_tys)
          -- #9262 says to bring vars into scope, like in HsForAllTy case
          -- of rnHsTyKi
        ; tv_rdrs <- filterInScopeM $ extractHsTyRdrTyVars rdr_ty
          -- Rename  to HsType Name
        ; ((tv_names, rn_ty), _fvs)
            <- checkNoErrs $ -- If there are out-of-scope Names here, then we
                             -- must error before proceeding to typecheck the
                             -- renamed type, as that will result in GHC
                             -- internal errors (#13837).
               rnImplicitTvOccs Nothing tv_rdrs $ \ tv_names ->
               do { (rn_ty, fvs) <- rnLHsType doc rdr_ty
                  ; return ((tv_names, rn_ty), fvs) }
        ; skol_info <- mkSkolemInfo ReifySkol
        ; (tclvl, wanted, (tvs, ty))
            <- pushLevelAndSolveEqualitiesX "reifyInstances"  $
               bindImplicitTKBndrs_Skol skol_info tv_names              $
               tcInferLHsType rn_ty

        ; tvs <- zonkAndScopedSort tvs

        -- Avoid error cascade if there are unsolved
        ; reportUnsolvedEqualities skol_info tvs tclvl wanted

        ; ty <- zonkTcTypeToType ty
                -- Substitute out the meta type variables
                -- In particular, the type might have kind
                -- variables inside it (#7477)

        ; traceTc "reifyInstances'" (ppr ty $$ ppr (typeKind ty))
        ; case splitTyConApp_maybe ty of   -- This expands any type synonyms
            Just (tc, tys)                 -- See #7910
               | Just cls <- tyConClass_maybe tc
               -> do { inst_envs <- tcGetInstEnvs
                     ; let (matches, unifies, _) = lookupInstEnv False inst_envs cls tys
                     ; traceTc "reifyInstances'1" (ppr matches)
                     ; return $ Left (cls, map fst matches ++ getCoherentUnifiers unifies) }
               | isOpenFamilyTyCon tc
               -> do { inst_envs <- tcGetFamInstEnvs
                     ; let matches = lookupFamInstEnv inst_envs tc tys
                     ; traceTc "reifyInstances'2" (ppr matches)
                     ; return $ Right (tc, map fim_instance matches) }
            _  -> bale_out $ TcRnTHError $ THReifyError $ CannotReifyInstance ty }
  where
    doc = ClassInstanceCtx
    bale_out msg = failWithTc msg

    cvt :: Origin -> SrcSpan -> TH.Type -> TcM (LHsType GhcPs)
    cvt origin loc th_ty = case convertToHsType origin loc th_ty of
      Left msg -> failWithTc (TcRnTHError $ THSpliceFailed $ RunSpliceFailure msg)
      Right ty -> return ty

{-
************************************************************************
*                                                                      *
                        Reification
*                                                                      *
************************************************************************
-}

lookupName :: Bool      -- True  <=> type namespace
                        -- False <=> value namespace
           -> String -> TcM (Maybe TH.Name)
lookupName is_type_name s
  = do { mb_nm <- lookupOccRn_maybe rdr_name
       ; return (fmap (reifyName . greName) mb_nm) }
  where
    th_name = TH.mkName s       -- Parses M.x into a base of 'x' and a module of 'M'

    occ_fs :: FastString
    occ_fs = mkFastString (TH.nameBase th_name)

    occ :: OccName
    occ | is_type_name
        = if isLexVarSym occ_fs || isLexCon occ_fs
                             then mkTcOccFS    occ_fs
                             else mkTyVarOccFS occ_fs
        | otherwise
        = if isLexCon occ_fs then mkDataOccFS occ_fs
                             else mkVarOccFS  occ_fs
                               -- NB: when we pick the variable namespace, we
                               -- might well obtain an identifier in a record
                               -- field namespace, as lookupOccRn_maybe looks in
                               -- record field namespaces when looking up variables.
                               -- This ensures we can look up record fields using
                               -- this function (#24293).

    rdr_name = case TH.nameModule th_name of
                 Nothing  -> mkRdrUnqual occ
                 Just mod -> mkRdrQual (mkModuleName mod) occ

-- | We only want to produce warnings for TH-splices if the user requests so.
-- See Note [Warnings for TH splices].
getThSpliceOrigin :: TcM Origin
getThSpliceOrigin = do
  warn <- goptM Opt_EnableThSpliceWarnings
  if warn then return FromSource else return (Generated OtherExpansion SkipPmc)

getThing :: TH.Name -> TcM TcTyThing
getThing th_name
  = do  { name <- lookupThName th_name
        ; traceIf (text "reify" <+> text (show th_name) <+> brackets (ppr_ns th_name) <+> ppr name)
        ; tcLookupTh name }
        -- ToDo: this tcLookup could fail, which would give a
        --       rather unhelpful error message
  where
    ppr_ns (TH.Name _ (TH.NameG TH.DataName     _pkg _mod)) = text "data"
    ppr_ns (TH.Name _ (TH.NameG TH.TcClsName    _pkg _mod)) = text "tc"
    ppr_ns (TH.Name _ (TH.NameG TH.VarName      _pkg _mod)) = text "var"
    ppr_ns (TH.Name _ (TH.NameG (TH.FldName {}) _pkg _mod)) = text "fld"
    ppr_ns _ = panic "reify/ppr_ns"

reify :: TH.Name -> TcM TH.Info
reify th_name
  = do  { traceTc "reify 1" (text (TH.showName th_name))
        ; thing <- getThing th_name
        ; traceTc "reify 2" (ppr thing)
        ; reifyThing thing }

lookupThName :: TH.Name -> TcM Name
lookupThName th_name = do
    mb_name <- lookupThName_maybe th_name
    case mb_name of
        Nothing   -> failWithTc (notInScope th_name)
        Just name -> return name

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
lookupThName_maybe th_name
  =  do { let guesses = thRdrNameGuesses th_name
        ; case guesses of
        { [for_sure] -> lookupSameOccRn_maybe for_sure
        ; _ ->
     do { gres <- mapMaybeM lookupOccRn_maybe guesses
          -- Pick the first that works
          -- E.g. reify (mkName "A") will pick the class A in preference to the data constructor A
        ; return (fmap greName $ listToMaybe gres) } } }

tcLookupTh :: Name -> TcM TcTyThing
-- This is a specialised version of GHC.Tc.Utils.Env.tcLookup; specialised mainly in that
-- it gives a reify-related error message on failure, whereas in the normal
-- tcLookup, failure is a bug.
tcLookupTh name
  = do  { (gbl_env, lcl_env) <- getEnvs
        ; case lookupNameEnv (getLclEnvTypeEnv lcl_env) name of {
                Just thing -> return thing;
                Nothing    ->

          case lookupNameEnv (tcg_type_env gbl_env) name of {
                Just thing -> return (AGlobal thing);
                Nothing    ->

          -- EZY: I don't think this choice matters, no TH in signatures!
          if nameIsLocalOrFrom (tcg_semantic_mod gbl_env) name
          then  -- It's defined in this module
                failWithTc (notInEnv name)

          else
     do { mb_thing <- tcLookupImported_maybe name
        ; case mb_thing of
            Succeeded thing -> return (AGlobal thing)
            Failed msg      -> failWithTc (TcRnInterfaceError msg)
    }}}}

notInScope :: TH.Name -> TcRnMessage
notInScope th_name =
  TcRnTHError $ THReifyError $ CannotReifyOutOfScopeThing th_name

notInEnv :: Name -> TcRnMessage
notInEnv name = TcRnTHError $ THReifyError $ CannotReifyThingNotInTypeEnv name

------------------------------
reifyRoles :: TH.Name -> TcM [TH.Role]
reifyRoles th_name
  = do { thing <- getThing th_name
       ; case thing of
           AGlobal (ATyCon tc) -> return (map reify_role (tyConRoles tc))
           _ -> failWithTc $ TcRnTHError $ THReifyError $
                  NoRolesAssociatedWithThing thing
       }
  where
    reify_role Nominal          = TH.NominalR
    reify_role Representational = TH.RepresentationalR
    reify_role Phantom          = TH.PhantomR

------------------------------
reifyThing :: TcTyThing -> TcM TH.Info
-- The only reason this is monadic is for error reporting,
-- which in turn is mainly for the case when TH can't express
-- some random GHC extension

reifyThing (AGlobal (AnId id))
  = do  { ty <- reifyType (idType id)
        ; let v = reifyName id
        ; case idDetails id of
            ClassOpId cls _ -> return (TH.ClassOpI v ty (reifyName cls))
            _               -> return (TH.VarI     v ty Nothing)
    }

reifyThing (AGlobal (ATyCon tc))   = reifyTyCon tc
reifyThing (AGlobal (AConLike (RealDataCon dc)))
  = mkDataConI dc

reifyThing (AGlobal (AConLike (PatSynCon ps)))
  = do { let name = reifyName ps
       ; ty <- reifyPatSynType (patSynSigBndr ps)
       ; return (TH.PatSynI name ty) }

reifyThing (ATcId {tct_id = id})
  = do  { ty1 <- liftZonkM $ zonkTcType (idType id) -- Make use of all the info we have, even
                                        -- though it may be incomplete
        ; ty2 <- reifyType ty1
        ; return (TH.VarI (reifyName id) ty2 Nothing) }

reifyThing (ATyVar tv tv1)
  = do { ty1 <- liftZonkM $ zonkTcTyVar tv1
       ; ty2 <- reifyType ty1
       ; return (TH.TyVarI (reifyName tv) ty2) }

reifyThing thing = pprPanic "reifyThing" (pprTcTyThingCategory thing)

-------------------------------------------
reifyAxBranch :: TyCon -> CoAxBranch -> TcM TH.TySynEqn
reifyAxBranch fam_tc (CoAxBranch { cab_tvs = tvs
                                 , cab_lhs = lhs
                                 , cab_rhs = rhs })
            -- remove kind patterns (#8884)
  = do { tvs' <- reifyTyVarsToMaybe tvs
       ; let lhs_types_only = filterOutInvisibleTypes fam_tc lhs
       ; lhs' <- reifyTypes lhs_types_only
       ; annot_th_lhs <- zipWith3M annotThType (tyConArgsPolyKinded fam_tc)
                                   lhs_types_only lhs'
       ; let lhs_type = mkThAppTs (TH.ConT $ reifyName fam_tc) annot_th_lhs
       ; rhs'  <- reifyType rhs
       ; return (TH.TySynEqn tvs' lhs_type rhs') }

reifyTyCon :: TyCon -> TcM TH.Info
reifyTyCon tc
  | Just cls <- tyConClass_maybe tc
  = reifyClass cls

{-  Seems to be just a short cut for the next equation -- omit
  | tc `hasKey` fUNTyConKey -- I'm not quite sure what is happening here
  = return (TH.PrimTyConI (reifyName tc) 2 False)
-}

  | isPrimTyCon tc
  = return (TH.PrimTyConI (reifyName tc) (length (tyConVisibleTyVars tc))
                          (isUnliftedTypeKind (tyConResKind tc)))

  | isTypeFamilyTyCon tc
  = do { let tvs      = tyConTyVars tc
             res_kind = tyConResKind tc
             resVar   = tyConFamilyResVar_maybe tc

       ; kind' <- reifyKind res_kind
       ; let (resultSig, injectivity) =
                 case resVar of
                   Nothing   -> (TH.KindSig kind', Nothing)
                   Just name ->
                     let thName   = reifyName name
                         injAnnot = tyConInjectivityInfo tc
                         sig = TH.TyVarSig (TH.KindedTV thName () kind')
                         inj = case injAnnot of
                                 NotInjective -> Nothing
                                 Injective ms ->
                                     Just (TH.InjectivityAnn thName injRHS)
                                   where
                                     injRHS = map (reifyName . tyVarName)
                                                  (filterByList ms tvs)
                     in (sig, inj)
       ; tvs' <- reifyTyConBinders tc
       ; let tfHead =
               TH.TypeFamilyHead (reifyName tc) tvs' resultSig injectivity
       ; if isOpenTypeFamilyTyCon tc
         then do { fam_envs <- tcGetFamInstEnvs
                 ; instances <- reifyFamilyInstances tc
                                  (familyInstances fam_envs tc)
                 ; return (TH.FamilyI (TH.OpenTypeFamilyD tfHead) instances) }
         else do { eqns <-
                     case isClosedSynFamilyTyConWithAxiom_maybe tc of
                       Just ax -> mapM (reifyAxBranch tc) $
                                  fromBranches $ coAxiomBranches ax
                       Nothing -> return []
                 ; return (TH.FamilyI (TH.ClosedTypeFamilyD tfHead eqns)
                      []) } }

  | isDataFamilyTyCon tc
  = do { let res_kind = tyConResKind tc

       ; kind' <- fmap Just (reifyKind res_kind)

       ; tvs' <- reifyTyConBinders tc
       ; fam_envs <- tcGetFamInstEnvs
       ; instances <- reifyFamilyInstances tc (familyInstances fam_envs tc)
       ; return (TH.FamilyI
                       (TH.DataFamilyD (reifyName tc) tvs' kind') instances) }

  | Just (_, rhs) <- synTyConDefn_maybe tc  -- Vanilla type synonym
  = do { rhs' <- reifyType rhs
       ; tvs' <- reifyTyConBinders tc
       ; return (TH.TyConI
                   (TH.TySynD (reifyName tc) tvs' rhs'))
       }

  -- Special case for `type data` data constructors, which are reified as
  -- `ATyCon`s rather than `ADataCon`s (#22818).
  -- See Note [Type data declarations] in GHC.Rename.Module.
  | Just dc <- isPromotedDataCon_maybe tc
  , isTypeDataCon dc
  = mkDataConI dc

  | otherwise
  = do  { cxt <- reifyCxt (tyConStupidTheta tc)
        ; let tvs      = tyConTyVars tc
              dataCons = tyConDataCons tc
              isGadt   = isGadtSyntaxTyCon tc
        ; cons <- mapM (reifyDataCon isGadt (mkTyVarTys tvs)) dataCons
        ; r_tvs <- reifyTyConBinders tc
        ; let name = reifyName tc
              deriv = []        -- Don't know about deriving
              decl | isTypeDataTyCon tc =
                       -- `type data` declarations have a special `Dec`,
                       -- separate from other `DataD`s. See
                       -- [Type data declarations] in GHC.Rename.Module.
                       TH.TypeDataD name r_tvs Nothing cons
                   | isNewTyCon tc =
                       TH.NewtypeD cxt name r_tvs Nothing (head cons) deriv
                   | otherwise     =
                       TH.DataD    cxt name r_tvs Nothing       cons  deriv
        ; return (TH.TyConI decl) }

reifyDataCon :: Bool -> [Type] -> DataCon -> TcM TH.Con
reifyDataCon isGadtDataCon tys dc
  = do { let -- used for H98 data constructors
             (ex_tvs, theta, arg_tys)
                 = dataConInstSig dc tys
             -- used for GADTs data constructors
             g_user_tvs' = dataConUserTyVarBinders dc
             (g_univ_tvs, _, g_eq_spec, g_theta', g_arg_tys', g_res_ty')
                 = dataConFullSig dc
             (srcUnpks, srcStricts)
                 = mapAndUnzip reifySourceBang (dataConSrcBangs dc)
             dcdBangs  = zipWith TH.Bang srcUnpks srcStricts
             fields    = dataConFieldLabels dc
             name      = reifyName dc
             -- Universal tvs present in eq_spec need to be filtered out, as
             -- they will not appear anywhere in the type.
             eq_spec_tvs = mkVarSet (map eqSpecTyVar g_eq_spec)

       ; (univ_subst, _)
              -- See Note [Freshen reified GADT constructors' universal tyvars]
           <- freshenTyVarBndrs $
              filterOut (`elemVarSet` eq_spec_tvs) g_univ_tvs
       ; let (tvb_subst, g_user_tvs) = subst_tv_binders univ_subst g_user_tvs'
             g_theta   = substTys tvb_subst g_theta'
             g_arg_tys = substTys tvb_subst (map scaledThing g_arg_tys')
             g_res_ty  = substTy  tvb_subst g_res_ty'

       ; r_arg_tys <- reifyTypes (if isGadtDataCon then g_arg_tys else arg_tys)

       ; main_con <-
           if | not (null fields) && not isGadtDataCon ->
                  return $ TH.RecC name (zip3 (map reifyFieldLabel fields)
                                         dcdBangs r_arg_tys)
              | not (null fields) -> do
                  { res_ty <- reifyType g_res_ty
                  ; return $ TH.RecGadtC [name]
                                     (zip3 (map reifyFieldLabel fields)
                                      dcdBangs r_arg_tys) res_ty }
                -- We need to check not isGadtDataCon here because GADT
                -- constructors can be declared infix.
                -- See Note [Infix GADT constructors] in GHC.Tc.TyCl.
              | dataConIsInfix dc && not isGadtDataCon ->
                  assert (r_arg_tys `lengthIs` 2) $ do
                  { let [r_a1, r_a2] = r_arg_tys
                        [s1,   s2]   = dcdBangs
                  ; return $ TH.InfixC (s1,r_a1) name (s2,r_a2) }
              | isGadtDataCon -> do
                  { res_ty <- reifyType g_res_ty
                  ; return $ TH.GadtC [name]
                                 (dcdBangs `zip` r_arg_tys) res_ty }
              | otherwise ->
                  return $ TH.NormalC name (dcdBangs `zip` r_arg_tys)

       ; let (ex_tvs', theta') | isGadtDataCon = (g_user_tvs, g_theta)
                               | otherwise     = assert (all isTyVar ex_tvs)
                                                 -- no covars for haskell syntax
                                                 (map mk_specified ex_tvs, theta)
             ret_con | null ex_tvs' && null theta' = return main_con
                     | otherwise                   = do
                         { cxt <- reifyCxt theta'
                         ; ex_tvs'' <- reifyTyVarBndrs ex_tvs'
                         ; return (TH.ForallC ex_tvs'' cxt main_con) }
       ; assert (r_arg_tys `equalLength` dcdBangs)
         ret_con }
  where
    mk_specified tv = Bndr tv SpecifiedSpec

    subst_tv_binders subst tv_bndrs =
      let tvs            = binderVars tv_bndrs
          flags          = binderFlags tv_bndrs
          (subst', tvs') = substTyVarBndrs subst tvs
          tv_bndrs'      = map (\(tv,fl) -> Bndr tv fl) (zip tvs' flags)
      in (subst', tv_bndrs')

mkDataConI :: DataCon -> TcM TH.Info
mkDataConI dc
  = do  { let name = dataConName dc
        ; ty <- reifyType (idType (dataConWrapId dc))
        ; return (TH.DataConI (reifyName name) ty
                              (reifyName (dataConOrigTyCon dc)))
        }

{-
Note [Freshen reified GADT constructors' universal tyvars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose one were to reify this GADT:

  data a :~: b where
    Refl :: forall a b. (a ~ b) => a :~: b

We ought to be careful here about the uniques we give to the occurrences of `a`
and `b` in this definition. That is because in the original DataCon, all uses
of `a` and `b` have the same unique, since `a` and `b` are both universally
quantified type variables--that is, they are used in both the (:~:) tycon as
well as in the constructor type signature. But when we turn the DataCon
definition into the reified one, the `a` and `b` in the constructor type
signature becomes differently scoped than the `a` and `b` in `data a :~: b`.

While it wouldn't technically be *wrong* per se to re-use the same uniques for
`a` and `b` across these two different scopes, it's somewhat annoying for end
users of Template Haskell, since they wouldn't be able to rely on the
assumption that all TH names have globally distinct uniques (#13885). For this
reason, we freshen the universally quantified tyvars that go into the reified
GADT constructor type signature to give them distinct uniques from their
counterparts in the tycon.
-}

------------------------------
reifyClass :: Class -> TcM TH.Info
reifyClass cls
  = do  { cxt <- reifyCxt theta
        ; inst_envs <- tcGetInstEnvs
        ; insts <- reifyClassInstances cls (InstEnv.classInstances inst_envs cls)
        ; assocTys <- concatMapM reifyAT ats
        ; ops <- concatMapM reify_op op_stuff
        ; tvs' <- reifyTyConBinders (classTyCon cls)
        ; let dec = TH.ClassD cxt (reifyName cls) tvs' fds' (assocTys ++ ops)
        ; return (TH.ClassI dec insts) }
  where
    (_, fds, theta, _, ats, op_stuff) = classExtraBigSig cls
    fds' = map reifyFunDep fds
    reify_op (op, def_meth)
      = do { let (_, _, ty) = tcSplitMethodTy (idType op)
               -- Use tcSplitMethodTy to get rid of the extraneous class
               -- variables and predicates at the beginning of op's type
               -- (see #15551).
           ; ty' <- reifyType ty
           ; let nm' = reifyName op
           ; case def_meth of
                Just (_, GenericDM gdm_ty) ->
                  do { gdm_ty' <- reifyType gdm_ty
                     ; return [TH.SigD nm' ty', TH.DefaultSigD nm' gdm_ty'] }
                _ -> return [TH.SigD nm' ty'] }

    reifyAT :: ClassATItem -> TcM [TH.Dec]
    reifyAT (ATI tycon def) = do
      tycon' <- reifyTyCon tycon
      case tycon' of
        TH.FamilyI dec _ -> do
          let (tyName, tyArgs) = tfNames dec
          (dec :) <$> maybe (return [])
                            (fmap (:[]) . reifyDefImpl tyName tyArgs . fst)
                            def
        _ -> pprPanic "reifyAT" (text (show tycon'))

    reifyDefImpl :: TH.Name -> [TH.Name] -> Type -> TcM TH.Dec
    reifyDefImpl n args ty =
      TH.TySynInstD . TH.TySynEqn Nothing (mkThAppTs (TH.ConT n) (map TH.VarT args))
                                  <$> reifyType ty

    tfNames :: TH.Dec -> (TH.Name, [TH.Name])
    tfNames (TH.OpenTypeFamilyD (TH.TypeFamilyHead n args _ _))
      = (n, map bndrName args)
    tfNames d = pprPanic "tfNames" (text (show d))

    bndrName :: TH.TyVarBndr flag -> TH.Name
    bndrName (TH.PlainTV n _)    = n
    bndrName (TH.KindedTV n _ _) = n

------------------------------
-- | Annotate (with TH.SigT) a type if the first parameter is True
-- and if the type contains a free variable.
-- This is used to annotate type patterns for poly-kinded tyvars in
-- reifying class and type instances.
-- See @Note [Reified instances and explicit kind signatures]@.
annotThType :: Bool   -- True <=> annotate
            -> TyCoRep.Type -> TH.Type -> TcM TH.Type
  -- tiny optimization: if the type is annotated, don't annotate again.
annotThType _    _  th_ty@(TH.SigT {}) = return th_ty
annotThType True ty th_ty
  | not $ isEmptyVarSet $ filterVarSet isTyVar $ tyCoVarsOfType ty
  = do { let ki = typeKind ty
       ; th_ki <- reifyKind ki
       ; return (TH.SigT th_ty th_ki) }
annotThType _    _ th_ty = return th_ty

-- | For every argument type that a type constructor accepts,
-- report whether or not the argument is poly-kinded. This is used to
-- eventually feed into 'annotThType'.
-- See @Note [Reified instances and explicit kind signatures]@.
tyConArgsPolyKinded :: TyCon -> [Bool]
tyConArgsPolyKinded tc =
     map (is_poly_ty . tyVarKind)      tc_vis_tvs
     -- See "Wrinkle: Oversaturated data family instances" in
     -- @Note [Reified instances and explicit kind signatures]@
  ++ map (is_poly_ty . piTyBinderType) tc_res_kind_vis_bndrs -- (1) in Wrinkle
  ++ repeat True                                             -- (2) in Wrinkle
  where
    is_poly_ty :: Type -> Bool
    is_poly_ty ty = not $
                    isEmptyVarSet $
                    filterVarSet isTyVar $
                    tyCoVarsOfType ty

    tc_vis_tvs :: [TyVar]
    tc_vis_tvs = tyConVisibleTyVars tc

    tc_res_kind_vis_bndrs :: [PiTyBinder]
    tc_res_kind_vis_bndrs = filter isVisiblePiTyBinder $ fst $ splitPiTys $ tyConResKind tc

{-
Note [Reified instances and explicit kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Reified class instances and type family instances often include extra kind
information to disambiguate instances. Here is one such example that
illustrates this (#8953):

    type family Poly (a :: k) :: Type
    type instance Poly (x :: Bool)    = Int
    type instance Poly (x :: Maybe k) = Double

If you're not careful, reifying these instances might yield this:

    type instance Poly x = Int
    type instance Poly x = Double

To avoid this, we go through some care to annotate things with extra kind
information. Some functions which accomplish this feat include:

* annotThType: This annotates a type with a kind signature if the type contains
  a free variable.
* tyConArgsPolyKinded: This checks every argument that a type constructor can
  accept and reports if the type of the argument is poly-kinded. This
  information is ultimately fed into annotThType.

-----
-- Wrinkle: Oversaturated data family instances
-----

What constitutes an argument to a type constructor in the definition of
tyConArgsPolyKinded? For most type constructors, it's simply the visible
type variable binders (i.e., tyConVisibleTyVars). There is one corner case
we must keep in mind, however: data family instances can appear oversaturated
(#17296). For instance:

    data family   Foo :: Type -> Type
    data instance Foo x

    data family Bar :: k
    data family Bar x

For these sorts of data family instances, tyConVisibleTyVars isn't enough,
as they won't give you the kinds of the oversaturated arguments. We must
also consult:

1. The kinds of the arguments in the result kind (i.e., the tyConResKind).
   This will tell us, e.g., the kind of `x` in `Foo x` above.
2. If we go beyond the number of arguments in the result kind (like the
   `x` in `Bar x`), then we conservatively assume that the argument's
   kind is poly-kinded.

-----
-- Wrinkle: data family instances with return kinds
-----

Another squirrelly corner case is this:

    data family Foo (a :: k)
    data instance Foo :: Bool -> Type
    data instance Foo :: Char -> Type

If you're not careful, reifying these instances might yield this:

    data instance Foo
    data instance Foo

We can fix this ambiguity by reifying the instances' explicit return kinds. We
should only do this if necessary (see
Note [When does a tycon application need an explicit kind signature?] in GHC.Core.Type),
but more importantly, we *only* do this if either of the following are true:

1. The data family instance has no constructors.
2. The data family instance is declared with GADT syntax.

If neither of these are true, then reifying the return kind would yield
something like this:

    data instance (Bar a :: Type) = MkBar a

Which is not valid syntax.
-}

------------------------------
reifyClassInstances :: Class -> [ClsInst] -> TcM [TH.Dec]
reifyClassInstances cls insts
  = mapM (reifyClassInstance (tyConArgsPolyKinded (classTyCon cls))) insts

reifyClassInstance :: [Bool]  -- True <=> the corresponding tv is poly-kinded
                              -- includes only *visible* tvs
                   -> ClsInst -> TcM TH.Dec
reifyClassInstance is_poly_tvs i
  = do { cxt <- reifyCxt theta
       ; let vis_types = filterOutInvisibleTypes cls_tc types
       ; thtypes <- reifyTypes vis_types
       ; annot_thtypes <- zipWith3M annotThType is_poly_tvs vis_types thtypes
       ; let head_ty = mkThAppTs (TH.ConT (reifyName cls)) annot_thtypes
       ; return $ (TH.InstanceD over cxt head_ty []) }
  where
     (_tvs, theta, cls, types) = tcSplitDFunTy (idType dfun)
     cls_tc   = classTyCon cls
     dfun     = instanceDFunId i
     over     = case overlapMode (is_flag i) of
                  NoOverlap _     -> Nothing
                  Overlappable _  -> Just TH.Overlappable
                  Overlapping _   -> Just TH.Overlapping
                  Overlaps _      -> Just TH.Overlaps
                  Incoherent _    -> Just TH.Incoherent
                  NonCanonical _  -> Just TH.Incoherent

------------------------------
reifyFamilyInstances :: TyCon -> [FamInst] -> TcM [TH.Dec]
reifyFamilyInstances fam_tc fam_insts
  = mapM (reifyFamilyInstance (tyConArgsPolyKinded fam_tc)) fam_insts

reifyFamilyInstance :: [Bool] -- True <=> the corresponding tv is poly-kinded
                              -- includes only *visible* tvs
                    -> FamInst -> TcM TH.Dec
reifyFamilyInstance is_poly_tvs (FamInst { fi_flavor = flavor
                                         , fi_axiom = ax
                                         , fi_fam = fam })
  | let fam_tc = coAxiomTyCon ax
        branch = coAxiomSingleBranch ax
  , CoAxBranch { cab_tvs = tvs, cab_lhs = lhs, cab_rhs = rhs } <- branch
  = case flavor of
      SynFamilyInst ->
               -- remove kind patterns (#8884)
        do { th_tvs <- reifyTyVarsToMaybe tvs
           ; let lhs_types_only = filterOutInvisibleTypes fam_tc lhs
           ; th_lhs <- reifyTypes lhs_types_only
           ; annot_th_lhs <- zipWith3M annotThType is_poly_tvs lhs_types_only
                                                   th_lhs
           ; let lhs_type = mkThAppTs (TH.ConT $ reifyName fam) annot_th_lhs
           ; th_rhs <- reifyType rhs
           ; return (TH.TySynInstD (TH.TySynEqn th_tvs lhs_type th_rhs)) }

      DataFamilyInst rep_tc ->
        do { let -- eta-expand lhs types, because sometimes data/newtype
                 -- instances are eta-reduced; See #9692
                 -- See Note [Eta reduction for data families] in GHC.Core.Coercion.Axiom
                 (ee_tvs, ee_lhs, _) = etaExpandCoAxBranch branch
                 fam'     = reifyName fam
                 dataCons = tyConDataCons rep_tc
                 isGadt   = isGadtSyntaxTyCon rep_tc
           ; th_tvs <- reifyTyVarsToMaybe ee_tvs
           ; cons <- mapM (reifyDataCon isGadt (mkTyVarTys ee_tvs)) dataCons
           ; let types_only = filterOutInvisibleTypes fam_tc ee_lhs
           ; th_tys <- reifyTypes types_only
           ; annot_th_tys <- zipWith3M annotThType is_poly_tvs types_only th_tys
           ; let lhs_type = mkThAppTs (TH.ConT fam') annot_th_tys
           ; mb_sig <-
               -- See "Wrinkle: data family instances with return kinds" in
               -- Note [Reified instances and explicit kind signatures]
               if (null cons || isGadtSyntaxTyCon rep_tc)
                     && tyConAppNeedsKindSig False fam_tc (length ee_lhs)
               then do { let full_kind = typeKind (mkTyConApp fam_tc ee_lhs)
                       ; th_full_kind <- reifyKind full_kind
                       ; pure $ Just th_full_kind }
               else pure Nothing
           ; return $
               if isNewTyCon rep_tc
               then TH.NewtypeInstD [] th_tvs lhs_type mb_sig (head cons) []
               else TH.DataInstD    [] th_tvs lhs_type mb_sig       cons  []
           }

------------------------------
reifyType :: TyCoRep.Type -> TcM TH.Type
-- Monadic only because of failure
reifyType ty                | tcIsLiftedTypeKind ty = return TH.StarT
  -- Make sure to use tcIsLiftedTypeKind here, since we don't want to confuse it
  -- with Constraint (#14869).
reifyType ty@(ForAllTy (Bndr _ argf) _)
                            = reify_for_all argf ty
reifyType (LitTy t)         = do { r <- reifyTyLit t; return (TH.LitT r) }
reifyType (TyVarTy tv)      = return (TH.VarT (reifyName tv))
reifyType (TyConApp tc tys) = reify_tc_app tc tys   -- Do not expand type synonyms here
reifyType ty@(AppTy {})     = do
  let (ty_head, ty_args) = splitAppTys ty
  ty_head' <- reifyType ty_head
  ty_args' <- reifyTypes (filter_out_invisible_args ty_head ty_args)
  pure $ mkThAppTs ty_head' ty_args'
  where
    -- Make sure to filter out any invisible arguments. For instance, if you
    -- reify the following:
    --
    --   newtype T (f :: forall a. a -> Type) = MkT (f Bool)
    --
    -- Then you should receive back `f Bool`, not `f Type Bool`, since the
    -- `Type` argument is invisible (#15792).
    filter_out_invisible_args :: Type -> [Type] -> [Type]
    filter_out_invisible_args ty_head ty_args =
      filterByList (map isVisibleForAllTyFlag $ appTyForAllTyFlags ty_head ty_args)
                   ty_args
reifyType ty@(FunTy { ft_af = af, ft_mult = ManyTy, ft_arg = t1, ft_res = t2 })
  | isInvisibleFunArg af = reify_for_all Inferred ty  -- Types like ((?x::Int) => Char -> Char)
  | otherwise            = do { [r1,r2] <- reifyTypes [t1,t2]
                              ; return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2) }
reifyType ty@(FunTy { ft_af = af, ft_mult = tm, ft_arg = t1, ft_res = t2 })
  | isInvisibleFunArg af = noTH LinearInvisibleArgument ty
  | otherwise            = do { [rm,r1,r2] <- reifyTypes [tm,t1,t2]
                              ; return (TH.MulArrowT `TH.AppT` rm `TH.AppT` r1 `TH.AppT` r2) }
reifyType (CastTy t _)      = reifyType t -- Casts are ignored in TH
reifyType ty@(CoercionTy {})= noTH CoercionsInTypes ty

reify_for_all :: TyCoRep.ForAllTyFlag -> TyCoRep.Type -> TcM TH.Type
-- Arg of reify_for_all is always ForAllTy or a predicate FunTy
reify_for_all argf ty
  | isVisibleForAllTyFlag argf
  = do let (req_bndrs, phi) = tcSplitForAllReqTVBinders ty
       tvbndrs' <- reifyTyVarBndrs req_bndrs
       phi' <- reifyType phi
       pure $ TH.ForallVisT tvbndrs' phi'
  | otherwise
  = do let (inv_bndrs, phi) = tcSplitForAllInvisTVBinders ty
       tvbndrs' <- reifyTyVarBndrs inv_bndrs
       let (cxt, tau) = tcSplitPhiTy phi
       cxt' <- reifyCxt cxt
       tau' <- reifyType tau
       pure $ TH.ForallT tvbndrs' cxt' tau'

reifyTyLit :: TyCoRep.TyLit -> TcM TH.TyLit
reifyTyLit (NumTyLit n) = return (TH.NumTyLit n)
reifyTyLit (StrTyLit s) = return (TH.StrTyLit (unpackFS s))
reifyTyLit (CharTyLit c) = return (TH.CharTyLit c)

reifyTypes :: [Type] -> TcM [TH.Type]
reifyTypes = mapM reifyType

reifyPatSynType
  :: ([InvisTVBinder], ThetaType, [InvisTVBinder], ThetaType, [Scaled Type], Type) -> TcM TH.Type
-- reifies a pattern synonym's type and returns its *complete* type
-- signature; see Note [Pattern synonym type signatures and Template
-- Haskell] in GHC.ThToHs
reifyPatSynType (univTyVars, req, exTyVars, prov, argTys, resTy)
  = do { univTyVars' <- reifyTyVarBndrs univTyVars
       ; req'        <- reifyCxt req
       ; exTyVars'   <- reifyTyVarBndrs exTyVars
       ; prov'       <- reifyCxt prov
       ; tau'        <- reifyType (mkScaledFunTys argTys resTy)
       ; return $ TH.ForallT univTyVars' req'
                $ TH.ForallT exTyVars' prov' tau' }

reifyKind :: Kind -> TcM TH.Kind
reifyKind = reifyType

reifyCxt :: [PredType] -> TcM [TH.Pred]
reifyCxt   = mapM reifyType

reifyFunDep :: ([TyVar], [TyVar]) -> TH.FunDep
reifyFunDep (xs, ys) = TH.FunDep (map reifyName xs) (map reifyName ys)

class ReifyFlag flag flag' | flag -> flag' where
    reifyFlag :: flag -> flag'

instance ReifyFlag () () where
    reifyFlag () = ()

instance ReifyFlag Specificity TH.Specificity where
    reifyFlag SpecifiedSpec = TH.SpecifiedSpec
    reifyFlag InferredSpec  = TH.InferredSpec

instance ReifyFlag TyConBndrVis (Maybe TH.BndrVis) where
    reifyFlag AnonTCB              = Just TH.BndrReq
    reifyFlag (NamedTCB Required)  = Just TH.BndrReq
    reifyFlag (NamedTCB (Invisible _)) =
      Nothing -- See Note [Reifying invisible type variable binders] and #22828.

-- Currently does not return invisible type variable binders (@k-binders).
-- See Note [Reifying invisible type variable binders] and #22828.
reifyTyConBinders :: TyCon -> TcM [TH.TyVarBndr TH.BndrVis]
reifyTyConBinders tc = fmap (mapMaybe get_bndr) (reifyTyVarBndrs (tyConBinders tc))
  where
    get_bndr :: TH.TyVarBndr (Maybe flag) -> Maybe (TH.TyVarBndr flag)
    get_bndr = sequenceA

{- Note [Reifying invisible type variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In reifyFlag for TyConBndrVis, we have the following clause:

    reifyFlag (NamedTCB (Invisible _)) = Nothing

This means that reifyTyConBinders doesn't reify invisible type variables as
@k-binders. However, it is possible (and not hard) to change this.
Just replace the above clause with:

    reifyFlag (NamedTCB Specified) = Just TH.BndrInvis
    reifyFlag (NamedTCB Inferred)  = Nothing    -- Inferred variables can not be bound

There are two reasons we opt not to do that for now.
  1. It would be a (sometimes silent) breaking change affecting th-abstraction,
     aeson, and other libraries that assume that reified binders are visible.
  2. It would create an asymmetry with visible kind applications, which are
     not reified either.

This decision is not set in stone. If a use case for reifying invisible type
variable binders presents itself, we can reconsider. See #22828.
-}

reifyTyVars :: [TyVar] -> TcM [TH.TyVarBndr ()]
reifyTyVars = reifyTyVarBndrs . map mk_bndr
  where
    mk_bndr tv = Bndr tv ()

reifyTyVarBndrs :: ReifyFlag flag flag'
                => [VarBndr TyVar flag] -> TcM [TH.TyVarBndr flag']
reifyTyVarBndrs = mapM reify_tvbndr
  where
    -- even if the kind is *, we need to include a kind annotation,
    -- in case a poly-kind would be inferred without the annotation.
    -- See #8953 or test th/T8953
    reify_tvbndr (Bndr tv fl) = TH.KindedTV (reifyName tv)
                                            (reifyFlag fl)
                                            <$> reifyKind (tyVarKind tv)

reifyTyVarsToMaybe :: [TyVar] -> TcM (Maybe [TH.TyVarBndr ()])
reifyTyVarsToMaybe []  = pure Nothing
reifyTyVarsToMaybe tys = Just <$> reifyTyVars tys

reify_tc_app :: TyCon -> [Type.Type] -> TcM TH.Type
reify_tc_app tc tys
  = do { tys' <- reifyTypes (filterOutInvisibleTypes tc tys)
       ; maybe_sig_t (mkThAppTs r_tc tys') }
  where
    arity       = tyConArity tc

    r_tc | isUnboxedSumTyCon tc           = TH.UnboxedSumT (arity `div` 2)
         | isUnboxedTupleTyCon tc         = TH.UnboxedTupleT (arity `div` 2)
         | isPromotedTupleTyCon tc        = TH.PromotedTupleT (arity `div` 2)
             -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
         | isTupleTyCon tc                = if isPromotedDataCon tc
                                            then TH.PromotedTupleT arity
                                            else TH.TupleT arity
         | tc `hasKey` constraintKindTyConKey
                                          = TH.ConstraintT
         | tc `hasKey` unrestrictedFunTyConKey = TH.ArrowT
         | tc `hasKey` listTyConKey       = TH.ListT
         | tc `hasKey` nilDataConKey      = TH.PromotedNilT
         | tc `hasKey` consDataConKey     = TH.PromotedConsT
         | tc `hasKey` heqTyConKey        = TH.EqualityT
         | tc `hasKey` eqPrimTyConKey     = TH.EqualityT
         | tc `hasKey` eqReprPrimTyConKey = TH.ConT (reifyName coercibleTyCon)
         | isDataKindsPromotedDataCon tc  = TH.PromotedT (reifyName tc)
         | otherwise                      = TH.ConT (reifyName tc)

    -- See Note [When does a tycon application need an explicit kind
    -- signature?] in GHC.Core.TyCo.Rep
    maybe_sig_t th_type
      | tyConAppNeedsKindSig
          False -- We don't reify types using visible kind applications, so
                -- don't count specified binders as contributing towards
                -- injective positions in the kind of the tycon.
          tc (length tys)
      = do { let full_kind = typeKind (mkTyConApp tc tys)
           ; th_full_kind <- reifyKind full_kind
           ; return (TH.SigT th_type th_full_kind) }
      | otherwise
      = return th_type

------------------------------
reifyName :: NamedThing n => n -> TH.Name
reifyName thing
  | isExternalName name
              = mk_varg pkg_str mod_str occ_str
  | otherwise = TH.mkNameU occ_str (toInteger $ getKey (getUnique name))
        -- Many of the things we reify have local bindings, and
        -- NameL's aren't supposed to appear in binding positions, so
        -- we use NameU.  When/if we start to reify nested things, that
        -- have free variables, we may need to generate NameL's for them.
  where
    name    = getName thing
    mod     = assert (isExternalName name) $ nameModule name
    pkg_str = unitString (moduleUnit mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = occNameString occ
    occ     = nameOccName name
    mk_varg | OccName.isDataOcc occ = TH.mkNameG_d
            | OccName.isVarOcc  occ = TH.mkNameG_v
            | OccName.isTcOcc   occ = TH.mkNameG_tc
            | Just con_fs <- OccName.fieldOcc_maybe occ
            = \ pkg mod occ -> TH.mkNameG_fld pkg mod (unpackFS con_fs) occ
            | otherwise             = pprPanic "reifyName" (ppr name)

reifyFieldLabel :: FieldLabel -> TH.Name
reifyFieldLabel fl = reifyName $ flSelector fl

------------------------------
reifyFixity :: Name -> TcM (Maybe TH.Fixity)
reifyFixity name
  = do { (found, fix) <- lookupFixityRn_help name
       ; return (if found then Just (conv_fix fix) else Nothing) }
    where
      conv_fix (Hs.Fixity i d) = TH.Fixity i (conv_dir d)
      conv_dir Hs.InfixR = TH.InfixR
      conv_dir Hs.InfixL = TH.InfixL
      conv_dir Hs.InfixN = TH.InfixN

reifyUnpackedness :: DataCon.SrcUnpackedness -> TH.SourceUnpackedness
reifyUnpackedness NoSrcUnpack = TH.NoSourceUnpackedness
reifyUnpackedness SrcNoUnpack = TH.SourceNoUnpack
reifyUnpackedness SrcUnpack   = TH.SourceUnpack

reifyStrictness :: DataCon.SrcStrictness -> TH.SourceStrictness
reifyStrictness NoSrcStrict = TH.NoSourceStrictness
reifyStrictness SrcStrict   = TH.SourceStrict
reifyStrictness SrcLazy     = TH.SourceLazy

reifySourceBang :: DataCon.HsSrcBang
                -> (TH.SourceUnpackedness, TH.SourceStrictness)
reifySourceBang (HsSrcBang _ (HsBang u s)) = (reifyUnpackedness u, reifyStrictness s)

reifyDecidedStrictness :: DataCon.HsImplBang -> TH.DecidedStrictness
reifyDecidedStrictness HsLazy       = TH.DecidedLazy
reifyDecidedStrictness (HsStrict _) = TH.DecidedStrict
reifyDecidedStrictness HsUnpack{}   = TH.DecidedUnpack

reifyTypeOfThing :: TH.Name -> TcM TH.Type
reifyTypeOfThing th_name = do
  thing <- getThing th_name
  case thing of
    AGlobal (AnId id) -> reifyType (idType id)
    AGlobal (ATyCon tc) -> reifyKind (tyConKind tc)
    AGlobal (AConLike (RealDataCon dc)) ->
      reifyType (idType (dataConWrapId dc))
    AGlobal (AConLike (PatSynCon ps)) ->
      reifyPatSynType (patSynSigBndr ps)
    ATcId{tct_id = id} -> liftZonkM (zonkTcType (idType id)) >>= reifyType
    ATyVar _ tctv -> liftZonkM (zonkTcTyVar tctv) >>= reifyType
    -- Impossible cases, supposedly:
    AGlobal (ACoAxiom _) -> panic "reifyTypeOfThing: ACoAxiom"
    ATcTyCon _ -> panic "reifyTypeOfThing: ATcTyCon"
    APromotionErr _ -> panic "reifyTypeOfThing: APromotionErr"

------------------------------
lookupThAnnLookup :: TH.AnnLookup -> TcM CoreAnnTarget
lookupThAnnLookup (TH.AnnLookupName th_nm) = fmap NamedTarget (lookupThName th_nm)
lookupThAnnLookup (TH.AnnLookupModule (TH.Module pn mn))
  = return $ ModuleTarget $
    mkModule (stringToUnit $ TH.pkgString pn) (mkModuleName $ TH.modString mn)

reifyAnnotations :: Data a => TH.AnnLookup -> TcM [a]
reifyAnnotations th_name
  = do { name <- lookupThAnnLookup th_name
       ; topEnv <- getTopEnv
       ; epsHptAnns <- liftIO $ prepareAnnotations topEnv Nothing
       ; tcg <- getGblEnv
       ; let selectedEpsHptAnns = findAnns deserializeWithData epsHptAnns name
       ; let selectedTcgAnns = findAnns deserializeWithData (tcg_ann_env tcg) name
       ; return (selectedEpsHptAnns ++ selectedTcgAnns) }

------------------------------
modToTHMod :: Module -> TH.Module
modToTHMod m = TH.Module (TH.PkgName $ unitString  $ moduleUnit m)
                         (TH.ModName $ moduleNameString $ moduleName m)

reifyModule :: TH.Module -> TcM TH.ModuleInfo
reifyModule (TH.Module (TH.PkgName pkgString) (TH.ModName mString)) = do
  this_mod <- getModule
  let reifMod = mkModule (stringToUnit pkgString) (mkModuleName mString)
  if (reifMod == this_mod) then reifyThisModule else reifyFromIface reifMod
    where
      reifyThisModule = do
        usages <- fmap (map modToTHMod . Map.keys . imp_mods) getImports
        return $ TH.ModuleInfo usages

      reifyFromIface reifMod = do
        iface <- loadInterfaceForModule (text "reifying module from TH for" <+> ppr reifMod) reifMod
        let usages = [modToTHMod m | usage <- mi_usages iface,
                                     Just m <- [usageToModule (moduleUnit reifMod) usage] ]
        return $ TH.ModuleInfo usages

      usageToModule :: Unit -> Usage -> Maybe Module
      usageToModule _ (UsageFile {}) = Nothing
      usageToModule this_pkg (UsageHomeModule { usg_mod_name = mn }) = Just $ mkModule this_pkg mn
      usageToModule _ (UsagePackageModule { usg_mod = m }) = Just m
      usageToModule _ (UsageMergedRequirement { usg_mod = m }) = Just m
      usageToModule this_pkg (UsageHomeModuleInterface { usg_mod_name = mn }) = Just $ mkModule this_pkg mn

------------------------------
mkThAppTs :: TH.Type -> [TH.Type] -> TH.Type
mkThAppTs fun_ty arg_tys = foldl' TH.AppT fun_ty arg_tys

noTH :: UnrepresentableTypeDescr -> Type -> TcM a
noTH s d = failWithTc $ TcRnTHError $ THReifyError $ CannotRepresentType s d

ppr_th :: TH.Ppr a => a -> SDoc
ppr_th x = text (TH.pprint x)

tcGetInterp :: TcM Interp
tcGetInterp = do
   hsc_env <- getTopEnv
   case hsc_interp hsc_env of
      Nothing -> liftIO $ throwIO (InstallationError "Template haskell requires a target code interpreter")
      Just i  -> pure i

-- Note [Bootstrapping Template Haskell]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Staged Metaprogramming as implemented in Template Haskell introduces a whole
-- new dimension of staging to the already staged bootstrapping process.
-- While users depend on the user-facing `template-haskell` library, the TH
-- interface (all wired-in identifiers) is defined in `ghc-internal` and for
-- bootstrapping purposes, re-exported from `ghc-boot-th`.
--
-- Nomenclature:
--
--   boot/stage0 compiler: An already released compiler used to compile GHC
--   stage(N+1) compiler: The result of compiling GHC from source with stage(N)
--       Recall that any code compiled by the stage1 compiler should be binary
--       identical to the same code compiled by later stages.
--   boot `ghc-boot-th`: the `ghc-boot-th` that comes with (and is linked to) the
--       boot/stage0 compiler
--   in-tree `ghc-boot-th`: the `ghc-boot-th` library that lives in GHC's repository.
--
-- Here is how we bootstrap TH in tandem with GHC:
--
--  1. Build the stage1 compiler with the boot compiler.
--     The latter comes with its own boot `ghc-boot-th` library, but we do not import it.
--  2. Instead, the stage1 compiler depends on the in-tree `ghc-boot-th`.
--       * To avoid clashes with the boot `ghc-boot-th`, we change its
--         package-id `ghc-boot-th-next`.
--       * There is a bit of CPP to vendor the stage1 TH AST defined in
--         `ghc-internal`, which we cannot build with the boot compiler.
--  3. Build `ghc-internal` and in-tree `ghc-boot-th` with the stage1 compiler.
--     From here on `ghc-boot-th` re-exposes the TH modules from `ghc-internal`.
--  4. Build and link the stage2 compiler against the in-tree `ghc-boot-th`.
--     NB: No dependency on `ghc-boot-th-next`.
--
-- Observations:
--
--  A. The vendoring in (2) means that the fully qualified name of the in-tree TH
--     AST will be, e.g., `ghc-boot-th-next:...VarE`, not `ghc-internal:...VarE`.
--     That is OK, because we need it just for the `Binary` instance and to
--     convert TH ASTs returned by splices into the Hs AST, both of which do not
--     depend on the fully qualified name of the type to serialise! Importantly,
--     Note [Hard-wiring in-tree template-haskell for desugaring quotes] is
--     unaffected, because the desugaring refers to names in the in-tree TH
--     library, which is built in the next stage, stage1, and later.
--
-- When we decided in favour of the current design, `template-haskell`
-- still contained the wired-in Ids that meanwhile were moved to
-- `ghc-internal`.
-- These were the (rejected) alternative designs back then:
--
--  1b. Build the in-tree TH with the stage0 compiler and link the stage1 compiler
--      against it. This is what we did until Apr 24 and it is problematic (#23536):
--        * (It rules out using TH in GHC, for example to derive GHC.Core.Map types,
--           because the boot compiler expects the boot TH AST in splices, but, e.g.,
--           splice functions in GHC.Core.Map.TH would return the in-tree TH AST.
--           However, at the moment, we are not using TH in GHC anyway.)
--        * Ultimately, we must link the stage1 compiler against a
--          single version of template-haskell.
--            (Beyond the fact that doing otherwise would invite even
--            more "which `template-haskell` is this" confusion, it
--            would also result in confusing linker errors: see for
--            example #21981.  In principle we could likely lift this
--            restriction with more aggressive name mangling, but the
--            knock-on effects of doing so are unexplored.)
--        * If the single version is the in-tree TH, we have to recompile all boot
--          libraries (e.g. bytestring, containers) with this new TH version.
--        * But the boot libraries must *not* be built against a non-boot TH version.
--          The reason is Note [Hard-wiring in-tree template-haskell for desugaring quotes]:
--          The boot compiler will desugar quotes wrt. names in the boot TH version.
--          A quote like `[| unsafePackLenLiteral |]` in bytestring will desugar
--          to `varE (mkNameS "unsafePackLenLiteral")`, and all
--          those smart constructors refer to locations in *boot TH*, because that
--          is all that the boot GHC knows about.
--          If the in-tree TH were to move or rename the definition of
--          `mkNameS`, the boot compiler would report a linker error when
--          compiling bytestring.
--        * (Stopping to use quotes in bytestring is no solution, either, because
--           the `Lift` type class is wired-in as well.
--           Only remaining option: provide an entirely TH-less variant of every
--           boot library. That would place a huge burden on maintainers and is
--           thus rejected.)
--        * We have thus made it impossible to refactor in-tree TH.
--          This problem was discussed in #23536.
--  1c. Do not build the stage1 compiler against any library exposing the in-tree TH AST.
--      This is viable because no splices need to be run as part of the
--      bootstrapping process, so we could CPP away all the code in the stage1
--      compiler that refers to template-haskell types. However,
--        * it is not so simple either: a surprising example is GHC.Tc.Errors.Types
--          where we would need to replace all TH types with dummy types.
--          (We *cannot* simply CPP away TH-specific error constructors because
--          that affects binary compatibility with the stage2 compiler.)
--        * we would still need to vendor the updated Extension enum, so even
--          though we had to use a lot of CPP, we still end up depending on names
--          that are not present in the stage2 compiler.
--        * this design would never allow us to use TH in GHC's code base, for
--          example in GHC.Core.Map.
--      It seems simpler just to depend on a template-haskell library in a fake
--      namespace.
--  2b. Alternatively vendor the parts relevant to serialising
--      the (new, in-tree) TH AST into `ghc-boot`, thus shadowing definitions in the
--      implicitly linked boot TH.
--        * We found that this led to quite a bit of duplication in the
--          `ghc-boot` cabal file.

-- Note [Hard-wiring in-tree template-haskell for desugaring quotes]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- To desugar Template Haskell quotes, GHC needs to wire in a bunch of Names in the
-- `ghc-internal` library as Note [Known-key names], in GHC.Builtin.Names.TH.
-- Consider
-- > foo :: Q Exp
-- > foo = [| unwords ["hello", "world"] |]
-- this desugars to Core that looks like this
-- > varE (mkNameS "unwords") `appE` listE [litE (stringE "hello"), litE (stringE "world")]
-- And all these smart constructors are known-key.
-- NB: Since the constructors are known-key, it is impossible to link this program
-- against another `ghc-internal` library in which, e.g., `varE` was moved into a
-- different module. So effectively, GHC is hard-wired against the in-tree
-- `ghc-internal` library.

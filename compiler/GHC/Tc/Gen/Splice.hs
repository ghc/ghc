{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TypeFamilies           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

     runMetaE, runMetaP, runMetaT, runMetaTWithIndices, runMetaD, runQinTcM,
     tcTopSpliceExpr, lookupThName_maybe,
     defaultRunMeta, runMeta', runRemoteModFinalizers,
     finishTH, runTopSplice, tcRunClosedCall, tcPrepareClosedApplication, tcPrepareClosedFunctionValue,
     tcPrepareClosedResult, tcPrepareClosedObservation
     , tcCheckClosedCallContext, tcPreflightClosedCall, tcDecodeClosedCallOperand, tcHasClosedCallExecutable
      ) where

import GHC.Prelude

import GHC.Driver.Errors
import GHC.Driver.Plugins
import GHC.Driver.Session (safeInferOn, safeLanguageOn)
import GHC.Driver.Main
import GHC.Driver.Main.Passes (hscCompileCoreExprWithOptionalName)
import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Driver.Ppr (showPpr, showSDocForUser)
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
import GHC.Tc.Types.BasicTypes (TcTyThing(ATcTypeLet, ATcCaseLet))
import GHC.Tc.Types.Origin
import GHC.Tc.Gen.SourceTable (checkOptionalSourceUses)
import GHC.Tc.Types.LclEnv
import GHC.Tc.Types.Constraint (WantedConstraints)
import GHC.Tc.Types.Evidence
import GHC.Tc.Zonk.Type
import GHC.Tc.Zonk.TcType
import GHC.Tc.Solver
import GHC.Tc.Utils.TcMType
import GHC.Tc.Gen.HsType
import GHC.Tc.Instance.Family
import GHC.Tc.Utils.Instantiate

import qualified GHC.Core as Core
import GHC.Core.FVs (exprSomeFreeVarsList)
import GHC.Core.Make (mkCoreApp, mkCoreApps, mkCoreConWrapApps, mkCharExpr, mkListExpr
                     , mkConsExpr, mkNaturalExpr)
import GHC.Core.Multiplicity
import GHC.Core.Coercion
  ( coercionLKind, coercionRKind, coercionRole, etaExpandCoAxBranch
  , liftCoSubstWith, mkCoVarCo, mkSelCo, mkSubCo, mkSymCo )
import GHC.Core.Type as Type
import GHC.Core.TyCo.Rep as TyCoRep
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv as InstEnv
import GHC.Core.Utils (dataConRepInstPat, exprType, mkCast, mkLamType)

import GHC.Builtin.TH
import GHC.Builtin.Modules (gHC_INTERNAL_BASE, gHC_TYPES, mkGhcInternalModule)
import GHC.Builtin.KnownKeys
import GHC.Builtin.KnownOccs (eitherTyConOcc, returnIOIdOcc, toAnnotationWrapperIdOcc)
import GHC.Builtin.WiredIn.Types

import GHC.ThToHs
import GHC.HsToCore.Docs
import GHC.HsToCore.Expr
import GHC.HsToCore.Monad
import GHC.Iface.Load
import GHC.Iface.Env (lookupOrig)

import GHCi.Message
import GHCi.RemoteTypes
import GHC.Runtime.Interpreter

import GHC.Rename.Splice( traceSplice, SpliceInfo(..) )
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
import GHC.Types.SourceFile (isHsBootOrSig)
import GHC.Types.SourceText
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence as OccName
import GHC.Types.Var
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Unique
import GHC.Types.Unique.Supply (getUniquesM)
import GHC.Types.Var.Set
import GHC.Types.Meta
import GHC.Types.Basic hiding( SuccessFlag(..) )
import GHC.Types.Error
import GHC.Types.Fixity as Hs
import GHC.Types.Annotations
import GHC.Types.Name
import GHC.Types.Unique.Map
import GHC.Serialized

import GHC.Linker.Loader (executableDependenciesAvailable)
import GHC.Unit.Finder
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.IfaceToCore ( tcIfaceImport )

import GHC.Utils.Misc
import GHC.Utils.Panic as Panic
import GHC.Utils.Lexeme
import GHC.Utils.Outputable
import GHC.Utils.Logger
import GHC.Utils.Exception (throwIO, ErrorCall(..), SomeAsyncException)

import GHC.Utils.TmpFs ( newTempName, TempFileLifetime(..) )

import GHC.Data.Bag (Bag)
import GHC.Data.FastString
import GHC.Data.Maybe( MaybeErr(..), expectJust )
import GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.LanguageExtensions as LangExt

-- THSyntax gives access to internal functions and data types
import qualified GHC.Boot.TH.Syntax as TH
import qualified GHC.Boot.TH.Monad  as TH
import GHC.Boot.TH.Monad  (MetaHandlers(..))
import qualified GHC.Boot.TH.Ppr    as TH

#if defined(HAVE_INTERNAL_INTERPRETER)
import Unsafe.Coerce    ( unsafeCoerce )
#if !MIN_VERSION_base(4,22,0)
import GHC.Desugar ( AnnotationWrapper(..) )
#else
import GHC.Internal.Desugar ( AnnotationWrapper(..) )
#endif
import Control.DeepSeq
#endif

import Control.Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Short (ShortByteString)
import Data.Containers.ListUtils ( nubOrd )
import Data.List ( find, sortBy, stripPrefix )
import Data.Maybe
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic  ( fromDynamic, toDyn )
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Typeable ( typeOf, Typeable, TypeRep, typeRep )
import Data.Data (Data)
import Data.Proxy    ( Proxy (..) )
import Data.IORef
import Numeric.Natural (Natural)
import Text.Read (readMaybe)
import GHC.Parser.HaddockLex (lexHsDoc)
import GHC.Parser (parseIdentifier)
import GHC.Rename.Doc (rnHsDoc)

{-
Note [Template Haskell state diagram]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here are the ThLevels, their corresponding level numbers
(the result of (thLevelIndex s)), and their state transitions.
The top level of the program is stage Comp:

     Start here
         |
         V
      -----------     $      ------------   $    -----------------
      |  Comp   | ---------> |  Splice  | -----> | Splice Splice |
      |   0     |            |    -1    | <----  |     -2        |
      -----------            ------------  [||]  -----------------
        ^     |                ^      |
      $ |     | [||]         $ |      | [||]
        |     v                |      v
   --------------          ----------------
   | Brack Comp |          | Brack Splice |
   |     1      |          |      0       |
   --------------          ----------------

* Normal top-level declarations start in state Comp
       (which has level 0).
  Annotations start in state Splice, since they are
       treated very like a splice (only without a '$')

* Code compiled in state Splice (and only such code)
  will be *run at compile time*, with the result replacing
  the splice

* The original paper did not allow a splice within a
  splice, but there is no reason not to. This is the
  $ transition in the top right.

Note [Template Haskell levels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* The level checks are implemented for terms in `checkThLocalName`

* Imported things are level 0

* Top-level things are level 0

* In GHCi, variables bound by a previous command are treated
  as imported, because we have bytecode for them.

* Variables are bound at the "current level"

* The current level starts off at 0

* The level is decremented by splicing $(..)
               incremented by brackets [| |]
               incremented by name-quoting 'f

* When a variable is used, checkThLocalName compares
        bind:  binding level, and
        use:   current level at usage site

  Generally
        bind > use      Always error (bound later than used)
                        [| \x -> $(f x) |]

        bind = use      Always OK (bound same stage as used)
                        [| \x -> $(f [| x |]) |]

        bind < use      Inside brackets, it depends on what cross stage
                        persistence rules are used.

  For (bind < use) inside brackets, there are three cases:
    - Imported things (if ImplicitStagePersistence is enabled)   OK      f = [| map |]
    - Top-level things (if ImplicitStagePersistence is enabled)  OK      g = [| f |]
    - Non-top-level     Only if there is a liftable instance
                                h = \(x:Int) -> [| x |]

  If ExplicitLevelImports is used, then imports can bring identifiers into scope
  at non-zero levels.

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

1. Typechecking nested splices (immediately in tcUntypedSplice)
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
life cycle. Quasi-quotes are very like splices; see Note [Quasi-quote overview]).

The type structure is

  data HsExpr p = ...
    | HsUntypedSplice (XUntypedSplice p) (HsUntypedSplice p)

  data HsUntypedSplice p
    = HsUntypedSpliceExpr (XUntypedSpliceExpr p) (LHsExpr p)
    | HsQuasiQuote (XQuasiQuote p) (IdP id) (XRec p FastString)
    | XXUntypedSplice (XUntypedSplice p)

Untyped splices can occur in expressions, patterns,
types, and declarations.  So we have a HsUntypedSplice data
constructor in all four of these types.

Untyped splices never occur in (HsExpr GhcTc), and similarly
patterns etc, because the body of a untyped quotation is not typechecked. So we have

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

* Set the ThLevel to (Brack s (RnPendingUntyped ps_var))

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
        [PendingRnSplice spn (HsUntypedSpliceExpr UntypedExpSplice (g x  :: LHsExpr GHcRn))]
        (HsApp (HsVar f) (HsUntypedSplice (HsUntypedSpliceNested spn)
                                          (HsUntypedSpliceExpr UntypedExpSplice (g x :: LHsExpr GhcRn))))

Note that a nested splice, such as the `$(g x)` now appears twice:
  - In the PendingRnSplice: this is the version that will later be typechecked
  - In the HsUntypedSpliceExpr in the body of the bracket. This copy is used
    only for pretty printing.

NB: a single untyped bracket can contain many splices, each of a different
`UntypedSpliceFlavour`. For example

   [| let $e0 in (f :: $e1) $e2 (\ $e -> body ) |] + 1

Here $e0 is a declaration splice, $e1 is a type splice, $e2 is an
expression splice, and $e3 is a pattern splice. The `SpliceFlavour` is stored
in the extension field of HsUntypedSpliceExpr.

TYPECHECKER (tcUntypedBracket): see also Note [Typechecking Overloaded Quotes]

* Typecheck the [PendingRnSplice] individually, to give [PendingTcSplice].
  PendingTcSplice is used for both typed and untyped splices.

* Ignore the body of the bracket; just check that the context
  expects a bracket of that type (e.g. a [p| pat |] bracket should
  be in a context needing a (m Pat)

* Stash the whole lot inside a HsBracketTc

Result is:
    HsUntypedBracket
        (HsBracketTc { hsb_splices = [PendingTcSplice spn (g x  :: LHsExpr GHcTc)]
                     , hsb_quote = HsApp (HsVar f)
                                         (HsUntypedSplice (HsUntypedSpliceNested spn)
                                            (HsUntypedSpliceExpr UntypedExpSplice (g x :: LHsExpr GhcRn)))
                     })
        (XQuote noExtField)

NB in the typechecker output, the original payload (which would now
have type (HsQuote GhcTc) is stubbed off with (XQuote noExtField). The payload
is now in the hsb_quote field of the HsBracketTc.

-------------------------------------
Implicit lifting
-------------------------------------

When a variable is bound at level n and used at level n+1, the renamer will decide,
in `checkThLocalNameWithLift`, to implicitly lift this variable to level n+1.
This is done by replacing the occurrence of `x` with `$(lift x)`.
This decision is then stored in the extension points 'XXUntypedSplice'/'XXTypedSplice'.
When we come to typechecking these splices, the typechecker will emit the appropriate
'Lift' constraint, with a `ImplicitLiftOrigin` `CtOrigin`. If we fail to solve this constraint,
a level error is reported to the user.



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
`PendingTcSplice`, very similar to Note [Pending Splices]. Again, the
splice needs a SplicePointName, for the desugarer to use to connect
the splice expression with the point in the syntax tree where it is
used.  Example:
     [||  f $$(g 2)||]

Parser: this is parsed as

    HsTypedBracket _ (HsApp (HsVar "f")
                            (HsTypedSplice _ (HsTypedSpliceExpr _ (g 2 :: LHsExpr GhcPs))))

RENAMER (rnTypedSplice): the renamer adds a SplicePointName, spn:

    HsTypedBracket _ (HsApp (HsVar "f")
                            (HsTypedSplice (HsTypedSpliceNested spn) (HsTypedSpliceExpr _ (g x :: LHsExpr GhcRn))))

TYPECHECKER (tcTypedBracket):

* Set the ThLevel to (Brack s (TcPending ps_var lie_var lexical_givens quote_wrapper))
  Save the outer lexical native Givens with the outer constraint accumulator.
  Restore both when checking a nested splice. Evidence parameters introduced
  inside the quoted body must not enter the emitted splice expression.

* Typecheck the body, and keep the elaborated result (despite never using it!, todo: it should be stored in the AST)

* Nested splices (which must be typed) are typechecked by tcTypedSplice, and
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
tcTypedSplice     :: HsTypedSpliceResult -> HsTypedSplice GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)

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
  = addErrCtxt (TypedTHBracketCtxt expr) $
    do { cur_lvl <- getThLevel
       ; ps_var <- newMutVar []
       ; lie_var <- getConstraintVar   -- Any constraints arising from nested splices
                                       -- should get thrown into the constraint set
                                       -- from outside the bracket

       ; lexical_givens <- tcl_lexical_givens . tcl_lcl_ctxt <$> getLclEnv

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
       ; (tc_expr, expr_ty) <- setThLevel
                                (Brack cur_lvl (TcPending ps_var lie_var lexical_givens wrapper)) $
                                tcScalingUsage ManyTy $
                                -- Scale by Many, TH lifting is currently nonlinear (#18465)
                                tcInferRhoNC expr
                                -- NC for no context; tcBracket does that
       ; let rep = getRuntimeRep expr_ty
       ; meta_ty <- tcCodeTy m_var expr_ty
       ; ps' <- readMutVar ps_var
       ; codeco <- tcLookupKnownOccId unsafeCodeCoerceOcc
       ; bracket_ty <- mkAppTy m_var <$> tcMetaKnownOccTy expTyConOcc
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
       ; ps' <- case brack_info of
                  Just q -> mapM (tc_nested_splice q) ps
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
    where
      tc_nested_splice :: QuoteWrapper -> PendingRnSplice -> TcM PendingTcSplice
      tc_nested_splice q (PendingRnSplice splice_name expr) = tcUntypedSplice q splice_name expr

-- | A type variable with kind * -> * named "m"
mkMetaTyVar :: TcM TyVar
mkMetaTyVar =
  newNamedFlexiTyVar (fsLit "m") (mkVisFunTyManyM liftedTypeKind liftedTypeKind)


-- | For a type 'm', emit the constraint 'Quote m'.
emitQuoteWanted :: Type -> TcM EvVar
emitQuoteWanted m_var =  do
        quote_con <- tcLookupKnownOccTyCon quoteClassOcc
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
        final_ty <- mkAppTy m_var <$> tcMetaKnownOccTy n
        -- Return the evidence variable and metavariable to be used during
        -- desugaring.
        let wrapper = QuoteWrapper ev_var m_var
        return (Just wrapper, final_ty)
  in
  case b of
    (VarBr {}) -> (Nothing,) <$> tcMetaKnownOccTy nameTyConOcc
                                           -- Result type is Var (not Quote-monadic)
    (ExpBr {})  -> mkTy expTyConOcc  -- Result type is m Exp
    (TypBr {})  -> mkTy typeTyConOcc -- Result type is m Type
    (DecBrG {}) -> mkTy decsTyConOcc -- Result type is m [Dec]
    (PatBr {})  -> mkTy patTyConOcc  -- Result type is m Pat
    (DecBrL {}) -> panic "tcBrackTy: Unexpected DecBrL"


untypedSpliceResultType :: UntypedSpliceFlavour -> TcType -> TcM TcType
untypedSpliceResultType flavour meta_ty = do
  sp_ty <- tcMetaKnownOccTy sp_ty_name
  return $ mkAppTy meta_ty sp_ty
  where
    sp_ty_name = case flavour of
      UntypedExpSplice  -> expTyConOcc
      UntypedPatSplice  -> patTyConOcc
      UntypedTypeSplice -> typeTyConOcc
      UntypedDeclSplice -> decsTyConOcc

---------------
-- | Typechecking a pending splice from a untyped bracket
-- TODO: Should return HsUntypedSplice GhcTc, and the desugarer should
--       construct the relevant CoreExpr rather than performing desugaring here.
tcUntypedSplice :: QuoteWrapper -- Metavariable for the expected overall type of the
                          -- quotation.
                -> SplicePointName
                -> HsUntypedSplice GhcRn
                -> TcM PendingTcSplice
tcUntypedSplice (QuoteWrapper _ m_var) splice_name (HsUntypedSpliceExpr (HsUserSpliceExt flavour) expr)
  -- See Note [Typechecking Overloaded Quotes]
  = do { expected_type <- untypedSpliceResultType flavour m_var
       ; expr' <- tcScalingUsage ManyTy $ tcCheckPolyExpr expr expected_type
                  -- Scale by Many, TH lifting is currently nonlinear (#18465)
       ; return (PendingTcSplice splice_name expr') }
tcUntypedSplice (QuoteWrapper _ m_var) splice_name (HsQuasiQuote (HsQuasiQuoteExt flavour) quoter s) = do
   -- 1. Check that the quoter is of type 'QuasiQuoter'
   qq_type <- tcMetaKnownOccTy quasiQuoterTyConOcc
   quoter' <- setSrcSpan (getLocA quoter) $ tcCheckId (unLoc quoter) (Check qq_type)

   -- 2. Check that the quasi-quote has type Q Exp/Q Pat/Q Dec/Q Decs (as appropriate)
   qTy <- mkTyConTy <$> tcLookupKnownOccTyCon qTyConOcc
   quote_ty <- untypedSpliceResultType flavour m_var
   splice_ty <- untypedSpliceResultType flavour qTy
   res_co <- unifyInvisibleType InvisibleKind splice_ty quote_ty

   -- 3. Lookup the relevant field selector from QuasiQuoter
   sel <- tcLookupKnownOccId qq_sel_name

   -- 4. Apply the selector to the quasi-quoter
   let expr' = mkLHsWrapCo res_co $
                nlHsApp (nlHsApp (nlHsVar sel) (noLocA quoter')) (nlHsLit (HsString NoSourceText (unLoc s)))

   return (PendingTcSplice splice_name expr')
   where
    qq_sel_name = case flavour of
      UntypedExpSplice  -> quoteExpName
      UntypedPatSplice  -> quotePatName
      UntypedTypeSplice -> quoteTypeName
      UntypedDeclSplice -> quoteDecName

  -- Identifiers that are lifted implicitly, such as 'x' in
  -- E.g. \x -> [| h x |]
  -- We must behave as if the reference to x was
  --      h $(lift x)
  -- We use 'x' itself as the SplicePointName, used by
  -- the desugarer to stitch it all back together.
  -- If 'x' occurs many times we may get many identical
  -- bindings of the same SplicePointName, but that doesn't
  -- matter, although it's a mite untidy.
tcUntypedSplice q splice_name (XUntypedSplice ils)
  = do { let id_name = implicit_lift_lid ils
       ; id_ty <- newOpenFlexiTyVarTy
       ; let v_expr = noLocA (HsVar noExtField id_name)
       ; v_expr' <- tcCheckMonoExpr v_expr id_ty
       -- lift :: Quote m' => a -> m' Exp
       ; lift <- setSrcSpan (getLocA id_name) $
                  newKnownOccMethod (ImplicitLiftOrigin ils)
                                    GHC.Builtin.TH.liftIdOcc
                                    [getRuntimeRep id_ty, id_ty]
       ; let res = nlHsApp (mkLHsWrap (applyQuoteWrapper q) (noLocA lift)) v_expr'

       ; return (PendingTcSplice splice_name res) }

tcPendingSpliceTyped :: QuoteWrapper -> SplicePointName -> HsTypedSplice GhcRn -> ExpRhoType -> TcM PendingTcSplice
tcPendingSpliceTyped q@(QuoteWrapper _ m_var) splice_name (HsTypedSpliceExpr _ expr) res_ty
  = do { res_ty <- expTypeToType res_ty
       ; let rep = getRuntimeRep res_ty
       ; meta_exp_ty <- tcCodeTy m_var res_ty
       ; expr' <- tcCheckMonoExpr expr meta_exp_ty
       ; untype_code <- tcLookupKnownOccId unTypeCodeOcc
       ; let expr'' = mkHsApp
                         (mkLHsWrap (applyQuoteWrapper q)
                           (nlHsTyApp untype_code [rep, res_ty])) expr'
       ; return (PendingTcSplice splice_name expr'') }
tcPendingSpliceTyped q splice_name (XTypedSplice ils) res_ty
  = do { let id_name = implicit_lift_lid ils
       ; res_ty <- expTypeToType res_ty
       ; let rep = getRuntimeRep res_ty
       ; let v_expr = noLocA (HsVar noExtField id_name)
       ; v_expr' <- tcCheckMonoExpr v_expr res_ty
       -- lift :: Quote m' => a -> m' Exp
       ; lift <- setSrcSpan (getLocA id_name) $
                  newKnownOccMethod (ImplicitLiftOrigin ils)
                                    GHC.Builtin.TH.liftIdOcc
                                    [rep, res_ty]
       ; let res = nlHsApp (mkLHsWrap (applyQuoteWrapper q) (noLocA lift)) v_expr'
       ; return (PendingTcSplice splice_name res) }


---------------
-- Takes a m and tau and returns the type m (TExp tau)
tcCodeTy :: TcType -> TcType -> TcM TcType
tcCodeTy m_ty exp_ty
  = do { unless (isTauTy exp_ty) $ addErr $
          TcRnTHError $ TypedTHError $ TypedTHWithPolyType exp_ty
       ; codeCon <- tcLookupKnownOccTyCon codeTyConOcc
       ; let rep = getRuntimeRep exp_ty
       ; return (mkTyConApp codeCon [m_ty, rep, exp_ty]) }


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

tcTypedSplice HsTypedSpliceTop ctxt@(HsTypedSpliceExpr _ expr) res_ty
  = addErrCtxt (TypedSpliceCtxt Nothing ctxt) $
    setSrcSpan (getLocA expr)    $
      tcTopSplice expr res_ty
tcTypedSplice (HsTypedSpliceNested sp) expr res_ty
  -- The expr is checked in tcPendingSpliceTyped
  = addErrCtxt (TypedSpliceCtxt (Just sp) expr) $
    setSrcSpan loc    $ do
      lvl <- getThLevel
      case lvl of
        Brack _ (TcPending ps_var lie_var lexical_givens q) -> do
          tc_splice <- setConstraintVar lie_var $
                       updLclCtxt (\ctxt -> ctxt { tcl_lexical_givens = lexical_givens }) $
                       tcPendingSpliceTyped q sp expr res_ty
          updTcRef ps_var (tc_splice : )
          -- stubNestedSplice: the returned expression is ignored; it's in the pending splices.
          --
          -- Unfortunately, this means that tooling that works with typechecked
          -- expressions will not be able to identify where the nested splices are.
          -- Instead, we should modify the extension point of HsTypedSplice, in
          -- order to propagate the result of typechecking.

          return stubNestedSplice
        _ -> panic "tcTypedSplice: invalid level"
  where
    loc = case expr of
            HsTypedSpliceExpr _ expr -> getLocA expr
            XTypedSplice (HsImplicitLiftSplice _ _ _ expr) -> getLocA expr
tcTypedSplice _ _ _ = panic "tcTypedSplice: invalid splice"


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
       ; q_type <- tcMetaKnownOccTy qTyConOcc
       -- Top level splices must still be of type Q (TExp a)
       ; meta_exp_ty <- tcCodeTy q_type res_ty
       ; q_expr <- tcTopSpliceExpr Typed $
                   tcCheckMonoExpr expr meta_exp_ty
       ; lcl_env <- getLclEnv
       ; let delayed_splice
              = DelayedSplice lcl_env expr res_ty q_expr
       ; return (HsTypedSplice delayed_splice (HsTypedSpliceExpr noExtField q_expr))

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

tcTopSpliceExpr = tcTopSpliceExprWith simplifyTop

-- | Check a staged expression and attach the evidence from the supplied solver.
tcTopSpliceExprWith :: (WantedConstraints -> TcM (Bag EvBind))
                    -> SpliceType -> TcM (LHsExpr GhcTc) -> TcM (LHsExpr GhcTc)
tcTopSpliceExprWith solve isTypedSplice tc_action
  = checkNoErrs $  -- checkNoErrs: must not try to run the thing
                   -- if the type checker fails!

    setThLevel (Splice isTypedSplice Comp) $
    do {    -- Typecheck the expression
         (mb_expr', wanted) <- tryCaptureConstraints tc_action
             -- If tc_action fails (perhaps because of insoluble constraints)
             -- we want to capture and report those constraints, else we may
             -- just get a silent failure (#20179). Hence the 'try' part.

       ; const_binds <- solve wanted

       ; case mb_expr' of
            Nothing    -> failM   -- The solver should have
                                  -- reported some errors
            Just expr' -> return $ mkHsDictLet (EvBinds const_binds) expr' }

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
       ; expr2 <- setThLevel (RunSplice modfinalizers_ref) $
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
              addErrCtxt (TypedSpliceResultCtxt zonked_q_expr) $ do
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
    data_class <- tcLookupKnownKeyClass dataClassKey
    to_annotation_wrapper_id <- tcLookupKnownOccId toAnnotationWrapperIdOcc

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
                                 (mkHsVar (L (noAnnSrcSpan loc) to_annotation_wrapper_id)))
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
               rnf serialized `seq` serialized

#endif

{-
************************************************************************
*                                                                      *
\subsection{Running an expression}
*                                                                      *
************************************************************************
-}

runQinTcM :: TH.Q a -> TcM a
runQinTcM (TH.Q act) = withRunInIO $ \runInIO -> act (metaHandlersTcM runInIO)

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
      runQinTcM $ sequence_ qs
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
  -> (EnumSet LangExt.Extension -> Origin -> SrcSpan -> a -> b)
  -> (ForeignHValue -> TcM a)
  -> SrcSpan
  -> ForeignHValue {- TH.Q a -}
  -> TcM b
runQResult show_th f runQ expr_span hval
  = do { exts <- fmap extensionFlags getDynFlags
       ; th_result <- runQ hval
       ; th_origin <- getThSpliceOrigin
       ; traceTc "Got TH result:" (text (show_th th_result))
       ; return (f exts th_origin expr_span th_result) }


-----------------
runMeta :: (MetaHook TcM -> LHsExpr GhcTc -> TcM hs_syn)
        -> LHsExpr GhcTc
        -> TcM hs_syn
runMeta unwrap e = do
    env <- getGblEnv
    checkOptionalSourceUses env
    hooks <- getHooks
    case runMetaHook hooks of
        Nothing -> unwrap defaultRunMeta e
        Just h  -> unwrap h e

{- Note [Retained indices in meta hooks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A nested type splice can refer to a retained binder by its exact Name.
The renamer records the eligible names for this splice. A runMetaHook
plugin can inspect the request and delegate it to defaultRunMeta. Put
the names in the local context for the duration of the hook so both
paths convert the result with the same binders. Each type splice sets
its own scope.
-}

defaultRunMeta :: MetaHook TcM
defaultRunMeta (MetaE r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToHsExpr runTHExp)
defaultRunMeta (MetaP r)
  = fmap r . runMeta' True ppr (runQResult TH.pprint convertToPat runTHPat)
defaultRunMeta (MetaT r)
  = \e -> do
      indices <- tcl_th_retained_indices . tcl_lcl_ctxt <$> getLclEnv
      r <$> runMeta' True ppr (runQResult TH.pprint (convertToHsTypeWithIndices indices) runTHType) e
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

-- | Run a type splice with the scope of a retained signature.
runMetaTWithIndices :: [Name] -> LHsExpr GhcTc -> TcM (LHsType GhcPs)
runMetaTWithIndices indices expr =
  updLclCtxt (\ctxt -> ctxt { tcl_th_retained_indices = indices }) (runMetaT expr)

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

-- | Reconstruct a runtime operand from a checked ground literal type.
tcDecodeClosedCallOperand :: Name -> Type -> TcM (Maybe Core.CoreExpr)
tcDecodeClosedCallOperand owner operand = do
  platform <- targetPlatform <$> getDynFlags
  rdr_env <- tcg_rdr_env <$> getGblEnv
  case decode platform rdr_env operand of
    Left UnsupportedClosedShape -> return Nothing
    Left err -> reportClosedShapeError owner err
    Right (value, gres)
      | exprType value `tcEqType` typeKind operand -> do
          -- Import usage depends only on the set of constructor Names.
          addUsedGREs NoDeprecationWarnings (nonDetNameEnvElts gres)
          return (Just value)
      | otherwise -> return Nothing
  where
    decode platform rdr_env ty
      | Just atom <- closedScalar (typeKind ty) =
          maybe (Left UnsupportedClosedShape) (\value -> Right (value, emptyNameEnv)) (decodeScalar platform atom ty)
      | Just (list, [element]) <- splitTyConApp_maybe (typeKind ty)
      , list == listTyCon, closedGroundField element =
          decodeList platform rdr_env element [] emptyNameEnv ty
      | otherwise = do
          let kind = typeKind ty
          (tc, kinds) <- maybe (Left UnsupportedClosedShape) Right (splitTyConApp_maybe kind)
          (promoted, args) <- maybe (Left UnsupportedClosedShape) Right (splitTyConApp_maybe ty)
          con <- maybe (Left UnsupportedClosedShape) Right (isPromotedDataCon_maybe promoted)
          ensure (dataConTyCon con == tc)
          own_gres <- if closedBuiltinData tc
            then return emptyNameEnv
            else do
              ensure (ordinaryClosedTyCon tc kinds)
              case closedImportedGRE rdr_env con of
                Just gre -> return (unitNameEnv (dataConName con) gre)
                Nothing -> Left (InaccessibleClosedOperand tc)
          let (type_args, values) = splitAt (length (dataConUserTyVars con)) args
          (ClosedConstructorPlan source_args _, fields) <-
            closedConstructorPlan (Just type_args) kind kinds con
          ensure (closedConstructorArgumentsMatch fields values)
          found <- traverse (decode platform rdr_env) values
          return (mkCoreConWrapApps con (map Core.Type source_args ++ map fst found),
                  plusNameEnvList (own_gres : map snd found))

    decodeList platform rdr_env element acc used ty = case splitTyConApp_maybe ty of
      Just (con, [k]) | con == promotedNilDataCon, k `tcEqType` element ->
        return (mkListExpr element (reverse acc), used)
      Just (con, [k, value, rest])
        | con == promotedConsDataCon, k `tcEqType` element
        , typeKind value `tcEqType` element -> do
            (argument, gres) <- decode platform rdr_env value
            decodeList platform rdr_env element (argument : acc) (gres `plusNameEnv` used) rest
      _ -> Left UnsupportedClosedShape

    ensure ok = unless ok (Left UnsupportedClosedShape)

    decodeScalar platform ClosedNatural ty = do
      number <- isNumLitTy ty
      guard (number >= 0)
      return (mkNaturalExpr platform number)
    decodeScalar _ ClosedChar ty = mkCharExpr <$> isCharLitTy ty
    decodeScalar _ ClosedBool ty = do
      (tc, []) <- splitTyConApp_maybe ty
      con <- isPromotedDataCon_maybe tc
      guard (dataConTyCon con == boolTyCon)
      pure (Core.Var (dataConWorkId con))

-- | Scalar types with a checked runtime and promoted representation.
data ClosedCallScalar = ClosedNatural | ClosedChar | ClosedBool

-- | Value shapes supported in both directions by the closed-call evaluator.
data ClosedCallShape
  = ClosedScalar ClosedCallScalar
  | ClosedList ClosedCallShape
  | ClosedData TyCon [Kind] [ClosedConstructor]
  | ClosedRef Kind

-- | Check source arity and field kinds before applying the constructor.
closedConstructorArgumentsMatch :: [Kind] -> [Type] -> Bool
closedConstructorArgumentsMatch fields values =
  equalLength fields values && and (zipWith tcEqType fields (map typeKind values))

-- | An independently quantified datatype whose one index changes in recursion.
data ClosedOpenShape = ClosedOpenShape TyVar TyCon [Type] [ClosedOpenAlternative]

-- | Nothing denotes the one recursive field at the case-bound predecessor.
data ClosedOpenAlternative = ClosedOpenAlternative DataCon ClosedOpenIndex [Maybe ClosedCallShape]

-- | The promoted constructor that fixes the index or extends a predecessor.
data ClosedOpenIndex
  = ClosedFixedIndex { closedIndexCon :: DataCon }
  | ClosedSuccessor  { closedIndexCon :: DataCon }

-- | A finite graph of codecs keyed by their exact instantiated kinds.
data ClosedCodec = ClosedCodec ClosedCallShape [(Kind, ClosedCallShape)]

lookupCodecNode :: Kind -> [(Kind, a)] -> Maybe a
lookupCodecNode kind nodes = snd <$> find (tcEqType kind . fst) nodes

-- | The wire tag (absent for a product), constructor and field codecs.
data ClosedConstructor
  = ClosedConstructor (Maybe String) DataCon ClosedConstructorFields

-- | Field codecs use static indices or recover one index from a runtime witness.
data ClosedConstructorFields
  = ClosedStaticFields ClosedConstructorPlan [ClosedCallShape]
  | ClosedWitnessedFields ClosedOpenShape ClosedOpenShape [ClosedCallShape]

-- | Source type arguments and nominal paths for the Case-bound existentials.
data ClosedConstructorPlan = ClosedConstructorPlan [Type] [ClosedIndex]

-- | Recover the kind at a leaf of the result.
closedScalarKind :: ClosedCallScalar -> Kind
closedScalarKind ClosedNatural = naturalTy
closedScalarKind ClosedChar = charTy
closedScalarKind ClosedBool = boolTy

-- | Build the kind used for a case binder or a promoted constructor.
closedShapeKind :: ClosedCallShape -> Kind
closedShapeKind (ClosedScalar scalar) = closedScalarKind scalar
closedShapeKind (ClosedList element) = mkListTy (closedShapeKind element)
closedShapeKind (ClosedData tc kinds _) = mkTyConApp tc kinds
closedShapeKind (ClosedRef kind) = kind

-- | A shape can be unsupported, inaccessible, or recursively expanding.
data ClosedShapeError
  = UnsupportedClosedShape
  | InaccessibleClosedData TyCon Bool
  | InaccessibleClosedOperand TyCon
  | NonDecreasingClosedShape Kind Kind
  | UnrecoverableClosedIndex
  | UnsupportedClosedWitness
  | UnsupportedClosedPayload

-- | Classify a checked result and record the constructors inspected by its codec.
-- See Note [Constructor visibility in closed calls].
tcClosedCallValueShape :: Name -> Kind -> TcM (Maybe ClosedCodec)
tcClosedCallValueShape owner kind = do
  env <- getGblEnv
  case closedCallValueShape (tcg_rdr_env env) kind of
    Right codec@(ClosedCodec _ nodes) -> do
      let datatypes = concatMap dependencies (map snd nodes)
          dependencies (ClosedData tc _ alternatives) =
            (tc, [con | ClosedConstructor _ con _ <- alternatives]) :
            concatMap witness_dependencies alternatives
          dependencies _ = []
          witness_dependencies (ClosedConstructor _ _ (ClosedWitnessedFields witness payload _)) =
            open_dependencies witness ++ open_dependencies payload
          witness_dependencies _ = []
          open_dependencies (ClosedOpenShape _ tc _ alternatives) =
            (tc, [con | ClosedOpenAlternative con _ _ <- alternatives]) :
            [(dataConTyCon con, [con]) | ClosedOpenAlternative _ index _ <- alternatives
                                     , let con = closedIndexCon index]
          used = [name | (tc, constructors) <- datatypes
                       , name <- tyConName tc : map dataConName constructors]
      addUsedGREs NoDeprecationWarnings
        [gre | (_, constructors) <- datatypes, con <- constructors
             , Just gre <- [closedImportedGRE (tcg_rdr_env env) con]]
      -- The parent declaration fingerprint also covers constructors that
      -- were impossible at the index, and constructors added later.
      updTcRef (tcg_evaluated_term_uses env) (`extendNameSetList` used)
      return (Just codec)
    Left err -> reportClosedShapeError owner err

-- | Report inaccessible constructors without naming them.
reportClosedShapeError :: Name -> ClosedShapeError -> TcM (Maybe a)
reportClosedShapeError owner err = case err of
  UnsupportedClosedShape -> return Nothing
  InaccessibleClosedData tc indexed -> do
      flags <- getDynFlags
      ctx <- getNamePprCtx
      units <- hsc_units <$> getTopEnv
      let shape = if isEnumerationTyCon tc then "an enumeration" else "an algebraic datatype"
          constructors | indexed = "constructor that can produce the result index"
                       | otherwise = "constructor"
      failWithTc (TcRnClosedTypeEval owner
        ("Cannot inspect the constructors of " ++ showSDocForUser flags units ctx (ppr tc) ++
         ". To evaluate " ++ shape ++ " in a type, bring every " ++ constructors ++
         " into scope from a non-SOURCE import."))
  InaccessibleClosedOperand tc -> do
      flags <- getDynFlags
      ctx <- getNamePprCtx
      units <- hsc_units <$> getTopEnv
      failWithTc (TcRnClosedTypeEval owner
        ("A selected constructor of " ++ showSDocForUser flags units ctx (ppr tc) ++
         " is not in scope from a non-SOURCE import."))
  UnrecoverableClosedIndex ->
    failWithTc (TcRnClosedTypeEval owner
      "Cannot recover a constructor-local type argument from the checked result type.")
  UnsupportedClosedWitness ->
    failWithTc (TcRnClosedTypeEval owner
      "Cannot recover the package index from its first field. Each witness constructor must have no fields or carry only its predecessor witness.")
  UnsupportedClosedPayload ->
    failWithTc (TcRnClosedTypeEval owner
      "Cannot reify the second field of the package at the index given by its first field.")
  NonDecreasingClosedShape previous current -> do
      flags <- getDynFlags
      ctx <- getNamePprCtx
      units <- hsc_units <$> getTopEnv
      let show_type ty = showSDocForUser flags units ctx (ppr ty)
      failWithTc (TcRnClosedTypeEval owner
        ("Cannot plan the recursive constructor fields of " ++ show_type current ++
         ". The recursive type must repeat an earlier type or be a proper subterm of " ++
         show_type previous ++ ", or differ only in one index that is a proper subterm of its earlier value."))

-- | Classify a checked value and intern recursive constructor shapes.
closedCallValueShape :: GlobalRdrEnv -> Kind -> Either ClosedShapeError ClosedCodec
closedCallValueShape rdr_env kind = do
  (root, nodes) <- State.runStateT (shape [] kind) []
  return (ClosedCodec root nodes)
  where
    shape active current = case splitTyConApp_maybe current of
      Just (tc, [arg]) | tc == listTyCon -> ClosedList <$> shape active arg
      Just (tc, [arg]) | tc == maybeTyCon -> do
        field <- shape active arg
        Trans.lift $ boxed tc [arg] [(Just "0", nothingDataCon, []),
                                     (Just "1", justDataCon, [field])]
      Just (tc, [a, b]) | tc == pairTyCon -> do
        left <- shape active a
        right <- shape active b
        Trans.lift $ boxed tc [a, b]
          [(Nothing, tupleDataCon Boxed 2, [left, right])]
      Just (tc, [a, b])
        | closedEitherTyCon tc
        , [left_con, right_con] <- tyConDataCons tc -> do
          left <- shape active a
          right <- shape active b
          Trans.lift $ boxed tc [a, b] [(Just "0", left_con, [left]),
                                        (Just "1", right_con, [right])]
      _ | Just atom <- closedScalar current -> return (ClosedScalar atom)
      Just (tc, args)
        | ordinaryClosedTyCon tc args
        , let constructors = tyConDataCons tc
        , not (null constructors)
        -> do
          let here = mkTyConApp tc args
          nodes <- State.get
          if any (tcEqType here . snd) active || isJust (lookupCodecNode here nodes)
            then return (ClosedRef here)
            else do
              forM_ (find (\(head_tc, old) -> head_tc == tc && not (proper_subterm here old || smaller_index tc args old)) active) $
                \(_, old) -> Trans.lift (Left (NonDecreasingClosedShape old here))
              let possible = filter (not . dataConCannotMatch args) constructors
                  visible = all (isJust . closedImportedGRE rdr_env) possible
                  inaccessible = InaccessibleClosedData tc (not (equalLength possible constructors))
                  next = (tc, here) : active
                  plan con
                    | Just fields <- closedWitnessedFields here args con = Right (Right fields)
                    | otherwise = Left <$> closedConstructorPlan Nothing here args con
              when (null possible) $ Trans.lift (Left UnsupportedClosedShape)
              plans <- Trans.lift $ case traverse plan possible of
                Left UnrecoverableClosedIndex | not visible -> Left inaccessible
                checked -> checked
              unless visible $ Trans.lift (Left inaccessible)
              alternatives <- forM (zip possible plans) $ \(con, fields) -> do
                codecs <- case fields of
                  Left (static, ground) -> ClosedStaticFields static <$> traverse (shape next) ground
                  Right (index, witness, payload, ground) ->
                    ClosedWitnessedFields <$> open_shape True next index witness
                                         <*> open_shape False next index payload
                                         <*> traverse (shape next) ground
                return (ClosedConstructor (Just (closedConstructorTag con)) con codecs)
              State.modify' ((here, ClosedData tc args alternatives) :)
              return (ClosedRef here)
      Just (tc, _)
        | isVanillaAlgTyCon tc, isLiftedAlgTyCon tc, isAbstractTyCon tc
        -> Trans.lift (Left (InaccessibleClosedData tc False))
      _ -> Trans.lift (Left UnsupportedClosedShape)

    open_shape witness active index field = do
      (tc, args, alternatives) <- Trans.lift $
        maybe (Left (if witness then UnsupportedClosedWitness else UnsupportedClosedPayload))
              Right (closedOpenAlternatives index field)
      when (witness && or [any isJust fields | (_, _, fields) <- alternatives]) $
        Trans.lift (Left UnsupportedClosedWitness)
      let constructors = [con | (con, _, _) <- alternatives]
      unless (all (isJust . closedImportedGRE rdr_env) constructors) $
        Trans.lift (Left (InaccessibleClosedData tc False))
      plans <- forM alternatives $ \(con, result_index, fields) -> do
        let index_con = closedIndexCon result_index
        unless (isJust (closedImportedGRE rdr_env index_con)) $
          Trans.lift (Left (InaccessibleClosedData (dataConTyCon index_con) False))
        codecs <- traverse (traverse (shape active)) fields
        return (ClosedOpenAlternative con result_index codecs)
      return (ClosedOpenShape index tc args plans)

    -- A repeated datatype may also be a strictly smaller type argument.
    -- Exact repetitions above become graph edges, without recursing.
    proper_subterm child parent
      | Just (_, args) <- splitTyConApp_maybe parent =
          any (\arg -> tcEqType child arg || proper_subterm child arg) args
      | otherwise = False

    -- This decreases the size of the whole type as well. Only a ground
    -- index can change. Ordinary type parameters must stay the same.
    smaller_index tc current old
      | Just (old_tc, previous) <- splitTyConApp_maybe old
      , tc == old_tc
      , let changed = [(tv, new, orig) | (tv, (new, orig)) <- zip (tyConTyVars tc) (zip current previous)
                                      , not (new `tcEqType` orig)]
      , [(tv, new, orig)] <- changed
      = not (tcIsLiftedTypeKind (tyVarKind tv)) && proper_subterm new orig
      | otherwise = False

    -- These constructor alternatives have no evidence or representation fields.
    boxed tc kinds alternatives
      | all (constructor_ok tc kinds) alternatives
      = Right (ClosedData tc kinds
          [ClosedConstructor tag con (ClosedStaticFields (ClosedConstructorPlan kinds []) fields)
          | (tag, con, fields) <- alternatives])
      | otherwise
      = Left UnsupportedClosedShape

    constructor_ok tc kinds (_, con, fields)
      | dataConTyCon con == tc, isVanillaDataCon con
      , null (dataConRepArgBinders con), null (dataConTheta con)
      , equalLength (dataConUnivTyVars con) kinds
      , dataConRepArity con == length fields
      , let args = dataConInstArgTys con kinds
      = equalLength args fields &&
        and (zipWith (\arg field -> scaledThing arg `tcEqType` closedShapeKind field)
                     args fields)
      | otherwise = False

-- | Check the field representation before planning a runtime witness.
closedWitnessedFields :: Kind -> [Type] -> DataCon -> Maybe (TyVar, Type, Type, [Type])
closedWitnessedFields here args con = do
  (universals, [index], [], [], fields, result) <- pure (dataConFullSig con)
  guard (isTyVar index && closedGroundField (tyVarKind index))
  guard (closedOpenRepresentation con && equalLength universals args &&
         equalLength (universals ++ [index]) (dataConUserTyVars con) &&
         all (`elem` (universals ++ [index])) (dataConUserTyVars con))
  let instantiate = substTyWith universals args
      rep = map (instantiate . scaledThing) (dataConRepArgTys con)
      source = map (instantiate . scaledThing) fields
  witness : payload : ground <- pure source
  guard (instantiate result `tcEqType` here &&
         equalLength rep source && and (zipWith tcEqType rep source) &&
         all closedGroundField ground)
  return (index, witness, payload, ground)

-- | Check that worker values have the source fields' boxed representation.
closedOpenRepresentation :: DataCon -> Bool
closedOpenRepresentation con =
  null (dataConRepArgBinders con) && not (any isUnpacked (dataConImplBangs con))

-- | Validate the open datatype before generating its polymorphic encoder.
closedOpenAlternatives :: TyVar -> Type
                       -> Maybe (TyCon, [Type], [(DataCon, ClosedOpenIndex, [Maybe Type])])
closedOpenAlternatives index field = do
  (tc, args) <- splitTyConApp_maybe field
  guard (isVanillaAlgTyCon tc && isBoxedDataTyCon tc && isLiftedAlgTyCon tc &&
         null (tyConStupidTheta tc) && lengthIs args (tyConArity tc))
  let positions = [i | (i, arg) <- zip [0 :: Int ..] args, arg `tcEqType` mkTyVarTy index]
  [position] <- pure positions
  guard (all (\arg -> arg `tcEqType` mkTyVarTy index || closedGroundType arg) args)
  guard (not (null (tyConDataCons tc)))
  alternatives <- traverse (check position args) (tyConDataCons tc)
  return (tc, args, alternatives)
  where
    check position args con = do
      (universals, locals, [equation], [], fields, result) <- pure (dataConFullSig con)
      let (refined, rhs) = eqSpecPair equation
          source_vars = filter (/= refined) universals ++ locals
      guard (closedOpenRepresentation con && equalLength universals args &&
             listToMaybe (drop position universals) == Just refined &&
             equalLength source_vars (dataConUserTyVars con) &&
             all (`elem` source_vars) (dataConUserTyVars con))
      let instantiate = substTyWith universals args
          here_index = instantiate (mkTyVarTy refined)
          constructor_index = instantiate rhs
          result_type = instantiate result
          field_types = map (instantiate . scaledThing) fields
          rep = map (instantiate . scaledThing) (dataConRepArgTys con)
          expected_evidence = map instantiate (eqSpecPreds [equation])
      (tc, _) <- splitTyConApp_maybe field
      (result_index, recursive_type) <- case locals of
        [] | Just (base, []) <- splitTyConApp_maybe constructor_index
           , Just base_con <- isPromotedDataCon_maybe base, tyConArity base == 0 ->
          return (ClosedFixedIndex base_con, Nothing)
        [local] | isTyVar local, tyVarKind local `tcEqType` tyVarKind index -> do
          (successor, [arg]) <- splitTyConApp_maybe constructor_index
          successor_con <- isPromotedDataCon_maybe successor
          guard (nominalClosedIndexTyCon successor &&
                 tyConArity successor == 1 && arg `tcEqType` mkTyVarTy local)
          return (ClosedSuccessor successor_con,
                  Just (substTyWith [index] [mkTyVarTy local] field))
        _ -> Nothing
      let typed_fields = [if maybe False (tcEqType arg) recursive_type then Nothing else Just arg
                         | arg <- field_types]
      guard (maybe True (const (lengthIs (filter isNothing typed_fields) 1)) recursive_type &&
             all (maybe True closedGroundField) typed_fields &&
             result_type `tcEqType` substTyWith [index] [constructor_index] (mkTyConApp tc args) &&
             here_index `tcEqType` mkTyVarTy index &&
             dataConRepArity con == length rep &&
             equalLength rep (expected_evidence ++ field_types) &&
             and (zipWith tcEqType rep (expected_evidence ++ field_types)))
      return (con, result_index, typed_fields)

-- | Instantiate an open codec at one nominal index.
closedOpenArgs :: ClosedOpenShape -> Type -> [Type]
closedOpenArgs (ClosedOpenShape index _ args _) val =
  substTys (zipTvSubst [index] [val]) args

-- | Get the type consumed by a polymorphic renderer at this index.
closedOpenType :: ClosedOpenShape -> Type -> Type
closedOpenType shape@(ClosedOpenShape _ tc _ _) val = mkTyConApp tc (closedOpenArgs shape val)

-- | A terminated tag distinguishes each constructor on the wire.
closedConstructorTag :: DataCon -> String
closedConstructorTag con = show (dataConTagZ con) ++ ";"

-- | The canonical fixed datatypes do not require client constructor imports.
closedBuiltinData :: TyCon -> Bool
closedBuiltinData tc = tc == maybeTyCon || tc == pairTyCon || closedEitherTyCon tc

closedEitherTyCon :: TyCon -> Bool
closedEitherTyCon tc =
  let name = tyConName tc
  in nameOccName name == eitherTyConOcc &&
     nameModule_maybe name == Just (mkGhcInternalModule (fsLit "GHC.Internal.Data.Either"))

closedScalar :: Type -> Maybe ClosedCallScalar
closedScalar ty
  | ty `tcEqType` naturalTy = Just ClosedNatural
  | ty `tcEqType` charTy = Just ClosedChar
  | ty `tcEqType` boolTy = Just ClosedBool
  | otherwise = Nothing

ordinaryClosedTyCon :: TyCon -> [Type] -> Bool
ordinaryClosedTyCon tc args =
  isVanillaAlgTyCon tc && isBoxedDataTyCon tc && isLiftedAlgTyCon tc &&
  lengthIs args (tyConArity tc) && null (tyConStupidTheta tc) &&
  all (closedGroundField . tyVarKind) (tyConTyVars tc) && all closedGroundType args

-- | Check that a kind or type has no unresolved information.
closedGroundType :: Type -> Bool
closedGroundType ty = noFreeVarsOfType ty && isTyFamFree ty && isTauTy ty

closedGroundField :: Type -> Bool
closedGroundField ty = tcIsLiftedTypeKind (typeKind ty) && closedGroundType ty

-- | A ground existential and the path to its nominal equality in a case arm.
data ClosedIndex = ClosedIndex Type Int [Int]

-- | Solve erased indices only through a constructor's nominal EqSpec.
-- See Note [Recovering constructor-local indices in closed calls].
closedConstructorIndices :: [TyVar] -> [(Type, Type)] -> Maybe [ClosedIndex]
closedConstructorIndices locals equations = do
  matches <- traverse (\(i, (ground, template)) -> match i [] ground template)
                      (zip [0 ..] equations)
  traverse (find_solution (concat matches)) locals
  where
    match evidence path ground template
      | Just tv <- getTyVar_maybe template
      , tv `elem` locals
      , closedGroundType ground
      , typeKind ground `tcEqType` tyVarKind tv
      = Just [(tv, ClosedIndex ground evidence (reverse path))]
      | ground `tcEqType` template = Just []
      | Just (head_ground, args_ground) <- splitTyConApp_maybe ground
      , Just (head_template, args_template) <- splitTyConApp_maybe template
      , head_ground == head_template, nominalClosedIndexTyCon head_ground
      , lengthIs args_ground (tyConArity head_ground)
      , equalLength args_ground args_template
      = concat <$> traverse (\(n, (g, t)) -> match evidence (n : path) g t)
                            (zip [0 ..] (zip args_ground args_template))
      | otherwise = Nothing

    find_solution matches tv = case [solution | (v, solution) <- matches, tv == v] of
      first@(ClosedIndex ground _ _) : rest
        | all (\(ClosedIndex other _ _) -> ground `tcEqType` other) rest -> Just first
      _ -> Nothing

-- | Check the constructor class used for both matching and projection.
nominalClosedIndexTyCon :: TyCon -> Bool
nominalClosedIndexTyCon tc =
  (isPromotedDataCon tc || isVanillaAlgTyCon tc) && isInjectiveTyCon tc Nominal

-- | Instantiate the source fields, erased arguments and worker evidence.
-- A checked promoted operand can supply its source arguments directly.
-- See Note [Recovering constructor-local indices in closed calls].
closedConstructorPlan :: Maybe [Type] -> Kind -> [Type] -> DataCon
                      -> Either ClosedShapeError (ClosedConstructorPlan, [Type])
closedConstructorPlan checked_args here args con
  | (universals, existentials, specs, [], _, _) <- dataConFullSig con
  , null (dataConRepArgBinders con)
  , equalLength universals args
  , all isTyVar existentials, all (closedGroundType . tyVarKind) existentials
  , let bangs = dataConImplBangs con
  , not (any isUnpacked bangs)
  , not (null specs) || not (null existentials) || isVanillaDataCon con
  , not (null specs) || not (null existentials) || isNothing (dataConWrapId_maybe con) ||
      any isBanged bangs || dataConUserTyVars con /= universals
  , let user_vars = dataConUserTyVars con
        refined = map (fst . eqSpecPair) specs
        allowed = filter (`notElem` refined) universals ++ existentials
  , equalLength user_vars allowed, all (`elem` allowed) user_vars
  = do
      let instantiate_universals = substTyWith universals args
          equations = [(instantiate_universals (mkTyVarTy tv), instantiate_universals rhs)
                      | spec <- specs, let (tv, rhs) = eqSpecPair spec]
          recovery_error | null existentials = UnsupportedClosedShape
                         | otherwise = UnrecoverableClosedIndex
      (local_args, indices) <- case checked_args of
        Just provided
          | equalLength provided user_vars, all closedGroundType provided -> do
            locals <- maybe (Left UnsupportedClosedShape) Right $
              traverse (`lookup` zip user_vars provided) existentials
            return (locals, [])
          | otherwise -> Left UnsupportedClosedShape
        Nothing -> do
          inferred <- maybe (Left recovery_error) Right
                        (closedConstructorIndices existentials equations)
          return ([ty | ClosedIndex ty _ _ <- inferred], inferred)
      let variables = universals ++ existentials
          arguments = args ++ local_args
          instantiate = substTyWith variables arguments
          fields = map (instantiate . scaledThing) (dataConOrigArgTys con)
          rep = map (instantiate . scaledThing) (dataConRepArgTys con)
          (evidence, rep_fields) = splitAt (length specs) rep
          expected_evidence = map instantiate (eqSpecPreds specs)
      unless (instantiate (dataConOrigResTy con) `tcEqType` here &&
              all (\(lhs, rhs) -> instantiate lhs `tcEqType` instantiate rhs) equations &&
              all closedGroundField fields && dataConRepArity con == length rep &&
              equalLength evidence expected_evidence && and (zipWith tcEqType evidence expected_evidence) &&
              equalLength fields rep_fields && and (zipWith tcEqType fields rep_fields)) $
        Left UnsupportedClosedShape
      source_args <- maybe (Left UnsupportedClosedShape) Right
                       (traverse (`lookup` zip variables arguments) user_vars)
      unless (and (zipWith (\tv arg -> instantiate (tyVarKind tv) `tcEqType` typeKind arg)
                          user_vars source_args) &&
              maybe True (and . zipWith tcEqType source_args) checked_args) $
        Left UnsupportedClosedShape
      return (ClosedConstructorPlan source_args indices, fields)
  | otherwise = Left UnsupportedClosedShape

-- | Project the actual case evidence and orient it from the local index to ground.
closedIndexCoercion :: [Var] -> TyVar -> ClosedIndex -> Maybe Coercion
closedIndexCoercion evidence tv (ClosedIndex ground evidence_pos path) = do
  ev <- listToMaybe (drop evidence_pos evidence)
  guard (isCoVar ev)
  let co = mkCoVarCo ev
  guard (coercionRole co == Nominal)
  projected <- foldM project co path
  guard (coercionLKind projected `tcEqType` ground &&
         coercionRKind projected `tcEqType` mkTyVarTy tv)
  return (mkSymCo projected)
  where
    project co i = do
      (left, ls) <- splitTyConApp_maybe (coercionLKind co)
      (right, rs) <- splitTyConApp_maybe (coercionRKind co)
      guard (left == right && nominalClosedIndexTyCon left &&
             lengthIs ls (tyConArity left) && equalLength ls rs &&
             i < length ls)
      return (mkSelCo (SelTyCon i Nominal) co)

-- | Find an ordinary import for this exact constructor.
closedImportedGRE :: GlobalRdrEnv -> DataCon -> Maybe GlobalRdrElt
closedImportedGRE rdr_env con = do
  gre <- lookupGRE_Name rdr_env (dataConName con)
  guard (any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre))
  return gre

-- | Execute a checked closed call through the target interpreter.
-- See Note [Closed literal type evaluation].
tcRunClosedCall :: Id -> Core.CoreExpr -> Type -> TcM Type
tcRunClosedCall fun argument result_kind = do
  apply <- tcPrepareClosedApplication fun [exprType argument] result_kind
  reify <- tcPrepareClosedResult (idName fun) result_kind
  apply [argument] >>= reify

-- | Typecheck the ordinary runtime application without observing its value.
tcPrepareClosedApplication :: Id -> [Type] -> Kind -> TcM ([Core.CoreExpr] -> TcM Core.CoreExpr)
tcPrepareClosedApplication fun domains result_kind = do
  let name = idName fun
      bad = failWithTc . TcRnClosedTypeEval name
      expected = foldr mkVisFunTyManyU result_kind domains
  function <- tcPrepareClosedFunctionValue fun expected
  loc <- getSrcSpanM
  return $ \arguments -> setSrcSpan loc $ do
    unless (length arguments == length domains &&
            and (zipWith tcEqType (map exprType arguments) domains)) $
      bad "The runtime operands do not have the expected types."
    return (mkCoreApps function arguments)

-- | Prepare an imported function at its checked ordinary runtime type.
tcPrepareClosedFunctionValue :: Id -> Type -> TcM Core.CoreExpr
tcPrepareClosedFunctionValue fun expected = do
  let name = idName fun
  tcPreflightClosedCall name
  function <- if idType fun `tcEqType` expected
                then pure (Core.Var fun)
                else specializeClosedFunction name expected
  unless (exprType function `tcEqType` expected) $
    failWithTc (TcRnClosedTypeEval name
      "The closed call does not have the expected ordinary function type.")
  -- Even a fused call can be simplified using an imported unfolding.
  env <- getGblEnv
  updTcRef (tcg_evaluated_term_uses env) (`extendNameSet` name)
  return function

-- | Render and interpret a complete runtime expression at a type boundary.
tcPrepareClosedResult :: Name -> Kind -> TcM (Core.CoreExpr -> TcM Type)
tcPrepareClosedResult name result_kind = do
  reify <- tcPrepareClosedResultWithOptionalName name result_kind Nothing
  pure (fmap expectJust . reify)

-- | Observe an executable entry when the interpreter can load its symbol.
tcPrepareClosedObservation :: Name -> Kind -> Name -> TcM (Core.CoreExpr -> TcM (Maybe Type))
tcPrepareClosedObservation name result_kind entry =
  tcPrepareClosedResultWithOptionalName name result_kind (Just entry)

tcPrepareClosedResultWithOptionalName
  :: Name -> Kind -> Maybe Name -> TcM (Core.CoreExpr -> TcM (Maybe Type))
tcPrepareClosedResultWithOptionalName name result_kind optional = do
  hsc_env <- getTopEnv
  checked_shape <- tcClosedCallValueShape name result_kind
  shape <- case checked_shape of
    Just shape -> return shape
    Nothing -> bad ("The function returns " ++ showPpr (hsc_dflags hsc_env) result_kind ++
                    ". Closed calls in types currently support results of type Natural, Char, Bool, " ++
                    "lists, boxed pairs, Maybe, and Either of supported results, " ++
                    "or ordinary boxed datatypes with supported fields and imported constructors.")
  interp <- checkClosedTypeEvalStage hsc_env name
  render <- closedCallRenderer name shape
  ret_id <- lookupOrig gHC_INTERNAL_BASE returnIOIdOcc >>= tcLookupId
  io_tc <- lookupOrig gHC_TYPES ioTyConOcc >>= tcLookupTyCon
  let ret = mkCoreApp (Core.Var ret_id) (Core.Type stringTy)
      io_string = mkTyConApp io_tc [stringTy]
  unless (exprType ret `tcEqType` mkVisFunTyManyU stringTy io_string) $
    bad "Unexpected type for the interpreter response."
  loc <- getSrcSpanM
  return $ \call -> setSrcSpan loc $ do
    failIfErrsM
    unless (exprType call `tcEqType` result_kind) $
      bad "The runtime expression does not have the checked result type."
    let response = render call
    unless (exprType response `tcEqType` stringTy) $
      bad "Unexpected type for the interpreter response."
    compiled <- tryMostM $ liftIO $ case optional of
      Nothing -> do
        (value, links, packages) <- hscCompileCoreExpr hsc_env loc (mkCoreApp ret response)
        pure (Just value, links, packages)
      Just entry -> hscCompileCoreExprWithOptionalName hsc_env loc entry (mkCoreApp ret response)
    (value, links, packages) <- either (bad . displayException) pure compiled
    recordThNeededRuntimeDeps links packages
    forM value $ \value -> do
      outcome <- tryAllM $ liftIO $ evalString interp value
      rendered <- case outcome of
        Left exn | Just async <- (fromException exn :: Maybe SomeAsyncException) ->
          liftIO $ throwIO async
        Left exn -> bad (displayException exn)
        Right rendered -> pure rendered
      case decodeClosedCallResult shape rendered of
        Just ty | typeKind ty `tcEqType` result_kind -> return ty
        _ -> bad "The value returned by the interpreter does not match the checked result type."
  where
    bad :: String -> TcM a
    bad = failWithTc . TcRnClosedTypeEval name

-- | Build the renderer before any nested operand executes.
closedCallRenderer :: Name -> ClosedCodec -> TcM (Core.CoreExpr -> Core.CoreExpr)
closedCallRenderer name (ClosedCodec shape nodes) = do
  show_class <- lookupOrig (mkGhcInternalModule (fsLit "GHC.Internal.Show"))
                           (mkTcOcc "Show") >>= tcLookupClass
  inst_env <- tcGetInstEnvs
  let empty_string = mkListExpr charTy []
      show_s = mkVisFunTyManyU stringTy stringTy
      builtinShow scalar is_list = do
        let dictionary_kind = closedScalarKind scalar
            method = if is_list then "showList" else "show"
            domain = if is_list then mkListTy dictionary_kind else dictionary_kind
            range = if is_list then show_s else stringTy
        case (find ((== method) . getOccString) (classMethods show_class),
              lookupUniqueInstEnv inst_env show_class [dictionary_kind]) of
          (Just show_id, Right (inst, []))
            | let dfun = instanceDFunId inst
            , nameModule_maybe (idName dfun) == nameModule_maybe (className show_class)
            , idType dfun `tcEqType` mkClassPred show_class [dictionary_kind] -> do
                let render = mkCoreApps (Core.Var show_id)
                               [Core.Type dictionary_kind, Core.Var dfun]
                unless (exprType render `tcEqType` mkVisFunTyManyU domain range) $
                  failWithTc (TcRnClosedTypeEval name "Unexpected type for the interpreter response.")
                return render
          _ -> failWithTc (TcRnClosedTypeEval name
                 "Cannot resolve the built-in Show dictionary for the result type.")
      renderScalar scalar = mkCoreApp <$> builtinShow scalar False
      renderList scalar = do
        render <- builtinShow scalar True
        return $ \value -> mkCoreApps render [value, empty_string]
      renderToken quote render value suffix = mkCoreApps quote [render value, suffix]
      cast_field existentials cos field var
        | varType var `tcEqType` closedShapeKind field = Just (Core.Var var)
        | otherwise = do
            let lifted = liftCoSubstWith Nominal existentials cos (varType var)
            guard (coercionLKind lifted `tcEqType` varType var &&
                   coercionRKind lifted `tcEqType` closedShapeKind field)
            return (mkCast (Core.Var var) (mkSubCo lifted))
      tag_constructor con suffix =
        foldr (\c rest -> mkConsExpr charTy (mkCharExpr c) rest) suffix (closedConstructorTag con)
      unexpected_rep :: TcM a
      unexpected_rep = failWithTc (TcRnClosedTypeEval name "Unexpected indexed constructor representation.")
      renderOpen refs quote shape@(ClosedOpenShape template _ _ alternatives) = do
        unique <- newUnique
        let index = mkTyVar (mkSysTvName unique (fsLit "closed_index")) (tyVarKind template)
            index_ty = mkTyVarTy index
            kind = closedOpenType shape index_ty
        loop <- mkSysLocalM (fsLit "closed_indexed") ManyTy
                  (mkLamType index (mkVisFunTyManyU kind show_s))
        current <- mkSysLocalM (fsLit "closed_indexed_value") ManyTy kind
        suffix <- mkSysLocalM (fsLit "closed_indexed_suffix") ManyTy stringTy
        binder <- mkSysLocalM (fsLit "closed_indexed_arm") ManyTy kind
        alts <- forM alternatives $ \(ClosedOpenAlternative con result_index fields) -> do
          uniques <- getUniquesM
          let (locals, binders) = dataConRepInstPat uniques ManyTy con (closedOpenArgs shape index_ty)
          case binders of
            evidence : values | isCoVar evidence, equalLength fields values -> do
              (target, previous) <- case (result_index, locals) of
                (ClosedFixedIndex base, []) -> return (mkTyConTy (promoteDataCon base), Nothing)
                (ClosedSuccessor successor, [local])
                  | isTyVar local, tyVarKind local `tcEqType` tyVarKind index ->
                  let previous = mkTyVarTy local
                  in return (mkTyConApp (promoteDataCon successor) [previous], Just previous)
                _ -> unexpected_rep
              let proof = mkCoVarCo evidence
              unless (coercionRole proof == Nominal &&
                      coercionLKind proof `tcEqType` index_ty &&
                      coercionRKind proof `tcEqType` target) unexpected_rep
              renderers <- forM (zip fields values) $ \(field, var) -> case field of
                Just field_shape | varType var `tcEqType` closedShapeKind field_shape -> do
                  render <- renderFields refs quote field_shape
                  return (render (Core.Var var))
                Nothing | Just child <- previous, varType var `tcEqType` closedOpenType shape child ->
                  return (\rest -> mkCoreApps (Core.Var loop) [Core.Type child, Core.Var var, rest])
                _ -> unexpected_rep
              let encoded = foldr ($) (Core.Var suffix) renderers
              return (Core.Alt (Core.DataAlt con) (locals ++ binders) (tag_constructor con encoded))
            _ -> unexpected_rep
        let body = Core.Case (Core.Var current) binder stringTy alts
            binding = Core.Rec [(loop, Core.Lam index (Core.Lam current (Core.Lam suffix body)))]
        return $ \idx value rest ->
          Core.Let binding (mkCoreApps (Core.Var loop) [Core.Type idx, value, rest])
      renderFields refs quote current = case current of
        ClosedRef kind
          | Just renderer <- lookupCodecNode kind refs ->
              return $ \value suffix -> mkCoreApps (Core.Var renderer) [value, suffix]
          | otherwise -> failWithTc (TcRnClosedTypeEval name "Missing recursive constructor renderer.")
        ClosedScalar scalar -> renderToken quote <$> renderScalar scalar
        ClosedList (ClosedScalar scalar) -> renderToken quote <$> renderList scalar
        ClosedList element -> do
          let kind = closedShapeKind current
              field_kind = closedShapeKind element
          loop <- mkSysLocalM (fsLit "closed_list") ManyTy (mkVisFunTyManyU kind show_s)
          spine <- mkSysLocalM (fsLit "closed_spine") ManyTy kind
          suffix <- mkSysLocalM (fsLit "closed_suffix") ManyTy stringTy
          binder <- mkSysLocalM (fsLit "closed_cell") ManyTy kind
          element_id <- mkSysLocalM (fsLit "closed_element") ManyTy field_kind
          rest <- mkSysLocalM (fsLit "closed_tail") ManyTy kind
          render <- renderFields refs quote element
          let continue = mkCoreApps (Core.Var loop) [Core.Var rest, Core.Var suffix]
              encode = Core.Case (Core.Var spine) binder stringTy
                [ Core.Alt (Core.DataAlt nilDataCon) []
                    (tag '0' (Core.Var suffix))
                , Core.Alt (Core.DataAlt consDataCon) [element_id, rest]
                    (tag '1' (render (Core.Var element_id) continue))
                ]
              tag c = mkConsExpr charTy (mkCharExpr c)
              binding = Core.Rec [(loop, Core.Lam spine (Core.Lam suffix encode))]
          return $ \value tail_string ->
            Core.Let binding (mkCoreApps (Core.Var loop) [value, tail_string])
        ClosedData _ kinds alternatives -> do
          binder <- mkSysLocalM (fsLit "closed_data") ManyTy (closedShapeKind current)
          render_alts <- forM alternatives $ \(ClosedConstructor tag con fields) -> do
            uniques <- getUniquesM
            let (existentials, binders) = dataConRepInstPat uniques ManyTy con kinds
            render_body <- case fields of
              ClosedStaticFields (ClosedConstructorPlan _ indices) codecs -> do
                let (evidence, value_binders) = splitAt (length (dataConTheta con)) binders
                    checked = do
                      guard (equalLength indices existentials && all isTyVar existentials &&
                             all isCoVar evidence && equalLength codecs value_binders)
                      cos <- zipWithM (closedIndexCoercion evidence) existentials indices
                      zipWithM (cast_field existentials cos) codecs value_binders
                values <- maybe (failWithTc (TcRnClosedTypeEval name "Unexpected constructor representation."))
                                return checked
                renderers <- mapM (renderFields refs quote) codecs
                return (foldr (.) id (zipWith ($) renderers values))
              ClosedWitnessedFields witness@(ClosedOpenShape template _ _ _) payload ground
                | [index] <- existentials, isTyVar index
                , tyVarKind index `tcEqType` tyVarKind template
                , let idx = mkTyVarTy index
                , witness_value : payload_value : values <- binders
                , varType witness_value `tcEqType` closedOpenType witness idx
                , varType payload_value `tcEqType` closedOpenType payload idx
                , equalLength ground values
                , and (zipWith (tcEqType . closedShapeKind) ground (map varType values)) -> do
                    render_witness <- renderOpen refs quote witness
                    render_payload <- renderOpen refs quote payload
                    renderers <- mapM (renderFields refs quote) ground
                    return (render_witness idx (Core.Var witness_value) .
                            render_payload idx (Core.Var payload_value) .
                            foldr (.) id (zipWith ($) renderers (map Core.Var values)))
                | otherwise -> unexpected_rep
            let alt suffix =
                  let body = render_body suffix
                      emit c = mkConsExpr charTy (mkCharExpr c)
                      tagged = maybe body (foldr emit body) tag
                  in Core.Alt (Core.DataAlt con) (existentials ++ binders) tagged
            return alt
          let select value suffix = Core.Case value binder stringTy (map ($ suffix) render_alts)
          case render_alts of
            [_] -> return select
            _ -> do
              rest <- mkSysLocalM (fsLit "closed_suffix") ManyTy stringTy
              return $ \value suffix -> Core.Let (Core.NonRec rest suffix)
                                           (select value (Core.Var rest))
  case shape of
    ClosedScalar scalar -> renderScalar scalar
    ClosedList (ClosedScalar scalar) -> renderList scalar
    _ -> do
      quote <- builtinShow ClosedChar True
      refs <- forM nodes $ \(kind, _) -> do
        f <- mkSysLocalM (fsLit "closed_constructor") ManyTy (mkVisFunTyManyU kind show_s)
        return (kind, f)
      bindings <- forM (zip nodes refs) $ \((kind, node), (_, f)) -> do
        value <- mkSysLocalM (fsLit "closed_value") ManyTy kind
        suffix <- mkSysLocalM (fsLit "closed_suffix") ManyTy stringTy
        render <- renderFields refs quote node
        return (f, Core.Lam value (Core.Lam suffix (render (Core.Var value) (Core.Var suffix))))
      render <- renderFields refs quote shape
      let with_bindings = if null bindings then id else Core.Let (Core.Rec bindings)
      return $ \value -> with_bindings (render value empty_string)

-- | Use ordinary term checking to supply type and dictionary arguments.
specializeClosedFunction :: Name -> Type -> TcM Core.CoreExpr
specializeClosedFunction name expected = do
  loc <- getSrcSpanM
  let occurrence = L (noAnnSrcSpan loc) (mkHsVar (L (noAnnSrcSpan loc) name))
  typed <- unsetGOptM Opt_DeferTypeErrors $
           unsetGOptM Opt_DeferTypedHoles $
           unsetGOptM Opt_DeferOutOfScopeVariables $
           tcTopSpliceExprWith simplifyTopNoDefaulting Untyped $
           tcCheckMonoExpr occurrence expected
  zonked <- zonkTopLExpr typed
  core <- desugarStagedExpr zonked
  env <- getGblEnv
  let free = exprSomeFreeVarsList (const True) core
      unavailable v = isLocalVar v || nameIsLocalOrFrom (tcg_semantic_mod env) (getName v)
  when (any unavailable free) $
    failWithTc (TcRnClosedTypeEval name
      "The specialization uses an unfinished local binding or dictionary.")
  updTcRef (tcg_evaluated_term_uses env)
           (`extendNameSetList` [getName v | v <- free, isGlobalId v])
  pure core

-- | Desugar code that will run during compilation. Emit any diagnostics
-- before evaluation. See Note [Errors in desugaring a splice].
desugarStagedExpr :: LHsExpr GhcTc -> TcM Core.CoreExpr
desugarStagedExpr expr = do
  (messages, result) <- initDsTc (dsLExpr expr)
  logger <- getLogger
  flags <- getDynFlags
  liftIO $ printMessages logger (initDsMessageOpts flags) (initDiagOpts flags) messages
  when (errorsOrFatalWarningsFound messages) failM
  maybe failM pure result

-- | Decode the canonical response from the interpreter.
decodeClosedCallResult :: ClosedCodec -> String -> Maybe Type
decodeClosedCallResult (ClosedCodec shape nodes) rendered = case shape of
  ClosedScalar scalar -> decodeScalar scalar False rendered
  ClosedList (ClosedScalar scalar) -> decodeScalar scalar True rendered
  _ -> do
    (value, rest) <- decodeFields shape rendered
    guard (null rest)
    return value
  where
    decodeFields current input = case current of
      ClosedRef kind -> lookupCodecNode kind nodes >>= \node -> decodeFields node input
      ClosedScalar scalar -> decodeToken scalar False input
      ClosedList (ClosedScalar scalar) -> decodeToken scalar True input
      ClosedList element -> decodeList element [] input
      ClosedData _ kinds alternatives -> do
        (ClosedConstructor _ con fields, encoded) <- selectConstructor alternatives input
        let kind = closedShapeKind current
        case fields of
          ClosedStaticFields (ClosedConstructorPlan source_args _) codecs -> do
            (values, rest) <- decodeArguments codecs encoded
            let value = mkTyConApp (promoteDataCon con) (source_args ++ values)
            guard (typeKind value `tcEqType` kind)
            return (value, rest)
          ClosedWitnessedFields witness payload ground -> do
            (index, witness_value, remaining) <- decodeOpen witness Nothing encoded
            (_, payload_value, remaining') <- decodeOpen payload (Just index) remaining
            (values, rest) <- decodeArguments ground remaining'
            value <- packConstructor kind kinds con [index] (witness_value : payload_value : values)
            return (value, rest)

    -- The payload follows the checked witness index. Its wire tags may
    -- confirm a branch but cannot supply a different erased argument.
    decodeOpen shape@(ClosedOpenShape _ _ _ alternatives) expected input = do
      (ClosedOpenAlternative con result_index fields, payload) <- listToMaybe
        [(alt, rest) | alt@(ClosedOpenAlternative ctor _ _) <- alternatives
                     , Just rest <- [stripPrefix (closedConstructorTag ctor) input]]
      expected_child <- case (expected, result_index) of
        (Nothing, _) -> return Nothing
        (Just index, ClosedFixedIndex fixed) -> do
          guard (index `tcEqType` mkTyConTy (promoteDataCon fixed))
          return Nothing
        (Just index, ClosedSuccessor successor) -> do
          (tc, [child]) <- splitTyConApp_maybe index
          guard (tc == promoteDataCon successor)
          return (Just child)
      (children, values, rest) <- decodeOpenFields shape expected_child fields payload
      index <- case (result_index, children) of
        (ClosedFixedIndex fixed, []) -> return (mkTyConTy (promoteDataCon fixed))
        (ClosedSuccessor successor, [child]) -> return (mkTyConApp (promoteDataCon successor) [child])
        _ -> Nothing
      let kind = closedOpenType shape index
          args = closedOpenArgs shape index
      value <- packConstructor kind args con children values
      return (index, value, rest)

    decodeOpenFields _ _ [] input = Just ([], [], input)
    decodeOpenFields shape expected (field : fields) input = do
      (indices, value, after_field) <- case field of
        Just field_shape -> do
          (value, rest) <- decodeFields field_shape input
          return ([], value, rest)
        Nothing -> do
          (index, value, rest) <- decodeOpen shape expected input
          return ([index], value, rest)
      (other_indices, values, rest) <- decodeOpenFields shape expected fields after_field
      return (indices ++ other_indices, value : values, rest)

    packConstructor kind kinds con indices values = do
      let variables = dataConUnivTyVars con ++ dataConExTyCoVars con
          arguments = kinds ++ indices
      guard (equalLength variables arguments)
      source_args <- traverse (`lookup` zip variables arguments) (dataConUserTyVars con)
      (ClosedConstructorPlan _ _, fields) <-
        either (const Nothing) Just (closedConstructorPlan (Just source_args) kind kinds con)
      guard (closedConstructorArgumentsMatch fields values)
      let promoted = mkTyConApp (promoteDataCon con) (source_args ++ values)
      guard (typeKind promoted `tcEqType` kind)
      return promoted

    decodeList element acc ('0' : rest) =
      Just (mkPromotedListTy (closedShapeKind element) (reverse acc), rest)
    decodeList element acc ('1' : input) = do
      (value, rest) <- decodeFields element input
      decodeList element (value : acc) rest
    decodeList _ _ _ = Nothing

    selectConstructor [constructor@(ClosedConstructor Nothing _ _)] input =
      Just (constructor, input)
    selectConstructor alternatives input = listToMaybe
      [(constructor, rest) | constructor@(ClosedConstructor (Just tag) _ _) <- alternatives
                           , Just rest <- [stripPrefix tag input]]

    decodeArguments [] input = Just ([], input)
    decodeArguments (field : fields) input = do
      (value, rest) <- decodeFields field input
      (values, residual) <- decodeArguments fields rest
      return (value : values, residual)

    decodeToken scalar is_list input = do
      [(token, _)] <- pure (reads @String input)
      rest <- stripPrefix (show token) input
      value <- decodeScalar scalar is_list token
      return (value, rest)

    decodeScalar scalar is_list = case scalar of
      ClosedNatural -> decodeLiteral scalar is_list (mkNumLitTy . toInteger @Natural)
      ClosedChar -> decodeLiteral scalar is_list mkCharLitTy
      ClosedBool -> decodeLiteral scalar is_list promoteBool

    promoteBool b = mkTyConTy (if b then promotedTrueDataCon else promotedFalseDataCon)

    decodeLiteral :: (Read a, Show a) => ClosedCallScalar -> Bool -> (a -> Type)
                  -> String -> Maybe Type
    decodeLiteral scalar is_list promote
      | is_list = decodeCanonical (mkPromotedListTy (closedScalarKind scalar) . map promote)
      | otherwise = decodeCanonical promote

    decodeCanonical :: (Read a, Show a) => (a -> Type) -> String -> Maybe Type
    decodeCanonical promote input = do
      value <- readMaybe input
      guard (show value == input)
      return (promote value)

{- Note [Closed results with an existential witness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor can bind one erased index. Its first field witnesses the
index and its second field carries a payload at that index. Trailing
fields use the finite graph of ground codecs. A trailing field can
refer to the same datatype, whose next constructor selects its own index.

Preflight checks the independent recursive schema of each indexed field.
A base constructor fixes its index to a nullary promoted constructor.
A recursive constructor refines its index to an injective promoted
application of its case-bound predecessor. It has one recursive field
at that predecessor. Other fields use the ordinary ground codec.

The enclosing Core case binds the existential with
dataConRepInstPat. Check its kind and both fields using that binder.
The first two fields have separate polymorphic Core workers. Each
recursive case checks its actual nominal equality against the planned
index pattern. The case evidence supplies no data to the wire. The host
decoder recovers each index from its witness, checks the payload at that
index, and builds the checked promoted constructor. The preflight plan
determines the constructor tags and visibility checks.
-}

{- Note [Recovering constructor-local indices in closed calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Plan against the checked ground result type. An alternative such as
VCons binds an erased predecessor n and nominal evidence S (S Z) ~ S n.
The constructor's EqSpec identifies a structural path to n. Match only
saturated nominally injective constructors on this path. Never invert
a type family or recover an existential that has no such path.

At rendering time, dataConRepInstPat supplies actual local binders.
Project the nominal equality down the same path and check its endpoints.
Reverse the result to cast a field from its local index to the planned
codec type. Lift through the field at Nominal before downgrading that
whole field coercion to Representational. The wire has only the tag and
source fields. Promoted applications and runtime wrappers use the
statically solved source arguments.

A checked promoted operand already carries the source constructor's
erased arguments. Use those arguments directly. Check the resulting
index equations and field kinds before applying the ordinary wrapper.
This operand path does not learn indices from an interpreter response.

The codec admits a recursive indexed type with the same ordinary
arguments when one index strictly decreases to its structural subterm.
Both that rule and the existing whole-type rule decrease structural
size for a fresh repeated head. Exact repeats still form graph edges.
-}

{- Note [Constructor visibility in closed calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Use ordinary DataKinds constructor identities in both directions. An
interface can contain constructors hidden from the client. To inspect an
unknown result, require and record non-SOURCE imports for every
constructor that the renderer may inspect. Record the parent datatype
declaration as an interface usage. For a known operand, inspect its promoted constructors
directly. Require and record just the selected constructors. Check again
after expanding a type synonym. Use the constructor wrapper when building a
value with a strict boxed field.

Plan the result as a finite graph of instantiated datatypes. An exact
recursive occurrence uses the renderer for that node. A different
instantiation of the same datatype on the path must be a proper subterm
of the earlier type. This admits Tree (Tree Natural) while terminating
the representation check for a datatype such as Grow a with a Grow [a]
field. Mutually recursive Core bindings implement the graph. Constructor
tags use delimited decimal ordinals. The checked datatype supplies their
identities. Maybe, Either and pairs keep their earlier wire formats.
-}

{- Note [Closed literal type evaluation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The caller checks the source call and its literal argument before execution.
Compile an ordinary Core application at the checked function type. Use the
built-in Show instance for the scalar type and returnIO to construct an
IO String for evalString. If the result is a list, use the showList method
of the element dictionary. Do not select a Show instance for the list.
A user-defined overlapping list instance could encode a different value.
The interpreter expects the target library's IO String. Resolve its IO,
returnIO and Show by their defining modules. A client can rebind the
names used for its own code. Those names do not determine this response.
Resolve the producer's own constraints in the client as usual.

The interpreter forces the string before it returns it. The compiler
checks the canonical form before it constructs a literal or promoted
constructor. A checked result can also be the operand of a nested call.
Rebuild its Core literals and constructors from the checked type.
The checked kind supplies the element type of an empty list.

For a boxed constructor, use one Core case on the producer application.
Render each field with its checked codec. Quote scalar and list fields as
separate string tokens with the built-in Char showList. Its suffix
concatenates the tokens. Maybe and Either emit a tag before the fields of
the selected constructor. A pair has one constructor and needs no tag.
The compiler checks the tag and fields against the planned datatype and
rebuilds the promoted constructor. Identify Either by its defining module
and occurrence. The client can rebind its in-scope known names.
The interpreter forces each field of the selected constructor when it
forces the response, even if a later caller ignores that field.
Use one recursive Core binding to walk the spine of a list of compound
elements. Each cons emits a tag, then the element, then the remaining
cells. Nil emits a distinct tag. These tags delimit an empty list inside
another value without first computing its length. The scalar-list fast
path retains its existing encoding, including the String representation.
In both directions, the checked list kind fixes the element kind.

Bind the string suffix once for a sum. Copying it into each alternative
would duplicate the generated cases for every later field.

For an overloaded producer, check the resolved occurrence against the
specialized ordinary arrow. Use the same type and dictionary elaboration as
a splice. Disable defaulting so that nested instance requirements cannot
introduce ambiguous type arguments. Unsolved constraints are errors even
when the caller defers term errors. A producer can ignore a dictionary.
Reject free local references after evidence desugaring. Ordinary instance
selection can choose a dictionary from the module currently compiling.
Its code is unavailable at this stage.

Record the imported names in this specialization before the interactive
simplifier runs. A class selector can reduce using a dictionary unfolding.
The resulting bytecode can refer directly to a method implementation and
have no reference to the original dictionary object.

Record the loaded dependencies through the same path as a splice. The
producer object can change even when its interface ABI remains the same.
See Note [Object File Dependencies] in GHC.HsToCore.Usage.
-}

-- | Check the outer call before entering a nested operand.
tcPreflightClosedCall :: Name -> TcM ()
tcPreflightClosedCall name = do
  hsc_env <- getTopEnv
  _ <- checkClosedTypeEvalStage hsc_env name
  return ()

-- | Check the execution context before loading code for a type.
checkClosedTypeEvalStage :: HscEnv -> Name -> TcM Interp
checkClosedTypeEvalStage hsc_env name = do
  tcCheckClosedCallContext name
  let reject :: String -> TcM a
      reject = failWithTc . TcRnClosedTypeEval name
  dflags <- getDynFlags
  iface <- loadInterfaceForName (text "closed call in a type") name
  when (isHsBootOrSig (mi_hsc_src iface)) $
    reject "The defining interface is a boot file or a module signature and provides no executable definition."
  when (safeInferOn dflags) $
    recordUnsafeInfer emptyMessages
  case hsc_interp hsc_env of
    Just interp -> return interp
    Nothing -> reject "Compile-time evaluation requires a GHC interpreter for the target."

-- | Test the execution route without preflighting, linking, or running code.
tcHasClosedCallExecutable :: Name -> TcM Bool
tcHasClosedCallExecutable name = do
  env <- getTopEnv
  dflags <- getDynFlags
  level <- getThLevel
  boot <- tcIsHsBootOrSig
  explicit <- xoptM LangExt.ExplicitLevelImports
  case (nameModule_maybe name, hsc_interp env, level) of
    (Just mod, Just interp, Comp)
      | not boot, not explicit, not (safeLanguageOn dflags) -> do
          loc <- getSrcSpanM
          liftIO $ executableDependenciesAvailable interp env loc mod
    _ -> pure False

-- | Check policy at the location where the call occurs.
tcCheckClosedCallContext :: Name -> TcM ()
tcCheckClosedCallContext name = do
  failIfErrsM
  dflags <- getDynFlags
  let reject = failWithTc . TcRnClosedTypeEval name
  unlessXOptM LangExt.ExperimentalDependentTypes $
    reject "Compile-time evaluation in types requires ExperimentalDependentTypes."
  when (safeLanguageOn dflags) $
    reject "Safe Haskell disallows executing imported functions in types."
  level <- getThLevel
  case level of
    Comp -> return ()
    _    -> reject "Closed calls in types are only supported outside quotations and splices."
  whenXOptM LangExt.ExplicitLevelImports $
    reject "Closed calls in types do not yet support ExplicitLevelImports."
  boot <- tcIsHsBootOrSig
  when boot $
    reject "Closed calls in types are not supported in boot files or module signatures."

{- Note [Running imported code in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A closed call can load and execute code from an ordinary Haskell module.
Apply the same Safe Haskell boundary as tcAnnotation in GHC.Tc.Gen.Annotation.
An inferred-safe client must also lose that status when it executes a call.
A checked source equation reduces in the ordinary solver and never enters
this path.

Start with a separately compiled definition and an ordinary compilation
level. In a quotation or an explicitly staged import, the level at which
a name occurs and the level at which we need its object code can differ.
Check the staging policy before loading code. Let the configured interpreter
and linker determine which target objects they can execute.
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

        ; ds_expr <- desugarStagedExpr expr'

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

-- 'msg' is forced to ensure exceptions don't escape,
-- see Note [Exceptions in TH]
report :: Bool -> [Char] -> TcM ()
report True msg  = seqList msg $ addErr        $ TcRnTHError $ ReportCustomQuasiError True  msg
report False msg = seqList msg $ addDiagnostic $ TcRnTHError $ ReportCustomQuasiError False msg

addTopDecls :: [TH.Dec] -> TcM ()
addTopDecls thds = do
      exts <- fmap extensionFlags getDynFlags
      l <- getSrcSpanM
      th_origin <- getThSpliceOrigin
      let either_hval = convertToHsDecls exts th_origin l thds
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
      bindName rdr_name
        | Just n <- rdrNameExactName_maybe rdr_name
        = do { th_topnames_var <- fmap tcg_th_topnames getGblEnv
             ; updTcRef th_topnames_var (\ns -> extendNameSet ns n) }
        | otherwise
        = addErr $ TcRnTHError $ THNameError $ NonExactName rdr_name

putDoc :: TH.DocLoc -> String -> TcM ()
putDoc doc_loc s = do
    th_doc_var <- tcg_th_docs <$> getGblEnv
    resolved_doc_loc <- resolve_loc doc_loc
    is_local <- checkLocalName resolved_doc_loc
    unless is_local $ failWithTc $ TcRnTHError $ AddDocToNonLocalDefn doc_loc
    let ds = mkGeneratedHsDocStringGhc s
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

getDoc :: TH.DocLoc -> TcM (Maybe String)
getDoc (TH.DeclDoc n) = lookupThName n >>= lookupDeclDoc
getDoc (TH.InstDoc t) = lookupThInstName t >>= lookupDeclDoc
getDoc (TH.ArgDoc n i) = lookupThName n >>= lookupArgDoc i
getDoc TH.ModuleDoc = do
    df <- getDynFlags
    docs <- getGblEnv >>= extractDocs df
    return (renderHsDocString . hsDocString <$> (docs_mod_hdr =<< docs))

getQ :: forall a. Typeable a => TcM (Maybe a)
getQ = do
    th_state_var <- fmap tcg_th_state getGblEnv
    th_state <- readTcRef th_state_var
    -- See #10596 for why we use a scoped type variable here.
    return (Map.lookup (typeRep (Proxy :: Proxy a)) th_state >>= fromDynamic)

location :: TcM TH.Loc
location = do { m <- getModule
              ; l <- getSrcSpanM
              ; r <- case l of
                        RealSrcSpan s _ -> return s
                        GeneratedSrcSpan{} -> pprPanic "qLocation: generatedSrcSpan"
                                                    (pprGeneratedSrcSpanDetails)
                        UnhelpfulSpan _ -> pprPanic "qLocation: Unhelpful location"
                                                    (ppr l)
              ; return (TH.Loc { TH.loc_filename = unpackFS (srcSpanFile r)
                               , TH.loc_module   = moduleNameString (moduleName m)
                               , TH.loc_package  = unitString (moduleUnit m)
                               , TH.loc_start = (srcSpanStartLine r, srcSpanStartCol r)
                               , TH.loc_end = (srcSpanEndLine   r, srcSpanEndCol   r) }) }

metaHandlersTcM :: (forall x. TcM x -> IO x) -> TH.MetaHandlers
metaHandlersTcM runInIO = TH.MetaHandlers {
      mLiftIO = id
    -- We are careful to use the TcM instance not the one for IO, since that would lead to a different error.
    , mFail = \s -> runInIO $ fail @TcM s
    , mNewName = \s -> runInIO $ do { u <- newUnique
                      ; let i = toInteger (getKey u)
                      ; return (TH.mkNameU s i) }

    , mReport = fmap runInIO . report

    , mLocation = runInIO location

    , mLookupName       = fmap runInIO . lookupName
    , mReify            = runInIO . reify
    , mReifyFixity      = \nm -> runInIO $ lookupThName nm >>= reifyFixity
    , mReifyType        = runInIO . reifyTypeOfThing
    , mReifyInstances   = fmap runInIO . reifyInstances
    , mReifyRoles       = runInIO . reifyRoles
    , mReifyAnnotations = runInIO . reifyAnnotations
    , mReifyModule      = runInIO . reifyModule
    , mReifyConStrictness = \nm -> runInIO $ do
                                      { nm' <- lookupThName nm
                                      ; dc  <- tcLookupDataCon nm'
                                      ; let bangs = dataConImplBangs dc
                                      ; return (map reifyDecidedStrictness bangs) }

    -- For qRecover, discard error messages if
    -- the recovery action is chosen.  Otherwise
    -- we'll only fail higher up.
    , mRecover = \recover main -> runInIO $ tryTcDiscardingErrs (runQinTcM recover) (runQinTcM main)

    , mGetPackageRoot = runInIO $ do
        dflags <- getDynFlags
        return $ fromMaybe "." (workingDirectory dflags)

    , mAddDependentFile = \fp -> runInIO $ do
        ref <- fmap tcg_dependent_files getGblEnv
        dep_files <- readTcRef ref
        writeTcRef ref (fp:dep_files)

    , mAddDependentDirectory = \dp -> runInIO $ do
        ref <- fmap tcg_dependent_dirs getGblEnv
        dep_dirs <- readTcRef ref
        writeTcRef ref (dp:dep_dirs)

    , mAddTempFile = \suffix -> runInIO $ do
        dflags <- getDynFlags
        logger <- getLogger
        tmpfs  <- hsc_tmpfs <$> getTopEnv
        liftIO $ newTempName logger tmpfs (tmpDir dflags) TFL_GhcSession suffix

    , mAddTopDecls = runInIO . addTopDecls

    , mAddForeignFilePath = \lang fp -> runInIO $ do
        var <- fmap tcg_th_foreign_files getGblEnv
        updTcRef var ((lang, fp) :)

    , mAddModFinalizer = \fin -> runInIO $ do
        r <- liftIO $ mkRemoteRef fin
        fref <- liftIO $ mkForeignRef r (freeRemoteRef r)
        addModFinalizerRef fref

    , mAddCorePlugin = \plugin -> runInIO $ do
        hsc_env <- getTopEnv
        let fc        = hsc_FC hsc_env
        let home_unit = hsc_home_unit hsc_env
        let dflags    = hsc_dflags hsc_env
        let fopts     = initFinderOpts dflags
        r <- liftIO $ runFinderM $ findHomeModule fc fopts home_unit (mkModuleName plugin)
        let err = TcRnTHError $ AddInvalidCorePlugin plugin
        case r of
          Found {} -> addErr err
          FoundMultiple {} -> addErr err
          _ -> return ()
        th_coreplugins_var <- tcg_th_coreplugins <$> getGblEnv
        updTcRef th_coreplugins_var (plugin:)

    , mGetQ = runInIO getQ

    , mPutQ = \x -> runInIO $ do
        th_state_var <- fmap tcg_th_state getGblEnv
        updTcRef th_state_var (\m -> Map.insert (typeOf x) (toDyn x) m)

    , mIsExtEnabled = runInIO . xoptM

    , mExtsEnabled = runInIO $
        EnumSet.toList . extensionFlags . hsc_dflags <$> getTopEnv

    , mPutDoc = fmap runInIO . putDoc

    , mGetDoc = runInIO . getDoc
  }

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
    inst_cls_name TH.ForEachT{}               = inst_cls_name_err
    inst_cls_name TH.IndexVarT{}              = inst_cls_name_err
    inst_cls_name TH.CaseT{}                  = inst_cls_name_err
    inst_cls_name (TH.ForallMatchT _ _)      = inst_cls_name_err
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
    th_lvl <- getThLevel
    case th_lvl of
      RunSplice th_modfinalizers_var -> updTcRef th_modfinalizers_var (finRef :)
      -- This case happens only if a splice is executed and the caller does
      -- not set the 'ThLevel' to 'RunSplice' to collect finalizers.
      -- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
      _ ->
        pprPanic "addModFinalizer was called when no finalizers were collected"
                 (ppr th_lvl)

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
      r <- runQinTcM (unsafeCoerce hv :: TH.Q a)
      return r
#endif

    ExternalInterp ext -> withExtInterp ext $ \inst -> do
      -- Run it on the server.  For an overview of how TH works with
      -- Remote GHCi, see Note [Remote Template Haskell] in
      -- libraries/ghci/GHCi/TH.hs.
      rstate <- getTHState inst
      loc <- location
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
  NewName a -> wrapTHResult $ runQinTcM $ TH.newName a
  Report b str -> wrapTHResult $ runQinTcM $ TH.report b str
  LookupName b str -> wrapTHResult $ runQinTcM $ TH.lookupName b str
  Reify n -> wrapTHResult $ runQinTcM $ TH.reify n
  ReifyFixity n -> wrapTHResult $ runQinTcM $ TH.reifyFixity n
  ReifyType n -> wrapTHResult $ runQinTcM $ TH.reifyType n
  ReifyInstances n ts -> wrapTHResult $ runQinTcM $ TH.reifyInstances n ts
  ReifyRoles n -> wrapTHResult $ runQinTcM $ TH.reifyRoles n
  ReifyAnnotations lookup tyrep ->
    wrapTHResult $ getAnnotationsByTypeRep lookup tyrep
  ReifyModule m -> wrapTHResult $ runQinTcM $ TH.reifyModule m
  ReifyConStrictness nm -> wrapTHResult $ runQinTcM $ TH.reifyConStrictness nm
  GetPackageRoot -> wrapTHResult $ runQinTcM $ TH.getPackageRoot
  AddDependentFile f -> wrapTHResult $ runQinTcM $ TH.addDependentFile f
  AddDependentDirectory d -> wrapTHResult $ runQinTcM $ TH.addDependentDirectory d
  AddTempFile s -> wrapTHResult $ runQinTcM $ TH.addTempFile s
  AddModFinalizer r -> do
    interp <- hscInterp <$> getTopEnv
    wrapTHResult $ liftIO (mkFinalizedHValue interp r) >>= addModFinalizerRef
  AddCorePlugin str -> wrapTHResult $ runQinTcM $ TH.addCorePlugin str
  AddTopDecls decs -> wrapTHResult $ runQinTcM $ TH.addTopDecls decs
  AddForeignFilePath lang str -> wrapTHResult $ runQinTcM $ TH.addForeignFilePath lang str
  IsExtEnabled ext -> wrapTHResult $ runQinTcM $ TH.isExtEnabled ext
  ExtsEnabled -> wrapTHResult $ runQinTcM $ TH.extsEnabled
  PutDoc l s -> wrapTHResult $ runQinTcM $ TH.putDoc l s
  GetDoc l -> wrapTHResult $ runQinTcM $ TH.getDoc l
  FailIfErrs -> wrapTHResult failIfErrsM
  _ -> panic ("handleTHMessage: unexpected message " ++ show msg)

getAnnotationsByTypeRep :: TH.AnnLookup -> TypeRep -> TcM [ShortByteString]
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
   = addErrCtxt (ReifyInstancesCtxt th_nm th_tys) $
     do { exts <- fmap extensionFlags getDynFlags
        ; loc <- getSrcSpanM
        ; th_origin <- getThSpliceOrigin
        ; rdr_ty <- cvt exts th_origin loc (mkThAppTs (TH.ConT th_nm) th_tys)
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
    doc = ReifyInstancesCtx
    bale_out msg = failWithTc msg

    cvt :: EnumSet LangExt.Extension -> Origin -> SrcSpan -> TH.Type -> TcM (LHsType GhcPs)
    cvt exts origin loc th_ty = case convertToHsType exts origin loc th_ty of
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
        ; thing <- tcLookupTh name
        ; case thing of
            -- Generated RHS helpers are scoped to checked type-case lifting.
            ATcCaseLet{} -> failWithTc (notInScope th_name)
            _ -> return thing }
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
  =  do { listTuplePuns <- xoptM LangExt.ListTuplePuns
        ; let guesses = thRdrNameGuesses listTuplePuns th_name
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

reifyThing (ATcTypeLet name ty _) = do
  rhs <- liftZonkM (zonkTcType ty) >>= reifyType
  pure (TH.TyConI (TH.TySynD (reifyName name) [] rhs))

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
         else do { eqns <- case closedFamilyTyConCoAxiom_maybe tc of
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
             retained  = dataConRepArgBinders dc
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

       ; r_arg_tys <- if null retained
         then reifyTypes (if isGadtDataCon then g_arg_tys else arg_tys)
         else pure []

       ; main_con <-
           if | not (null retained) -> do
                  { (args, result) <- reifyRetainedGadtArgs retained dcdBangs
                                      (map (substScaledTy tvb_subst) g_arg_tys') g_res_ty
                  ; pure (TH.RetainedGadtC [name] args result) }
              | not (null fields) && not isGadtDataCon ->
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
                         ; ex_tvs'' <- case to_invis_bndrs ex_tvs' of
                             Nothing  -> noTH DataConVisibleForall (dataConWrapperType dc)
                             Just tvs -> reifyTyVarBndrs tvs
                         ; ma <- reifyType unmatchableDataConTy
                         ; let con = TH.ForallC ex_tvs'' cxt main_con
                         ; return $ if null ex_tvs'' then con
                                    else TH.ForallMatchC ma con }
       ; assert (not (null retained) || r_arg_tys `equalLength` dcdBangs)
         ret_con }
  where
    mk_specified tv = Bndr tv Specified

    to_invis_bndrs :: [TyVarBinder] -> Maybe [InvisTVBinder]
    to_invis_bndrs = traverse $ \(Bndr tv vis) ->
      case vis of
        Invisible spec -> Just (Bndr tv spec)
        Required -> Nothing

    subst_tv_binders subst tv_bndrs =
      let tvs            = binderVars tv_bndrs
          flags          = binderFlags tv_bndrs
          (subst', tvs') = substTyVarBndrs subst tvs
          tv_bndrs'      = map (\(tv,fl) -> Bndr tv fl) (zip tvs' flags)
      in (subst', tv_bndrs')

-- | Reify the actual field binders. The wrapper type can omit a vacuous
-- retained binder even when the constructor's field metadata retains it.
reifyRetainedGadtArgs :: [Maybe Id] -> [TH.Bang] -> [Scaled Type] -> Type
                     -> TcM ([TH.RetainedGadtArg], TH.Type)
reifyRetainedGadtArgs binders bangs fields result =
  assert (binders `equalLength` bangs && binders `equalLength` fields) $
    go emptyNameSet (zip3 binders bangs fields)
  where
    go scope [] = ([],) <$> reifyTypeInScope scope result
    go scope ((binder, bang, Scaled mult ty) : rest) = do
      field_ty <- reifyTypeInScope scope ty
      (arg, rest_scope) <- case binder of
        Nothing -> do
          mult' <- reifyTypeInScope scope mult
          pure (TH.GadtValueArg bang mult' field_ty, scope)
        Just index ->
          pure (TH.GadtIndexArg (reifyName index) field_ty,
                extendNameSet scope (getName index))
      (args, result') <- go rest_scope rest
      pure (arg : args, result')

mkDataConI :: DataCon -> TcM TH.Info
mkDataConI dc
  = do  { let name = dataConName dc
        -- Pack quotes can retain its name. Its marker type is private.
        ; pack <- isExistsPackName name
        ; when pack $ failWithTc (TcRnExistsPack ExistsPackInvalidContext)
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
    tc_res_kind_vis_bndrs = let (_, bndrs, _) = splitPiTys (tyConResKind tc)
                            in filter isVisiblePiTyBinder bndrs

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
reifyType = reifyTypeInScope emptyNameSet

-- | Keep the scope of retained term binders while reifying their result types.
reifyTypeInScope :: NameSet -> TyCoRep.Type -> TcM TH.Type
reifyTypeInScope _ ty      | tcIsLiftedTypeKind ty = return TH.StarT
  -- Make sure to use tcIsLiftedTypeKind here, since we don't want to confuse it
  -- with Constraint (#14869).
reifyTypeInScope _ ty@TermAppTy{} = noTH RetainedRuntimeIndex ty
-- TH.Type has no binder or reference for the exact scoped dictionary.
reifyTypeInScope _ ty@DepDictTy{} = noTH RetainedRuntimeIndex ty
reifyTypeInScope _ ty@ClassAppTy{} = noTH RetainedRuntimeIndex ty
reifyTypeInScope scope ty@(TermVarTy v)
  | getName v `elemNameSet` scope = pure (TH.IndexVarT (reifyName v))
  | otherwise                    = noTH RetainedRuntimeIndex ty
reifyTypeInScope scope (DepFunTy vis v body)
  = do { domain' <- reifyTypeInScope scope (idType v)
       ; body' <- reifyTypeInScope (extendNameSet scope (getName v)) body
       ; pure (TH.ForEachT visibility (reifyName v) domain' body') }
  where
    visibility = case vis of
      DepFunVisible   -> TH.BndrReq
      DepFunInvisible -> TH.BndrInvis
reifyTypeInScope _ ty@ExistsTy{} = noTH FirstClassExistential ty
reifyTypeInScope scope (ForAllTy ma (Bndr tv argf) body)
  -- See Note [Reifying forall matchability].
  = do { ma' <- reifyTypeInScope scope ma
       ; body' <- reifyTypeInScope scope body
       ; group <- case argf of
           Required -> do { tvs <- reifyTyVarBndrsWith (reifyTypeInScope scope) [Bndr tv ()]
                          ; pure (TH.ForallVisT tvs body') }
           Invisible spec -> do
             { tvs <- reifyTyVarBndrsWith (reifyTypeInScope scope) [Bndr tv spec]
             ; pure (TH.ForallT tvs [] body') }
       ; pure (TH.ForallMatchT ma' group) }
reifyTypeInScope _ (LitTy t)         = do { r <- reifyTyLit t; return (TH.LitT r) }
reifyTypeInScope _ (TyVarTy tv)      = return (TH.VarT (reifyName tv))
reifyTypeInScope scope (TyConApp tc tys) = reify_tc_app scope tc tys   -- Do not expand type synonyms here
reifyTypeInScope scope ty@AppTy{}     = do
  let (ty_head, ty_args) = splitAppTys ty
  ty_head' <- reifyTypeInScope scope ty_head
  ty_args' <- mapM (reifyTypeInScope scope) (filter_out_invisible_args ty_head ty_args)
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
-- Explicit metadata preserves arrows in both type and kind quotation contexts.
reifyTypeInScope scope (FunTy { ft_af = FTF_T_T, ft_ma = ma, ft_mult = mult, ft_arg = arg, ft_res = res })
  = do { [ma',mult',arg',res'] <- mapM (reifyTypeInScope scope) [ma,mult,arg,res]
       ; let fun = TH.AppKindT (TH.ConT (reifyName (funTyFlagTyCon FTF_T_T))) ma'
       ; return (mkThAppTs fun [mult',arg',res']) }
reifyTypeInScope scope ty@(FunTy { ft_af = af, ft_mult = ManyTy, ft_arg = t1, ft_res = t2 })
  | isInvisibleFunArg af = reify_context scope ty  -- Types like ((?x::Int) => Char -> Char)
  | otherwise            = do { [r1,r2] <- mapM (reifyTypeInScope scope) [t1,t2]
                              ; return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2) }
reifyTypeInScope scope ty@(FunTy { ft_af = af, ft_mult = tm, ft_arg = t1, ft_res = t2 })
  | isInvisibleFunArg af = noTH LinearInvisibleArgument ty
  | otherwise            = do { [rm,r1,r2] <- mapM (reifyTypeInScope scope) [tm,t1,t2]
                              ; return (TH.MulArrowT `TH.AppT` rm `TH.AppT` r1 `TH.AppT` r2) }
reifyTypeInScope scope (CastTy t _) = reifyTypeInScope scope t -- Casts are ignored in TH
reifyTypeInScope _ ty@CoercionTy{} = noTH CoercionsInTypes ty

-- | Reify predicate arrows without introducing a quantified group.
reify_context :: NameSet -> TyCoRep.Type -> TcM TH.Type
-- The argument is a predicate FunTy. Context-only nodes have no metadata.
reify_context scope ty
  = do let (cxt, tau) = tcSplitPhiTy ty
       cxt' <- mapM (reifyTypeInScope scope) cxt
       tau' <- reifyTypeInScope scope tau
       pure $ TH.ForallT [] cxt' tau'

{- Note [Reifying forall matchability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Reify each Core forall as a separate annotated group. Metadata can refer to
an earlier binder. A larger group would put that binder outside its scope.
Reify the actual metadata type. Do not replace an alias or a variable with a
default. See Note [Forall matchability wrappers] in GHC.Internal.TH.Syntax.

Data constructor and pattern synonym binders describe runtime abstractions.
These binders have U metadata. Their reified nonempty groups retain U too.
Empty context-only groups have no binder to annotate. Preserve those groups
in pattern synonym signatures to separate required and provided contexts.
-}

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
       ; tau'        <- reifyType (mkScaledFunTysU argTys resTy)
       ; ma <- reifyType unmatchableDataConTy
       ; let slot tvs ctxt body =
               let ty = TH.ForallT tvs ctxt body
               in if null tvs then ty else TH.ForallMatchT ma ty
       ; return $ slot univTyVars' req' $ slot exTyVars' prov' tau' }

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
reifyTyVarBndrs = reifyTyVarBndrsWith reifyKind

-- | Reify binder kinds in the caller's scope.
reifyTyVarBndrsWith :: ReifyFlag flag flag'
                   => (Kind -> TcM TH.Kind) -> [VarBndr TyVar flag] -> TcM [TH.TyVarBndr flag']
reifyTyVarBndrsWith reify_kind = mapM reify_tvbndr
  where
    -- even if the kind is *, we need to include a kind annotation,
    -- in case a poly-kind would be inferred without the annotation.
    -- See #8953 or test th/T8953
    reify_tvbndr (Bndr tv fl) = TH.KindedTV (reifyName tv)
                                            (reifyFlag fl)
                                            <$> reify_kind (tyVarKind tv)

reifyTyVarsToMaybe :: [TyVar] -> TcM (Maybe [TH.TyVarBndr ()])
reifyTyVarsToMaybe []  = pure Nothing
reifyTyVarsToMaybe tys = Just <$> reifyTyVars tys

reify_tc_app :: NameSet -> TyCon -> [Type.Type] -> TcM TH.Type
reify_tc_app scope tc tys
  | tc `hasKey` fUNTyConKey
  , ma : _ <- tys
  = do { ma' <- reifyTypeInScope scope ma
       ; tys' <- mapM (reifyTypeInScope scope) (filterOutInvisibleTypes tc tys)
       ; maybe_sig_t (mkThAppTs (TH.AppKindT (TH.ConT (reifyName tc)) ma') tys') }
  | otherwise
  = do { tys' <- mapM (reifyTypeInScope scope) (filterOutInvisibleTypes tc tys)
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
           ; th_full_kind <- reifyTypeInScope scope full_kind
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
reifySourceBang (HsSrcBang _ u s) = (reifyUnpackedness u, reifyStrictness s)

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
    ATcTypeLet _ _ kind -> liftZonkM (zonkTcType kind) >>= reifyKind
    ATcCaseLet{} -> failWithTc (notInScope th_name)
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
        import_decls <- tcg_import_decls <$> getGblEnv
        return $ TH.ModuleInfo (thImportedModules import_decls)

      reifyFromIface reifMod = do
        iface <- loadInterfaceForModule (text "reifying module from TH for" <+> ppr reifMod) reifMod
        let IfaceTopEnv _ imports = mi_top_env iface
        return $ TH.ModuleInfo (thImportedModules (map tcIfaceImport imports))

-- | The modules imported by a module, as reported to Template Haskell:
-- deduplicated, in a deterministic order, and without dependency-only
-- imports ('isDependOnlyImport') which bring nothing into scope.
thImportedModules :: [ImportUserSpec] -> [TH.Module]
thImportedModules import_decls
  = [ modToTHMod m
    | m <- sortBy stableModuleCmp $ nubOrd
             [ is_mod (ius_decl spec)
             | spec <- import_decls
             , not (isDependOnlyImport (ius_imports spec)) ] ]

------------------------------
mkThAppTs :: TH.Type -> [TH.Type] -> TH.Type
mkThAppTs fun_ty arg_tys = foldl' TH.AppT fun_ty arg_tys

noTH :: UnrepresentableTypeDescr -> Type -> TcM a
noTH s d = failWithTc $ TcRnTHError $ THReifyError $ CannotRepresentType s d

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
-- To desugar Template Haskell quotes, GHC needs to known a bunch of OccNames,
-- see Note [Overview of known entities] in GHC.Builtin.
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

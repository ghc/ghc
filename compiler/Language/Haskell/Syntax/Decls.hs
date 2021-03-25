{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ViewPatterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}


{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*

-- | Abstract syntax of global declarations.
--
-- Definitions for: @SynDecl@ and @ConDecl@, @ClassDecl@,
-- @InstDecl@, @DefaultDecl@ and @ForeignDecl@.
module Language.Haskell.Syntax.Decls (
  -- * Toplevel declarations
  HsDecl(..), LHsDecl, HsDataDefn(..), HsDeriving, LHsFunDep, FunDep(..),
  HsDerivingClause(..), LHsDerivingClause, DerivClauseTys(..), LDerivClauseTys,
  NewOrData(..), newOrDataToFlavour,
  StandaloneKindSig(..), LStandaloneKindSig,

  -- ** Class or type declarations
  TyClDecl(..), LTyClDecl, DataDeclRn(..),
  TyClGroup(..),
  tyClGroupTyClDecls, tyClGroupInstDecls, tyClGroupRoleDecls,
  tyClGroupKindSigs,
  isClassDecl, isDataDecl, isSynDecl,
  isFamilyDecl, isTypeFamilyDecl, isDataFamilyDecl,
  isOpenTypeFamilyInfo, isClosedTypeFamilyInfo,
  countTyClDecls,
  tyClDeclTyVars,
  FamilyDecl(..), LFamilyDecl,

  -- ** Instance declarations
  InstDecl(..), LInstDecl, FamilyInfo(..), pprFlavour,
  TyFamInstDecl(..), LTyFamInstDecl,
  TyFamDefltDecl, LTyFamDefltDecl,
  DataFamInstDecl(..), LDataFamInstDecl,
  FamEqn(..), TyFamInstEqn, LTyFamInstEqn, HsTyPats,
  LClsInstDecl, ClsInstDecl(..),

  -- ** Standalone deriving declarations
  DerivDecl(..), LDerivDecl,
  -- ** Deriving strategies
  DerivStrategy(..), LDerivStrategy,
  derivStrategyName,
  -- ** @RULE@ declarations
  LRuleDecls,RuleDecls(..),RuleDecl(..),LRuleDecl,HsRuleRn(..),
  RuleBndr(..),LRuleBndr,
  collectRuleBndrSigTys,
  pprFullRuleName,
  -- ** @default@ declarations
  DefaultDecl(..), LDefaultDecl,
  -- ** Template haskell declaration splice
  SpliceExplicitFlag(..),
  SpliceDecl(..), LSpliceDecl,
  -- ** Foreign function interface declarations
  ForeignDecl(..), LForeignDecl, ForeignImport(..), ForeignExport(..),
  CImportSpec(..),
  -- ** Data-constructor declarations
  ConDecl(..), LConDecl,
  HsConDeclH98Details, HsConDeclGADTDetails(..),
  -- ** Document comments
  DocDecl(..), LDocDecl, docDeclDoc,
  -- ** Deprecations
  WarnDecl(..),  LWarnDecl,
  WarnDecls(..), LWarnDecls,
  -- ** Annotations
  AnnDecl(..), LAnnDecl,
  AnnProvenance(..), annProvenanceName_maybe,
  -- ** Role annotations
  RoleAnnotDecl(..), LRoleAnnotDecl,
  -- ** Injective type families
  FamilyResultSig(..), LFamilyResultSig, InjectivityAnn(..), LInjectivityAnn,

  -- * Grouping
  HsGroup(..), hsGroupInstDecls,
    ) where

-- friends:
import GHC.Prelude

import {-# SOURCE #-} Language.Haskell.Syntax.Expr
  ( HsExpr, HsSplice )
        -- Because Expr imports Decls via HsBracket

import Language.Haskell.Syntax.Binds
import Language.Haskell.Syntax.Type
import GHC.Hs.Doc
import GHC.Core.TyCon
import GHC.Types.Basic
import GHC.Types.ForeignCall
import Language.Haskell.Syntax.Extension
import GHC.Types.Name.Set
import GHC.Types.Fixity

-- others:
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Core.Type
import GHC.Unit.Module.Warnings

import GHC.Data.Maybe
import Data.Data        hiding (TyCon,Fixity, Infix)
import Data.Void

{-
************************************************************************
*                                                                      *
\subsection[HsDecl]{Declarations}
*                                                                      *
************************************************************************
-}

type LHsDecl p = XRec p (HsDecl p)
        -- ^ When in a list this may have
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnSemi'
        --

-- For details on above see note [exact print annotations] in GHC.Parser.Annotation

-- | A Haskell Declaration
data HsDecl p
  = TyClD      (XTyClD p)      (TyClDecl p)      -- ^ Type or Class Declaration
  | InstD      (XInstD p)      (InstDecl  p)     -- ^ Instance declaration
  | DerivD     (XDerivD p)     (DerivDecl p)     -- ^ Deriving declaration
  | ValD       (XValD p)       (HsBind p)        -- ^ Value declaration
  | SigD       (XSigD p)       (Sig p)           -- ^ Signature declaration
  | KindSigD   (XKindSigD p)   (StandaloneKindSig p) -- ^ Standalone kind signature
  | DefD       (XDefD p)       (DefaultDecl p)   -- ^ 'default' declaration
  | ForD       (XForD p)       (ForeignDecl p)   -- ^ Foreign declaration
  | WarningD   (XWarningD p)   (WarnDecls p)     -- ^ Warning declaration
  | AnnD       (XAnnD p)       (AnnDecl p)       -- ^ Annotation declaration
  | RuleD      (XRuleD p)      (RuleDecls p)     -- ^ Rule declaration
  | SpliceD    (XSpliceD p)    (SpliceDecl p)    -- ^ Splice declaration
                                                 -- (Includes quasi-quotes)
  | DocD       (XDocD p)       (DocDecl)  -- ^ Documentation comment declaration
  | RoleAnnotD (XRoleAnnotD p) (RoleAnnotDecl p) -- ^Role annotation declaration
  | XHsDecl    !(XXHsDecl p)

{-
Note [Top-level fixity signatures in an HsGroup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An `HsGroup p` stores every top-level fixity declarations in one of two places:

1. hs_fixds :: [LFixitySig p]

   This stores fixity signatures for top-level declarations (e.g., functions,
   data constructors, classes, type families, etc.) as well as fixity
   signatures for class methods written outside of the class, as in this
   example:

     infixl 4 `m1`
     class C1 a where
       m1 :: a -> a -> a

2. hs_tyclds :: [TyClGroup p]

   Each type class can be found in a TyClDecl inside a TyClGroup, and that
   TyClDecl stores the fixity signatures for its methods written inside of the
   class, as in this example:

     class C2 a where
       infixl 4 `m2`
       m2 :: a -> a -> a

The story for fixity signatures for class methods is made slightly complicated
by the fact that they can appear both inside and outside of the class itself,
and both forms of fixity signatures are considered top-level. This matters
in `GHC.Rename.Module.rnSrcDecls`, which must create a fixity environment out
of all top-level fixity signatures before doing anything else. Therefore,
`rnSrcDecls` must be aware of both (1) and (2) above. The
`hsGroupTopLevelFixitySigs` function is responsible for collecting this
information from an `HsGroup`.

One might wonder why we even bother separating top-level fixity signatures
into two places at all. That is, why not just take the fixity signatures
from `hs_tyclds` and put them into `hs_fixds` so that they are all in one
location? This ends up causing problems for `GHC.HsToCore.Quote.repTopDs`,
which translates each fixity signature in `hs_fixds` and `hs_tyclds` into a
Template Haskell `Dec`. If there are any duplicate signatures between the two
fields, this will result in an error (#17608).
-}

-- | Haskell Group
--
-- A 'HsDecl' is categorised into a 'HsGroup' before being
-- fed to the renamer.
data HsGroup p
  = HsGroup {
        hs_ext    :: XCHsGroup p,
        hs_valds  :: HsValBinds p,
        hs_splcds :: [LSpliceDecl p],

        hs_tyclds :: [TyClGroup p],
                -- A list of mutually-recursive groups;
                -- This includes `InstDecl`s as well;
                -- Parser generates a singleton list;
                -- renamer does dependency analysis

        hs_derivds :: [LDerivDecl p],

        hs_fixds  :: [LFixitySig p],
                -- A list of fixity signatures defined for top-level
                -- declarations and class methods (defined outside of the class
                -- itself).
                -- See Note [Top-level fixity signatures in an HsGroup]

        hs_defds  :: [LDefaultDecl p],
        hs_fords  :: [LForeignDecl p],
        hs_warnds :: [LWarnDecls p],
        hs_annds  :: [LAnnDecl p],
        hs_ruleds :: [LRuleDecls p],

        hs_docs   :: [LDocDecl p]
    }
  | XHsGroup !(XXHsGroup p)


hsGroupInstDecls :: HsGroup id -> [LInstDecl id]
hsGroupInstDecls = (=<<) group_instds . hs_tyclds

-- | Located Splice Declaration
type LSpliceDecl pass = XRec pass (SpliceDecl pass)

-- | Splice Declaration
data SpliceDecl p
  = SpliceDecl                  -- Top level splice
        (XSpliceDecl p)
        (XRec p (HsSplice p))
        SpliceExplicitFlag
  | XSpliceDecl !(XXSpliceDecl p)

{-
************************************************************************
*                                                                      *
            Type and class declarations
*                                                                      *
************************************************************************

Note [The Naming story]
~~~~~~~~~~~~~~~~~~~~~~~
Here is the story about the implicit names that go with type, class,
and instance decls.  It's a bit tricky, so pay attention!

"Implicit" (or "system") binders
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Each data type decl defines
        a worker name for each constructor
        to-T and from-T convertors
  Each class decl defines
        a tycon for the class
        a data constructor for that tycon
        the worker for that constructor
        a selector for each superclass

All have occurrence names that are derived uniquely from their parent
declaration.

None of these get separate definitions in an interface file; they are
fully defined by the data or class decl.  But they may *occur* in
interface files, of course.  Any such occurrence must haul in the
relevant type or class decl.

Plan of attack:
 - Ensure they "point to" the parent data/class decl
   when loading that decl from an interface file
   (See RnHiFiles.getSysBinders)

 - When typechecking the decl, we build the implicit TyCons and Ids.
   When doing so we look them up in the name cache (GHC.Rename.Env.lookupSysName),
   to ensure correct module and provenance is set

These are the two places that we have to conjure up the magic derived
names.  (The actual magic is in GHC.Types.Name.Occurrence.mkWorkerOcc, etc.)

Default methods
~~~~~~~~~~~~~~~
 - Occurrence name is derived uniquely from the method name
   E.g. $dmmax

 - If there is a default method name at all, it's recorded in
   the ClassOpSig (in GHC.Hs.Binds), in the DefMethInfo field.
   (DefMethInfo is defined in GHC.Core.Class)

Source-code class decls and interface-code class decls are treated subtly
differently, which has given me a great deal of confusion over the years.
Here's the deal.  (We distinguish the two cases because source-code decls
have (Just binds) in the tcdMeths field, whereas interface decls have Nothing.

In *source-code* class declarations:

 - When parsing, every ClassOpSig gets a DefMeth with a suitable RdrName
   This is done by GHC.Parser.PostProcess.mkClassOpSigDM

 - The renamer renames it to a Name

 - During typechecking, we generate a binding for each $dm for
   which there's a programmer-supplied default method:
        class Foo a where
          op1 :: <type>
          op2 :: <type>
          op1 = ...
   We generate a binding for $dmop1 but not for $dmop2.
   The Class for Foo has a Nothing for op2 and
                         a Just ($dm_op1, VanillaDM) for op1.
   The Name for $dmop2 is simply discarded.

In *interface-file* class declarations:
  - When parsing, we see if there's an explicit programmer-supplied default method
    because there's an '=' sign to indicate it:
        class Foo a where
          op1 = :: <type>       -- NB the '='
          op2   :: <type>
    We use this info to generate a DefMeth with a suitable RdrName for op1,
    and a NoDefMeth for op2
  - The interface file has a separate definition for $dmop1, with unfolding etc.
  - The renamer renames it to a Name.
  - The renamer treats $dmop1 as a free variable of the declaration, so that
    the binding for $dmop1 will be sucked in.  (See RnHsSyn.tyClDeclFVs)
    This doesn't happen for source code class decls, because they *bind* the default method.

Dictionary functions
~~~~~~~~~~~~~~~~~~~~
Each instance declaration gives rise to one dictionary function binding.

The type checker makes up new source-code instance declarations
(e.g. from 'deriving' or generic default methods --- see
GHC.Tc.TyCl.Instance.tcInstDecls1).  So we can't generate the names for
dictionary functions in advance (we don't know how many we need).

On the other hand for interface-file instance declarations, the decl
specifies the name of the dictionary function, and it has a binding elsewhere
in the interface file:
        instance {Eq Int} = dEqInt
        dEqInt :: {Eq Int} <pragma info>

So again we treat source code and interface file code slightly differently.

Source code:
  - Source code instance decls have a Nothing in the (Maybe name) field
    (see data InstDecl below)

  - The typechecker makes up a Local name for the dict fun for any source-code
    instance decl, whether it comes from a source-code instance decl, or whether
    the instance decl is derived from some other construct (e.g. 'deriving').

  - The occurrence name it chooses is derived from the instance decl (just for
    documentation really) --- e.g. dNumInt.  Two dict funs may share a common
    occurrence name, but will have different uniques.  E.g.
        instance Foo [Int]  where ...
        instance Foo [Bool] where ...
    These might both be dFooList

  - The CoreTidy phase externalises the name, and ensures the occurrence name is
    unique (this isn't special to dict funs).  So we'd get dFooList and dFooList1.

  - We can take this relaxed approach (changing the occurrence name later)
    because dict fun Ids are not captured in a TyCon or Class (unlike default
    methods, say).  Instead, they are kept separately in the InstEnv.  This
    makes it easy to adjust them after compiling a module.  (Once we've finished
    compiling that module, they don't change any more.)


Interface file code:
  - The instance decl gives the dict fun name, so the InstDecl has a (Just name)
    in the (Maybe name) field.

  - RnHsSyn.instDeclFVs treats the dict fun name as free in the decl, so that we
    suck in the dfun binding
-}

-- | Located Declaration of a Type or Class
type LTyClDecl pass = XRec pass (TyClDecl pass)

-- | A type or class declaration.
data TyClDecl pass
  = -- | @type/data family T :: *->*@
    --
    --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnType',
    --             'GHC.Parser.Annotation.AnnData',
    --             'GHC.Parser.Annotation.AnnFamily','GHC.Parser.Annotation.AnnDcolon',
    --             'GHC.Parser.Annotation.AnnWhere','GHC.Parser.Annotation.AnnOpenP',
    --             'GHC.Parser.Annotation.AnnDcolon','GHC.Parser.Annotation.AnnCloseP',
    --             'GHC.Parser.Annotation.AnnEqual','GHC.Parser.Annotation.AnnRarrow',
    --             'GHC.Parser.Annotation.AnnVbar'

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
    FamDecl { tcdFExt :: XFamDecl pass, tcdFam :: FamilyDecl pass }

  | -- | @type@ declaration
    --
    --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnType',
    --             'GHC.Parser.Annotation.AnnEqual',

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
    SynDecl { tcdSExt   :: XSynDecl pass          -- ^ Post renameer, FVs
            , tcdLName  :: LIdP pass              -- ^ Type constructor
            , tcdTyVars :: LHsQTyVars pass        -- ^ Type variables; for an
                                                  -- associated type these
                                                  -- include outer binders
            , tcdFixity :: LexicalFixity          -- ^ Fixity used in the declaration
            , tcdRhs    :: LHsType pass }         -- ^ RHS of type declaration

  | -- | @data@ declaration
    --
    --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnData',
    --              'GHC.Parser.Annotation.AnnFamily',
    --              'GHC.Parser.Annotation.AnnNewType',
    --              'GHC.Parser.Annotation.AnnNewType','GHC.Parser.Annotation.AnnDcolon'
    --              'GHC.Parser.Annotation.AnnWhere',

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
    DataDecl { tcdDExt     :: XDataDecl pass       -- ^ Post renamer, CUSK flag, FVs
             , tcdLName    :: LIdP pass             -- ^ Type constructor
             , tcdTyVars   :: LHsQTyVars pass      -- ^ Type variables
                              -- See Note [TyVar binders for associated declarations]
             , tcdFixity   :: LexicalFixity        -- ^ Fixity used in the declaration
             , tcdDataDefn :: HsDataDefn pass }

  | ClassDecl { tcdCExt    :: XClassDecl pass,         -- ^ Post renamer, FVs
                tcdCtxt    :: Maybe (LHsContext pass), -- ^ Context...
                tcdLName   :: LIdP pass,               -- ^ Name of the class
                tcdTyVars  :: LHsQTyVars pass,         -- ^ Class type variables
                tcdFixity  :: LexicalFixity, -- ^ Fixity used in the declaration
                tcdFDs     :: [LHsFunDep pass],         -- ^ Functional deps
                tcdSigs    :: [LSig pass],              -- ^ Methods' signatures
                tcdMeths   :: LHsBinds pass,            -- ^ Default methods
                tcdATs     :: [LFamilyDecl pass],       -- ^ Associated types;
                tcdATDefs  :: [LTyFamDefltDecl pass],   -- ^ Associated type defaults
                tcdDocs    :: [LDocDecl pass]           -- ^ Haddock docs
    }
        -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnClass',
        --           'GHC.Parser.Annotation.AnnWhere','GHC.Parser.Annotation.AnnOpen',
        --           'GHC.Parser.Annotation.AnnClose'
        --   - The tcdFDs will have 'GHC.Parser.Annotation.AnnVbar',
        --                          'GHC.Parser.Annotation.AnnComma'
        --                          'GHC.Parser.Annotation.AnnRarrow'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | XTyClDecl !(XXTyClDecl pass)

data FunDep pass
  = FunDep (XCFunDep pass)
           [LIdP pass]
           [LIdP pass]
  | XFunDep !(XXFunDep pass)

type LHsFunDep pass = XRec pass (FunDep pass)

data DataDeclRn = DataDeclRn
             { tcdDataCusk :: Bool    -- ^ does this have a CUSK?
                 -- See Note [CUSKs: complete user-supplied kind signatures]
             , tcdFVs      :: NameSet }
  deriving Data

{- Note [TyVar binders for associated decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For an /associated/ data, newtype, or type-family decl, the LHsQTyVars
/includes/ outer binders.  For example
    class T a where
       data D a c
       type F a b :: *
       type F a b = a -> a
Here the data decl for 'D', and type-family decl for 'F', both include 'a'
in their LHsQTyVars (tcdTyVars and fdTyVars resp).

Ditto any implicit binders in the hsq_implicit field of the LHSQTyVars.

The idea is that the associated type is really a top-level decl in its
own right.  However we are careful to use the same name 'a', so that
we can match things up.

c.f. Note [Associated type tyvar names] in GHC.Core.Class
     Note [Family instance declaration binders]
-}

{- Note [Class LayoutInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The LayoutInfo is used to associate Haddock comments with parts of the declaration.
Compare the following examples:

    class C a where
      f :: a -> Int
      -- ^ comment on f

    class C a where
      f :: a -> Int
    -- ^ comment on C

Notice how "comment on f" and "comment on C" differ only by indentation level.
Thus we have to record the indentation level of the class declarations.

See also Note [Adding Haddock comments to the syntax tree] in GHC.Parser.PostProcess.Haddock
-}

-- Simple classifiers for TyClDecl
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | @True@ <=> argument is a @data@\/@newtype@
-- declaration.
isDataDecl :: TyClDecl pass -> Bool
isDataDecl (DataDecl {}) = True
isDataDecl _other        = False

-- | type or type instance declaration
isSynDecl :: TyClDecl pass -> Bool
isSynDecl (SynDecl {})   = True
isSynDecl _other        = False

-- | type class
isClassDecl :: TyClDecl pass -> Bool
isClassDecl (ClassDecl {}) = True
isClassDecl _              = False

-- | type/data family declaration
isFamilyDecl :: TyClDecl pass -> Bool
isFamilyDecl (FamDecl {})  = True
isFamilyDecl _other        = False

-- | type family declaration
isTypeFamilyDecl :: TyClDecl pass -> Bool
isTypeFamilyDecl (FamDecl _ (FamilyDecl { fdInfo = info })) = case info of
  OpenTypeFamily      -> True
  ClosedTypeFamily {} -> True
  _                   -> False
isTypeFamilyDecl _ = False

-- | open type family info
isOpenTypeFamilyInfo :: FamilyInfo pass -> Bool
isOpenTypeFamilyInfo OpenTypeFamily = True
isOpenTypeFamilyInfo _              = False

-- | closed type family info
isClosedTypeFamilyInfo :: FamilyInfo pass -> Bool
isClosedTypeFamilyInfo (ClosedTypeFamily {}) = True
isClosedTypeFamilyInfo _                     = False

-- | data family declaration
isDataFamilyDecl :: TyClDecl pass -> Bool
isDataFamilyDecl (FamDecl _ (FamilyDecl { fdInfo = DataFamily })) = True
isDataFamilyDecl _other      = False

-- Dealing with names

tyClDeclTyVars :: TyClDecl pass -> LHsQTyVars pass
tyClDeclTyVars (FamDecl { tcdFam = FamilyDecl { fdTyVars = tvs } }) = tvs
tyClDeclTyVars d = tcdTyVars d

countTyClDecls :: [TyClDecl pass] -> (Int, Int, Int, Int, Int)
        -- class, synonym decls, data, newtype, family decls
countTyClDecls decls
 = (count isClassDecl    decls,
    count isSynDecl      decls,  -- excluding...
    count isDataTy       decls,  -- ...family...
    count isNewTy        decls,  -- ...instances
    count isFamilyDecl   decls)
 where
   isDataTy DataDecl{ tcdDataDefn = HsDataDefn { dd_ND = DataType } } = True
   isDataTy _                                                       = False

   isNewTy DataDecl{ tcdDataDefn = HsDataDefn { dd_ND = NewType } } = True
   isNewTy _                                                      = False


{- Note [CUSKs: complete user-supplied kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We kind-check declarations differently if they have a complete, user-supplied
kind signature (CUSK). This is because we can safely generalise a CUSKed
declaration before checking all of the others, supporting polymorphic recursion.
See https://gitlab.haskell.org/ghc/ghc/wikis/ghc-kinds/kind-inference#proposed-new-strategy
and #9200 for lots of discussion of how we got here.

The detection of CUSKs is enabled by the -XCUSKs extension, switched on by default.
Under -XNoCUSKs, all declarations are treated as if they have no CUSK.
See https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0036-kind-signatures.rst

PRINCIPLE:
  a type declaration has a CUSK iff we could produce a separate kind signature
  for it, just like a type signature for a function,
  looking only at the header of the declaration.

Examples:
  * data T1 (a :: *->*) (b :: *) = ....
    -- Has CUSK; equivalant to   T1 :: (*->*) -> * -> *

 * data T2 a b = ...
   -- No CUSK; we do not want to guess T2 :: * -> * -> *
   -- because the full decl might be   data T a b = MkT (a b)

  * data T3 (a :: k -> *) (b :: *) = ...
    -- CUSK; equivalent to   T3 :: (k -> *) -> * -> *
    -- We lexically generalise over k to get
    --    T3 :: forall k. (k -> *) -> * -> *
    -- The generalisation is here is purely lexical, just like
    --    f3 :: a -> a
    -- means
    --    f3 :: forall a. a -> a

  * data T4 (a :: j k) = ...
     -- CUSK; equivalent to   T4 :: j k -> *
     -- which we lexically generalise to  T4 :: forall j k. j k -> *
     -- and then, if PolyKinds is on, we further generalise to
     --   T4 :: forall kk (j :: kk -> *) (k :: kk). j k -> *
     -- Again this is exactly like what happens as the term level
     -- when you write
     --    f4 :: forall a b. a b -> Int

NOTE THAT
  * A CUSK does /not/ mean that everything about the kind signature is
    fully specified by the user.  Look at T4 and f4: we had to do kind
    inference to figure out the kind-quantification.  But in both cases
    (T4 and f4) that inference is done looking /only/ at the header of T4
    (or signature for f4), not at the definition thereof.

  * The CUSK completely fixes the kind of the type constructor, forever.

  * The precise rules, for each declaration form, for whether a declaration
    has a CUSK are given in the user manual section "Complete user-supplied
    kind signatures and polymorphic recursion".  But they simply implement
    PRINCIPLE above.

  * Open type families are interesting:
      type family T5 a b :: *
    There simply /is/ no accompanying declaration, so that info is all
    we'll ever get.  So we it has a CUSK by definition, and we default
    any un-fixed kind variables to *.

  * Associated types are a bit tricker:
      class C6 a where
         type family T6 a b :: *
         op :: a Int -> Int
    Here C6 does not have a CUSK (in fact we ultimately discover that
    a :: * -> *).  And hence neither does T6, the associated family,
    because we can't fix its kind until we have settled C6.  Another
    way to say it: unlike a top-level, we /may/ discover more about
    a's kind from C6's definition.

  * A data definition with a top-level :: must explicitly bind all
    kind variables to the right of the ::. See test
    dependent/should_compile/KindLevels, which requires this
    case. (Naturally, any kind variable mentioned before the :: should
    not be bound after it.)

    This last point is much more debatable than the others; see
    #15142 comment:22

    Because this is fiddly to check, there is a field in the DataDeclRn
    structure (included in a DataDecl after the renamer) that stores whether
    or not the declaration has a CUSK.
-}


{- *********************************************************************
*                                                                      *
                         TyClGroup
        Strongly connected components of
      type, class, instance, and role declarations
*                                                                      *
********************************************************************* -}

{- Note [TyClGroups and dependency analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A TyClGroup represents a strongly connected components of type/class/instance
decls, together with the role annotations for the type/class declarations.

The hs_tyclds :: [TyClGroup] field of a HsGroup is a dependency-order
sequence of strongly-connected components.

Invariants
 * The type and class declarations, group_tyclds, may depend on each
   other, or earlier TyClGroups, but not on later ones

 * The role annotations, group_roles, are role-annotations for some or
   all of the types and classes in group_tyclds (only).

 * The instance declarations, group_instds, may (and usually will)
   depend on group_tyclds, or on earlier TyClGroups, but not on later
   ones.

See Note [Dependency analysis of type, class, and instance decls]
in GHC.Rename.Module for more info.
-}

-- | Type or Class Group
data TyClGroup pass  -- See Note [TyClGroups and dependency analysis]
  = TyClGroup { group_ext    :: XCTyClGroup pass
              , group_tyclds :: [LTyClDecl pass]
              , group_roles  :: [LRoleAnnotDecl pass]
              , group_kisigs :: [LStandaloneKindSig pass]
              , group_instds :: [LInstDecl pass] }
  | XTyClGroup !(XXTyClGroup pass)


tyClGroupTyClDecls :: [TyClGroup pass] -> [LTyClDecl pass]
tyClGroupTyClDecls = concatMap group_tyclds

tyClGroupInstDecls :: [TyClGroup pass] -> [LInstDecl pass]
tyClGroupInstDecls = concatMap group_instds

tyClGroupRoleDecls :: [TyClGroup pass] -> [LRoleAnnotDecl pass]
tyClGroupRoleDecls = concatMap group_roles

tyClGroupKindSigs :: [TyClGroup pass] -> [LStandaloneKindSig pass]
tyClGroupKindSigs = concatMap group_kisigs


{- *********************************************************************
*                                                                      *
               Data and type family declarations
*                                                                      *
********************************************************************* -}

{- Note [FamilyResultSig]
~~~~~~~~~~~~~~~~~~~~~~~~~

This data type represents the return signature of a type family.  Possible
values are:

 * NoSig - the user supplied no return signature:
      type family Id a where ...

 * KindSig - the user supplied the return kind:
      type family Id a :: * where ...

 * TyVarSig - user named the result with a type variable and possibly
   provided a kind signature for that variable:
      type family Id a = r where ...
      type family Id a = (r :: *) where ...

   Naming result of a type family is required if we want to provide
   injectivity annotation for a type family:
      type family Id a = r | r -> a where ...

See also: Note [Injectivity annotation]

Note [Injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A user can declare a type family to be injective:

   type family Id a = r | r -> a where ...

 * The part after the "|" is called "injectivity annotation".
 * "r -> a" part is called "injectivity condition"; at the moment terms
   "injectivity annotation" and "injectivity condition" are synonymous
   because we only allow a single injectivity condition.
 * "r" is the "LHS of injectivity condition". LHS can only contain the
   variable naming the result of a type family.

 * "a" is the "RHS of injectivity condition". RHS contains space-separated
   type and kind variables representing the arguments of a type
   family. Variables can be omitted if a type family is not injective in
   these arguments. Example:
         type family Foo a b c = d | d -> a c where ...

Note that:
 (a) naming of type family result is required to provide injectivity
     annotation
 (b) for associated types if the result was named then injectivity annotation
     is mandatory. Otherwise result type variable is indistinguishable from
     associated type default.

It is possible that in the future this syntax will be extended to support
more complicated injectivity annotations. For example we could declare that
if we know the result of Plus and one of its arguments we can determine the
other argument:

   type family Plus a b = (r :: Nat) | r a -> b, r b -> a where ...

Here injectivity annotation would consist of two comma-separated injectivity
conditions.

See also Note [Injective type families] in GHC.Core.TyCon
-}

-- | Located type Family Result Signature
type LFamilyResultSig pass = XRec pass (FamilyResultSig pass)

-- | type Family Result Signature
data FamilyResultSig pass = -- see Note [FamilyResultSig]
    NoSig (XNoSig pass)
  -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' :

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

  | KindSig  (XCKindSig pass) (LHsKind pass)
  -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' :
  --             'GHC.Parser.Annotation.AnnOpenP','GHC.Parser.Annotation.AnnDcolon',
  --             'GHC.Parser.Annotation.AnnCloseP'

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

  | TyVarSig (XTyVarSig pass) (LHsTyVarBndr () pass)
  -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' :
  --             'GHC.Parser.Annotation.AnnOpenP','GHC.Parser.Annotation.AnnDcolon',
  --             'GHC.Parser.Annotation.AnnCloseP', 'GHC.Parser.Annotation.AnnEqual'
  | XFamilyResultSig !(XXFamilyResultSig pass)

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation


-- | Located type Family Declaration
type LFamilyDecl pass = XRec pass (FamilyDecl pass)

-- | type Family Declaration
data FamilyDecl pass = FamilyDecl
  { fdExt            :: XCFamilyDecl pass
  , fdInfo           :: FamilyInfo pass              -- type/data, closed/open
  , fdTopLevel       :: TopLevelFlag                 -- used for printing only
  , fdLName          :: LIdP pass                    -- type constructor
  , fdTyVars         :: LHsQTyVars pass              -- type variables
                       -- See Note [TyVar binders for associated declarations]
  , fdFixity         :: LexicalFixity                -- Fixity used in the declaration
  , fdResultSig      :: LFamilyResultSig pass        -- result signature
  , fdInjectivityAnn :: Maybe (LInjectivityAnn pass) -- optional injectivity ann
  }
  | XFamilyDecl !(XXFamilyDecl pass)
  -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnType',
  --             'GHC.Parser.Annotation.AnnData', 'GHC.Parser.Annotation.AnnFamily',
  --             'GHC.Parser.Annotation.AnnWhere', 'GHC.Parser.Annotation.AnnOpenP',
  --             'GHC.Parser.Annotation.AnnDcolon', 'GHC.Parser.Annotation.AnnCloseP',
  --             'GHC.Parser.Annotation.AnnEqual', 'GHC.Parser.Annotation.AnnRarrow',
  --             'GHC.Parser.Annotation.AnnVbar'

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation


-- | Located Injectivity Annotation
type LInjectivityAnn pass = XRec pass (InjectivityAnn pass)

-- | If the user supplied an injectivity annotation it is represented using
-- InjectivityAnn. At the moment this is a single injectivity condition - see
-- Note [Injectivity annotation]. `Located name` stores the LHS of injectivity
-- condition. `[Located name]` stores the RHS of injectivity condition. Example:
--
--   type family Foo a b c = r | r -> a c where ...
--
-- This will be represented as "InjectivityAnn `r` [`a`, `c`]"
data InjectivityAnn pass
  = InjectivityAnn (XCInjectivityAnn pass)
                   (LIdP pass) [LIdP pass]
  -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' :
  --             'GHC.Parser.Annotation.AnnRarrow', 'GHC.Parser.Annotation.AnnVbar'

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | XInjectivityAnn !(XXInjectivityAnn pass)

data FamilyInfo pass
  = DataFamily
  | OpenTypeFamily
     -- | 'Nothing' if we're in an hs-boot file and the user
     -- said "type family Foo x where .."
  | ClosedTypeFamily (Maybe [LTyFamInstEqn pass])


------------- Pretty printing FamilyDecls -----------

pprFlavour :: FamilyInfo pass -> SDoc
pprFlavour DataFamily            = text "data"
pprFlavour OpenTypeFamily        = text "type"
pprFlavour (ClosedTypeFamily {}) = text "type"

instance Outputable (FamilyInfo pass) where
  ppr info = pprFlavour info <+> text "family"



{- *********************************************************************
*                                                                      *
               Data types and data constructors
*                                                                      *
********************************************************************* -}

-- | Haskell Data type Definition
data HsDataDefn pass   -- The payload of a data type defn
                       -- Used *both* for vanilla data declarations,
                       --       *and* for data family instances
  = -- | Declares a data type or newtype, giving its constructors
    -- @
    --  data/newtype T a = <constrs>
    --  data/newtype instance T [a] = <constrs>
    -- @
    HsDataDefn { dd_ext    :: XCHsDataDefn pass,
                 dd_ND     :: NewOrData,
                 dd_ctxt   :: Maybe (LHsContext pass), -- ^ Context
                 dd_cType  :: Maybe (XRec pass CType),
                 dd_kindSig:: Maybe (LHsKind pass),
                     -- ^ Optional kind signature.
                     --
                     -- @(Just k)@ for a GADT-style @data@,
                     -- or @data instance@ decl, with explicit kind sig
                     --
                     -- Always @Nothing@ for H98-syntax decls

                 dd_cons   :: [LConDecl pass],
                     -- ^ Data constructors
                     --
                     -- For @data T a = T1 | T2 a@
                     --   the 'LConDecl's all have 'ConDeclH98'.
                     -- For @data T a where { T1 :: T a }@
                     --   the 'LConDecls' all have 'ConDeclGADT'.

                 dd_derivs :: HsDeriving pass  -- ^ Optional 'deriving' clause

             -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
   }
  | XHsDataDefn !(XXHsDataDefn pass)

-- | Haskell Deriving clause
type HsDeriving pass = [LHsDerivingClause pass]
  -- ^ The optional @deriving@ clauses of a data declaration. "Clauses" is
  -- plural because one can specify multiple deriving clauses using the
  -- @-XDerivingStrategies@ language extension.
  --
  -- The list of 'LHsDerivingClause's corresponds to exactly what the user
  -- requested to derive, in order. If no deriving clauses were specified,
  -- the list is empty.

type LHsDerivingClause pass = XRec pass (HsDerivingClause pass)

-- | A single @deriving@ clause of a data declaration.
--
--  - 'GHC.Parser.Annotation.AnnKeywordId' :
--       'GHC.Parser.Annotation.AnnDeriving', 'GHC.Parser.Annotation.AnnStock',
--       'GHC.Parser.Annotation.AnnAnyClass', 'GHC.Parser.Annotation.AnnNewtype',
--       'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnClose'
data HsDerivingClause pass
  -- See Note [Deriving strategies] in GHC.Tc.Deriv
  = HsDerivingClause
    { deriv_clause_ext :: XCHsDerivingClause pass
    , deriv_clause_strategy :: Maybe (LDerivStrategy pass)
      -- ^ The user-specified strategy (if any) to use when deriving
      -- 'deriv_clause_tys'.
    , deriv_clause_tys :: LDerivClauseTys pass
      -- ^ The types to derive.
    }
  | XHsDerivingClause !(XXHsDerivingClause pass)

type LDerivClauseTys pass = XRec pass (DerivClauseTys pass)

-- | The types mentioned in a single @deriving@ clause. This can come in two
-- forms, 'DctSingle' or 'DctMulti', depending on whether the types are
-- surrounded by enclosing parentheses or not. These parentheses are
-- semantically different than 'HsParTy'. For example, @deriving ()@ means
-- \"derive zero classes\" rather than \"derive an instance of the 0-tuple\".
--
-- 'DerivClauseTys' use 'LHsSigType' because @deriving@ clauses can mention
-- type variables that aren't bound by the datatype, e.g.
--
-- > data T b = ... deriving (C [a])
--
-- should produce a derived instance for @C [a] (T b)@.
data DerivClauseTys pass
  = -- | A @deriving@ clause with a single type. Moreover, that type can only
    -- be a type constructor without any arguments.
    --
    -- Example: @deriving Eq@
    DctSingle (XDctSingle pass) (LHsSigType pass)

    -- | A @deriving@ clause with a comma-separated list of types, surrounded
    -- by enclosing parentheses.
    --
    -- Example: @deriving (Eq, C a)@
  | DctMulti (XDctMulti pass) [LHsSigType pass]

  | XDerivClauseTys !(XXDerivClauseTys pass)

-- | Located Standalone Kind Signature
type LStandaloneKindSig pass = XRec pass (StandaloneKindSig pass)

data StandaloneKindSig pass
  = StandaloneKindSig (XStandaloneKindSig pass)
      (LIdP pass)           -- Why a single binder? See #16754
      (LHsSigType pass)     -- Why not LHsSigWcType? See Note [Wildcards in standalone kind signatures]
  | XStandaloneKindSig !(XXStandaloneKindSig pass)

{- Note [Wildcards in standalone kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Standalone kind signatures enable polymorphic recursion, and it is unclear how
to reconcile this with partial type signatures, so we disallow wildcards in
them.

We reject wildcards in 'rnStandaloneKindSignature' by returning False for
'StandaloneKindSigCtx' in 'wildCardsAllowed'.

The alternative design is to have special treatment for partial standalone kind
signatures, much like we have special treatment for partial type signatures in
terms. However, partial standalone kind signatures are not a proper replacement
for CUSKs, so this would be a separate feature.
-}

data NewOrData
  = NewType                     -- ^ @newtype Blah ...@
  | DataType                    -- ^ @data Blah ...@
  deriving( Eq, Data )                -- Needed because Demand derives Eq

-- | Convert a 'NewOrData' to a 'TyConFlavour'
newOrDataToFlavour :: NewOrData -> TyConFlavour
newOrDataToFlavour NewType  = NewtypeFlavour
newOrDataToFlavour DataType = DataTypeFlavour


-- | Located data Constructor Declaration
type LConDecl pass = XRec pass (ConDecl pass)
      -- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnSemi' when
      --   in a GADT constructor list

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

-- |
--
-- @
-- data T b = forall a. Eq a => MkT a b
--   MkT :: forall b a. Eq a => MkT a b
--
-- data T b where
--      MkT1 :: Int -> T Int
--
-- data T = Int `MkT` Int
--        | MkT2
--
-- data T a where
--      Int `MkT` Int :: T Int
-- @
--
-- - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnOpen',
--            'GHC.Parser.Annotation.AnnDotdot','GHC.Parser.Annotation.AnnCLose',
--            'GHC.Parser.Annotation.AnnEqual','GHC.Parser.Annotation.AnnVbar',
--            'GHC.Parser.Annotation.AnnDarrow','GHC.Parser.Annotation.AnnDarrow',
--            'GHC.Parser.Annotation.AnnForall','GHC.Parser.Annotation.AnnDot'

-- For details on above see note [exact print annotations] in GHC.Parser.Annotation

-- | data Constructor Declaration
data ConDecl pass
  = ConDeclGADT
      { con_g_ext   :: XConDeclGADT pass
      , con_names   :: [LIdP pass]

      -- The following fields describe the type after the '::'
      -- See Note [GADT abstract syntax]
      , con_bndrs   :: XRec pass (HsOuterSigTyVarBndrs pass)
        -- ^ The outermost type variable binders, be they explicit or
        --   implicit.  The 'XRec' is used to anchor exact print
        --   annotations, AnnForall and AnnDot.
      , con_mb_cxt  :: Maybe (LHsContext pass)   -- ^ User-written context (if any)
      , con_g_args  :: HsConDeclGADTDetails pass -- ^ Arguments; never infix
      , con_res_ty  :: LHsType pass              -- ^ Result type

      , con_doc     :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }

  | ConDeclH98
      { con_ext     :: XConDeclH98 pass
      , con_name    :: LIdP pass

      , con_forall  :: Bool
                              -- ^ True <=> explicit user-written forall
                              --     e.g. data T a = forall b. MkT b (b->a)
                              --     con_ex_tvs = {b}
                              -- False => con_ex_tvs is empty
      , con_ex_tvs :: [LHsTyVarBndr Specificity pass] -- ^ Existentials only
      , con_mb_cxt :: Maybe (LHsContext pass)         -- ^ User-written context (if any)
      , con_args   :: HsConDeclH98Details pass        -- ^ Arguments; can be infix

      , con_doc       :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }
  | XConDecl !(XXConDecl pass)

{- Note [GADT abstract syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The types of both forms of GADT constructors are very structured, as they
must consist of the quantified type variables (if provided), followed by the
context (if provided), followed by the argument types (if provided), followed
by the result type. (See "Wrinkle: No nested foralls or contexts" below for
more discussion on the restrictions imposed here.) As a result, instead of
storing the type of a GADT constructor as a single LHsType, we split it up
into its constituent components for easier access.

There are two broad ways to classify GADT constructors:

* Record-syntax constructors. For example:

    data T a where
      K :: forall a. Ord a => { x :: [a], ... } -> T a

* Prefix constructors, which do not use record syntax. For example:

    data T a where
      K :: forall a. Ord a => [a] -> ... -> T a

This distinction is recorded in the `con_args :: HsConDetails pass`, which
tracks if we're dealing with a RecCon or PrefixCon. It is easy to distinguish
the two in the AST since record GADT constructors use HsRecTy. This distinction
is made in GHC.Parser.PostProcess.mkGadtDecl.

It is worth elaborating a bit more on the process of splitting the argument
types of a GADT constructor, since there are some non-obvious details involved.
While splitting the argument types of a record GADT constructor is easy (they
are stored in an HsRecTy), splitting the arguments of a prefix GADT constructor
is trickier. The basic idea is that we must split along the outermost function
arrows ((->) and (%1 ->)) in the type, which GHC.Hs.Type.splitHsFunType
accomplishes. But what about type operators? Consider:

  C :: a :*: b -> a :*: b -> a :+: b

This could parse in many different ways depending on the precedences of each
type operator. In particular, if (:*:) were to have lower precedence than (->),
then it could very well parse like this:

  a :*: ((b -> a) :*: ((b -> a) :+: b)))

This would give the false impression that the whole type is part of one large
return type, with no arguments. Note that we do not fully resolve the exact
precedences of each user-defined type operator until the renamer, so this a
more difficult task for the parser.

Fortunately, there is no risk of the above happening. GHC's parser gives
special treatment to function arrows, and as a result, they are always parsed
with a lower precedence than any other type operator. As a result, the type
above is actually parsed like this:

  (a :*: b) -> ((a :*: b) -> (a :+: b))

While we won't know the exact precedences of (:*:) and (:+:) until the renamer,
all we are concerned about in the parser is identifying the overall shape of
the argument and result types, which we can accomplish by piggybacking on the
special treatment given to function arrows. In a future where function arrows
aren't given special status in the parser, we will likely have to modify
GHC.Parser.PostProcess.mkHsOpTyPV to preserve this trick.

-----
-- Wrinkle: No nested foralls or contexts
-----

GADT constructors provide some freedom to change the order of foralls in their
types (see Note [DataCon user type variable binders] in GHC.Core.DataCon), but
this freedom is still limited. GADTs still require that all quantification
occurs "prenex". That is, any explicitly quantified type variables must occur
at the front of the GADT type, followed by any contexts, followed by the body of
the GADT type, in precisely that order. For instance:

  data T where
    MkT1 :: forall a b. (Eq a, Eq b) => a -> b -> T
      -- OK
    MkT2 :: forall a. Eq a => forall b. a -> b -> T
      -- Rejected, `forall b` is nested
    MkT3 :: forall a b. Eq a => Eq b => a -> b -> T
      -- Rejected, `Eq b` is nested
    MkT4 :: Int -> forall a. a -> T
      -- Rejected, `forall a` is nested
    MkT5 :: forall a. Int -> Eq a => a -> T
      -- Rejected, `Eq a` is nested
    MkT6 :: (forall a. a -> T)
      -- Rejected, `forall a` is nested due to the surrounding parentheses
    MkT7 :: (Eq a => a -> t)
      -- Rejected, `Eq a` is nested due to the surrounding parentheses

For the full details, see the "Formal syntax for GADTs" section of the GHC
User's Guide. GHC enforces that GADT constructors do not have nested `forall`s
or contexts in two parts:

1. GHC, in the process of splitting apart a GADT's type,
   extracts out the leading `forall` and context (if they are provided). To
   accomplish this splitting, the renamer uses the
   GHC.Hs.Type.splitLHsGADTPrefixTy function, which is careful not to remove
   parentheses surrounding the leading `forall` or context (as these
   parentheses can be syntactically significant). If the third result returned
   by splitLHsGADTPrefixTy contains any `forall`s or contexts, then they must
   be nested, so they will be rejected.

   Note that this step applies to both prefix and record GADTs alike, as they
   both have syntax which permits `forall`s and contexts. The difference is
   where this step happens:

   * For prefix GADTs, this happens in the renamer (in rnConDecl), as we cannot
     split until after the type operator fixities have been resolved.
   * For record GADTs, this happens in the parser (in mkGadtDecl).
2. If the GADT type is prefix, the renamer (in the ConDeclGADTPrefixPs case of
   rnConDecl) will then check for nested `forall`s/contexts in the body of a
   prefix GADT type, after it has determined what all of the argument types are.
   This step is necessary to catch examples like MkT4 above, where the nested
   quantification occurs after a visible argument type.
-}

-- | The arguments in a Haskell98-style data constructor.
type HsConDeclH98Details pass
   = HsConDetails Void (HsScaled pass (LBangType pass)) (XRec pass [LConDeclField pass])
-- The Void argument to HsConDetails here is a reflection of the fact that
-- type applications are not allowed in data constructor declarations.

-- | The arguments in a GADT constructor. Unlike Haskell98-style constructors,
-- GADT constructors cannot be declared with infix syntax. As a result, we do
-- not use 'HsConDetails' here, as 'InfixCon' would be an unrepresentable
-- state. (There is a notion of infix GADT constructors for the purposes of
-- derived Show instances—see Note [Infix GADT constructors] in
-- GHC.Tc.TyCl—but that is an orthogonal concern.)
data HsConDeclGADTDetails pass
   = PrefixConGADT [HsScaled pass (LBangType pass)]
   | RecConGADT (XRec pass [LConDeclField pass])

instance Outputable NewOrData where
  ppr NewType  = text "newtype"
  ppr DataType = text "data"

{-
************************************************************************
*                                                                      *
                Instance declarations
*                                                                      *
************************************************************************

Note [Type family instance declarations in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type FamEqn represents one equation of a type family instance.
Aside from the pass, it is also parameterised over another field, feqn_rhs.
feqn_rhs is either an HsDataDefn (for data family instances) or an LHsType
(for type family instances).

Type family instances also include associated type family default equations.
That is because a default for a type family looks like this:

  class C a where
    type family F a b :: Type
    type F c d = (c,d)   -- Default instance

The default declaration is really just a `type instance` declaration, but one
with particularly simple patterns: they must all be distinct type variables.
That's because we will instantiate it (in an instance declaration for `C`) if
we don't give an explicit instance for `F`. Note that the names of the
variables don't need to match those of the class: it really is like a
free-standing `type instance` declaration.
-}

----------------- Type synonym family instances -------------

-- | Located Type Family Instance Equation
type LTyFamInstEqn pass = XRec pass (TyFamInstEqn pass)
  -- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnSemi'
  --   when in a list

-- For details on above see note [exact print annotations] in GHC.Parser.Annotation

-- | Haskell Type Patterns
type HsTyPats pass = [LHsTypeArg pass]

{- Note [Family instance declaration binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The feqn_pats field of FamEqn (family instance equation) stores the LHS type
(and kind) patterns. Any type (and kind) variables contained
in these type patterns are bound in the feqn_bndrs field.
Note that in particular:

* The feqn_bndrs *include* any anonymous wildcards.  For example
     type instance F a _ = a
  The feqn_bndrs will be HsOuterImplicit {a, _}.  Remember that each separate
  wildcard '_' gets its own unique.  In this context wildcards behave just like
  an ordinary type variable, only anonymous.

* The feqn_bndrs *include* type variables that are already in scope

   Eg   class C s t where
          type F t p :: *
        instance C w (a,b) where
          type F (a,b) x = x->a
   The feqn_bndrs of the F decl is HsOuterImplicit {a,b,x}, even though the
   F decl is nested inside the 'instance' decl.

   However after the renamer, the uniques will match up:
        instance C w7 (a8,b9) where
          type F (a8,b9) x10 = x10->a8
   so that we can compare the type pattern in the 'instance' decl and
   in the associated 'type' decl

c.f. Note [TyVar binders for associated decls]
-}

-- | Type Family Instance Equation
type TyFamInstEqn pass = FamEqn pass (LHsType pass)
            -- Here, the @pats@ are type patterns (with kind and type bndrs).
            -- See Note [Family instance declaration binders]

-- | Type family default declarations.
-- A convenient synonym for 'TyFamInstDecl'.
-- See @Note [Type family instance declarations in HsSyn]@.
type TyFamDefltDecl = TyFamInstDecl

-- | Located type family default declarations.
type LTyFamDefltDecl pass = XRec pass (TyFamDefltDecl pass)

-- | Located Type Family Instance Declaration
type LTyFamInstDecl pass = XRec pass (TyFamInstDecl pass)

-- | Type Family Instance Declaration
data TyFamInstDecl pass
  = TyFamInstDecl { tfid_xtn :: XCTyFamInstDecl pass
                  , tfid_eqn :: TyFamInstEqn pass }
    -- ^
    --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnType',
    --           'GHC.Parser.Annotation.AnnInstance',

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | XTyFamInstDecl !(XXTyFamInstDecl pass)

----------------- Data family instances -------------

-- | Located Data Family Instance Declaration
type LDataFamInstDecl pass = XRec pass (DataFamInstDecl pass)

-- | Data Family Instance Declaration
newtype DataFamInstDecl pass
  = DataFamInstDecl { dfid_eqn :: FamEqn pass (HsDataDefn pass) }
    -- ^
    --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnData',
    --           'GHC.Parser.Annotation.AnnNewType','GHC.Parser.Annotation.AnnInstance',
    --           'GHC.Parser.Annotation.AnnDcolon'
    --           'GHC.Parser.Annotation.AnnWhere','GHC.Parser.Annotation.AnnOpen',
    --           'GHC.Parser.Annotation.AnnClose'

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

----------------- Family instances (common types) -------------

-- | Family Equation
--
-- One equation in a type family instance declaration, data family instance
-- declaration, or type family default.
-- See Note [Type family instance declarations in HsSyn]
-- See Note [Family instance declaration binders]
data FamEqn pass rhs
  = FamEqn
       { feqn_ext    :: XCFamEqn pass rhs
       , feqn_tycon  :: LIdP pass
       , feqn_bndrs  :: HsOuterFamEqnTyVarBndrs pass -- ^ Optional quantified type vars
       , feqn_pats   :: HsTyPats pass
       , feqn_fixity :: LexicalFixity -- ^ Fixity used in the declaration
       , feqn_rhs    :: rhs
       }
    -- ^
    --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnEqual'
  | XFamEqn !(XXFamEqn pass rhs)

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

----------------- Class instances -------------

-- | Located Class Instance Declaration
type LClsInstDecl pass = XRec pass (ClsInstDecl pass)

-- | Class Instance Declaration
data ClsInstDecl pass
  = ClsInstDecl
      { cid_ext     :: XCClsInstDecl pass
      , cid_poly_ty :: LHsSigType pass    -- Context => Class Instance-type
                                          -- Using a polytype means that the renamer conveniently
                                          -- figures out the quantified type variables for us.
      , cid_binds         :: LHsBinds pass       -- Class methods
      , cid_sigs          :: [LSig pass]         -- User-supplied pragmatic info
      , cid_tyfam_insts   :: [LTyFamInstDecl pass]   -- Type family instances
      , cid_datafam_insts :: [LDataFamInstDecl pass] -- Data family instances
      , cid_overlap_mode  :: Maybe (XRec pass OverlapMode)
         -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
         --                                    'GHC.Parser.Annotation.AnnClose',

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
      }
    -- ^
    --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnInstance',
    --           'GHC.Parser.Annotation.AnnWhere',
    --           'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnClose',

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | XClsInstDecl !(XXClsInstDecl pass)

----------------- Instances of all kinds -------------

-- | Located Instance Declaration
type LInstDecl pass = XRec pass (InstDecl pass)

-- | Instance Declaration
data InstDecl pass  -- Both class and family instances
  = ClsInstD
      { cid_d_ext :: XClsInstD pass
      , cid_inst  :: ClsInstDecl pass }
  | DataFamInstD              -- data family instance
      { dfid_ext  :: XDataFamInstD pass
      , dfid_inst :: DataFamInstDecl pass }
  | TyFamInstD              -- type family instance
      { tfid_ext  :: XTyFamInstD pass
      , tfid_inst :: TyFamInstDecl pass }
  | XInstDecl !(XXInstDecl pass)

{-
************************************************************************
*                                                                      *
\subsection[DerivDecl]{A stand-alone instance deriving declaration}
*                                                                      *
************************************************************************
-}

-- | Located stand-alone 'deriving instance' declaration
type LDerivDecl pass = XRec pass (DerivDecl pass)

-- | Stand-alone 'deriving instance' declaration
data DerivDecl pass = DerivDecl
        { deriv_ext          :: XCDerivDecl pass
        , deriv_type         :: LHsSigWcType pass
          -- ^ The instance type to derive.
          --
          -- It uses an 'LHsSigWcType' because the context is allowed to be a
          -- single wildcard:
          --
          -- > deriving instance _ => Eq (Foo a)
          --
          -- Which signifies that the context should be inferred.

          -- See Note [Inferring the instance context] in GHC.Tc.Deriv.Infer.

        , deriv_strategy     :: Maybe (LDerivStrategy pass)
        , deriv_overlap_mode :: Maybe (XRec pass OverlapMode)
         -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDeriving',
         --        'GHC.Parser.Annotation.AnnInstance', 'GHC.Parser.Annotation.AnnStock',
         --        'GHC.Parser.Annotation.AnnAnyClass', 'GHC.Parser.Annotation.AnnNewtype',
         --        'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnClose'

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
        }
  | XDerivDecl !(XXDerivDecl pass)

{-
************************************************************************
*                                                                      *
                Deriving strategies
*                                                                      *
************************************************************************
-}

-- | A 'Located' 'DerivStrategy'.
type LDerivStrategy pass = XRec pass (DerivStrategy pass)

-- | Which technique the user explicitly requested when deriving an instance.
data DerivStrategy pass
  -- See Note [Deriving strategies] in GHC.Tc.Deriv
  = StockStrategy (XStockStrategy pass)
                     -- ^ GHC's \"standard\" strategy, which is to implement a
                     --   custom instance for the data type. This only works
                     --   for certain types that GHC knows about (e.g., 'Eq',
                     --   'Show', 'Functor' when @-XDeriveFunctor@ is enabled,
                     --   etc.)
  | AnyclassStrategy (XAnyClassStrategy pass) -- ^ @-XDeriveAnyClass@
  | NewtypeStrategy  (XNewtypeStrategy pass)  -- ^ @-XGeneralizedNewtypeDeriving@
  | ViaStrategy (XViaStrategy pass)
                     -- ^ @-XDerivingVia@

-- | A short description of a @DerivStrategy'@.
derivStrategyName :: DerivStrategy a -> SDoc
derivStrategyName = text . go
  where
    go StockStrategy    {} = "stock"
    go AnyclassStrategy {} = "anyclass"
    go NewtypeStrategy  {} = "newtype"
    go ViaStrategy      {} = "via"

{-
************************************************************************
*                                                                      *
\subsection[DefaultDecl]{A @default@ declaration}
*                                                                      *
************************************************************************

There can only be one default declaration per module, but it is hard
for the parser to check that; we pass them all through in the abstract
syntax, and that restriction must be checked in the front end.
-}

-- | Located Default Declaration
type LDefaultDecl pass = XRec pass (DefaultDecl pass)

-- | Default Declaration
data DefaultDecl pass
  = DefaultDecl (XCDefaultDecl pass) [LHsType pass]
        -- ^ - 'GHC.Parser.Annotation.AnnKeywordId's : 'GHC.Parser.Annotation.AnnDefault',
        --          'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnClose'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | XDefaultDecl !(XXDefaultDecl pass)

{-
************************************************************************
*                                                                      *
\subsection{Foreign function interface declaration}
*                                                                      *
************************************************************************
-}

-- foreign declarations are distinguished as to whether they define or use a
-- Haskell name
--
--  * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used

-- | Located Foreign Declaration
type LForeignDecl pass = XRec pass (ForeignDecl pass)

-- | Foreign Declaration
data ForeignDecl pass
  = ForeignImport
      { fd_i_ext  :: XForeignImport pass   -- Post typechecker, rep_ty ~ sig_ty
      , fd_name   :: LIdP pass             -- defines this name
      , fd_sig_ty :: LHsSigType pass       -- sig_ty
      , fd_fi     :: ForeignImport }

  | ForeignExport
      { fd_e_ext  :: XForeignExport pass   -- Post typechecker, rep_ty ~ sig_ty
      , fd_name   :: LIdP pass             -- uses this name
      , fd_sig_ty :: LHsSigType pass       -- sig_ty
      , fd_fe     :: ForeignExport }
        -- ^
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnForeign',
        --           'GHC.Parser.Annotation.AnnImport','GHC.Parser.Annotation.AnnExport',
        --           'GHC.Parser.Annotation.AnnDcolon'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | XForeignDecl !(XXForeignDecl pass)

{-
    In both ForeignImport and ForeignExport:
        sig_ty is the type given in the Haskell code
        rep_ty is the representation for this type, i.e. with newtypes
               coerced away and type functions evaluated.
    Thus if the declaration is valid, then rep_ty will only use types
    such as Int and IO that we know how to make foreign calls with.
-}

-- Specification Of an imported external entity in dependence on the calling
-- convention
--
data ForeignImport = -- import of a C entity
                     --
                     --  * the two strings specifying a header file or library
                     --   may be empty, which indicates the absence of a
                     --   header or object specification (both are not used
                     --   in the case of `CWrapper' and when `CFunction'
                     --   has a dynamic target)
                     --
                     --  * the calling convention is irrelevant for code
                     --   generation in the case of `CLabel', but is needed
                     --   for pretty printing
                     --
                     --  * `Safety' is irrelevant for `CLabel' and `CWrapper'
                     --
                     CImport  (Located CCallConv) -- ccall or stdcall
                              (Located Safety)  -- interruptible, safe or unsafe
                              (Maybe Header)       -- name of C header
                              CImportSpec          -- details of the C entity
                              (Located SourceText) -- original source text for
                                                   -- the C entity
  deriving Data

-- details of an external C entity
--
data CImportSpec = CLabel    CLabelString     -- import address of a C label
                 | CFunction CCallTarget      -- static or dynamic function
                 | CWrapper                   -- wrapper to expose closures
                                              -- (former f.e.d.)
  deriving Data

-- specification of an externally exported entity in dependence on the calling
-- convention
--
data ForeignExport = CExport  (Located CExportSpec) -- contains the calling
                                                    -- convention
                              (Located SourceText)  -- original source text for
                                                    -- the C entity
  deriving Data

-- pretty printing of foreign declarations
--

instance Outputable ForeignImport where
  ppr (CImport  cconv safety mHeader spec (L _ srcText)) =
    ppr cconv <+> ppr safety
      <+> pprWithSourceText srcText (pprCEntity spec "")
    where
      pp_hdr = case mHeader of
               Nothing -> empty
               Just (Header _ header) -> ftext header

      pprCEntity (CLabel lbl) _ =
        doubleQuotes $ text "static" <+> pp_hdr <+> char '&' <> ppr lbl
      pprCEntity (CFunction (StaticTarget st _lbl _ isFun)) src =
        if dqNeeded then doubleQuotes ce else empty
          where
            dqNeeded = (take 6 src == "static")
                    || isJust mHeader
                    || not isFun
                    || st /= NoSourceText
            ce =
                  -- We may need to drop leading spaces first
                  (if take 6 src == "static" then text "static" else empty)
              <+> pp_hdr
              <+> (if isFun then empty else text "value")
              <+> (pprWithSourceText st empty)
      pprCEntity (CFunction DynamicTarget) _ =
        doubleQuotes $ text "dynamic"
      pprCEntity CWrapper _ = doubleQuotes $ text "wrapper"

instance Outputable ForeignExport where
  ppr (CExport  (L _ (CExportStatic _ lbl cconv)) _) =
    ppr cconv <+> char '"' <> ppr lbl <> char '"'

{-
************************************************************************
*                                                                      *
\subsection{Rewrite rules}
*                                                                      *
************************************************************************
-}

-- | Located Rule Declarations
type LRuleDecls pass = XRec pass (RuleDecls pass)

  -- Note [Pragma source text] in GHC.Types.SourceText
-- | Rule Declarations
data RuleDecls pass = HsRules { rds_ext   :: XCRuleDecls pass
                              , rds_src   :: SourceText
                              , rds_rules :: [LRuleDecl pass] }
  | XRuleDecls !(XXRuleDecls pass)

-- | Located Rule Declaration
type LRuleDecl pass = XRec pass (RuleDecl pass)

-- | Rule Declaration
data RuleDecl pass
  = HsRule -- Source rule
       { rd_ext  :: XHsRule pass
           -- ^ After renamer, free-vars from the LHS and RHS
       , rd_name :: XRec pass (SourceText,RuleName)
           -- ^ Note [Pragma source text] in "GHC.Types.Basic"
       , rd_act  :: Activation
       , rd_tyvs :: Maybe [LHsTyVarBndr () (NoGhcTc pass)]
           -- ^ Forall'd type vars
       , rd_tmvs :: [LRuleBndr pass]
           -- ^ Forall'd term vars, before typechecking; after typechecking
           --    this includes all forall'd vars
       , rd_lhs  :: XRec pass (HsExpr pass)
       , rd_rhs  :: XRec pass (HsExpr pass)
       }
    -- ^
    --  - 'GHC.Parser.Annotation.AnnKeywordId' :
    --           'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnTilde',
    --           'GHC.Parser.Annotation.AnnVal',
    --           'GHC.Parser.Annotation.AnnClose',
    --           'GHC.Parser.Annotation.AnnForall','GHC.Parser.Annotation.AnnDot',
    --           'GHC.Parser.Annotation.AnnEqual',
  | XRuleDecl !(XXRuleDecl pass)

data HsRuleRn = HsRuleRn NameSet NameSet -- Free-vars from the LHS and RHS
  deriving Data

-- | Located Rule Binder
type LRuleBndr pass = XRec pass (RuleBndr pass)

-- | Rule Binder
data RuleBndr pass
  = RuleBndr (XCRuleBndr pass)  (LIdP pass)
  | RuleBndrSig (XRuleBndrSig pass) (LIdP pass) (HsPatSigType pass)
  | XRuleBndr !(XXRuleBndr pass)
        -- ^
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
        --     'GHC.Parser.Annotation.AnnDcolon','GHC.Parser.Annotation.AnnClose'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

collectRuleBndrSigTys :: [RuleBndr pass] -> [HsPatSigType pass]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ _ ty <- bndrs]

pprFullRuleName :: Located (SourceText, RuleName) -> SDoc
pprFullRuleName (L _ (st, n)) = pprWithSourceText st (doubleQuotes $ ftext n)

{-
************************************************************************
*                                                                      *
\subsection[DocDecl]{Document comments}
*                                                                      *
************************************************************************
-}

-- | Located Documentation comment Declaration
type LDocDecl pass = XRec pass (DocDecl)

-- | Documentation comment Declaration
data DocDecl
  = DocCommentNext HsDocString
  | DocCommentPrev HsDocString
  | DocCommentNamed String HsDocString
  | DocGroup Int HsDocString
  deriving Data

-- Okay, I need to reconstruct the document comments, but for now:
instance Outputable DocDecl where
  ppr _ = text "<document comment>"

docDeclDoc :: DocDecl -> HsDocString
docDeclDoc (DocCommentNext d) = d
docDeclDoc (DocCommentPrev d) = d
docDeclDoc (DocCommentNamed _ d) = d
docDeclDoc (DocGroup _ d) = d

{-
************************************************************************
*                                                                      *
\subsection[DeprecDecl]{Deprecations}
*                                                                      *
************************************************************************

We use exported entities for things to deprecate.
-}

-- | Located Warning Declarations
type LWarnDecls pass = XRec pass (WarnDecls pass)

 -- Note [Pragma source text] in GHC.Types.SourceText
-- | Warning pragma Declarations
data WarnDecls pass = Warnings { wd_ext      :: XWarnings pass
                               , wd_src      :: SourceText
                               , wd_warnings :: [LWarnDecl pass]
                               }
  | XWarnDecls !(XXWarnDecls pass)

-- | Located Warning pragma Declaration
type LWarnDecl pass = XRec pass (WarnDecl pass)

-- | Warning pragma Declaration
data WarnDecl pass = Warning (XWarning pass) [LIdP pass] WarningTxt
                   | XWarnDecl !(XXWarnDecl pass)

{-
************************************************************************
*                                                                      *
\subsection[AnnDecl]{Annotations}
*                                                                      *
************************************************************************
-}

-- | Located Annotation Declaration
type LAnnDecl pass = XRec pass (AnnDecl pass)

-- | Annotation Declaration
data AnnDecl pass = HsAnnotation
                      (XHsAnnotation pass)
                      SourceText -- Note [Pragma source text] in GHC.Types.SourceText
                      (AnnProvenance pass) (XRec pass (HsExpr pass))
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
      --           'GHC.Parser.Annotation.AnnType'
      --           'GHC.Parser.Annotation.AnnModule'
      --           'GHC.Parser.Annotation.AnnClose'

      -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | XAnnDecl !(XXAnnDecl pass)

-- | Annotation Provenance
data AnnProvenance pass = ValueAnnProvenance (LIdP pass)
                        | TypeAnnProvenance (LIdP pass)
                        | ModuleAnnProvenance
-- deriving instance Functor     AnnProvenance
-- deriving instance Foldable    AnnProvenance
-- deriving instance Traversable AnnProvenance
-- deriving instance (Data pass) => Data (AnnProvenance pass)

annProvenanceName_maybe :: forall p. UnXRec p => AnnProvenance p -> Maybe (IdP p)
annProvenanceName_maybe (ValueAnnProvenance (unXRec @p -> name)) = Just name
annProvenanceName_maybe (TypeAnnProvenance (unXRec @p -> name))  = Just name
annProvenanceName_maybe ModuleAnnProvenance                      = Nothing

{-
************************************************************************
*                                                                      *
\subsection[RoleAnnot]{Role annotations}
*                                                                      *
************************************************************************
-}

-- | Located Role Annotation Declaration
type LRoleAnnotDecl pass = XRec pass (RoleAnnotDecl pass)

-- See #8185 for more info about why role annotations are
-- top-level declarations
-- | Role Annotation Declaration
data RoleAnnotDecl pass
  = RoleAnnotDecl (XCRoleAnnotDecl pass)
                  (LIdP pass)              -- type constructor
                  [XRec pass (Maybe Role)] -- optional annotations
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnType',
      --           'GHC.Parser.Annotation.AnnRole'

      -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | XRoleAnnotDecl !(XXRoleAnnotDecl pass)

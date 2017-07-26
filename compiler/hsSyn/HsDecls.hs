{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Abstract syntax of global declarations.
--
-- Definitions for: @SynDecl@ and @ConDecl@, @ClassDecl@,
-- @InstDecl@, @DefaultDecl@ and @ForeignDecl@.
module HsDecls where

-- friends:
import {-# SOURCE #-}   HsExpr( LHsExpr, HsExpr, HsSplice, pprExpr,
                                pprSpliceDecl )
        -- Because Expr imports Decls via HsBracket

import HsBinds
import HsTypes
import HsDoc
import TyCon
import Name
import BasicTypes
import Coercion
import ForeignCall
import PlaceHolder ( PlaceHolder(..) )
import HsExtension
import NameSet

-- others:
import InstEnv
import Class
import Outputable
import Util
import SrcLoc

import Bag
import Maybes
import Data.Data        hiding (TyCon,Fixity, Infix)

import qualified AST

-- -----------------------------------------------------------------------------
-- * Data Declarations
-- -----------------------------------------------------------------------------


type
  HsDecl pass = AST.Decl (GHC pass)
  -- ^ A Haskell Declaration
pattern
  TyClD ::
    (TyClDecl pass) ->
    HsDecl pass
  -- ^ Type or Class Declaration
pattern
  InstD ::
    (InstDecl pass) ->
    HsDecl pass
  -- ^ Instance declaration
pattern
  DerivD ::
    (DerivDecl pass) ->
    HsDecl pass
  -- ^ Deriving declaration
pattern
  ValD ::
    (HsBind pass) ->
    HsDecl pass
  -- ^ Value declaration
pattern
  SigD ::
    (Sig pass) ->
    HsDecl pass
  -- ^ Signature declaration
pattern
  DefD ::
    (DefaultDecl pass) ->
    HsDecl pass
  -- ^ 'default' declaration
pattern
  ForD ::
    (ForeignDecl pass) ->
    HsDecl pass
  -- ^ Foreign declaration
pattern
  WarningD ::
    (WarnDecls pass) ->
    HsDecl pass
  -- ^ Warning declaration
pattern
  AnnD ::
    (AnnDecl pass) ->
    HsDecl pass
  -- ^ Annotation declaration
pattern
  RuleD ::
    (RuleDecls pass) ->
    HsDecl pass
  -- ^ Rule declaration
pattern
  VectD ::
    (VectDecl pass) ->
    HsDecl pass
  -- ^ Vectorise declaration
pattern
  SpliceD ::
    (SpliceDecl pass) ->
    HsDecl pass
  -- ^ Splice declaration
  -- (Includes quasi-quotes)
pattern
  DocD ::
    (DocDecl) ->
    HsDecl pass
  -- ^ Documentation comment declaration
pattern
  RoleAnnotD ::
    (RoleAnnotDecl pass) ->
    HsDecl pass
  -- ^ Role annotation declaration

pattern
  TyClD a
    = AST.TyClD NoFieldExt a
pattern
  InstD a
    = AST.InstD NoFieldExt a
pattern
  DerivD a
    = AST.DerivD NoFieldExt a
pattern
  ValD a
    = AST.ValD NoFieldExt a
pattern
  SigD a
    = AST.SigD NoFieldExt a
pattern
  DefD a
    = AST.DefD NoFieldExt a
pattern
  ForD a
    = AST.ForD NoFieldExt a
pattern
  WarningD a
    = AST.WarningD NoFieldExt a
pattern
  AnnD a
    = AST.AnnD NoFieldExt a
pattern
  RuleD a
    = AST.RuleD NoFieldExt a
pattern
  VectD a
    = AST.VectD NoFieldExt a
pattern
  SpliceD a
    = AST.SpliceD NoFieldExt a
pattern
  DocD a
    = AST.DocD NoFieldExt a
pattern
  RoleAnnotD a
    = AST.RoleAnnotD NoFieldExt a

{-#
  COMPLETE
    TyClD,
    InstD,
    DerivD,
    ValD,
    SigD,
    DefD,
    ForD,
    WarningD,
    AnnD,
    RuleD,
    VectD,
    SpliceD,
    DocD,
    RoleAnnotD
  #-}

type instance
  AST.XTyClD      (GHC pass) = NoFieldExt
type instance
  AST.XInstD      (GHC pass) = NoFieldExt
type instance
  AST.XDerivD     (GHC pass) = NoFieldExt
type instance
  AST.XValD       (GHC pass) = NoFieldExt
type instance
  AST.XSigD       (GHC pass) = NoFieldExt
type instance
  AST.XDefD       (GHC pass) = NoFieldExt
type instance
  AST.XForD       (GHC pass) = NoFieldExt
type instance
  AST.XWarningD   (GHC pass) = NoFieldExt
type instance
  AST.XAnnD       (GHC pass) = NoFieldExt
type instance
  AST.XRuleD      (GHC pass) = NoFieldExt
type instance
  AST.XVectD      (GHC pass) = NoFieldExt
type instance
  AST.XSpliceD    (GHC pass) = NoFieldExt
type instance
  AST.XDocD       (GHC pass) = NoFieldExt
type instance
  AST.XRoleAnnotD (GHC pass) = NoFieldExt
type instance
  AST.XNewDecl  (GHC pass) = NoConExt

type
  LHsDecl pass = AST.LDecl (GHC pass)
  -- ^ When in a list this may have
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
  --

  -- For details on above see note [Api annotations] in ApiAnnotation

-- -----------------------------------------------------------------------------

-- NB: all top-level fixity decls are contained EITHER
-- EITHER SigDs
-- OR     in the ClassDecls in TyClDs
--
-- The former covers
--      a) data constructors
--      b) class methods (but they can be also done in the
--              signatures of class decls)
--      c) imported functions (that have an IfacSig)
--      d) top level decls
--
-- The latter is for class methods only



type
  HsGroup pass = AST.Group (GHC pass)
  -- ^ Haskell Group
  --
  -- A 'HsDecl' is categorised into a 'HsGroup' before being
  -- fed to the renamer.

pattern
  HsGroup ::
    (HsValBinds pass) ->
    [LSpliceDecl pass] ->
    [TyClGroup pass] ->
    -- A list of mutually-recursive groups;
    -- This includes `InstDecl`s as well;
    -- Parser generates a singleton list;
    -- renamer does dependency analysis

    [LDerivDecl pass] ->
    [LFixitySig pass] ->
                -- Snaffled out of both top-level fixity signatures,
                -- and those in class declarations

    [LDefaultDecl pass] ->
    [LForeignDecl pass] ->
    [LWarnDecls pass] ->
    [LAnnDecl pass] ->
    [LRuleDecls pass] ->
    [LVectDecl pass] ->
    [LDocDecl] ->
    HsGroup pass

pattern
  HsGroup { hs_valds,
            hs_splcds,
            hs_tyclds,
            hs_derivds,
            hs_fixds,
            hs_defds,
            hs_fords,
            hs_warnds,
            hs_annds,
            hs_ruleds,
            hs_vects,
            hs_docs }
    = AST.Group NoFieldExt
            hs_valds
            hs_splcds
            hs_tyclds
            hs_derivds
            hs_fixds
            hs_defds
            hs_fords
            hs_warnds
            hs_annds
            hs_ruleds
            hs_vects
            hs_docs

{-#
  COMPLETE
    HsGroup
  #-}

type instance
  AST.XGroup    (GHC pass) = NoFieldExt
type instance
  AST.XNewGroup (GHC pass) = NoConExt

type
  LHsGroup pass = AST.LGroup (GHC pass)
-- -----------------------------------------------------------------------------

type
  SpliceDecl pass = AST.SpliceDecl (GHC pass)
  -- ^ Splice Declaration
pattern
  SpliceDecl :: -- Top level splice
    (Located (HsSplice pass)) ->
    SpliceExplicitFlag ->
    SpliceDecl pass

pattern
  SpliceDecl a b
    = AST.SpliceDecl NoFieldExt a b

{-#
  COMPLETE
    SpliceDecl
  #-}

type instance
  AST.XSpliceDecl    (GHC pass) = NoFieldExt
type instance
  AST.XNewSpliceDecl (GHC pass) = NoConExt


type
  LSpliceDecl pass = AST.LSpliceDecl (GHC pass)
  -- ^ Located Splice Declaration


-- -----------------------------------------------------------------------------

type
  TyClDecl pass = AST.TyClDecl (GHC pass)
  -- ^ A type or class declaration.
pattern
  FamDecl ::
    (FamilyDecl pass) ->
    TyClDecl pass
  -- ^ @type/data family T :: *->*@
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
  --             'ApiAnnotation.AnnData',
  --             'ApiAnnotation.AnnFamily','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpenP',
  --             'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnCloseP',
  --             'ApiAnnotation.AnnEqual','ApiAnnotation.AnnRarrow',
  --             'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation

pattern
  SynDecl ::
    (LIdP pass) ->
    -- ^ Type constructor
    (LHsQTyVars pass) ->
    -- ^ Type variables; for an
    -- associated type these
    -- include outer binders
    (LexicalFixity) ->
    -- ^ Fixity used in the declaration
    (LHsType pass) ->
    -- ^ RHS of type declaration
    (PostRn pass NameSet) ->
    TyClDecl pass
  -- ^ @type@ declaration
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
  --             'ApiAnnotation.AnnEqual',

  -- For details on above see note [Api annotations] in ApiAnnotation

pattern
  DataDecl ::
    (LIdP pass) ->
    -- ^ Type constructor
    (LHsQTyVars pass) ->
    -- ^ Type variables; for an
    -- associated type
    --   these include outer binders
    -- Eg  class T a where
    --       type F a :: *
    --       type F a = a -> a
    -- Here the type decl for 'f'
    -- includes 'a' in its tcdTyVars
    (LexicalFixity) ->
    -- ^ Fixity used in the declaration
    (HsDataDefn pass) ->
    (PostRn pass Bool) ->
    -- ^ does this have a CUSK?
    (PostRn pass NameSet) ->
    TyClDecl pass
  -- ^ @data@ declaration
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
  --              'ApiAnnotation.AnnFamily',
  --              'ApiAnnotation.AnnNewType',
  --              'ApiAnnotation.AnnNewType','ApiAnnotation.AnnDcolon'
  --              'ApiAnnotation.AnnWhere',

  -- For details on above see note [Api annotations] in ApiAnnotation

pattern
  ClassDecl ::
    (LHsContext pass) ->
    -- ^ Context...
    (LIdP pass) ->
    -- ^ Name of the class
    (LHsQTyVars pass) ->
    -- ^ Class type variables
    (LexicalFixity) ->
    -- ^ Fixity used in the declaration
    ([Located (FunDep (LIdP pass))]) ->
    -- ^ Functional deps
    ([LSig pass]) ->
    -- ^ Methods' signatures
    (LHsBinds pass) ->
    -- ^ Default methods
    ([LFamilyDecl pass]) ->
    -- ^ Associated types;
    ([LTyFamDefltEqn pass]) ->
    -- ^ Associated type defaults
    ([LDocDecl]) ->
    -- ^ Haddock docs
    (PostRn pass NameSet) ->
    TyClDecl pass
  -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnClass',
  --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
  --           'ApiAnnotation.AnnClose'
  --   - The tcdFDs will have 'ApiAnnotation.AnnVbar',
  --                          'ApiAnnotation.AnnComma'
  --                          'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation

pattern
  FamDecl { tcdFam }
    = AST.FamDecl NoFieldExt tcdFam
pattern
  SynDecl { tcdLNameS,
            tcdTyVarsS,
            tcdFixityS,
            tcdRhs,
            tcdFVsS }
    = AST.SynDecl tcdFVsS
            tcdLNameS
            tcdTyVarsS
            tcdFixityS
            tcdRhs
pattern
  DataDecl { tcdLNameD,
             tcdTyVarsD,
             tcdFixityD,
             tcdDataDefn,
             tcdDataCusk,
             tcdFVsD }
    = AST.DataDecl (tcdDataCusk, tcdFVsD)
             tcdLNameD
             tcdTyVarsD
             tcdFixityD
             tcdDataDefn
pattern
  ClassDecl { tcdCtxt,
              tcdLNameC,
              tcdTyVarsC,
              tcdFixityC,
              tcdFDs,
              tcdSigs,
              tcdMeths,
              tcdATs,
              tcdATDefs,
              tcdDocs,
              tcdFVsC }

    = AST.ClassDecl tcdFVsC
              tcdCtxt
              tcdLNameC
              tcdTyVarsC
              tcdFixityC
              tcdFDs
              tcdSigs
              tcdMeths
              tcdATs
              tcdATDefs
              tcdDocs

gettcdLName :: TyClDecl pass -> LIdP pass
gettcdLName m@SynDecl{}   = tcdLNameS m
gettcdLName m@DataDecl{}  = tcdLNameD m
gettcdLName m@ClassDecl{} = tcdLNameC m
gettcdLName _             = error "field name applied to bad con"


gettcdTyVars :: TyClDecl pass -> LHsQTyVars pass
gettcdTyVars m@SynDecl{}   = tcdTyVarsS m
gettcdTyVars m@DataDecl{}  = tcdTyVarsD m
gettcdTyVars m@ClassDecl{} = tcdTyVarsC m
gettcdTyVars _             = error "field name applied to bad con"


{-#
  COMPLETE
    FamDecl,
    SynDecl,
    DataDecl,
    ClassDecl
  #-}

type instance
  AST.XFamDecl     (GHC pass) = NoFieldExt
type instance
  AST.XSynDecl     (GHC pass) = PostRn pass NameSet
type instance
  AST.XDataDecl    (GHC pass) = (PostRn pass Bool, PostRn pass NameSet)
type instance
  AST.XClassDecl   (GHC pass) = PostRn pass NameSet
type instance
  AST.XNewTyClDecl (GHC pass) = NoConExt

type
  LTyClDecl pass = AST.LTyClDecl (GHC pass)
  -- ^ Located Declaration of a Type or Class


-- ** TyClGroup
--            Strongly connected components of
--      type, class, instance, and role declarations
-- -----------------------------------------------------------------------------

type
  TyClGroup pass = AST.TyClGroup (GHC pass)
  -- ^ Type or Class Group
  -- See Note [TyClGroups and dependency analysis]
pattern
  TyClGroup ::
    ([LTyClDecl pass]) ->
    ([LRoleAnnotDecl pass]) ->
    ([LInstDecl pass]) ->
    TyClGroup pass

pattern
  TyClGroup
    { group_tyclds
    , group_roles
    , group_instds }
    = AST.TyClGroup
        NoFieldExt
        group_tyclds
        group_roles
        group_instds

{-#
  COMPLETE
    TyClGroup
  #-}

type instance
  AST.XTyClGroup    (GHC pass) = NoFieldExt
type instance
  AST.XNewTyClGroup (GHC pass) = NoConExt

type
  LTyClGroup pass = AST.LTyClGroup (GHC pass)


-- ** Data and type family declarations
-- -----------------------------------------------------------------------------

type
  FamilyResultSig pass = AST.FamilyResultSig (GHC pass)
  -- ^ type Family Result Signature
pattern
  NoSig :: -- see Note [FamilyResultSig]
    FamilyResultSig pass
  -- ^ - 'ApiAnnotation.AnnKeywordId' :

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  KindSig ::
    (LHsKind pass) ->
    FamilyResultSig pass
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnOpenP','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnCloseP'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  TyVarSig ::
    (LHsTyVarBndr pass) ->
    FamilyResultSig pass
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnOpenP','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnCloseP', 'ApiAnnotation.AnnEqual'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  NoSig
    = AST.NoSig NoFieldExt
pattern
  KindSig a
    = AST.KindSigR NoFieldExt a
pattern
  TyVarSig a
    = AST.TyVarSig NoFieldExt a

{-#
  COMPLETE
    NoSig,
    KindSig,
    TyVarSig
  #-}

type instance
  AST.XNoSig              (GHC pass) = NoFieldExt
type instance
  AST.XKindSigR           (GHC pass) = NoFieldExt
type instance
  AST.XTyVarSig           (GHC pass) = NoFieldExt
type instance
  AST.XNewFamilyResultSig (GHC pass) = NoConExt


type
  LFamilyResultSig pass = AST.LFamilyResultSig (GHC pass)
  -- ^ Located type Family Result Signature

-- -----------------------------------------------------------------------------

type
  FamilyDecl pass = AST.FamilyDecl (GHC pass)
  -- ^ type Family Declaration
pattern
  FamilyDecl ::
    (FamilyInfo pass) ->
    -- type/data, closed/open
    (LIdP pass) ->
    -- type constructor
    (LHsQTyVars pass) ->
    -- type variables
    (LexicalFixity) ->
    -- Fixity used in the declaration
    (LFamilyResultSig pass) ->
    -- result signature
    (Maybe (LInjectivityAnn pass)) ->
    -- optional injectivity ann
    FamilyDecl pass
  -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
  --             'ApiAnnotation.AnnData', 'ApiAnnotation.AnnFamily',
  --             'ApiAnnotation.AnnWhere', 'ApiAnnotation.AnnOpenP',
  --             'ApiAnnotation.AnnDcolon', 'ApiAnnotation.AnnCloseP',
  --             'ApiAnnotation.AnnEqual', 'ApiAnnotation.AnnRarrow',
  --             'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation

pattern
  FamilyDecl
   { fdInfo
   , fdLName
   , fdTyVars
   , fdFixity
   , fdResultSig
   , fdInjectivityAnn
   }
    = AST.FamilyDecl NoFieldExt
         fdInfo
         fdLName
         fdTyVars
         fdFixity
         fdResultSig
         fdInjectivityAnn

{-#
  COMPLETE
    FamilyDecl
  #-}

type instance
  AST.XFamilyDecl    (GHC pass) = NoFieldExt
type instance
  AST.XNewFamilyDecl (GHC pass) = NoConExt

type
  LFamilyDecl pass = AST.LFamilyDecl (GHC pass)

-- -----------------------------------------------------------------------------

type
  InjectivityAnn pass = AST.InjectivityAnn (GHC pass)
  -- ^ If the user supplied an injectivity annotation it is represented using
  -- InjectivityAnn. At the moment this is a single injectivity condition - see
  -- Note [Injectivity annotation]. `Located name` stores the LHS of injectivity
  -- condition. `[Located name]` stores the RHS of injectivity condition. Example:
  --
  --   type family Foo a b c = r | r -> a c where ...
  --
  -- This will be represented as "InjectivityAnn `r` [`a`, `c`]"

pattern
  InjectivityAnn ::
    (LIdP pass) ->
    [LIdP pass] ->
    InjectivityAnn pass
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnRarrow', 'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation

pattern
  InjectivityAnn a b
    = AST.InjectivityAnn NoFieldExt a b

{-#
  COMPLETE
    InjectivityAnn
  #-}

type instance
  AST.XInjectivityAnn    (GHC pass) = NoFieldExt
type instance
  AST.XNewInjectivityAnn (GHC pass) = NoConExt

type
  LInjectivityAnn pass = AST.LInjectivityAnn (GHC pass)
  -- ^ Located Injectivity Annotation

-- -----------------------------------------------------------------------------

type
  FamilyInfo pass = AST.FamilyInfo (GHC pass)
pattern
  DataFamily ::
    FamilyInfo pass

pattern
  OpenTypeFamily ::
    FamilyInfo pass

pattern
  ClosedTypeFamily ::
    (Maybe [LTyFamInstEqn pass]) ->
    FamilyInfo pass

pattern
  DataFamily
    = AST.DataFamily NoFieldExt
pattern
  OpenTypeFamily
    = AST.OpenTypeFamily NoFieldExt
  -- ^ 'Nothing' if we're in an hs-boot file and the user
  -- said "type family Foo x where .."
pattern
  ClosedTypeFamily a
    = AST.ClosedTypeFamily NoFieldExt a

{-#
  COMPLETE
    DataFamily,
    OpenTypeFamily,
    ClosedTypeFamily
  #-}

type instance
  AST.XDataFamily       (GHC pass) = NoFieldExt
type instance
  AST.XOpenTypeFamily   (GHC pass) = NoFieldExt
type instance
  AST.XClosedTypeFamily (GHC pass) = NoFieldExt
type instance
  AST.XNewFamilyInfo    (GHC pass) = NoConExt

type
  LFamilyInfo pass = AST.LFamilyInfo (GHC pass)

-- ** Data types and data constructors
-- -----------------------------------------------------------------------------

type
  HsDataDefn pass = AST.DataDefn (GHC pass)
  -- ^ Haskell Data type Definition
  -- The payload of a data type defn
  -- Used *both* for vanilla data declarations,
  --       *and* for data family instances
pattern
  HsDataDefn ::
    (NewOrData) ->
    (LHsContext pass) ->
    -- ^ Context
    (Maybe (Located CType)) ->
    (Maybe (LHsKind pass)) ->
    -- ^ Optional kind signature.
    --
    -- @(Just k)@ for a GADT-style @data@,
    -- or @data instance@ decl, with explicit kind sig
    --
    -- Always @Nothing@ for H98-syntax decls
    ([LConDecl pass]) ->
    -- ^ Data constructors
    --
    -- For @data T a = T1 | T2 a@
    --   the 'LConDecl's all have 'ConDeclH98'.
    -- For @data T a where { T1 :: T a }@
    --   the 'LConDecls' all have 'ConDeclGADT'.
    (HsDeriving pass) ->
    -- ^ Optional 'deriving' claues
    HsDataDefn pass
  -- | Declares a data type or newtype, giving its constructors
  -- @
  --  data/newtype T a = <constrs>
  --  data/newtype instance T [a] = <constrs>
  -- @
pattern
  HsDataDefn { dd_ND
             , dd_ctxt
             , dd_cType
             , dd_kindSig
             , dd_cons
             , dd_derivs
             }
    = AST.DataDefn NoFieldExt
          dd_ND
          dd_ctxt
          dd_cType
          dd_kindSig
          dd_cons
          dd_derivs

{-#
  COMPLETE
    HsDataDefn
  #-}

type instance
  AST.XDataDefn    (GHC pass) = NoFieldExt
type instance
  AST.XNewDataDefn (GHC pass) = NoConExt

type
  LHsDataDefn pass = AST.LDataDefn (GHC pass)
-- -----------------------------------------------------------------------------

type
  HsDeriving pass = AST.Deriving (GHC pass)
  -- ^ Haskell Deriving clause
  -- The optional @deriving@ clauses of a data declaration. "Clauses" is
  -- plural because one can specify multiple deriving clauses using the
  -- @-XDerivingStrategies@ language extension.
  --
  -- The list of 'LHsDerivingClause's corresponds to exactly what the user
  -- requested to derive, in order. If no deriving clauses were specified,
  -- the list is empty.

type
  LHsDeriving pass = AST.LDeriving (GHC pass)

-- -----------------------------------------------------------------------------

type
  HsDerivingClause pass = AST.DerivingClause (GHC pass)
  -- See Note [Deriving strategies] in TcDeriv

  -- ^ A single @deriving@ clause of a data declaration.
  --
  --  - 'ApiAnnotation.AnnKeywordId' :
  --       'ApiAnnotation.AnnDeriving', 'ApiAnnotation.AnnStock',
  --       'ApiAnnotation.AnnAnyClass', 'Api.AnnNewtype',
  --       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

pattern
  HsDerivingClause ::
    (Maybe (Located DerivStrategy)) ->
    (Located [LHsSigType pass]) ->
    HsDerivingClause pass
  -- ^ The user-specified strategy (if any) to use when deriving
  -- 'deriv_clause_tys'.
pattern
  HsDerivingClause
    { deriv_clause_strategy
    , deriv_clause_tys
    }
    = AST.DerivingClause NoFieldExt deriv_clause_strategy deriv_clause_tys
  -- ^ The types to derive.
  --
  -- It uses 'LHsSigType's because, with @-XGeneralizedNewtypeDeriving@,
  -- we can mention type variables that aren't bound by the datatype, e.g.
  --
  -- > data T b = ... deriving (C [a])
  --
  -- should produce a derived instance for @C [a] (T b)@.

{-#
  COMPLETE
    HsDerivingClause
  #-}

type instance
  AST.XDerivingClause    (GHC pass) = NoFieldExt
type instance
  AST.XNewDerivingClause (GHC pass) = NoConExt

type
  LHsDerivingClause pass = AST.LDerivingClause (GHC pass)
-- -----------------------------------------------------------------------------

type
  NewOrData = AST.NewOrData
pattern
  NewType ::
    NewOrData
  -- ^ @newtype Blah ...@
pattern
  DataType ::
    NewOrData
  -- ^ @data Blah ...@
pattern
  NewType
    = AST.NewTypeN
pattern
  DataType
    = AST.DataType

{-#
  COMPLETE
    NewType,
    DataType
  #-}

type
  LNewOrData = AST.LNewOrData
-- -----------------------------------------------------------------------------

type
  ConDecl pass = AST.ConDecl (GHC pass)
  -- ^ data Constructor Declaration
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
  -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
  --            'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnCLose',
  --            'ApiAnnotation.AnnEqual','ApiAnnotation.AnnVbar',
  --            'ApiAnnotation.AnnDarrow','ApiAnnotation.AnnDarrow',
  --            'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  ConDeclGADT ::
    ([LIdP pass]) ->
    (LHsSigType pass) ->
    -- ^ The type after the ‘::’
    (Maybe LHsDocString) ->
    -- ^ A possible Haddock comment.
    ConDecl pass

pattern
  ConDeclH98 ::
    (LIdP pass) ->
    (Maybe (LHsQTyVars pass)) ->
    -- ^ User-written forall (if any), and its implicit
    -- kind variables
    -- Non-Nothing needs -XExistentialQuantification
    --               e.g. data T a = forall b. MkT b (b->a)
    --               con_qvars = {b}
    (Maybe (LHsContext pass)) ->
    -- ^ User-written context (if any)
    (HsConDeclDetails pass) ->
    -- ^ Arguments
    (Maybe LHsDocString) ->
    -- ^ A possible Haddock comment.
    ConDecl pass

pattern
  ConDeclGADT
      { con_names
      , con_type
      , con_docG
      }
    = AST.ConDeclGADT NoFieldExt con_names
         con_type
         con_docG
pattern
  ConDeclH98
      { con_name
      , con_qvars
      , con_cxt
      , con_details
      , con_docA
      }
    = AST.ConDeclH98 NoFieldExt
        con_name
        con_qvars
        con_cxt
        con_details
        con_docA

getConDoc ::  ConDecl pass -> Maybe LHsDocString
getConDoc m@ConDeclGADT{} = con_docG m
getConDoc m@ConDeclH98{}  = con_docA m

setConDoc ::  Maybe LHsDocString -> ConDecl pass -> ConDecl pass
setConDoc d m@ConDeclGADT{} = m {con_docG = d}
setConDoc d m@ConDeclH98{}  = m {con_docA = d}

{-#
  COMPLETE
    ConDeclGADT,
    ConDeclH98
  #-}

type instance
  AST.XConDeclGADT (GHC pass) = NoFieldExt
type instance
  AST.XConDeclH98  (GHC pass) = NoFieldExt
type instance
  AST.XNewConDecl  (GHC pass) = NoConExt

type
  LConDecl pass = AST.LConDecl (GHC pass)
  -- ^ Located data Constructor Declaration
  --   May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when
  --   in a GADT constructor list

  -- For details on above see note [Api annotations] in ApiAnnotation

-- -----------------------------------------------------------------------------

type
  HsConDeclDetails  pass = AST.ConDeclDetails  (GHC pass)
  -- ^ Haskell data Constructor Declaration Details

type
  LHsConDeclDetails pass = AST.LConDeclDetails (GHC pass)

-- ** Instance declarations
-- -----------------------------------------------------------------------------

type
  TyFamInstEqn  pass = AST.TyFamInstEqn  (GHC pass)
  -- ^ Type Family Instance Equation

type
  LTyFamInstEqn pass = AST.LTyFamInstEqn (GHC pass)
  -- ^ Located Type Family Instance Equation
  --  May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
  --   when in a list

  -- For details on above see note [Api annotations] in ApiAnnotation


-- -----------------------------------------------------------------------------

type
  TyFamDefltEqn  pass = AST.TyFamDefltEqn  (GHC pass)
  -- ^ Type Family Default Equation

  -- See Note [Type family instance declarations in HsSyn]
type
  LTyFamDefltEqn pass = AST.LTyFamDefltEqn (GHC pass)
  -- ^ Located Type Family Default Equation

-- -----------------------------------------------------------------------------


type
  HsTyPats pass = AST.TyPats (GHC pass)
  -- ^ Haskell Type Patterns
  -- Type patterns (with kind and type bndrs)
  -- See Note [Family instance declaration binders]
type
  LHsTyPats pass = AST.LTyPats (GHC pass)

-- -----------------------------------------------------------------------------

type
  TyFamEqn pass pats = AST.TyFamEqn (GHC pass) pats
  -- ^ Type Family Equation
  --
  -- One equation in a type family instance declaration
  -- See Note [Type family instance declarations in HsSyn]
pattern
  TyFamEqn ::
    (LIdP pass) ->
    pats ->
    (LexicalFixity) ->
    -- ^ Fixity used in the declaration
    (LHsType pass) ->
    TyFamEqn pass pats
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual'

    -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  TyFamEqn
   { tfe_tycon
   , tfe_pats
   , tfe_fixity
   , tfe_rhs }
    = AST.TyFamEqn NoFieldExt
        tfe_tycon
        tfe_pats
        tfe_fixity
        tfe_rhs

{-#
  COMPLETE
    TyFamEqn
  #-}

type instance
  AST.XTyFamEqn    (GHC pass) pats = NoFieldExt
type instance
  AST.XNewTyFamEqn (GHC pass) pats = NoConExt

type
  LTyFamEqn pass pats = AST.LTyFamEqn (GHC pass) pats
  -- ^ Located Type Family Instance Declaration


-- -----------------------------------------------------------------------------

type
  TyFamInstDecl pass = AST.TyFamInstDecl (GHC pass)
  -- ^ Type Family Instance Declaration

pattern
  TyFamInstDecl ::
    (LTyFamInstEqn pass) ->
    (PostRn pass NameSet) ->
    TyFamInstDecl pass
  -- ^
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
  --           'ApiAnnotation.AnnInstance',

  -- For details on above see note [Api annotations] in ApiAnnotation

pattern
  TyFamInstDecl
    { tfid_eqn
    , tfid_fvs }
    = AST.TyFamInstDecl tfid_fvs tfid_eqn

{-#
  COMPLETE
    TyFamInstDecl
  #-}

type instance
  AST.XTyFamInstDecl    (GHC pass) = PostRn pass NameSet
type instance
  AST.XNewTyFamInstDecl (GHC pass) = NoConExt

type
  LTyFamInstDecl pass = AST.LTyFamInstDecl (GHC pass)

-- ** Data family instances
-- -----------------------------------------------------------------------------

type
  DataFamInstDecl pass = AST.DataFamInstDecl (GHC pass)
  -- ^ Data Family Instance Declaration
pattern
  DataFamInstDecl ::
    (LIdP pass) ->
    (HsTyPats pass) -> -- LHS
    (LexicalFixity) ->
    -- ^ Fixity used in the declaration
    (HsDataDefn pass) ->
    -- RHS
    (PostRn pass NameSet) ->
    DataFamInstDecl pass
    -- Free vars for dependency analysis
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --           'ApiAnnotation.AnnNewType','ApiAnnotation.AnnInstance',
    --           'ApiAnnotation.AnnDcolon'
    --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
    --           'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation

pattern
  DataFamInstDecl
  { dfid_tycon
  , dfid_pats
  , dfid_fixity
  , dfid_defn
  , dfid_fvs }
    = AST.DataFamInstDecl dfid_fvs
         dfid_tycon
         dfid_pats
         dfid_fixity
         dfid_defn

{-#
  COMPLETE
    DataFamInstDecl
  #-}

type instance
  AST.XDataFamInstDecl    (GHC pass) = PostRn pass NameSet
type instance
  AST.XNewDataFamInstDecl (GHC pass) = NoConExt

type
  LDataFamInstDecl pass = AST.LDataFamInstDecl (GHC pass)
  -- ^ Located Data Family Instance Declaration

-- -----------------------------------------------------------------------------

type
  ClsInstDecl pass = AST.ClsInstDecl (GHC pass)
  -- ^ Class Instance Declaration
pattern
  ClsInstDecl ::
    (LHsSigType pass) ->
    -- Context => Class Instance-type
    -- Using a polytype means that the renamer conveniently
    -- figures out the quantified type variables for us.
    (LHsBinds pass) ->
    -- Class methods
    ([LSig pass]) ->
    -- User-supplied pragmatic info
    ([LTyFamInstDecl pass]) ->
    -- Type family instances
    ([LDataFamInstDecl pass]) ->
    -- Data family instances
    (Maybe (Located OverlapMode)) ->
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
    --                                    'ApiAnnotation.AnnClose',

    -- For details on above see note [Api annotations] in ApiAnnotation
    ClsInstDecl pass
  -- ^
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnInstance',
  --           'ApiAnnotation.AnnWhere',
  --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  ClsInstDecl
  { cid_poly_ty
  , cid_binds
  , cid_sigs
  , cid_tyfam_insts
  , cid_datafam_insts
  , cid_overlap_mode }
    = AST.ClsInstDecl NoFieldExt
        cid_poly_ty
        cid_binds
        cid_sigs
        cid_tyfam_insts
        cid_datafam_insts
        cid_overlap_mode

{-#
  COMPLETE
    ClsInstDecl
  #-}

type instance
  AST.XClsInstDecl    (GHC pass) = NoFieldExt
type instance
  AST.XNewClsInstDecl (GHC pass) = NoConExt

type
  LClsInstDecl pass = AST.LClsInstDecl (GHC pass)
  -- ^ Located Class Instance Declaration

-- ** Instances of all kinds
-- -----------------------------------------------------------------------------

type
  InstDecl pass = AST.InstDecl (GHC pass)  -- Both class and family instances
  -- ^ Instance Declaration
pattern
  ClsInstD ::
    (ClsInstDecl pass) ->
    InstDecl pass

pattern
  DataFamInstD :: -- data family instance
    (DataFamInstDecl pass) ->
    InstDecl pass

pattern
  TyFamInstD :: -- type family instance
    (TyFamInstDecl pass) ->
    InstDecl pass

pattern
  ClsInstD { cid_inst }
    = AST.ClsInstD NoFieldExt cid_inst
pattern
  DataFamInstD { dfid_inst }
    = AST.DataFamInstD NoFieldExt dfid_inst
pattern
  TyFamInstD { tfid_inst }
    = AST.TyFamInstD NoFieldExt tfid_inst

{-#
  COMPLETE
    ClsInstD,
    DataFamInstD,
    TyFamInstD
  #-}

type instance
  AST.XClsInstD     (GHC pass) = NoFieldExt
type instance
  AST.XDataFamInstD (GHC pass) = NoFieldExt
type instance
  AST.XTyFamInstD   (GHC pass) = NoFieldExt
type instance
  AST.XNewInstDecl  (GHC pass) = NoConExt

type
  LInstDecl pass = AST.LInstDecl (GHC pass)
  -- ^ Located Instance Declaration

-- ** A stand-alone instance deriving declaratio
-- -----------------------------------------------------------------------------

type
  DerivDecl pass = AST.DerivDecl (GHC pass)
  -- ^ Deriving Declaration
pattern
  DerivDecl ::
    (LHsSigType pass) ->
    (Maybe (Located DerivStrategy)) ->
    (Maybe (Located OverlapMode)) ->
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDeriving',
    --        'ApiAnnotation.AnnInstance', 'ApiAnnotation.AnnStock',
    --        'ApiAnnotation.AnnAnyClass', 'Api.AnnNewtype',
    --        'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation
    DerivDecl pass

pattern
  DerivDecl
   { deriv_type
   , deriv_strategy
   , deriv_overlap_mode }
    = AST.DerivDecl NoFieldExt
        deriv_type
        deriv_strategy
        deriv_overlap_mode

{-#
  COMPLETE
    DerivDecl
  #-}

type instance
  AST.XDerivDecl    (GHC pass) = NoFieldExt
type instance
  AST.XNewDerivDecl (GHC pass) = NoConExt

type
  LDerivDecl pass = AST.LDerivDecl (GHC pass)
  -- ^ Located Deriving Declaration

-- ** A @default@ declaration
-- -----------------------------------------------------------------------------

{-
There can only be one default declaration per module, but it is hard
for the parser to check that; we pass them all through in the abstract
syntax, and that restriction must be checked in the front end.
-}


type
  DefaultDecl pass = AST.DefaultDecl (GHC pass)
  -- ^ Default Declaration
pattern
  DefaultDecl ::
    [LHsType pass] ->
    DefaultDecl pass
  -- ^ - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnDefault',
  --          'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  DefaultDecl a
    = AST.DefaultDecl NoFieldExt a

{-#
  COMPLETE
    DefaultDecl
  #-}

type instance
  AST.XDefaultDecl    (GHC pass) = NoFieldExt
type instance
  AST.XNewDefaultDecl (GHC pass) = NoConExt

type
  LDefaultDecl pass = AST.LDefaultDecl (GHC pass)
  -- ^ Located Default Declaration

-- ** Foreign function interface declaration
-- -----------------------------------------------------------------------------

-- foreign declarations are distinguished as to whether they define or use a
-- Haskell name
--
--  * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used


type
  ForeignDecl pass = AST.ForeignDecl (GHC pass)
  -- ^ Foreign Declaration
pattern
  ForeignImport ::
    (LIdP pass) ->
    -- defines this name
    (LHsSigType pass) ->
     -- sig_ty
    (PostTc pass Coercion) ->
    -- rep_ty ~ sig_ty
    (ForeignImport) ->
    -- rep_ty ~ sig_ty
    ForeignDecl pass

pattern
  ForeignExport ::
    (LIdP pass) ->
    -- uses this name
    (LHsSigType pass) ->
    -- sig_ty
    (PostTc pass Coercion) ->
    (ForeignExport) ->
    ForeignDecl pass
  -- ^
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnForeign',
  --           'ApiAnnotation.AnnImport','ApiAnnotation.AnnExport',
  --           'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  ForeignImport
      { fd_nameI
      , fd_sig_tyI
      , fd_coI
      , fd_fi }
    = AST.ForeignImport
        fd_coI
        fd_nameI
        fd_sig_tyI
        fd_fi
pattern
  ForeignExport
      { fd_nameE
      , fd_sig_tyE
      , fd_coE
      , fd_fe }
    = AST.ForeignExport
        fd_coE
        fd_nameE
        fd_sig_tyE
        fd_fe

{-#
  COMPLETE
    ForeignImport,
    ForeignExport
  #-}

type instance
  AST.XForeignImport  (GHC pass) = PostTc pass Coercion
type instance
  AST.XForeignExport  (GHC pass) = PostTc pass Coercion
type instance
  AST.XNewForeignDecl (GHC pass) = NoConExt

type
  LForeignDecl pass = AST.LForeignDecl (GHC pass)
  -- ^ Located Foreign Declarations

-- -----------------------------------------------------------------------------

-- Specification Of an imported external entity in dependence on the calling
-- convention
--

type
  ForeignImport = AST.ForeignImport
  -- import of a C entity
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
pattern
  CImport ::
    (Located CCallConv) ->
    -- ccall or stdcall
    (Located Safety) ->
    -- interruptible, safe or unsafe
    (Maybe Header) ->
    -- name of C header
    CImportSpec ->
    -- details of the C entity
    (Located SourceText) ->
    -- original source text for
    -- the C entity
    ForeignImport

pattern
  CImport a b c d e
    = AST.CImport a b c d e

{-#
  COMPLETE
    CImport
  #-}

type
  LForeignImport = AST.LForeignImport
-- -----------------------------------------------------------------------------

type
  CImportSpec = AST.CImportSpec
  -- ^ details of an external C entity
pattern
  CLabel ::
    CLabelString ->
    CImportSpec
  -- ^ import address of a C label
pattern
  CFunction ::
    CCallTarget ->
    CImportSpec
  -- ^ static or dynamic function

pattern
  CWrapper ::
    CImportSpec
  -- ^ wrapper to expose closures
  -- (former f.e.d.)

pattern
  CLabel a
    = AST.CLabel  a
pattern
  CFunction a
    = AST.CFunction a
pattern
  CWrapper
    = AST.CWrapper

{-#
  COMPLETE
    CLabel,
    CFunction,
    CWrapper
  #-}


type
  LCImportSpec  = AST.LCImportSpec

-- -----------------------------------------------------------------------------

type
  ForeignExport = AST.ForeignExport
  -- ^ specification of an externally exported entity in dependence on the
  -- calling convention
pattern
  CExport ::
    (Located CExportSpec) ->
    -- contains the calling
    -- convention
    (Located SourceText) ->
    -- original source text for
    -- the C entity
    ForeignExport

pattern
  CExport a b
    = AST.CExport a b

{-#
  COMPLETE
    CExport
  #-}

type
  LForeignExport = AST.LForeignExport

-- ** Transformation rules
-- -----------------------------------------------------------------------------

-- Note [Pragma source text] in BasicTypes

type
  RuleDecls pass = AST.RuleDecls (GHC pass)
  -- ^ Rule Declarations
pattern
  HsRules :: -- Source rule
    (SourceText) ->
    ([LRuleDecl pass]) ->
    RuleDecls pass

pattern
  HsRules
    { rds_src
    , rds_rules }
    = AST.Rules NoFieldExt
      rds_src
      rds_rules

{-#
  COMPLETE
    HsRules
  #-}

type instance
  AST.XRules      (GHC pass) = NoFieldExt
type instance
  AST.XNewRuleDecls (GHC pass) = NoConExt

type
  LRuleDecls pass = AST.LRuleDecls (GHC pass)
  -- ^ Located Rule Declarations

-- -----------------------------------------------------------------------------

type
  RuleDecl pass = AST.RuleDecl (GHC pass)
  -- ^ Rule Declaration
pattern
  HsRule ::
    (Located (SourceText, RuleName)) ->
    -- Rule name
    -- Note [Pragma source text] in BasicTypes
    Activation ->
    [LRuleBndr pass] ->
    -- Forall'd vars; after typechecking this
    --   includes tyvars
    (Located (HsExpr pass)) ->
    -- LHS
    (PostRn pass NameSet) ->
    -- Free-vars from the LHS
    (Located (HsExpr pass)) ->
    -- RHS
    (PostRn pass NameSet) ->
    -- Free-vars from the RHS
    RuleDecl pass
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' :
    --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnTilde',
    --           'ApiAnnotation.AnnVal',
    --           'ApiAnnotation.AnnClose',
    --           'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot',
    --           'ApiAnnotation.AnnEqual',

    -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  HsRule a b c d e f g
    = AST.Rule (e, g) a b c d f

{-#
  COMPLETE
    HsRule
  #-}

type instance
  AST.XRule      (GHC pass) = (PostRn pass NameSet, PostRn pass NameSet)

type instance
  AST.XNewRuleDecl (GHC pass) = NoConExt

type
  LRuleDecl pass = AST.LRuleDecl (GHC pass)
  -- ^ Located Rule Declaration

-- -----------------------------------------------------------------------------

type
  RuleBndr pass = AST.RuleBndr (GHC pass)
  -- | Rule Binder
pattern
  RuleBndr ::
    (LIdP pass) ->
    RuleBndr pass

pattern
  RuleBndrSig ::
    (LIdP pass) ->
    (LHsSigWcType pass) ->
    RuleBndr pass
  -- ^
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --     'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  RuleBndr a
    = AST.RuleBndr NoFieldExt a
pattern
  RuleBndrSig a b
    = AST.RuleBndrSig NoFieldExt a b

{-#
  COMPLETE
    RuleBndr,
    RuleBndrSig
  #-}

type instance
  AST.XRuleBndr    (GHC pass) = NoFieldExt
type instance
  AST.XRuleBndrSig (GHC pass) = NoFieldExt
type instance
  AST.XNewRuleBndr (GHC pass) = NoConExt

type
  LRuleBndr pass = AST.LRuleBndr (GHC pass)
  -- ^ Located Rule Binder

-- ** Vectorisation declarations
-- -----------------------------------------------------------------------------

{-
A vectorisation pragma, one of

  {-# VECTORISE f = closure1 g (scalar_map g) #-}
  {-# VECTORISE SCALAR f #-}
  {-# NOVECTORISE f #-}

  {-# VECTORISE type T = ty #-}
  {-# VECTORISE SCALAR type T #-}
-}


type
  VectDecl pass = AST.VectDecl (GHC pass)
  -- ^ Vectorise Declaration
pattern
  HsVect ::
    SourceText ->
    -- Note [Pragma source text] in BasicTypes
    (LIdP pass) ->
    (LHsExpr pass) ->
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
    --           'ApiAnnotation.AnnEqual','ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation
    VectDecl pass

pattern
  HsNoVect ::
    SourceText ->
    -- Note [Pragma source text] in BasicTypes
    (LIdP pass) ->
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
    --                                    'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation
    VectDecl pass

pattern
  HsVectTypeIn :: -- pre type-checking
    SourceText -> -- Note [Pragma source text] in BasicTypes
    Bool ->       -- 'TRUE' => SCALAR declaration
    (LIdP pass) ->
    (Maybe (LIdP pass)) ->
    -- 'Nothing' => no right-hand side
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
    --           'ApiAnnotation.AnnType','ApiAnnotation.AnnClose',
    --           'ApiAnnotation.AnnEqual'

    -- For details on above see note [Api annotations] in ApiAnnotation
    VectDecl pass

pattern
  HsVectTypeOut ::   -- post type-checking
    Bool ->          -- 'TRUE' => SCALAR declaration
    TyCon ->
    (Maybe TyCon) -> -- 'Nothing' => no right-hand side
    VectDecl pass

pattern
  HsVectClassIn :: -- pre type-checking
    SourceText ->  -- Note [Pragma source text] in BasicTypes
    (LIdP pass) ->
    VectDecl pass
  -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --           'ApiAnnotation.AnnClass','ApiAnnotation.AnnClose',

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  HsVectClassOut :: -- post type-checking
    Class ->
    VectDecl pass

pattern
  HsVectInstIn ::
  -- pre type-checking (always SCALAR)  !!!FIXME: should be superfluous now
    (LHsSigType pass) ->
    VectDecl pass

pattern
  HsVectInstOut ::
  -- post type-checking (always SCALAR) !!!FIXME: should be superfluous now
    ClsInst ->
    VectDecl pass

pattern
  HsVect a b c
    = AST.Vect NoFieldExt a b c
pattern
  HsNoVect a b
    = AST.NoVect NoFieldExt a b
pattern
  HsVectTypeIn a b c d
    = AST.VectType  NoFieldExt a b c d
pattern
  HsVectTypeOut a b c
    = AST.NewVectDecl (NHsVectTypeOut a b c)
pattern
  HsVectClassIn a b
    = AST.VectClass   NoFieldExt a b
pattern
  HsVectClassOut a
    = AST.NewVectDecl (NHsVectClassOut a)
pattern
  HsVectInstIn a
    = AST.VectInst    NoFieldExt a
pattern
  HsVectInstOut a
    = AST.NewVectDecl (NHsVectInstOut a)

{-#
  COMPLETE
    HsVect,
    HsNoVect,
    HsVectTypeIn,
    HsVectTypeOut,
    HsVectClassIn,
    HsVectClassOut,
    HsVectInstIn,
    HsVectInstOut
  #-}

type instance
  AST.XVect         (GHC pass) = NoFieldExt
type instance
  AST.XNoVect       (GHC pass) = NoFieldExt
type instance
  AST.XVectType     (GHC pass) = NoFieldExt
type instance
  AST.XVectClass    (GHC pass) = NoFieldExt
type instance
  AST.XVectInst     (GHC pass) = NoFieldExt
type instance
  AST.XNewVectDecl    (GHC pass) = NewVectDecl pass

data NewVectDecl pass
  = NHsVectTypeOut
      Bool
      TyCon
      (Maybe TyCon)
  | NHsVectClassOut
      Class
  | NHsVectInstOut
      ClsInst

type
  LVectDecl pass = AST.LVectDecl (GHC pass)
  -- ^ Located Vectorise Declaration

-- ** Document comment
-- -----------------------------------------------------------------------------

type
  DocDecl = AST.DocDecl
  -- ^ Documentation comment Declaration

pattern
  DocCommentNext ::
    HsDocString ->
    DocDecl

pattern
  DocCommentPrev ::
    HsDocString ->
    DocDecl

pattern
  DocCommentNamed ::
    String ->
    HsDocString ->
    DocDecl

pattern
  DocGroup ::
    Int ->
    HsDocString ->
    DocDecl

pattern
  DocCommentNext a
    = AST.DocCommentNext a
pattern
  DocCommentPrev a
    = AST.DocCommentPrev a
pattern
  DocCommentNamed a b
    = AST.DocCommentNamed a b
pattern
  DocGroup a b
    = AST.DocGroup a b

{-#
  COMPLETE
    DocCommentNext,
    DocCommentPrev,
    DocCommentNamed,
    DocGroup
  #-}


type
  LDocDecl = AST.LDocDecl

-- ** Deprecations
-- -----------------------------------------------------------------------------

{-
We use exported entities for things to deprecate.
-}

type
  WarnDecls pass = AST.WarnDecls (GHC pass)
  -- Note [Pragma source text] in BasicTypes
  -- ^ Warning pragma Declarations
pattern
  Warnings ::
    (SourceText) ->
    ([LWarnDecl pass]) ->
    WarnDecls pass

pattern
  Warnings
    { wd_src
    , wd_warnings }
    = AST.Warnings NoFieldExt wd_src wd_warnings

{-#
  COMPLETE
    Warnings
  #-}

type instance
  AST.XWarnings     (GHC pass) = NoFieldExt
type instance
  AST.XNewWarnDecls (GHC pass) = NoConExt

type
  LWarnDecls pass = AST.LWarnDecls (GHC pass)
  -- ^ Located Warning Declarations

-- -----------------------------------------------------------------------------

type
  WarnDecl pass = AST.WarnDecl (GHC pass)
  -- ^ Warning pragma Declaration

pattern
  Warning ::
    [LIdP pass] ->
    WarningTxt ->
    WarnDecl pass

pattern
  Warning a b
    = AST.Warning NoFieldExt a b

{-#
  COMPLETE
    Warning
  #-}

type instance
  AST.XWarning     (GHC pass) = NoFieldExt
type instance
  AST.XNewWarnDecl (GHC pass) = NoConExt

type
  LWarnDecl pass = AST.LWarnDecl (GHC pass)
  -- ^ Located Warning pragma Declaration

-- ** Annotations
-- -----------------------------------------------------------------------------

type
  AnnDecl pass = AST.AnnDecl (GHC pass)
  -- ^ Annotation Declaration
pattern
  HsAnnotation ::
    SourceText -> -- Note [Pragma source text] in BasicTypes
    (AnnProvenance (IdP pass)) ->
    (Located (HsExpr pass)) ->
    AnnDecl pass
  -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --           'ApiAnnotation.AnnType'
  --           'ApiAnnotation.AnnModule'
  --           'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  HsAnnotation a b c
    = AST.Annotation NoFieldExt a b c

{-#
  COMPLETE
    HsAnnotation
  #-}

type instance
  AST.XAnnotation (GHC pass) = NoFieldExt
type instance
  AST.XNewAnnDecl   (GHC pass) = NoConExt

type
  LAnnDecl pass = AST.LAnnDecl (GHC pass)
  -- ^ Located Annotation Declaration

-- -----------------------------------------------------------------------------

type
  AnnProvenance name = AST.AnnProvenance name
  -- ^ Annotation Provenance
pattern
  ValueAnnProvenance ::
    (Located name) ->
    AnnProvenance name

pattern
  TypeAnnProvenance ::
    (Located name) ->
    AnnProvenance name

pattern
  ModuleAnnProvenance ::
    AnnProvenance name

pattern
  ValueAnnProvenance a
    = AST.ValueAnnProvenance a
pattern
  TypeAnnProvenance a
    = AST.TypeAnnProvenance a
pattern
  ModuleAnnProvenance
    = AST.ModuleAnnProvenance

{-#
  COMPLETE
    ValueAnnProvenance,
    TypeAnnProvenance,
    ModuleAnnProvenance
  #-}


type
  LAnnProvenance id = AST.LAnnProvenance id

-- ** Role annotation
-- -----------------------------------------------------------------------------

-- See #8185 for more info about why role annotations are
-- top-level declarations


type
  RoleAnnotDecl pass = AST.RoleAnnotDecl (GHC pass)
  -- ^ Role Annotation Declaration
pattern
  RoleAnnotDecl ::
    (LIdP pass) ->
    -- type constructor
    [Located (Maybe Role)] ->
    -- optional annotations
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --           'ApiAnnotation.AnnRole'

    -- For details on above see note [Api annotations] in ApiAnnotation
    RoleAnnotDecl pass

pattern
  RoleAnnotDecl a b
    = AST.RoleAnnotDecl NoFieldExt a b

{-#
  COMPLETE
    RoleAnnotDecl
  #-}

type instance
  AST.XRoleAnnotDecl    (GHC pass) = NoFieldExt
type instance
  AST.XNewRoleAnnotDecl (GHC pass) = NoConExt

type
  LRoleAnnotDecl pass = AST.LRoleAnnotDecl (GHC pass)
  -- ^ Located Role Annotation Declaration

-- -----------------------------------------------------------------------------
-- * Utilities
-- -----------------------------------------------------------------------------

deriving instance
  (DataId pass) => Data (NewVectDecl pass)
deriving instance
  (DataId id) => Data (HsDecl id)
deriving instance
  (DataId id) => Data (HsGroup id)
deriving instance
  (DataId id) => Data (SpliceDecl id)
deriving instance
  (DataId id) => Data (TyClDecl id)
deriving instance
  (DataId id) => Data (TyClGroup id)
deriving instance
  (DataId pass) => Data (FamilyResultSig pass)
deriving instance
  (DataId id) => Data (FamilyDecl id)
deriving instance
  (DataId pass) => Data (InjectivityAnn pass)
deriving instance
  (DataId pass) => Data (FamilyInfo pass)
deriving instance
  (DataId id) => Data (HsDataDefn id)
deriving instance
  (DataId id) => Data (HsDerivingClause id)
deriving instance
  Data  NewOrData
deriving instance
  (DataId pass) => Data (ConDecl pass)
deriving instance
  (DataId pass, Data pats) => Data (TyFamEqn pass pats)
deriving instance
  (DataId pass) => Data (TyFamInstDecl pass)
deriving instance
  (DataId pass) => Data (DataFamInstDecl pass)
deriving instance
  (DataId id) => Data (ClsInstDecl id)
deriving instance
  (DataId id) => Data (InstDecl id)
deriving instance
  (DataId pass) => Data (DerivDecl pass)
deriving instance
  (DataId pass) => Data (DefaultDecl pass)
deriving instance
  (DataId pass) => Data (ForeignDecl pass)
deriving instance
  Data ForeignImport
deriving instance
  Data CImportSpec
deriving instance
  Data ForeignExport
deriving instance
  (DataId pass) => Data (RuleDecls pass)
deriving instance
  (DataId pass) => Data (RuleDecl pass)
deriving instance
  (DataId pass) => Data (RuleBndr pass)
deriving instance
  (DataId pass) => Data (VectDecl pass)
deriving instance
  Data DocDecl
deriving instance
  (DataId pass) => Data (WarnDecls pass)
deriving instance
  (DataId pass) => Data (WarnDecl pass)
deriving instance
  (DataId pass) => Data (AnnDecl pass)
deriving instance
  (Data pass) => Data (AnnProvenance pass)
deriving instance
  (DataId pass) => Data (RoleAnnotDecl pass)

-- -----------------------------------------------------------------------------

emptyGroup, emptyRdrGroup, emptyRnGroup :: HsGroup a
emptyRdrGroup = emptyGroup { hs_valds = emptyValBindsIn }
emptyRnGroup  = emptyGroup { hs_valds = emptyValBindsOut }

hsGroupInstDecls :: HsGroup id -> [LInstDecl id]
hsGroupInstDecls = (=<<) group_instds . hs_tyclds

emptyGroup = HsGroup { hs_tyclds = [],
                       hs_derivds = [],
                       hs_fixds = [], hs_defds = [], hs_annds = [],
                       hs_fords = [], hs_warnds = [], hs_ruleds = [], hs_vects = [],
                       hs_valds = error "emptyGroup hs_valds: Can't happen",
                       hs_splcds = [],
                       hs_docs = [] }

appendGroups :: HsGroup a -> HsGroup a -> HsGroup a
appendGroups
    HsGroup {
        hs_valds  = val_groups1,
        hs_splcds = spliceds1,
        hs_tyclds = tyclds1,
        hs_derivds = derivds1,
        hs_fixds  = fixds1,
        hs_defds  = defds1,
        hs_annds  = annds1,
        hs_fords  = fords1,
        hs_warnds = warnds1,
        hs_ruleds = rulds1,
        hs_vects = vects1,
  hs_docs   = docs1 }
    HsGroup {
        hs_valds  = val_groups2,
        hs_splcds = spliceds2,
        hs_tyclds = tyclds2,
        hs_derivds = derivds2,
        hs_fixds  = fixds2,
        hs_defds  = defds2,
        hs_annds  = annds2,
        hs_fords  = fords2,
        hs_warnds = warnds2,
        hs_ruleds = rulds2,
        hs_vects  = vects2,
        hs_docs   = docs2 }
  =
    HsGroup {
        hs_valds  = val_groups1 `plusHsValBinds` val_groups2,
        hs_splcds = spliceds1 ++ spliceds2,
        hs_tyclds = tyclds1 ++ tyclds2,
        hs_derivds = derivds1 ++ derivds2,
        hs_fixds  = fixds1 ++ fixds2,
        hs_annds  = annds1 ++ annds2,
        hs_defds  = defds1 ++ defds2,
        hs_fords  = fords1 ++ fords2,
        hs_warnds = warnds1 ++ warnds2,
        hs_ruleds = rulds1 ++ rulds2,
        hs_vects  = vects1 ++ vects2,
        hs_docs   = docs1  ++ docs2 }

-- ** Simple classifiers for TyClDecl
-- -----------------------------------------------------------------------------

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
isTypeFamilyDecl (FamDecl (FamilyDecl { fdInfo = info })) = case info of
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
isDataFamilyDecl (FamDecl (FamilyDecl { fdInfo = DataFamily })) = True
isDataFamilyDecl _other      = False

-- Dealing with names

tyFamInstDeclName :: TyFamInstDecl pass -> (IdP pass)
tyFamInstDeclName = unLoc . tyFamInstDeclLName

tyFamInstDeclLName :: TyFamInstDecl pass -> LIdP pass
tyFamInstDeclLName (TyFamInstDecl { tfid_eqn =
                     (L _ (TyFamEqn { tfe_tycon = ln })) })
  = ln

tyClDeclLName :: TyClDecl pass -> LIdP pass
tyClDeclLName (FamDecl { tcdFam = FamilyDecl { fdLName = ln } }) = ln
tyClDeclLName decl = gettcdLName decl

tcdName :: TyClDecl pass -> (IdP pass)
tcdName = unLoc . tyClDeclLName

tyClDeclTyVars :: TyClDecl pass -> LHsQTyVars pass
tyClDeclTyVars (FamDecl { tcdFam = FamilyDecl { fdTyVars = tvs } }) = tvs
tyClDeclTyVars d = gettcdTyVars d

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

-- | Does this declaration have a complete, user-supplied kind signature?
-- See Note [Complete user-supplied kind signatures]
hsDeclHasCusk :: TyClDecl GhcRn -> Bool
hsDeclHasCusk (FamDecl { tcdFam = fam_decl }) = famDeclHasCusk Nothing fam_decl
hsDeclHasCusk (SynDecl { tcdTyVarsS = tyvars, tcdRhs = rhs })
  -- NB: Keep this synchronized with 'getInitialKind'
  = hsTvbAllKinded tyvars && rhs_annotated rhs
  where
    rhs_annotated (L _ ty) = case ty of
      HsParTy lty  -> rhs_annotated lty
      HsKindSig {} -> True
      _            -> False
hsDeclHasCusk (DataDecl { tcdDataCusk = cusk }) = cusk
hsDeclHasCusk (ClassDecl { tcdTyVarsC = tyvars }) = hsTvbAllKinded tyvars

-- -----------------------------------------------------------------------------

emptyTyClGroup :: TyClGroup pass
emptyTyClGroup = TyClGroup [] [] []

tyClGroupTyClDecls :: [TyClGroup pass] -> [LTyClDecl pass]
tyClGroupTyClDecls = concatMap group_tyclds

tyClGroupInstDecls :: [TyClGroup pass] -> [LInstDecl pass]
tyClGroupInstDecls = concatMap group_instds

tyClGroupRoleDecls :: [TyClGroup pass] -> [LRoleAnnotDecl pass]
tyClGroupRoleDecls = concatMap group_roles

mkTyClGroup :: [LTyClDecl pass] -> [LInstDecl pass] -> TyClGroup pass
mkTyClGroup decls instds = TyClGroup
  { group_tyclds = decls
  , group_roles = []
  , group_instds = instds
  }

-- -----------------------------------------------------------------------------


-- | Does this family declaration have a complete, user-supplied kind signature?
famDeclHasCusk :: Maybe Bool
                   -- ^ if associated, does the enclosing class have a CUSK?
               -> FamilyDecl pass -> Bool
famDeclHasCusk _ (FamilyDecl { fdInfo      = ClosedTypeFamily _
                             , fdTyVars    = tyvars
                             , fdResultSig = L _ resultSig })
  = hsTvbAllKinded tyvars && hasReturnKindSignature resultSig
famDeclHasCusk mb_class_cusk _ = mb_class_cusk `orElse` True
        -- all un-associated open families have CUSKs!

-- | Does this family declaration have user-supplied return kind signature?
hasReturnKindSignature :: FamilyResultSig a -> Bool
hasReturnKindSignature NoSig                          = False
hasReturnKindSignature (TyVarSig (L _ (UserTyVar _))) = False
hasReturnKindSignature _                              = True

-- | Maybe return name of the result type variable
resultVariableName :: FamilyResultSig a -> Maybe (IdP a)
resultVariableName (TyVarSig sig) = Just $ hsLTyVarName sig
resultVariableName _              = Nothing

-- -----------------------------------------------------------------------------

deriving instance Eq NewOrData -- Needed because Demand derives Eq

-- -----------------------------------------------------------------------------

getConNames :: ConDecl pass -> [LIdP pass]
getConNames ConDeclH98  {con_name  = name}  = [name]
getConNames ConDeclGADT {con_names = names} = names

-- don't call with RdrNames, because it can't deal with HsAppsTy
getConDetails :: ConDecl pass -> HsConDeclDetails pass
getConDetails ConDeclH98  {con_details  = details} = details
getConDetails ConDeclGADT {con_type     = ty     } = details
  where
    (details,_,_,_) = gadtDeclDetails ty

-- don't call with RdrNames, because it can't deal with HsAppsTy
gadtDeclDetails :: LHsSigType pass
                -> ( HsConDeclDetails pass
                   , LHsType pass
                   , LHsContext pass
                   , [LHsTyVarBndr pass] )
gadtDeclDetails HsIB {hsib_body = lbody_ty} = (details,res_ty,cxt,tvs)
  where
    (tvs, cxt, tau) = splitLHsSigmaTy lbody_ty
    (details, res_ty)           -- See Note [Sorting out the result type]
      = case tau of
          L _ (HsFunTy (L l (HsRecTy flds)) res_ty')
                  -> (RecCon (L l flds), res_ty')
          _other  -> (PrefixCon [], tau)

hsConDeclArgTys :: HsConDeclDetails pass -> [LBangType pass]
hsConDeclArgTys (PrefixCon tys)    = tys
hsConDeclArgTys (InfixCon ty1 ty2) = [ty1,ty2]
hsConDeclArgTys (RecCon flds)      = map (cd_fld_type . unLoc) (unLoc flds)


-- -----------------------------------------------------------------------------

-- Extract the declarations of associated data types from an instance

instDeclDataFamInsts :: [LInstDecl pass] -> [DataFamInstDecl pass]
instDeclDataFamInsts inst_decls
  = concatMap do_one inst_decls
  where
    do_one (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fam_insts } }))
      = map unLoc fam_insts
    do_one (L _ (DataFamInstD { dfid_inst = fam_inst }))      = [fam_inst]
    do_one (L _ (TyFamInstD {}))                              = []

-- -----------------------------------------------------------------------------

{-
    In both ForeignImport and ForeignExport:
        sig_ty is the type given in the Haskell code
        rep_ty is the representation for this type, i.e. with newtypes
               coerced away and type functions evaluated.
    Thus if the declaration is valid, then rep_ty will only use types
    such as Int and IO that we know how to make foreign calls with.
-}

noForeignImportCoercionYet :: PlaceHolder
noForeignImportCoercionYet = PlaceHolder

noForeignExportCoercionYet :: PlaceHolder
noForeignExportCoercionYet = PlaceHolder

-- -----------------------------------------------------------------------------

flattenRuleDecls :: [LRuleDecls pass] -> [LRuleDecl pass]
flattenRuleDecls decls = concatMap (rds_rules . unLoc) decls

-- -----------------------------------------------------------------------------

collectRuleBndrSigTys :: [RuleBndr pass] -> [LHsSigWcType pass]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ ty <- bndrs]

-- -----------------------------------------------------------------------------

lvectDeclName :: NamedThing (IdP pass) => LVectDecl pass -> Name
lvectDeclName (L _ (HsVect _       (L _ name) _))    = getName name
lvectDeclName (L _ (HsNoVect _     (L _ name)))      = getName name
lvectDeclName (L _ (HsVectTypeIn _  _ (L _ name) _)) = getName name
lvectDeclName (L _ (HsVectTypeOut  _ tycon _))       = getName tycon
lvectDeclName (L _ (HsVectClassIn _ (L _ name)))     = getName name
lvectDeclName (L _ (HsVectClassOut cls))             = getName cls
lvectDeclName (L _ (HsVectInstIn _))
  = panic "HsDecls.lvectDeclName: HsVectInstIn"
lvectDeclName (L _ (HsVectInstOut  _))
  = panic "HsDecls.lvectDeclName: HsVectInstOut"

lvectInstDecl :: LVectDecl pass -> Bool
lvectInstDecl (L _ (HsVectInstIn _))  = True
lvectInstDecl (L _ (HsVectInstOut _)) = True
lvectInstDecl _                       = False

-- -----------------------------------------------------------------------------

deriving instance Functor     AST.AnnProvenance
deriving instance Foldable    AST.AnnProvenance
deriving instance Traversable AST.AnnProvenance

annProvenanceName_maybe :: AnnProvenance name -> Maybe name
annProvenanceName_maybe (ValueAnnProvenance (L _ name)) = Just name
annProvenanceName_maybe (TypeAnnProvenance (L _ name))  = Just name
annProvenanceName_maybe ModuleAnnProvenance       = Nothing

-- -----------------------------------------------------------------------------

roleAnnotDeclName :: RoleAnnotDecl pass -> (IdP pass)
roleAnnotDeclName (RoleAnnotDecl (L _ name) _) = name

-- -----------------------------------------------------------------------------
-- * Pretty Printing
-- -----------------------------------------------------------------------------

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (HsDecl pass) where
    ppr (TyClD dcl)             = ppr dcl
    ppr (ValD binds)            = ppr binds
    ppr (DefD def)              = ppr def
    ppr (InstD inst)            = ppr inst
    ppr (DerivD deriv)          = ppr deriv
    ppr (ForD fd)               = ppr fd
    ppr (SigD sd)               = ppr sd
    ppr (RuleD rd)              = ppr rd
    ppr (VectD vect)            = ppr vect
    ppr (WarningD wd)           = ppr wd
    ppr (AnnD ad)               = ppr ad
    ppr (SpliceD dd)            = ppr dd
    ppr (DocD doc)              = ppr doc
    ppr (RoleAnnotD ra)         = ppr ra

instance (SourceTextX pass, OutputableBndrId pass)
      => Outputable (HsGroup pass) where
    ppr (HsGroup { hs_valds  = val_decls,
                   hs_tyclds = tycl_decls,
                   hs_derivds = deriv_decls,
                   hs_fixds  = fix_decls,
                   hs_warnds = deprec_decls,
                   hs_annds  = ann_decls,
                   hs_fords  = foreign_decls,
                   hs_defds  = default_decls,
                   hs_ruleds = rule_decls,
                   hs_vects  = vect_decls })
        = vcat_mb empty
            [ppr_ds fix_decls, ppr_ds default_decls,
             ppr_ds deprec_decls, ppr_ds ann_decls,
             ppr_ds rule_decls,
             ppr_ds vect_decls,
             if isEmptyValBinds val_decls
                then Nothing
                else Just (ppr val_decls),
             ppr_ds (tyClGroupTyClDecls tycl_decls),
             ppr_ds (tyClGroupInstDecls tycl_decls),
             ppr_ds deriv_decls,
             ppr_ds foreign_decls]
        where
          ppr_ds :: Outputable a => [a] -> Maybe SDoc
          ppr_ds [] = Nothing
          ppr_ds ds = Just (vcat (map ppr ds))

          vcat_mb :: SDoc -> [Maybe SDoc] -> SDoc
          -- Concatenate vertically with white-space between non-blanks
          vcat_mb _    []             = empty
          vcat_mb gap (Nothing : ds) = vcat_mb gap ds
          vcat_mb gap (Just d  : ds) = gap $$ d $$ vcat_mb blankLine ds


instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (SpliceDecl pass) where
   ppr (SpliceDecl (L _ e) f) = pprSpliceDecl e f

-- -----------------------------------------------------------------------------


instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (TyClDecl pass) where

    ppr (FamDecl { tcdFam = decl }) = ppr decl
    ppr (SynDecl { tcdLNameS = ltycon, tcdTyVarsS = tyvars, tcdFixityS = fixity
                 , tcdRhs = rhs })
      = hang (text "type" <+>
              pp_vanilla_decl_head ltycon tyvars fixity [] <+> equals)
          4 (ppr rhs)

    ppr (DataDecl { tcdLNameD = ltycon, tcdTyVarsD = tyvars, tcdFixityD = fixity
                  , tcdDataDefn = defn })
      = pp_data_defn (pp_vanilla_decl_head ltycon tyvars fixity) defn

    ppr (ClassDecl {tcdCtxt = context, tcdLNameC = lclas, tcdTyVarsC = tyvars,
                    tcdFixityC = fixity,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs})
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> text "where"
             , nest 2 $ pprDeclList (map (pprFamilyDecl NotTopLevel . unLoc) ats ++
                                     map ppr_fam_deflt_eqn at_defs ++
                                     pprLHsBindsForUser methods sigs) ]
      where
        top_matter = text "class"
                    <+> pp_vanilla_decl_head lclas tyvars fixity (unLoc context)
                    <+> pprFundeps (map unLoc fds)

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (TyClGroup pass) where
  ppr (TyClGroup { group_tyclds = tyclds
                 , group_roles = roles
                 , group_instds = instds
                 }
      )
    = ppr tyclds $$
      ppr roles $$
      ppr instds

pp_vanilla_decl_head :: (SourceTextX pass, OutputableBndrId pass)
   => LIdP pass
   -> LHsQTyVars pass
   -> LexicalFixity
   -> HsContext pass
   -> SDoc
pp_vanilla_decl_head thing (HsQTvs { hsq_explicit = tyvars }) fixity context
 = hsep [pprHsContext context, pp_tyvars tyvars]
  where
    pp_tyvars (varl:varsr)
      | fixity == Infix
         = hsep [ppr (unLoc varl), pprInfixOcc (unLoc thing)
         , hsep (map (ppr.unLoc) varsr)]
      | otherwise = hsep [ pprPrefixOcc (unLoc thing)
                  , hsep (map (ppr.unLoc) (varl:varsr))]
    pp_tyvars [] = ppr thing

pprTyClDeclFlavour :: TyClDecl a -> SDoc
pprTyClDeclFlavour (ClassDecl {})   = text "class"
pprTyClDeclFlavour (SynDecl {})     = text "type"
pprTyClDeclFlavour (FamDecl { tcdFam = FamilyDecl { fdInfo = info }})
  = pprFlavour info <+> text "family"
pprTyClDeclFlavour (DataDecl { tcdDataDefn = HsDataDefn { dd_ND = nd } })
  = ppr nd

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (FamilyDecl pass) where
  ppr = pprFamilyDecl TopLevel

pprFamilyDecl :: (SourceTextX pass, OutputableBndrId pass)
              => TopLevelFlag -> FamilyDecl pass -> SDoc
pprFamilyDecl top_level (FamilyDecl { fdInfo = info, fdLName = ltycon
                                    , fdTyVars = tyvars
                                    , fdFixity = fixity
                                    , fdResultSig = L _ result
                                    , fdInjectivityAnn = mb_inj })
  = vcat [ pprFlavour info <+> pp_top_level <+>
           pp_vanilla_decl_head ltycon tyvars fixity [] <+>
           pp_kind <+> pp_inj <+> pp_where
         , nest 2 $ pp_eqns ]
  where
    pp_top_level = case top_level of
                     TopLevel    -> text "family"
                     NotTopLevel -> empty

    pp_kind = case result of
                NoSig            -> empty
                KindSig  kind    -> dcolon <+> ppr kind
                TyVarSig tv_bndr -> text "=" <+> ppr tv_bndr
    pp_inj = case mb_inj of
               Just (L _ (InjectivityAnn lhs rhs)) ->
                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
               Nothing -> empty
    (pp_where, pp_eqns) = case info of
      ClosedTypeFamily mb_eqns ->
        ( text "where"
        , case mb_eqns of
            Nothing   -> text ".."
            Just eqns -> vcat $ map ppr_fam_inst_eqn eqns )
      _ -> (empty, empty)

pprFlavour :: FamilyInfo pass -> SDoc
pprFlavour DataFamily            = text "data"
pprFlavour OpenTypeFamily        = text "type"
pprFlavour (ClosedTypeFamily {}) = text "type"

instance Outputable (FamilyInfo pass) where
  ppr info = pprFlavour info <+> text "family"

-- -----------------------------------------------------------------------------

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (HsDerivingClause pass) where
  ppr (HsDerivingClause { deriv_clause_strategy = dcs
                        , deriv_clause_tys      = L _ dct })
    = hsep [ text "deriving"
           , ppDerivStrategy dcs
           , pp_dct dct ]
      where
        -- This complexity is to distinguish between
        --    deriving Show
        --    deriving (Show)
        pp_dct [a@(HsIB { hsib_body = L _ HsAppsTy{} })] = parens (ppr a)
        pp_dct [a] = ppr a
        pp_dct _   = parens (interpp'SP dct)


pp_data_defn :: (SourceTextX pass, OutputableBndrId pass)
                  => (HsContext pass -> SDoc)   -- Printing the header
                  -> HsDataDefn pass
                  -> SDoc
pp_data_defn pp_hdr (HsDataDefn { dd_ND = new_or_data, dd_ctxt = L _ context
                                , dd_cType = mb_ct
                                , dd_kindSig = mb_sig
                                , dd_cons = condecls, dd_derivs = derivings })
  | null condecls
  = ppr new_or_data <+> pp_ct <+> pp_hdr context <+> pp_sig
    <+> pp_derivings derivings

  | otherwise
  = hang (ppr new_or_data <+> pp_ct  <+> pp_hdr context <+> pp_sig)
       2 (pp_condecls condecls $$ pp_derivings derivings)
  where
    pp_ct = case mb_ct of
               Nothing   -> empty
               Just ct -> ppr ct
    pp_sig = case mb_sig of
               Nothing   -> empty
               Just kind -> dcolon <+> ppr kind
    pp_derivings (L _ ds) = vcat (map ppr ds)

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (HsDataDefn pass) where
   ppr d = pp_data_defn (\_ -> text "Naked HsDataDefn") d

instance Outputable NewOrData where
  ppr NewType  = text "newtype"
  ppr DataType = text "data"

pp_condecls :: (SourceTextX pass, OutputableBndrId pass)
            => [LConDecl pass] -> SDoc
pp_condecls cs@(L _ ConDeclGADT{} : _) -- In GADT syntax
  = hang (text "where") 2 (vcat (map ppr cs))
pp_condecls cs                    -- In H98 syntax
  = equals <+> sep (punctuate (text " |") (map ppr cs))

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (ConDecl pass) where
    ppr = pprConDecl

pprConDecl :: (SourceTextX pass, OutputableBndrId pass) => ConDecl pass -> SDoc
pprConDecl (ConDeclH98 { con_name = L _ con
                       , con_qvars = mtvs
                       , con_cxt = mcxt
                       , con_details = details
                       , con_docA = doc })
  = sep [ppr_mbDoc doc, pprHsForAll tvs cxt,         ppr_details details]
  where
    ppr_details (InfixCon t1 t2) = hsep [ppr t1, pprInfixOcc con, ppr t2]
    ppr_details (PrefixCon tys)  = hsep (pprPrefixOcc con
                                   : map (pprHsType . unLoc) tys)
    ppr_details (RecCon fields)  = pprPrefixOcc con
                                 <+> pprConDeclFields (unLoc fields)
    tvs = case mtvs of
      Nothing -> []
      Just (HsQTvs { hsq_explicit = tvs }) -> tvs

    cxt = fromMaybe (noLoc []) mcxt

pprConDecl (ConDeclGADT { con_names = cons, con_type = res_ty, con_docG = doc })
  = sep [ppr_mbDoc doc <+> ppr_con_names cons <+> dcolon
         <+> ppr res_ty]

ppr_con_names :: (OutputableBndr a) => [Located a] -> SDoc
ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (TyFamInstDecl pass) where
  ppr = pprTyFamInstDecl TopLevel

pprTyFamInstDecl :: (SourceTextX pass, OutputableBndrId pass)
                 => TopLevelFlag -> TyFamInstDecl pass -> SDoc
pprTyFamInstDecl top_lvl (TyFamInstDecl { tfid_eqn = eqn })
   = text "type" <+> ppr_instance_keyword top_lvl <+> ppr_fam_inst_eqn eqn

ppr_instance_keyword :: TopLevelFlag -> SDoc
ppr_instance_keyword TopLevel    = text "instance"
ppr_instance_keyword NotTopLevel = empty

ppr_fam_inst_eqn :: (SourceTextX pass, OutputableBndrId pass)
                 => LTyFamInstEqn pass -> SDoc
ppr_fam_inst_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                , tfe_pats  = pats
                                , tfe_fixity = fixity
                                , tfe_rhs   = rhs }))
    = pp_fam_inst_lhs tycon pats fixity [] <+> equals <+> ppr rhs

ppr_fam_deflt_eqn :: (SourceTextX pass, OutputableBndrId pass)
                  => LTyFamDefltEqn pass -> SDoc
ppr_fam_deflt_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                 , tfe_pats  = tvs
                                 , tfe_fixity = fixity
                                 , tfe_rhs   = rhs }))
    = text "type" <+> pp_vanilla_decl_head tycon tvs fixity []
                  <+> equals <+> ppr rhs

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (DataFamInstDecl pass) where
  ppr = pprDataFamInstDecl TopLevel

pprDataFamInstDecl :: (SourceTextX pass, OutputableBndrId pass)
                   => TopLevelFlag -> DataFamInstDecl pass -> SDoc
pprDataFamInstDecl top_lvl (DataFamInstDecl { dfid_tycon = tycon
                                            , dfid_pats  = pats
                                            , dfid_fixity = fixity
                                            , dfid_defn  = defn })
  = pp_data_defn pp_hdr defn
  where
    pp_hdr ctxt = ppr_instance_keyword top_lvl
              <+> pp_fam_inst_lhs tycon pats fixity ctxt

pprDataFamInstFlavour :: DataFamInstDecl pass -> SDoc
pprDataFamInstFlavour (DataFamInstDecl { dfid_defn = (HsDataDefn { dd_ND = nd }) })
  = ppr nd

pp_fam_inst_lhs :: (SourceTextX pass, OutputableBndrId pass)
   => LIdP pass
   -> HsTyPats pass
   -> LexicalFixity
   -> HsContext pass
   -> SDoc
pp_fam_inst_lhs thing (HsIB { hsib_body = typats }) fixity context
                                              -- explicit type patterns
   = hsep [ pprHsContext context, pp_pats typats]
   where
     pp_pats (patl:patsr)
       | fixity == Infix
          = hsep [pprHsType (unLoc patl), pprInfixOcc (unLoc thing)
          , hsep (map (pprHsType.unLoc) patsr)]
       | otherwise = hsep [ pprPrefixOcc (unLoc thing)
                   , hsep (map (pprHsType.unLoc) (patl:patsr))]
     pp_pats [] = empty

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (ClsInstDecl pass) where
    ppr (ClsInstDecl { cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      | null sigs, null ats, null adts, isEmptyBag binds  -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> text "where"
             , nest 2 $ pprDeclList $
               map (pprTyFamInstDecl NotTopLevel . unLoc)   ats ++
               map (pprDataFamInstDecl NotTopLevel . unLoc) adts ++
               pprLHsBindsForUser binds sigs ]
      where
        top_matter = text "instance" <+> ppOverlapPragma mbOverlap
                                             <+> ppr inst_ty

ppDerivStrategy :: Maybe (Located DerivStrategy) -> SDoc
ppDerivStrategy mb =
  case mb of
    Nothing       -> empty
    Just (L _ ds) -> ppr ds

ppOverlapPragma :: Maybe (Located OverlapMode) -> SDoc
ppOverlapPragma mb =
  case mb of
    Nothing           -> empty
    Just (L _ (NoOverlap s))    -> maybe_stext s "{-# NO_OVERLAP #-}"
    Just (L _ (Overlappable s)) -> maybe_stext s "{-# OVERLAPPABLE #-}"
    Just (L _ (Overlapping s))  -> maybe_stext s "{-# OVERLAPPING #-}"
    Just (L _ (Overlaps s))     -> maybe_stext s "{-# OVERLAPS #-}"
    Just (L _ (Incoherent s))   -> maybe_stext s "{-# INCOHERENT #-}"
  where
    maybe_stext NoSourceText     alt = text alt
    maybe_stext (SourceText src) _   = text src <+> text "#-}"


instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (InstDecl pass) where
    ppr (ClsInstD     { cid_inst  = decl }) = ppr decl
    ppr (TyFamInstD   { tfid_inst = decl }) = ppr decl
    ppr (DataFamInstD { dfid_inst = decl }) = ppr decl

-- -----------------------------------------------------------------------------

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (DerivDecl pass) where
    ppr (DerivDecl { deriv_type = ty
                   , deriv_strategy = ds
                   , deriv_overlap_mode = o })
        = hsep [ text "deriving"
               , ppDerivStrategy ds
               , text "instance"
               , ppOverlapPragma o
               , ppr ty ]

-- -----------------------------------------------------------------------------

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (DefaultDecl pass) where

    ppr (DefaultDecl tys)
      = text "default" <+> parens (interpp'SP tys)

-- -----------------------------------------------------------------------------

-- pretty printing of foreign declarations
--

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (ForeignDecl pass) where
  ppr (ForeignImport { fd_nameI = n, fd_sig_tyI = ty, fd_fi = fimport })
    = hang (text "foreign import" <+> ppr fimport <+> ppr n)
         2 (dcolon <+> ppr ty)
  ppr (ForeignExport { fd_nameE = n, fd_sig_tyE = ty, fd_fe = fexport }) =
    hang (text "foreign export" <+> ppr fexport <+> ppr n)
       2 (dcolon <+> ppr ty)

instance Outputable (ForeignImport) where
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

instance Outputable (ForeignExport) where
  ppr (CExport  (L _ (CExportStatic _ lbl cconv)) _) =
    ppr cconv <+> char '"' <> ppr lbl <> char '"'

-- -----------------------------------------------------------------------------

pprFullRuleName :: Located (SourceText, RuleName) -> SDoc
pprFullRuleName (L _ (st, n)) = pprWithSourceText st (doubleQuotes $ ftext n)

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (RuleDecls pass) where
  ppr (HsRules st rules)
    = pprWithSourceText st (text "{-# RULES")
          <+> vcat (punctuate semi (map ppr rules)) <+> text "#-}"

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (RuleDecl pass) where
  ppr (HsRule name act ns lhs _fv_lhs rhs _fv_rhs)
        = sep [pprFullRuleName name <+> ppr act,
               nest 4 (pp_forall <+> pprExpr (unLoc lhs)),
               nest 6 (equals <+> pprExpr (unLoc rhs)) ]
        where
          pp_forall | null ns   = empty
                    | otherwise = forAllLit <+> fsep (map ppr ns) <> dot

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (RuleBndr pass) where
   ppr (RuleBndr name) = ppr name
   ppr (RuleBndrSig name ty) = parens (ppr name <> dcolon <> ppr ty)

-- -----------------------------------------------------------------------------

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (VectDecl pass) where
  ppr (HsVect _ v rhs)
    = sep [text "{-# VECTORISE" <+> ppr v,
           nest 4 $
             pprExpr (unLoc rhs) <+> text "#-}" ]
  ppr (HsNoVect _ v)
    = sep [text "{-# NOVECTORISE" <+> ppr v <+> text "#-}" ]
  ppr (HsVectTypeIn _ False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn _ False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeIn _ True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn _ True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeOut False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeOut False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeOut True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeOut True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectClassIn _ c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectClassOut c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectInstIn ty)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr ty <+> text "#-}" ]
  ppr (HsVectInstOut i)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr i <+> text "#-}" ]

-- -----------------------------------------------------------------------------

-- Okay, I need to reconstruct the document comments, but for now:
instance Outputable DocDecl where
  ppr _ = text "<document comment>"

docDeclDoc :: DocDecl -> HsDocString
docDeclDoc (DocCommentNext d) = d
docDeclDoc (DocCommentPrev d) = d
docDeclDoc (DocCommentNamed _ d) = d
docDeclDoc (DocGroup _ d) = d

-- -----------------------------------------------------------------------------

instance OutputableBndr (IdP pass) => Outputable (WarnDecls pass) where
    ppr (Warnings (SourceText src) decls)
      = text src <+> vcat (punctuate comma (map ppr decls)) <+> text "#-}"
    ppr (Warnings NoSourceText _decls) = panic "WarnDecls"

instance OutputableBndr (IdP pass) => Outputable (WarnDecl pass) where
    ppr (Warning thing txt)
      = hsep ( punctuate comma (map ppr thing))
              <+> ppr txt

-- -----------------------------------------------------------------------------

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (AnnDecl pass) where
    ppr (HsAnnotation _ provenance expr)
      = hsep [text "{-#", pprAnnProvenance provenance, pprExpr (unLoc expr), text "#-}"]

-- -----------------------------------------------------------------------------

pprAnnProvenance :: OutputableBndr name => AnnProvenance name -> SDoc
pprAnnProvenance ModuleAnnProvenance       = text "ANN module"
pprAnnProvenance (ValueAnnProvenance (L _ name))
  = text "ANN" <+> ppr name
pprAnnProvenance (TypeAnnProvenance (L _ name))
  = text "ANN type" <+> ppr name

-- -----------------------------------------------------------------------------

instance OutputableBndr (IdP pass) => Outputable (RoleAnnotDecl pass) where
  ppr (RoleAnnotDecl ltycon roles)
    = text "type role" <+> ppr ltycon <+>
      hsep (map (pp_role . unLoc) roles)
    where
      pp_role Nothing  = underscore
      pp_role (Just r) = ppr r

-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- Notes
-- -----------------------------------------------------------------------------

{-
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
   When doing so we look them up in the name cache (RnEnv.lookupSysName),
   to ensure correct module and provenance is set

These are the two places that we have to conjure up the magic derived
names.  (The actual magic is in OccName.mkWorkerOcc, etc.)

Default methods
~~~~~~~~~~~~~~~
 - Occurrence name is derived uniquely from the method name
   E.g. $dmmax

 - If there is a default method name at all, it's recorded in
   the ClassOpSig (in HsBinds), in the DefMethInfo field.
   (DefMethInfo is defined in Class.hs)

Source-code class decls and interface-code class decls are treated subtly
differently, which has given me a great deal of confusion over the years.
Here's the deal.  (We distinguish the two cases because source-code decls
have (Just binds) in the tcdMeths field, whereas interface decls have Nothing.

In *source-code* class declarations:

 - When parsing, every ClassOpSig gets a DefMeth with a suitable RdrName
   This is done by RdrHsSyn.mkClassOpSigDM

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
TcInstDcls.tcInstDecls1).  So we can't generate the names for
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

Note [Complete user-supplied kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We kind-check declarations differently if they have a complete, user-supplied
kind signature (CUSK). This is because we can safely generalise a CUSKed
declaration before checking all of the others, supporting polymorphic recursion.
See ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindInference#Proposednewstrategy
and #9200 for lots of discussion of how we got here.

A declaration has a CUSK if we can know its complete kind without doing any
inference, at all. Here are the rules:

 - A class or datatype is said to have a CUSK if and only if all of its type
variables are annotated. Its result kind is, by construction, Constraint or *
respectively.

 - A type synonym has a CUSK if and only if all of its type variables and its
RHS are annotated with kinds.

 - A closed type family is said to have a CUSK if and only if all of its type
variables and its return type are annotated.

 - An open type family always has a CUSK -- unannotated type variables (and
return type) default to *.

 - Additionally, if -XTypeInType is on, then a data definition with a top-level
   :: must explicitly bind all kind variables to the right of the ::.
   See test dependent/should_compile/KindLevels, which requires this case.
   (Naturally, any kind variable mentioned before the :: should not be bound
   after it.)

Note [TyClGroups and dependency analysis]
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

See Note [Dependency analsis of type, class, and instance decls]
in RnSource for more info.

Note [FamilyResultSig]
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

See also Note [Injective type families] in TyCon

Note [Type family instance declarations in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type TyFamEqn represents one equation of a type family instance.
It is parameterised over its tfe_pats field:

 * An ordinary type family instance declaration looks like this in source Haskell
      type instance T [a] Int = a -> a
   (or something similar for a closed family)
   It is represented by a TyFamInstEqn, with *type* in the tfe_pats field.

 * On the other hand, the *default instance* of an associated type looks like
   this in source Haskell
      class C a where
        type T a b
        type T a b = a -> b   -- The default instance
   It is represented by a TyFamDefltEqn, with *type variables* in the tfe_pats
   field.

Note [Family instance declaration binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The HsTyPats field is LHS patterns or a type/data family instance.

The hsib_vars of the HsImplicitBndrs are the template variables of the
type patterns, i.e. fv(pat_tys).  Note in particular

* The hsib_vars *includes* any anonymous wildcards.  For example
     type instance F a _ = a
  The hsib_vars will be {a, _}.  Remember that each separate wildcard
  '_' gets its own unique.  In this context wildcards behave just like
  an ordinary type variable, only anonymous.

* The hsib_vars *including* type variables that are already in scope

   Eg   class C s t where
          type F t p :: *
        instance C w (a,b) where
          type F (a,b) x = x->a
   The hsib_vars of the F decl are {a,b,x}, even though the F decl
   is nested inside the 'instance' decl.

   However after the renamer, the uniques will match up:
        instance C w7 (a8,b9) where
          type F (a8,b9) x10 = x10->a8
   so that we can compare the type pattern in the 'instance' decl and
   in the associated 'type' decl

-}

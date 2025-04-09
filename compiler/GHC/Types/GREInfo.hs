{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Renamer-level information about 'Name's.
--
-- Renamer equivalent of 'TyThing'.
module GHC.Types.GREInfo where

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.DeepSeq ( NFData(..), deepseq )

import Data.Data ( Data )
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NonEmpty

{-**********************************************************************
*                                                                      *
                           GREInfo
*                                                                      *
************************************************************************

Note [GREInfo]
~~~~~~~~~~~~~~
In the renamer, we sometimes need a bit more information about a 'Name', e.g.
whether it is a type constructor, class, data constructor, record field, etc.

For example, when typechecking record construction, the renamer needs to look
up the fields of the data constructor being used (see e.g. GHC.Rename.Pat.rnHsRecFields).
Extra information also allows us to provide better error messages when a fatal
error occurs in the renamer, as it allows us to distinguish classes, type families,
type synonyms, etc.

For imported Names, we have access to the full type information in the form of
a TyThing (although see Note [Retrieving the GREInfo from interfaces]).
However, for Names in the module currently being renamed, we don't
yet have full information. Instead of using TyThing, we use the GREInfo type,
and this information gets affixed to each element in the GlobalRdrEnv.

This allows us to treat imported and local Names in a consistent manner:
always look at the GREInfo.

Note [Retrieving the GREInfo from interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a TyThing, we can easily compute the corresponding GREInfo: this is
done in GHC.Types.TyThing.tyThingGREInfo.

However, one often needs to produce GlobalRdrElts (and thus their GREInfos)
directly after loading interface files, before they are typechecked. For example:

  - GHC.Tc.Module.tcRnModuleTcRnM first calls tcRnImports, which starts off
    calling rnImports which transitively calls filterImports. That function
    is responsible for coughing up GlobalRdrElts (and their GREInfos) obtained
    from interfaces, but we will only typecheck the interfaces after we have
    finished processing the imports (see e.g. the logic at the start of tcRnImports
    which sets eps_is_boot, which decides whether we should look in the boot
    or non-boot interface for any particular module).
  - GHC.Tc.Utils.Backpack.mergeSignatures first loads the relevant signature
    interfaces to merge them, but only later on does it typecheck them.

In both of these examples, what's important is that we **lazily** produce the
GREInfo: it should only be consulted once the interfaces have been typechecked,
which will add the necessary information to the type-level environment.
In particular, the respective functions 'filterImports' and 'mergeSignatures'
should NOT force the gre_info field.

We delay the loading of interfaces by making the gre_info field of 'GlobalRdrElt'
a thunk which, when forced, loads the interface, looks up the 'Name' in the type
environment to get its associated TyThing, and computes the GREInfo from that.
See 'GHC.Rename.Env.lookupGREInfo'.

A possible alternative design would be to change the AvailInfo datatype to also
store GREInfo. We currently don't do that, as this would mean that every time
an interface re-exports something it has to also provide its GREInfo, which
could lead to bloat.

Note [Forcing GREInfo]
~~~~~~~~~~~~~~~~~~~~~~
The GREInfo field of a GlobalRdrElt needs to be lazy, as explained in
Note [Retrieving the GREInfo from interfaces]. For imported things, this field
is usually a thunk which looks up the GREInfo in a type environment
(see GHC.Rename.Env.lookupGREInfo).

We thus need to be careful not to introduce space leaks: such thunks could end
up retaining old type environments, which would violate invariant (5) of
Note [GHC Heap Invariants] in GHC.Driver.Make. This can happen, for example,
when reloading in GHCi (see e.g. test T15369, which can trigger the ghci leak check
if we're not careful).

A naive approach is to simply deeply force the whole GlobalRdrEnv. However,
forcing the GREInfo thunks can force the loading of interface files which we
otherwise might not need to load, so it leads to wasted work.

Instead, whenever we are about to store the GlobalRdrEnv somewhere (such as
in ModDetails), we dehydrate it by stripping away the GREInfo field, turning it
into (). See 'forceGlobalRdrEnv' and its cousin 'hydrateGlobalRdrEnv',
as well as Note [IfGlobalRdrEnv] in GHC.Types.Name.Reader.

Search for references to this note in the code for illustration.
-}

-- | Information about a 'Name' that is pertinent to the renamer.
--
-- See Note [GREInfo]
data GREInfo
      -- | A variable (an 'Id' or a 'TyVar')
    = Vanilla
      -- | An unbound GRE... could be anything
    | UnboundGRE
      -- | 'TyCon'
    | IAmTyCon    !(TyConFlavour Name)
      -- | 'ConLike'
    | IAmConLike  !ConInfo
      -- ^ The constructor fields.
      -- See Note [Local constructor info in the renamer].
      -- | Record field
    | IAmRecField !RecFieldInfo

    deriving Data


plusGREInfo :: GREInfo -> GREInfo -> GREInfo
plusGREInfo Vanilla Vanilla = Vanilla
plusGREInfo UnboundGRE UnboundGRE = UnboundGRE
plusGREInfo (IAmTyCon {})    info2@(IAmTyCon {}) = info2
plusGREInfo (IAmConLike {})  info2@(IAmConLike {}) = info2
plusGREInfo (IAmRecField {}) info2@(IAmRecField {}) = info2
plusGREInfo info1 info2 = pprPanic "plusInfo" $
  vcat [ text "info1:" <+> ppr info1
       , text "info2:" <+> ppr info2 ]

instance Outputable GREInfo where
  ppr Vanilla = text "Vanilla"
  ppr UnboundGRE = text "UnboundGRE"
  ppr (IAmTyCon flav)
    = text "TyCon" <+> ppr flav
  ppr (IAmConLike info)
    = text "ConLike" <+> ppr info
  ppr (IAmRecField info)
    = text "RecField" <+> ppr info

{-**********************************************************************
*                                                                      *
                      Constructor info
*                                                                      *
************************************************************************

Note [Local constructor info in the renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained in Note [GREInfo], information pertinent to the renamer is
stored using the GREInfo datatype. What information do we need about constructors?

Consider the following example:

  data T = T1 { x, y :: Int }
         | T2 { x :: Int }
         | T3
         | T4 Int Bool

We need to know:
* The fields of the data constructor, so that
  - We can complain if you say `T1 { v = 3 }`, where `v` is not a field of `T1`
    See the following call stack
    * GHC.Rename.Expr.rnExpr (RecordCon case)
    * GHC.Rename.Pat.rnHsRecFields
    * GHC.Rename.Env.lookupRecFieldOcc
  - Ditto if you pattern match on `T1 { v = x }`.
    See the following call stack
    * GHC.Rename.Pat.rnHsRecPatsAndThen
    * GHC.Rename.Pat.rnHsRecFields
    * GHC.Rename.Env.lookupRecFieldOcc
  - We can fill in the dots if you say `T1 {..}` in construction or pattern matching
    See GHC.Rename.Pat.rnHsRecFields.rn_dotdot

  This information is stored in ConFieldInfo.

* Whether the constructor is nullary.
  We need to know this to accept `T2 {..}`, and `T3 {..}`, but reject `T4 {..}`,
  in both construction and pattern matching.
  See GHC.Rename.Pat.rnHsRecFields.rn_dotdot
  and Note [Nullary constructors and empty record wildcards]

  This information is stored in ConFieldInfo.

* Whether the constructor is a data constructor or a pattern synonym, and,
  if it is a data constructor, what are the other data constructors of the
  parent type. This is used for computing irrefutability of pattern matches
  when deciding how to desugar do blocks (whether to use a fail operation).
  See GHC.Hs.Pat.isIrrefutableHsPat.

  This information is stored in ConLikeInfo.

Note [Nullary constructors and empty record wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A nullary constructor is one with no arguments.
For example, both `data T = MkT` and `data T = MkT {}` are nullary.

For consistency and TH convenience, it was agreed that a `{..}`
match or usage on nullary constructors would be accepted.
This is done as as per https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0496-empty-record-wildcards.rst
-}

-- | Information known to the renamer about a data constructor or pattern synonym.
--
-- See Note [Local constructor info in the renamer].
data ConInfo
  = ConInfo
  { conLikeInfo  :: !ConLikeInfo
  , conFieldInfo :: !ConFieldInfo
  }
  deriving stock Eq
  deriving Data

-- | Whether a constructor is a data constructor or a pattern synonym.
--
-- See Note [Local constructor info in the renamer].
data ConLikeInfo
  = ConIsData
    { conLikeDataCons :: [Name]
      -- ^ All the 'DataCon's of the parent 'TyCon',
      -- including the 'ConLike' itself.
      --
      -- Used in 'GHC.Hs.Pat.isIrrefutableHsPat'.
    }
  | ConIsPatSyn
  deriving stock Eq
  deriving Data

instance NFData ConInfo where
  rnf (ConInfo a b) = rnf a `seq` rnf b

instance NFData ConLikeInfo where
  rnf (ConIsData a) = rnf a
  rnf ConIsPatSyn = ()


-- | Information about the record fields of a constructor.
--
-- See Note [Local constructor info in the renamer]
data ConFieldInfo
  = ConHasRecordFields (NonEmpty FieldLabel)
  | ConHasPositionalArgs
  | ConIsNullary
  deriving stock Eq
  deriving Data

instance NFData ConFieldInfo where
  rnf ConIsNullary = ()
  rnf ConHasPositionalArgs = ()
  rnf (ConHasRecordFields flds) = rnf flds

mkConInfo :: ConLikeInfo -> VisArity -> [FieldLabel] -> ConInfo
mkConInfo con_ty n flds =
  ConInfo { conLikeInfo  = con_ty
          , conFieldInfo = mkConFieldInfo n flds }

mkConFieldInfo :: Arity -> [FieldLabel] -> ConFieldInfo
mkConFieldInfo 0 _ = ConIsNullary
mkConFieldInfo _ fields = maybe ConHasPositionalArgs ConHasRecordFields
                   $ NonEmpty.nonEmpty fields

conInfoFields :: ConInfo -> [FieldLabel]
conInfoFields = conFieldInfoFields . conFieldInfo

conFieldInfoFields :: ConFieldInfo -> [FieldLabel]
conFieldInfoFields (ConHasRecordFields fields) = NonEmpty.toList fields
conFieldInfoFields ConHasPositionalArgs = []
conFieldInfoFields ConIsNullary = []

instance Outputable ConInfo where
  ppr (ConInfo { conLikeInfo = con_ty, conFieldInfo = fld_info })
    = text "ConInfo" <+> braces
    (text "con_ty:" <+> ppr con_ty <> comma
    <+> text "fields:" <+> ppr fld_info)

instance Outputable ConLikeInfo where
  ppr (ConIsData cons) = text "ConIsData" <+> parens (ppr cons)
  ppr ConIsPatSyn      = text "ConIsPatSyn"

instance Outputable ConFieldInfo where
  ppr ConIsNullary = text "ConIsNullary"
  ppr ConHasPositionalArgs = text "ConHasPositionalArgs"
  ppr (ConHasRecordFields fieldLabels) =
    text "ConHasRecordFields" <+> braces (ppr fieldLabels)

-- | The 'Name' of a 'ConLike'.
--
-- Useful when we are in the renamer and don't yet have a full 'DataCon' or
-- 'PatSyn' to hand.
data ConLikeName
  = DataConName { conLikeName_Name :: !Name }
  | PatSynName  { conLikeName_Name :: !Name }
  deriving (Eq, Data)

instance NamedThing ConLikeName where
  getName = conLikeName_Name

instance Outputable ConLikeName where
  ppr = ppr . conLikeName_Name

instance OutputableBndr ConLikeName where
    pprInfixOcc con = pprInfixName (conLikeName_Name con)
    pprPrefixOcc con = pprPrefixName (conLikeName_Name con)

instance Uniquable ConLikeName where
  getUnique = getUnique . conLikeName_Name

instance NFData ConLikeName where
  rnf = rnf . conLikeName_Name

{-**********************************************************************
*                                                                      *
                      Record field info
*                                                                      *
**********************************************************************-}

data RecFieldInfo
  = RecFieldInfo
      { recFieldLabel :: !FieldLabel
      , recFieldCons  :: !(UniqSet ConLikeName)
         -- ^ The constructors which have this field label.
         -- Always non-empty.
         --
         -- NB: these constructors will always share a single parent,
         -- as the field label disambiguates between parents in the presence
         -- of duplicate record fields.
      }
  deriving (Eq, Data)

instance NFData RecFieldInfo where
  rnf (RecFieldInfo lbl cons)
    = rnf lbl `seq` nonDetStrictFoldUniqSet deepseq () cons

instance Outputable RecFieldInfo where
  ppr (RecFieldInfo { recFieldLabel = fl, recFieldCons = cons })
    = text "RecFieldInfo" <+> braces
      (text "recFieldLabel:" <+> ppr fl <> comma
      <+> text "recFieldCons:" <+> pprWithCommas ppr (nonDetEltsUniqSet cons))

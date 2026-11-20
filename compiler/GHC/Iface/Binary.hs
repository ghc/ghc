--
--  (c) The University of Glasgow 2002-2006
--

{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

-- | Binary interface file support.
module GHC.Iface.Binary (
        -- * Public API for interface file serialisation
        writeBinIface,
        readBinIface,
        readBinIfaceHeader,
        CompressionIFace(..),
        getSymtabName,
        CheckHiWay(..),
        TraceBinIFace(..),
        getIfaceWithExtFields,
        putIfaceWithExtFields,
        sourceTableLocations,
        getWithUserData,
        putWithUserData,

        -- * Internal serialisation functions
        getSymbolTable,
        putName,
        putSymbolTable,
        BinSymbolTable(..),
        initWriteIfaceType, initReadIfaceTypeTable,
        putAllTables,
    ) where

import GHC.Prelude

import GHC.Builtin   ( knownKeyOccMap, wiredInNamesMap )
import GHC.Builtin.KnownKeys (matchabilityTyConKey)
import GHC.Builtin.Uniques( knownUniqueTupleName )

import GHC.Utils.Panic
import GHC.Utils.Binary as Binary
import GHC.Utils.Misc (split)
import GHC.Utils.Outputable

import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.SrcLoc
import GHC.Types.Name.Cache
import GHC.Types.Unique.FM

import GHC.Unit
import GHC.Unit.Module.ModIface

import GHC.Platform.Profile
import GHC.Platform
import GHC.Settings.Constants
import GHC.Iface.Ext.Fields (readFieldWith, writeFieldWith, deleteField)
import GHC.Iface.Syntax (IfaceDecl(..), IfaceFamTyConFlav(..), IfaceClassBody(..), IfaceAT(..), IfaceIdDetails(..))
import GHC.Types.Basic (SourceTableOrigin(ExecutableSourceTable, FinalCoreSourceTable))
import GHC.Iface.Type (IfaceType(..), IfaceBndr(..), IfaceAppArgs(..), IfaceTyCon(..), IfaceTyConBinder,
                       getIfaceType, putIfaceType, ifaceTypeSharedByte,
                       IfaceEvidence(..), IfaceEvidenceVersion)
import GHC.Core.TyCon (isInferredTyConBinder)
import GHC.Types.Var (VarBndr(..))

import GHC.Data.FastMutInt
import GHC.Data.Maybe( orElse )

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.Array.Unsafe
import Data.Char
import Data.IORef
import Data.List (find, intercalate, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Word
import qualified Data.ByteString as BS
import System.IO (IOMode(WriteMode), withBinaryFile)
import System.IO.Unsafe
import Data.Typeable (Typeable)
import qualified GHC.Data.Strict as Strict
import Data.Function ((&))
import GHC.Types.Name.Env
import Data.Maybe

-- ---------------------------------------------------------------------------
-- Reading and writing binary interface files
--

data CheckHiWay = CheckHiWay | IgnoreHiWay
    deriving Eq

data TraceBinIFace
   = TraceBinIFace (SDoc -> IO ())
   | QuietBinIFace

-- | The compression/deduplication level of 'ModIface' files.
--
-- A 'ModIface' contains many duplicated symbols and names. To keep interface
-- files small, we deduplicate them during serialisation.
-- It is impossible to write an interface file with *no* compression/deduplication.
--
-- We support different levels of compression/deduplication, with different
-- trade-offs for run-time performance and memory usage.
-- If you don't have any specific requirements, then 'SafeExtraCompression' is a good default.
data CompressionIFace
  = NormalCompression
  -- ^ Perform the normal compression operations,
  -- such as deduplicating 'Name's and 'FastString's
  | SafeExtraCompression
  -- ^ Perform some extra compression steps that have minimal impact
  -- on the run-time of 'ghc'.
  --
  -- This reduces the size of '.hi' files significantly in some cases
  -- and reduces overall memory usage in certain scenarios.
  | MaximumCompression
  -- ^ Try to compress as much as possible.
  --
  -- Yields the smallest '.hi' files but at the cost of additional run-time.
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Outputable CompressionIFace where
  ppr = text . show

-- | Read an interface file header, checking the magic number, version, and
-- way. Returns the hash of the source file and a BinHandle which points at the
-- start of the rest of the interface file data.
readBinIfaceHeader
  :: Profile
  -> NameCache
  -> CheckHiWay
  -> TraceBinIFace
  -> FilePath
  -> IO ReadBinHandle
readBinIfaceHeader profile _name_cache checkHiWay traceBinIFace hi_path =
    snd <$> readBinIfaceHeaderVersion profile checkHiWay traceBinIFace hi_path

readBinIfaceHeaderVersion
  :: Profile -> CheckHiWay -> TraceBinIFace -> FilePath -> IO (IfaceEvidenceVersion, ReadBinHandle)
readBinIfaceHeaderVersion profile checkHiWay traceBinIFace hi_path = do
    let platform = profilePlatform profile

        wantedGot :: String -> a -> a -> (a -> SDoc) -> IO ()
        wantedGot what wanted got ppr' =
            case traceBinIFace of
               QuietBinIFace         -> return ()
               TraceBinIFace printer -> printer $
                     text what <> text ": " <>
                     vcat [text "Wanted " <> ppr' wanted <> text ",",
                           text "got    " <> ppr' got]

        errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> IO ()
        errorOnMismatch what wanted got =
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
            when (wanted /= got) $ throwGhcExceptionIO $ ProgramError
                         (what ++ " (wanted " ++ show wanted
                               ++ ", got "    ++ show got ++ ")")
    bh <- Binary.readBinMem hi_path

    -- Read the magic number to check that this really is a GHC .hi file
    -- (This magic number does not change when we change
    --  GHC interface file format)
    magic <- get bh
    wantedGot "Magic" (binaryInterfaceMagic platform) magic (ppr . unFixedLength)
    errorOnMismatch "magic number mismatch: old/corrupt interface file?"
        (unFixedLength $ binaryInterfaceMagic platform) (unFixedLength magic)

    -- Check the interface file version and profile tag.
    check_ver  <- get bh
    let our_ver = show hiVersion
    wantedGot "Version" our_ver check_ver text
    let versions = [our_ver, enumCallbackIfaceVersion, finalCoreIfaceVersion,
                    matchabilitySchemeIfaceVersion, openMatchabilityIfaceVersion]
    evidence <- case listToMaybe (mapMaybe (parse_version check_ver) versions) of
        Just requirements -> pure requirements
        Nothing -> do
            errorOnMismatch "mismatched interface file versions" our_ver check_ver
            pure Set.empty

    check_tag <- get bh
    let tag = profileBuildTag profile
    wantedGot "Way" tag check_tag text
    when (checkHiWay == CheckHiWay) $
        errorOnMismatch "mismatched interface file profile tag" tag check_tag

    pure (evidence, bh)
  where
    parse_version actual base = stripPrefix base actual >>= parseEvidenceSuffix

-- | Read an interface file.
--
-- See Note [Deduplication during iface binary serialisation] for details.
readBinIface
  :: Profile
  -> NameCache
  -> CheckHiWay
  -> TraceBinIFace
  -> FilePath
  -> IO ModIface
readBinIface profile name_cache checkHiWay traceBinIface hi_path = do
    (evidence, bh) <- readBinIfaceHeaderVersion profile checkHiWay traceBinIface hi_path

    mod_iface <- getIfaceWithExtFields evidence name_cache bh

    return $ mod_iface


getIfaceWithExtFields :: IfaceEvidenceVersion -> NameCache -> ReadBinHandle -> IO ModIface
getIfaceWithExtFields evidence name_cache bh = do
  -- Start offset for the byte array that contains the serialised 'ModIface'.
  start <- tellBinReader bh
  extFields_p_rel <- getRelBin bh

  mod_iface <- getWithUserData name_cache bh

  seekBinReaderRel bh extFields_p_rel
  extFields <- get bh
  stored <- readFieldWith sourceTableField (getWithUserData name_cache) extFields
  let located_iface = case stored of
        Nothing -> mod_iface
        Just entries ->
          let locations = mkOccEnv (entries :: [(OccName, [BinSrcSpan])])
              restore (fp, decl@IfaceAxiom{ifName = name})
                | Just locs <- lookupOccEnv locations (nameOccName name)
                = (fp, decl { ifAxSourceSpans = map unBinSrcSpan locs })
              restore other = other
          in set_mi_decls (map restore (mi_decls mod_iface)) mod_iface
  -- Store the 'ModIface' byte array, so that we can avoid serialisation if
  -- the 'ModIface' isn't modified.
  -- See Note [Sharing of ModIface]
  modIfaceBinData <- freezeBinHandle bh start
  pure $ located_iface
    & set_mi_ext_fields extFields
    & set_mi_hi_bytes (FullIfaceBinHandle $ Strict.Just (evidence, modIfaceBinData))

-- | This performs a get action after reading the dictionary and symbol
-- table. It is necessary to run this before trying to deserialise any
-- Names or FastStrings.
getWithUserData :: Binary a => NameCache -> ReadBinHandle -> IO a
getWithUserData name_cache bh = do
  bh <- getTables name_cache bh
  get bh

-- | Setup a BinHandle to read something written using putWithTables
--
-- Reading names has the side effect of adding them into the given NameCache.
getTables :: NameCache -> ReadBinHandle -> IO ReadBinHandle
getTables name_cache bh = do
    bhRef <- newIORef (error "used too soon")
    -- It is important this is passed to 'getTable'
    -- See Note [Lazy ReaderUserData during IfaceType serialisation]
    ud <- unsafeInterleaveIO (readIORef bhRef)

    fsReaderTable <- initFastStringReaderTable
    nameReaderTable <- initNameReaderTable name_cache
    ifaceTypeReaderTable <- initReadIfaceTypeTable ud

    let -- For any 'ReaderTable', we decode the table that is found at the location
        -- the forward reference points to.
        -- After decoding the table, we create a 'BinaryReader' and immediately
        -- add it to the 'ReaderUserData' of 'ReadBinHandle'.
        decodeReaderTable :: Typeable a => ReaderTable a -> ReadBinHandle -> IO ReadBinHandle
        decodeReaderTable tbl bh0 = do
          table <- Binary.forwardGetRel bh (getTable tbl bh0)
          let binaryReader = mkReaderFromTable tbl table
          pure $ addReaderToUserData binaryReader bh0

    -- Decode all the tables and populate the 'ReaderUserData'.
    bhFinal <- foldM (\bh0 act -> act bh0) bh
      -- The order of these deserialisation matters!
      --
      -- See Note [Order of deduplication tables during iface binary serialisation] for details.
      [ decodeReaderTable fsReaderTable
      , decodeReaderTable nameReaderTable
      , decodeReaderTable ifaceTypeReaderTable
      ]

    writeIORef bhRef (getReaderUserData bhFinal)
    pure bhFinal

-- | Write an interface file.
--
-- See Note [Deduplication during iface binary serialisation] for details.
writeBinIface :: Profile -> TraceBinIFace -> CompressionIFace -> FilePath -> ModIface -> IO ()
writeBinIface profile traceBinIface compressionLevel hi_path mod_iface = do
    case traceBinIface of
      QuietBinIFace -> pure ()
      TraceBinIFace printer -> do
        printer (text "writeBinIface compression level:" <+> ppr compressionLevel)

    bh <- openBinMem initBinMemSize
    let writeHeader h version = do
          put_ h (binaryInterfaceMagic (profilePlatform profile))
          put_ h version
          put_ h (profileBuildTag profile)

    -- Select the existing version first. The writer records extra features.
    let version | any (hasOpenMatchabilityScheme . snd) (mi_decls mod_iface) = openMatchabilityIfaceVersion
                | any hasMatchabilityScheme (mi_decls mod_iface) = matchabilitySchemeIfaceVersion
                | any hasFinalCore (mi_decls mod_iface) = finalCoreIfaceVersion
                | any hasEnumCallback (mi_decls mod_iface) = enumCallbackIfaceVersion
                | otherwise = show hiVersion
    writeHeader bh version
    payload <- tellBinWriter bh
    evidence <- putIfaceWithExtFields traceBinIface compressionLevel bh mod_iface
    if Set.null evidence
      then writeBinMem bh hi_path
      else do
        header <- openBinMem 128
        writeHeader header (version ++ evidenceSuffix evidence)
        -- The payload uses relative offsets, so it can follow the longer header.
        withBinaryFile hi_path WriteMode $ \ h ->
          withBinBuffer header $ \ prefix ->
            withBinBufferFrom bh payload $ \ body -> do
              BS.hPut h prefix
              BS.hPut h body

-- | Header suffix required by the retained evidence in the payload.
evidenceSuffix :: IfaceEvidenceVersion -> String
evidenceSuffix requirements
  | needs ErasedUserRuleEvidence || needs LexicalPackageRuleEvidence || needs PartialSynonymEvidence = feature_set
  | needs RetainedDefaultEvidence
  , needs ErasedExistentialEvidence || needs ScopedSigmaEvidence || needs ImplicitExistentialEvidence
  = feature_set
  | needs ImplicitExistentialEvidence && needs ScopedSigmaEvidence
  = feature_set
  | needs ScopedSigmaEvidence
  , needs IndexAliasEvidence || needs ErasedExistentialEvidence
  = combinedScopedSigmaSuffix
  | otherwise = maybe "" legacyEvidenceSuffix (Set.lookupMax requirements)
  where
    needs feature = Set.member feature requirements
    feature_set = featureSetPrefix ++ intercalate "+" [name | (name, feature) <- independentFeatures, needs feature]

legacyEvidenceSuffix :: IfaceEvidence -> String
legacyEvidenceSuffix PartialSynonymEvidence = featureSetPrefix ++ "partial-synonym"
legacyEvidenceSuffix ErasedUserRuleEvidence = featureSetPrefix ++ "user-rule"
legacyEvidenceSuffix LexicalPackageRuleEvidence = featureSetPrefix ++ "lexical-rule"
legacyEvidenceSuffix DepFunCoEvidence           = "-retained-depfunco-v1"
legacyEvidenceSuffix ConstructorCaseEvidence   = "-retained-constructor-case-v1"
legacyEvidenceSuffix RetainedSpecRuleEvidence  = "-retained-specialisation-rule-v1"
legacyEvidenceSuffix IndexAliasEvidence        = "-retained-index-alias-v1"
legacyEvidenceSuffix ErasedExistentialEvidence = "-erased-existentials-v1"
legacyEvidenceSuffix ImplicitExistentialEvidence = "-implicit-existentials-v1"
legacyEvidenceSuffix ScopedSigmaEvidence       = "-r04-scoped-sigma-experiment-v1"
legacyEvidenceSuffix RetainedDefaultEvidence   = "-retained-compulsory-default-v1"

combinedScopedSigmaSuffix :: String
combinedScopedSigmaSuffix = "-scoped-sigma-combined-v1"

featureSetPrefix :: String
featureSetPrefix = "-retained-features-v1-"

-- | Keep the wire order independent of declaration and compression order.
independentFeatures :: [(String, IfaceEvidence)]
independentFeatures = [ ("default", RetainedDefaultEvidence)
                      , ("erased", ErasedExistentialEvidence)
                      , ("implicit", ImplicitExistentialEvidence)
                      , ("scoped", ScopedSigmaEvidence)
                      , ("user-rule", ErasedUserRuleEvidence)
                      , ("lexical-rule", LexicalPackageRuleEvidence)
                      , ("partial-synonym", PartialSynonymEvidence) ]

-- | Accept the old suffixes and the canonical spelling of a new feature set.
parseEvidenceSuffix :: String -> Maybe IfaceEvidenceVersion
parseEvidenceSuffix "" = Just Set.empty
parseEvidenceSuffix suffix
  | suffix == combinedScopedSigmaSuffix
  = Just (Set.fromList [ScopedSigmaEvidence, ErasedExistentialEvidence])
  | Just feature <- find ((== suffix) . legacyEvidenceSuffix) [minBound..maxBound]
  = Just (Set.singleton feature)
  | Just names <- stripPrefix featureSetPrefix suffix
  , Just features <- traverse (`lookup` independentFeatures) (split '+' names)
  , let requirements = Set.fromList features
  , evidenceSuffix requirements == suffix
  = Just requirements
  | otherwise = Nothing

{- Note [Versioning retained evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In an optimized unfolding of second, a Pair alternative binds its two
fields and a nominal equality. A reader that expects only constructor
fields loses the equality and fails when the branch uses it. Give this
alternative its own binary tag. Select its version while emitting the
tag so a reader without support rejects the header before loading the
unfolding. A reader for constructor evidence also supports DepFunCo.
Cache the required version alongside the serialized payload. This
keeps the header consistent when the writer copies the cached bytes.
See Note [Native constructor evidence in interfaces] in GHC.IfaceToCore.

An automatically generated retained specialization rule uses its own
tag in the rule origin byte. Older readers reject a retained rule at
the header. Ordinary user and automatic rules retain their byte values.
See Note [Exported retained specialisations] in GHC.Core.Rules.

An erased user rule can construct or open a package on either side.
Record that requirement when writing the rule expression. A reader that
only supports calls at erased package types rejects the newer header.

An index alias uses a distinct DEFAULT alternative tag. An older reader
accepts ordinary DEFAULT alternatives from the same compiler. It must reject
the alias form at the header. See Note [Native index alias evidence in
interfaces] in GHC.IfaceToCore.

An interface can export an erased existential type with interface pragmas
omitted. Select the existential version when writing the existential type
tag. This also covers type entries in compression tables. An Unpack in an
unfolding can have an ordinary result type and refer to a package from
another interface. Select the version when writing each existential
expression tag as well. An existential coercion can serialize an ordinary
forall premise without an existential type tag. Select the version when
writing either of its built-in rule names. The dot existential reader also
knows the bar tags. It predates scoped sums and compulsory retained defaults.
Record the separate requirements when either occurs with a dot existential.

A retained-sum codomain uses a scoped family tag. A consumer can also
publish a proof that refers to the scoped equality rule. Either feature
requires this reader. Mark each feature when it is written, including
a proof in a consumer with no scoped family declarations. This
experiment uses its own version suffix. Its reader supports generated
specialization rules, constructor case evidence and dependent function
coercions. It predates index aliases and erased existentials. An interface
that also uses either feature needs a distinct header in both declaration
orders. Each historical reader must reject the mixed payload early.

A compulsory retained default includes the older index alias, rule and
coercion forms. Erased existentials and scoped sums have independent
requirements. Keep each requirement while writing. Use the existing
suffixes when their readers support the complete payload. Otherwise encode
the independent requirements in a canonical feature list. Check the list
before reading the payload. Reject unknown, duplicate or out-of-order names.
The cached payload keeps its requirements and writes the same suffix.
-}


-- Executable-origin declarations use a new IdDetails tag. Keep ordinary
-- interfaces readable by the older compiler. Its version check rejects
-- these producer interfaces before it reaches the new tag.
enumCallbackIfaceVersion :: String
enumCallbackIfaceVersion = show hiVersion ++ "-enum-callbacks-v1"

finalCoreIfaceVersion :: String
finalCoreIfaceVersion = show hiVersion ++ "-final-core-mapper-v1"

-- | Keep the old header for modules without a new type scheme.
-- A previous compiler must reject a changed family or synonym interface
-- before decoding old applications against its new binder list.
matchabilitySchemeIfaceVersion :: String
matchabilitySchemeIfaceVersion = show hiVersion ++ "-matchability-schemes-v1"

-- | Older readers must not load an associated family with a new binder list.
openMatchabilityIfaceVersion :: String
openMatchabilityIfaceVersion = show hiVersion ++ "-matchability-schemes-v2"

hasOpenMatchabilityScheme :: IfaceDecl -> Bool
hasOpenMatchabilityScheme IfaceFamily
  { ifBinders = b : _, ifFamFlav = IfaceOpenTypeFamilyTyCon } = matchabilityBinder b
hasOpenMatchabilityScheme IfaceClass{ifBody = IfConcreteClass{ifATs = ats}} =
  any (\(IfaceAT decl _) -> hasOpenMatchabilityScheme decl) ats
hasOpenMatchabilityScheme _ = False

hasMatchabilityScheme :: (a, IfaceDecl) -> Bool
hasMatchabilityScheme (_, IfaceSynonym{ifBinders = b : _}) = matchabilityBinder b
hasMatchabilityScheme (_, IfaceFamily{ifBinders = b : _}) = matchabilityBinder b
hasMatchabilityScheme _ = False

matchabilityBinder :: IfaceTyConBinder -> Bool
matchabilityBinder b@(Bndr (IfaceTvBndr (_, IfaceTyConApp tc IA_Nil)) _) =
  isInferredTyConBinder b && ifaceTyConName tc `hasKnownKey` matchabilityTyConKey
matchabilityBinder _ = False

hasFinalCore :: (a, IfaceDecl) -> Bool
hasFinalCore (_, IfaceId{ifIdDetails = IfSourceTableId _ FinalCoreSourceTable}) = True
hasFinalCore _ = False

-- | Test whether the declaration requires the executable-source tag.
hasEnumCallback :: (a, IfaceDecl) -> Bool
hasEnumCallback (_, IfaceId{ifIdDetails = IfSourceTableId _ ExecutableSourceTable}) = True
hasEnumCallback _ = False

-- | Puts the 'ModIface' to the 'WriteBinHandle'.
--
-- This avoids serialisation of the 'ModIface' if the fields 'mi_hi_bytes' contains a
-- 'Just' value. This field is populated by reading the 'ModIface' using
-- 'getIfaceWithExtFields' and not modifying it in any way afterwards.
putIfaceWithExtFields :: TraceBinIFace -> CompressionIFace -> WriteBinHandle -> ModIface -> IO IfaceEvidenceVersion
putIfaceWithExtFields traceBinIface compressionLevel bh mod_iface =
  case mi_hi_bytes mod_iface of
    FullIfaceBinHandle Strict.Nothing -> do
      let fields = mi_ext_fields mod_iface
      fields' <- case sourceTableLocations (mi_decls mod_iface) of
        []   -> pure (deleteField sourceTableField fields)
        locs -> writeFieldWith sourceTableField
                    (\h -> putWithUserData QuietBinIFace NormalCompression h locs) fields
      found <- newIORef Set.empty
      let notify _ required = modifyIORef' found (Set.insert required)
          feature_bh = addWriterToUserData (mkWriter notify) bh
      forwardPutRel_ feature_bh (\_ -> put_ feature_bh fields') $
        putWithUserData traceBinIface compressionLevel feature_bh mod_iface
      readIORef found
    FullIfaceBinHandle (Strict.Just (found, binData)) -> do
      putFullBinData bh binData
      pure found

{- Note [Source table locations in interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A source table supplies a checked equation for each constructor clause.
Keep its producer location for diagnostics in a separately compiled client.
Source coordinates do not affect type equality. IfaceAxiom's Binary instance
therefore excludes ifAxSourceSpans from the declaration and its ABI hash.
Store them in an optional field keyed by the local axiom name, in branch order.
This also keeps the interface format compatible with compilers without this
field. Use the ordinary interface dictionary within the field to share file
names as recorded in the source spans.

Include these locations in the full interface hash. This allows the producer
to rewrite its interface when a clause moves, even with -fno-write-if-self-recomp.
A client still checks the unchanged ABI of the equations. See addFingerprints
in GHC.Iface.Recomp.
-}

sourceTableField :: String
sourceTableField = "ghc.source-table-locations.v1"

-- | Collect locations from the interface declarations, which are in OccName order.
sourceTableLocations :: [(a, IfaceDecl)] -> [(OccName, [BinSrcSpan])]
sourceTableLocations decls =
    [ (nameOccName name, map BinSrcSpan spans)
    | (_, IfaceAxiom{ifName = name, ifAxSourceSpans = spans}) <- decls, not (null spans) ]

-- | Put a piece of data with an initialised `UserData` field. This
-- is necessary if you want to serialise Names or FastStrings.
-- It also writes a symbol table and the dictionary.
-- This segment should be read using `getWithUserData`.
putWithUserData :: Binary a => TraceBinIFace -> CompressionIFace -> WriteBinHandle -> a -> IO ()
putWithUserData traceBinIface compressionLevel bh payload = do
  (name_count, fs_count, ifacetype_count, _b) <- putWithTables compressionLevel bh (\bh' -> put bh' payload)

  case traceBinIface of
    QuietBinIFace         -> return ()
    TraceBinIFace printer -> do
       printer (text "writeBinIface:" <+> int name_count
                                      <+> text "Names")
       printer (text "writeBinIface:" <+> int fs_count
                                      <+> text "dict entries")
       printer (text "writeBinIface:" <+> int ifacetype_count
                                      <+> text "iface type entries")

-- | Write name/symbol/ifacetype tables
--
-- 1. setup the given BinHandle with Name/FastString/IfaceType table handling
-- 2. write the following
--    - FastString table pointer
--    - Name table pointer
--    - IfaceType table pointer
--    - payload
--    - IfaceType table
--    - Name table
--    - FastString table
--
-- It returns (number of names, number of FastStrings, number of IfaceTypes, payload write result)
--
-- See Note [Order of deduplication tables during iface binary serialisation]
putWithTables :: CompressionIFace -> WriteBinHandle -> (WriteBinHandle -> IO b) -> IO (Int, Int, Int, b)
putWithTables compressionLevel bh' put_payload = do
  -- Initialise deduplicating tables.
  (fast_wt, fsWriter) <- initFastStringWriterTable
  (name_wt, nameWriter) <- initNameWriterTable
  (ifaceType_wt, ifaceTypeWriter) <- initWriteIfaceType compressionLevel

  -- Initialise the 'WriterUserData'.
  --
  -- Similar to how 'getTables' calls 'addReaderToUserData', here we
  -- call 'addWriterToUserData' instead of 'setWriterUserData', to
  -- avoid overwriting existing writers of other types in bh'.
  let bh =
        addWriterToUserData fsWriter
          $ addWriterToUserData nameWriter
          -- We sometimes serialise binding and non-binding names differently, but
          -- not during 'ModIface' serialisation. Here, we serialise both to the same
          -- deduplication table.
          --
          -- See Note [Binary UserData]
          $ addWriterToUserData
            (mkWriter $ \bh name -> putEntry nameWriter bh $ getBindingName name)
          $ addWriterToUserData ifaceTypeWriter bh'

  ([fs_count, name_count, ifacetype_count] , r) <-
    -- The order of these entries matters!
    --
    -- See Note [Order of deduplication tables during iface binary serialisation] for details.
    putAllTables bh [fast_wt, name_wt, ifaceType_wt] $ do
      put_payload bh

  return (name_count, fs_count, ifacetype_count, r)

-- | Write all deduplication tables to disk after serialising the
-- main payload.
--
-- Writes forward pointers to the deduplication tables before writing the payload
-- to allow deserialisation *before* the payload is read again.
putAllTables :: WriteBinHandle -> [WriterTable] -> IO b -> IO ([Int], b)
putAllTables _ [] act = do
  a <- act
  pure ([], a)
putAllTables bh (x : xs) act = do
  (r, (res, a)) <- forwardPutRel bh (const $ putTable x bh) $ do
    putAllTables bh xs act
  pure (r : res, a)

-- | Initial ram buffer to allocate for writing interface files
initBinMemSize :: Int
initBinMemSize = 1024 * 1024

binaryInterfaceMagic :: Platform -> FixedLengthEncoding Word32
binaryInterfaceMagic platform
 | target32Bit platform = FixedLengthEncoding 0x1face
 | otherwise            = FixedLengthEncoding 0x1face64


{-
Note [Deduplication during iface binary serialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we serialise a 'ModIface', many symbols are redundant.
For example, there can be many duplicated 'FastString's and 'Name's.
To save space, we deduplicate duplicated symbols, such as 'FastString' and 'Name',
by maintaining a table of already seen symbols.

Besides saving a lot of disk space, this additionally enables us to automatically share
these symbols when we read the 'ModIface' from disk, without additional mechanisms such as 'FastStringTable'.

The general idea is, when serialising a value of type 'Name', we first have to create a deduplication
table (see 'putWithTables.initNameWriterTable' for example). Then, we create a 'BinaryWriter' function
which we add to the 'WriterUserData'. When this 'BinaryWriter' is used to serialise a value of type 'Name',
it looks up whether we have seen this value before. If so, we write an index to disk.
If we haven't seen the value before, we add it to the deduplication table and produce a new index.

Both the 'ReaderUserData' and 'WriterUserData' can contain many 'BinaryReader's and 'BinaryWriter's
respectively, which can each individually be tweaked to use a deduplication table, or to serialise
the value without deduplication.

After the payload (e.g., the 'ModIface') has been serialised to disk, we serialise the deduplication tables
to disk. This happens in 'putAllTables', where we serialise all tables that we use during 'ModIface'
serialisation. See 'initNameWriterTable' and 'putSymbolTable' for an implementation example.
This uses the 'real' serialisation function, e.g., 'serialiseName'.
However, these tables need to be deserialised before we can read the 'ModIface' from disk.
Thus, we write before the 'ModIface' a forward pointer to the deduplication table, so we can
read this table before deserialising the 'ModIface'.

To add a deduplication table for a type, let us assume 'IfaceTyCon', you need to do the following:

* The 'Binary' instance 'IfaceTyCon' needs to dynamically look up the serialiser function instead of
  serialising the value of 'IfaceTyCon'. It needs to look up the serialiser in the 'ReaderUserData' and
  'WriterUserData' respectively.
  This allows us to change the serialisation of 'IfaceTyCon' at run-time.
  We can still serialise 'IfaceTyCon' to disk directly, or use a deduplication table to reduce the size of
  the .hi file.

  For example:

  @
    instance Binary IfaceTyCon where
      put_ bh ty = case findUserDataWriter (Proxy @IfaceTyCon) bh of
        tbl -> putEntry tbl bh ty
      get bh     = case findUserDataReader (Proxy @IfaceTyCon) bh of
        tbl -> getEntry tbl bh
  @

  We include the signatures of 'findUserDataWriter' and 'findUserDataReader' to make this code example
  easier to understand:

  @
    findUserDataReader :: Typeable a => Proxy a -> ReadBinHandle -> BinaryReader a
    findUserDataWriter :: Typeable a => Proxy a -> WriteBinHandle -> BinaryWriter a
  @

  where 'BinaryReader' and 'BinaryWriter' correspond to the 'Binary' class methods
  'get' and 'put_' respectively, thus:

  @
    newtype BinaryReader s = BinaryReader { getEntry :: ReadBinHandle -> IO s }

    newtype BinaryWriter s = BinaryWriter { putEntry :: WriteBinHandle -> s -> IO () }
  @

  'findUserData*' looks up the serialisation function for 'IfaceTyCon', which we then subsequently
  use to serialise said 'IfaceTyCon'. If no such serialiser can be found, 'findUserData*'
  crashes at run-time.

* Whenever a value of 'IfaceTyCon' needs to be serialised, there are two serialisation functions involved:

  * The literal serialiser that puts/gets the value to/from disk:
      Writes or reads a value of type 'IfaceTyCon' from the 'Write/ReadBinHandle'.
      This serialiser is primarily used to write the values stored in the deduplication table.
      It is also used to read the values from disk.

  * The deduplicating serialiser:
      Replaces the serialised value of 'IfaceTyCon' with an offset that is stored in the
      deduplication table.
      This serialiser is used while serialising the payload.

  We need to add the deduplicating serialiser to the 'ReaderUserData' and 'WriterUserData'
  respectively, so that 'findUserData*' can find them.

  For example, adding a serialiser for writing 'IfaceTyCon's:

  @
    let bh0 :: WriteBinHandle = ...
        putIfaceTyCon = ... -- Serialises 'IfaceTyCon' to disk
        bh = addWriterToUserData (mkSomeBinaryWriter putIfaceTyCon) bh0
  @

  Naturally, you have to do something similar for reading values of 'IfaceTyCon'.

  The provided code example implements the previous behaviour:
  serialise all values of type 'IfaceTyCon' directly. No deduplication is happening.

  Now, instead of literally putting the value, we can introduce a deduplication table!
  Instead of specifying 'putIfaceTyCon', which writes a value of 'IfaceTyCon' directly to disk,
  we provide a function that looks up values in a table and provides an index of each value
  we have already seen.
  If the particular 'IfaceTyCon' we want to serialise isn't already in the de-dup table,
  we allocate a new index and extend the table.

  See the definition of 'initNameWriterTable' and 'initNameReaderTable' for example deduplication tables.

* Storing the deduplication table.

  After the deduplicating the elements in the payload (e.g., 'ModIface'), we now have a deduplication
  table full with all the values.
  We serialise this table to disk using the real serialiser (e.g., 'putIfaceTyCon').

  When serialisation is complete, we write out the de-dup table in 'putAllTables',
  serialising each 'IfaceTyCon' in the table.  Of course, doing so might in turn serialise
  another de-dup'd thing (e.g. a FastString), thereby extending its respective de-dup table.

Note [Order of deduplication tables during iface binary serialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Serialisation of 'ModIface' uses tables to deduplicate symbols that occur often.
See Note [Deduplication during iface binary serialisation].

After 'ModIface' has been written to disk, we write the deduplication tables.
Writing a table may add additional entries to *other* deduplication tables, thus
we need to make sure that the symbol table we serialise only depends on
deduplication tables that haven't been written to disk yet.

For example, assume we maintain deduplication tables for 'FastString' and 'Name'.
The symbol 'Name' depends on 'FastString', so serialising a 'Name' may add a 'FastString'
to the 'FastString' deduplication table.
Thus, 'Name' table needs to be serialised to disk before the 'FastString' table.

When we read the 'ModIface' from disk, we consequentially need to read the 'FastString'
deduplication table from disk, before we can deserialise the 'Name' deduplication table.
Therefore, before we serialise the tables, we write forward pointers that allow us to jump ahead
to the table we need to deserialise first.
What deduplication tables exist and the order of serialisation is currently statically specified
in 'putWithTables'. 'putWithTables' also takes care of the serialisation of used deduplication tables.
The deserialisation of the deduplication tables happens 'getTables', using 'Binary' utility
functions such as 'forwardGetRel'.

Here, a visualisation of the table structure we currently have (ignoring 'ExtensibleFields'):

┌──────────────┐
│   Headers    │
├──────────────┤
│   Ptr FS     ├────────┐
├──────────────┤        │
│   Ptr Name   ├─────┐  │
├──────────────┤     │  │
│              │     │  │
│   ModIface   │     │  │
│   Payload    │     │  │
│              │     │  │
├──────────────┤     │  │
│              │     │  │
│  Name Table  │◄────┘  │
│              │        │
├──────────────┤        │
│              │        │
│   FS Table   │◄───────┘
│              │
└──────────────┘

-}

{-
Note [Lazy ReaderUserData during IfaceType serialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Serialising recursive data types, such as 'IfaceType', requires some trickery
to inject the deduplication table at the right moment.

When we serialise a value of 'IfaceType', we might encounter new 'IfaceType' values.
For example, 'IfaceAppTy' has an 'IfaceType' field, which we want to deduplicate as well.
Thus, when we serialise an 'IfaceType', we might add new 'IfaceType's to the 'GenericSymbolTable'
(i.e., the deduplication table). These 'IfaceType's are then subsequently also serialised to disk,
and uncover new 'IfaceType' values, etc...
In other words, when we serialise an 'IfaceType' we write it out using a post-order traversal.
See 'putGenericSymbolTable' for the implementation.

Now, when we deserialise the deduplication table, reading the first element of the deduplication table
will fail, as deserialisation requires that we read the child elements first. Remember, we wrote them to disk
using a post-order traversal.
To make this work, we therefore use 'lazyGet'' to lazily read the parent 'IfaceType', but delay the actual
deserialisation. We just assume that once you need to force a value, the deduplication table for 'IfaceType'
will be available.

That's where 'bhRef' comes into play:

@
    bhRef <- newIORef (error "used too soon")
    ud <- unsafeInterleaveIO (readIORef bhRef)
    ...
    ifaceTypeReaderTable <- initReadIfaceTypeTable ud
    ...
    writeIORef bhRef (getReaderUserData bhFinal)
@

'ud' is the 'ReaderUserData' that will eventually contain the deduplication table for 'IfaceType'.
As deserialisation of the 'IfaceType' needs the deduplication table, we provide a
promise that it will exist in the future (represented by @unsafeInterleaveIO (readIORef bhRef)@).
We pass 'ud' to 'initReadIfaceTypeTable', so the deserialisation will use the promised deduplication table.

Once we have "read" the deduplication table, it will be available in 'bhFinal', and we fulfill the promise
that the deduplication table for 'IfaceType' exists when forced.
-}

-- -----------------------------------------------------------------------------
-- The symbol table
--

initReadIfaceTypeTable :: ReaderUserData -> IO (ReaderTable IfaceType)
initReadIfaceTypeTable ud = do
  pure $
    ReaderTable
      { getTable = getGenericSymbolTable (\bh -> lazyGet' getIfaceType (setReaderUserData bh ud))
      , mkReaderFromTable = \tbl -> mkReader (getGenericSymtab tbl)
      }

initWriteIfaceType :: CompressionIFace -> IO (WriterTable, BinaryWriter IfaceType)
initWriteIfaceType compressionLevel = do
  sym_tab <- initGenericSymbolTable @(Map IfaceType)
  pure
    ( WriterTable
        { putTable = putGenericSymbolTable sym_tab (lazyPut' putIfaceType)
        }
    , mkWriter $ ifaceWriter sym_tab
    )
  where
    ifaceWriter sym_tab = case compressionLevel of
      NormalCompression -> literalIfaceTypeSerialiser
      SafeExtraCompression -> ifaceTyConAppSerialiser sym_tab
      MaximumCompression -> fullIfaceTypeSerialiser sym_tab

    ifaceTyConAppSerialiser sym_tab bh ty = case ty of
      IfaceTyConApp {} -> do
        put_ bh ifaceTypeSharedByte
        putGenericSymTab sym_tab bh ty
      _ -> putIfaceType bh ty

    fullIfaceTypeSerialiser sym_tab bh ty = do
      put_ bh ifaceTypeSharedByte
      putGenericSymTab sym_tab bh ty

    literalIfaceTypeSerialiser = putIfaceType


initNameReaderTable :: NameCache -> IO (ReaderTable Name)
initNameReaderTable cache = do
  return $
    ReaderTable
      { getTable = \bh -> getSymbolTable bh cache
      , mkReaderFromTable = \tbl -> mkReader (getSymtabName tbl)
      }

data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt,
                        -- ^ The next index to use
        bin_symtab_map  :: !(IORef (NameEnv Int, ModuleEnv [(Int,Name)]))
                        -- ^ Deduplication indexed by Name
                        -- ; Group table data by module for serialization
  }

initNameWriterTable :: IO (WriterTable, BinaryWriter Name)
initNameWriterTable = do
  symtab_next <- newFastMutInt 0
  symtab_map <- newIORef (emptyNameEnv, emptyModuleEnv)
  let bin_symtab =
        BinSymbolTable
          { bin_symtab_next = symtab_next
          , bin_symtab_map  = symtab_map
          }

  let put_symtab bh = do
        name_count <- readFastMutInt symtab_next
        (_, symtab_tbl) <- readIORef symtab_map
        putSymbolTable bh name_count symtab_tbl
        pure name_count

  return
    ( WriterTable
        { putTable = put_symtab
        }
    , mkWriter $ putName bin_symtab
    )


{- |
The symbol table payload will look like:

  <total name count>
  $modules.size
  for (mod, names) in $modules:
    $mod
    $names.size
    for table_ix, occ in $names
      $table_ix
      $name.occ
      $name.is_known_key
-}
putSymbolTable :: WriteBinHandle -> Int -> ModuleEnv [(Int, Name)] -> IO ()
putSymbolTable bh name_count symtab = do
    put_ bh name_count
    put_ bh (sizeModuleEnv symtab)
    forM_ (moduleEnvToList symtab) $ \(mod,names) -> do
      put_ bh mod
      put_ bh (length names)
      forM_ names $ \(table_ix, name) -> do
        let occ = assertPpr (isExternalName name) (ppr name) (nameOccName name)
        put_ bh table_ix
        put_ bh occ
        put_ bh (isKnownKeyName name)

-- | Decode the symbol table -- layout set by 'putSymbolTable'.
getSymbolTable :: ReadBinHandle -> NameCache -> IO (SymbolTable Name)
-- Create an array of Names for the symbols and add them to the NameCache
getSymbolTable bh name_cache = do
    sz <- get bh :: IO Int
    updateNameCache' name_cache $ \cache0 -> do
      mut_arr <- newArray_ (0, sz-1) :: IO (IOArray Int Name)
      mods_sz <- get bh :: IO Int
      cache <-
        foldGet' (fromIntegral mods_sz) bh cache0 $ \_mod_ix (mod,mod_nms_sz) cache1 ->
          foldGet' (fromIntegral (mod_nms_sz::Int)) bh cache1 (deserialise_one mut_arr mod)
      arr <- unsafeFreeze mut_arr
      return (cache, arr)
  where
    deserialise_one :: (IOArray Int Name)
                    -> Module
                    -> Word -> (Int, OccName, Bool)
                    -> OrigNameCache -> IO OrigNameCache
    deserialise_one mut_arr mod _mod_nms_ix (table_ix, occ, is_known_key) cache2 = do
      case lookupOrigNameCache cache2 mod occ of
        Just name -> do
          writeArray mut_arr table_ix name
          return cache2
        Nothing
          | is_known_key -> do
              let uniq = lookupOccEnv knownKeyOccMap occ `orElse`
                           pprPanic "getSymbolTable" (ppr occ)
              extend_cache_with (mkKnownKeyName uniq mod occ noSrcSpan)
          | otherwise -> do
              uniq <- takeUniqFromNameCache name_cache
              extend_cache_with (mkExternalName uniq mod occ noSrcSpan)
      where
        extend_cache_with name = do
          let cache3 = extendOrigNameCache cache2 mod occ name
          writeArray mut_arr table_ix name
          return cache3


{-
Note [Symbol table representation of names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For serialisation we divide Names into two classes:
  * Compact     Names are serialised simply as their unique.
  * Non-compact Names are entered into the Symbol Table of the interface file,
                      and serialised as the index into that table

In more detail:
* Compact Names comprise
  - All WiredIn Names
  - All Names related to tuples, whether WiredIn or not.  See (ST2) below.

* The key property of a compact Name is that GHC can compute the Name from
  its Unique, via `lookupCompactName`:
    - For most wired-in names, we look up in the `wiredInNameMap`.
    - For names related to tuples, we use `knownUniqueTupleName`
  Tuples aren't included in the wired-in names map: see (ST1) below

* Serialisation is done by `putName`:
  - When we serialise a compact Name,
    we serialise it as a single 32-bit word:
      10xxxxxx xxyyyyyy yyyyyyyy yyyyyyyy
    where xxxx is the tag, and yyyy is the payload.
    The function `wiredInNamesOkay` checks that the wired-in names all have
    uniques that fit into the `yyy` field.

  - When we serialise a non-compact name:
      - We look it up in the (stateful, growing) symbol table
      - If it not there we add it to the symbol table
      - We serialise the occurrenc to a single 32-bit word:
          00xxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
        where `xxxxx` is an index into the symbol table.

* Deserialision is done by `getName`.  We read a 32-bit word
  - If the MSB is `10` it must be a compact name, so we use
    `lookupCompactName` to get from the Unique to the Name.
  - If the MSB is `00` it must be a non-compact Name,
    so we look it up in the symbol table.

Wrinkles:

(ST1) There are many, many tuple types and constructors, so we don't put
  them in the wiredInNameMap. Instead we put them in a distinct part of the
  Unique namespace, and provide
     knownUniqueTupleName :: Unique -> Maybe Name
  to identify such a Unique and map it to the corresponding Name.
  See Note [Infinite families of known-key names].

(ST2) A wired-in data constructor, like (#,#) has a related, also wired-in
  (promoted) type constructor; see `mkPromotedDataCon`.  That type constructor
  in turn contains its `TyConRepName` (e.g. $tc'(#,#)) to support Typeable.

  That `TyConRepName` is built by `mkPrelTyConRepName`, but it is /not/
  wired-in.  Why not? Because its definition involves fingerprints etc.  (Maybe
  it could be made wired-in, but it would tricky, and there is no point.)

  Nevertheless, although it is not wired-in, it is /compact/; that is, we can
  serialise and de-serialise it using the mechanisms above.

  This idea is, however, entirely optional.  You could delete the line in
  `isCompactName` that tests for `knownUniqueTupleName` and then the
  TyConRepNames would be serialised as non-compact names, and everything would
  work. Fewer tests, but Typeable-heavy code might have bigger interface files.
-}

isCompactName :: Name -> Bool
-- See Note [Symbol table representation of names]
isCompactName n
  | isWiredInName n                              = True   -- This will catch tuples too!
  | isJust (knownUniqueTupleName (nameUnique n)) = True   -- Optional: see wrinkle (ST2)
  | otherwise                                    = False

lookupCompactName :: Unique -> Name
-- See Note [Symbol table representation of names]
lookupCompactName u
  | Just n <- knownUniqueTupleName u               = n    -- See wrinkle (ST1)
  | Just n <- lookupUFM_Directly wiredInNamesMap u = n
  | otherwise = pprPanic "lookupCompactName" (ppr u $$ char tag $$ ppr ix)
  where
     (tag, ix) = unpkUniqueGrimily u

-- See Note [Symbol table representation of names]
putName :: BinSymbolTable -> WriteBinHandle -> Name -> IO ()
putName BinSymbolTable{
               bin_symtab_map = symtab_map_ref,
               bin_symtab_next = symtab_next }
        bh name
  | isCompactName name
  , let (c, u) = unpkUniqueGrimily (nameUnique name) -- INVARIANT: (ord c) fits in 8 bits
  = -- assert (u < 2^(22 :: Int))
    put_ bh (0x80000000
             .|. (fromIntegral (ord c) `shiftL` 22)
             .|. (fromIntegral u :: Word32))

  | otherwise
  = do (symtab_map,symtab_tbl) <- readIORef symtab_map_ref
       case lookupNameEnv symtab_map name of
         Just off -> put_ bh (fromIntegral off :: Word32)
         Nothing -> do
          off <- freshIndex
          let mod = nameModule name
          let mod_nms = fromMaybe [] (lookupModuleEnv symtab_tbl mod)

          let !symtab_map' = extendNameEnv symtab_map name off
          let !symtab_tbl' = extendModuleEnv symtab_tbl mod ((off,name):mod_nms)
          writeIORef symtab_map_ref $! ( symtab_map',  symtab_tbl' )

          put_ bh (fromIntegral off :: Word32)
  where
    freshIndex :: IO Int
    freshIndex = do
      off <- readFastMutInt symtab_next
      -- massert (off < 2^(30 :: Int))
      writeFastMutInt symtab_next (off+1)
      return off

-- See Note [Symbol table representation of names]
getSymtabName :: SymbolTable Name
              -> ReadBinHandle -> IO Name
getSymtabName symtab bh = do
    i :: Word32 <- get bh
    case i .&. 0xC0000000 of
      0x00000000 -> return $! symtab ! fromIntegral i
      0x80000000 -> return $! lookupCompactName u
        where
          tag = chr (fromIntegral ((i .&. 0x3FC00000) `shiftR` 22))
          ix  = fromIntegral i .&. 0x003FFFFF
          u   = mkUniqueGrimilyWithTag tag ix

      _ -> pprPanic "getSymtabName:unknown name tag" (ppr i)

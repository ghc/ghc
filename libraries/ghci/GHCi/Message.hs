{-# LANGUAGE RecursiveDo, GADTs, DeriveGeneric, StandaloneDeriving, ScopedTypeVariables,
    GeneralizedNewtypeDeriving, ExistentialQuantification, RecordWildCards, CPP,
    NamedFieldPuns, LambdaCase, TypeAbstractions, TypeApplications, PatternSynonyms,
    LinearTypes, RankNTypes, UnboxedTuples, BlockArguments, TupleSections, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}

-- |
-- Remote GHCi message types and serialization.
--
-- For details on Remote GHCi, see Note [Remote GHCi] in
-- compiler/GHC/Runtime/Interpreter.hs.
--
module GHCi.Message
  ( Message(..), Msg(..)
  , ConInfoTable(..)
  , THMessage(..), THMsg(..)
  , QResult(..)
  , EvalStatus_(..,EvalBreak), EvalStatus, EvalResult(..), EvalOpts(..), EvalExpr(..)
  , EvalBreak(..), EvalBreakpoint (..)
  , SerializableException(..)
  , toSerializableException, fromSerializableException
  , THResult(..), THResultType(..)
  , QState(..)
  , getMessage, putMessage, getTHMessage, putTHMessage

  , Pipe(..), mkPipeFromHandles, mkPipeFromContinuations, closePipe
  , remoteCall, remoteTHCall
  , handleRemoteCall, handleRemoteTHCall, writeReply
  , RemoteProcessException(..), PipeReadClosedException(..)

  , InboxId(..)
  , newMailbox
  , closeMailbox

  , BreakModule
  , BreakUnitId
  , LoadedDLL
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHCi.RemoteTypes
import GHCi.FFI
import GHCi.TH.Binary () -- For Binary instances
import GHCi.BreakArray
import GHCi.ResolvedBCO

import GHC.LanguageExtensions
import GHC.InfoProv
#if MIN_VERSION_ghc_internal(9,1500,0)
import qualified GHC.Exts.Heap as Heap
#else
import qualified GHC.Exts.Heap as Heap
#endif
import GHC.ForeignSrcLang
import GHC.Fingerprint
import GHC.Conc (pseq, par)
import GHC.IO (IO(..))
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as BS
import Data.Dynamic
import Data.Typeable (TypeRep)
import Data.IORef
import Data.Map (Map)
import Foreign
import GHC.Generics
import GHC.Stack.CCS
import qualified GHC.Boot.TH.Syntax        as TH
import qualified GHC.Boot.TH.Monad         as TH
import GHCi.Message.Async
import Control.Monad.STM

-- -----------------------------------------------------------------------------
-- The RPC protocol between GHC and the interactive server

-- | A @Message a@ is a message that returns a value of type @a@.
-- These are requests sent from GHC to the server.
data Message a where
  -- | Exit the iserv process
  Shutdown :: Message ()
  RtsRevertCAFs :: Message ()

  -- RTS Linker -------------------------------------------

  -- These all invoke the corresponding functions in the RTS Linker API.
  InitLinker :: Message ()
  LookupSymbol :: !BS.ShortByteString -> Message (Maybe (RemotePtr ()))
  LookupSymbolInDLL :: !(RemotePtr LoadedDLL) -> !BS.ShortByteString -> Message (Maybe (RemotePtr ()))
  LookupClosure :: !BS.ShortByteString -> Message (Maybe HValueRef)
  LoadDLLs :: [String] -> Message (Either String [RemotePtr LoadedDLL])
  LoadArchive :: String -> Message () -- error?
  LoadObj :: String -> Message () -- error?
  UnloadObj :: String -> Message () -- error?
  AddLibrarySearchPath :: String -> Message (RemotePtr ())
  RemoveLibrarySearchPath :: RemotePtr () -> Message Bool
  ResolveObjs :: Message Bool
  FindSystemLibrary :: String -> Message (Maybe String)

  -- Interpreter -------------------------------------------

  -- | Create a set of BCO objects, and return HValueRefs to them
  -- See @createBCOs@ in compiler/GHC/Runtime/Interpreter.hs.
  -- NB: this has a custom Binary behavior,
  -- see Note [Parallelize CreateBCOs serialization]
  CreateBCOs :: [ResolvedBCO] -> Message [HValueRef]

  -- | Release 'HValueRef's
  FreeHValueRefs :: [HValueRef] -> Message ()

  -- | Add entries to the Static Pointer Table
  AddSptEntry :: Fingerprint -> HValueRef -> Message ()
  -- | Add module to hpc
  AddHpcModule :: BS.ShortByteString -> Int -> Int -> BS.ShortByteString -> Message ()

  -- | Malloc some data and return a 'RemotePtr' to it
  MallocData :: ByteString -> Message (RemotePtr ())
  MallocStrings :: [ByteString] -> Message [RemotePtr ()]

  -- | Calls 'GHCi.FFI.prepareForeignCall'
  PrepFFI :: [FFIType] -> FFIType -> Message (RemotePtr C_ffi_cif)

  -- | Free data previously created by 'PrepFFI'
  FreeFFI :: RemotePtr C_ffi_cif -> Message ()

  -- | Create an info table for a constructor
  MkConInfoTable
   :: !ConInfoTable
   -> Message (RemotePtr Heap.StgInfoTable)

  -- | Evaluate a statement
  EvalStmt
    :: EvalOpts
    -> EvalExpr HValueRef {- IO [a] -}
    -> Message (RemoteRef ThreadId) {- 'WaitThreadResult' of this thread yields [a] -}

  -- | Resume evaluation of a statement after a breakpoint
  ResumeStmt
   :: EvalOpts
   -> RemoteRef ThreadId
   -> Message ()

  -- | Abandon evaluation of a statement after a breakpoint
  AbandonStmt
   :: RemoteRef ThreadId
   -> Message ()

  -- | Evaluate something of type @IO String@
  EvalString
    :: HValueRef {- IO String -}
    -> Message (EvalResult String)

  -- | Evaluate something of type @String -> IO String@
  EvalStringToString
    :: HValueRef {- String -> IO String -}
    -> String
    -> Message (EvalResult String)

  -- | Evaluate something of type @IO ()@
  EvalIO
   :: HValueRef {- IO a -}
   -> Message (EvalResult ())

  -- | Create a set of CostCentres with the same module name
  MkCostCentres
   :: !(RemotePtr ())                             -- ModuleName
   -> ![(BS.ShortByteString, BS.ShortByteString)] -- (name, SrcSpan)
   -> Message [RemotePtr CostCentre]

  -- | Show a 'CostCentreStack' as a @[String]@
  CostCentreStackInfo
   :: RemotePtr CostCentreStack
   -> Message [String]

  -- | Create a new array of breakpoint flags
  NewBreakArray
   :: Int                               -- size
   -> Message (RemoteRef BreakArray)

  -- | Set how many times a breakpoint should be ignored
  --   also used for enable/disable
  SetupBreakpoint
   :: RemoteRef BreakArray
   -> Int                           -- breakpoint index
   -> Int                           -- ignore count to be stored in the BreakArray
                                    -- -1 disable; 0 enable; >= 1 enable, ignore count.
   -> Message ()

  -- | Query the status of a breakpoint (True <=> enabled)
  BreakpointStatus
   :: RemoteRef BreakArray
   -> Int                               -- index
   -> Message Bool                      -- True <=> enabled

  -- | Get a reference to a free variable at a breakpoint
  GetBreakpointVar
   :: HValueRef                         -- the AP_STACK from EvalBreak
   -> Int
   -> Message (Maybe HValueRef)

  -- Template Haskell -------------------------------------------
  -- For more details on how TH works with Remote GHCi, see
  -- Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs.

  -- | Start a new TH module, return a state token that should be
  StartTH :: Message (RemoteRef (IORef QState))

  -- | Evaluate a TH computation.
  --
  -- Returns a ByteString, because we have to force the result
  -- before returning it to ensure there are no errors lurking
  -- in it.  The TH types don't have NFData instances, and even if
  -- they did, we have to serialize the value anyway, so we might
  -- as well serialize it to force it.
  RunTH
   :: InboxId -- ^ announce mailbox for related 'remoteTHCall's
   -> RemoteRef (IORef QState)
   -> HValueRef {- e.g. TH.Q TH.Exp -}
   -> THResultType
   -> Maybe TH.Loc
   -> Message (QResult ByteString)

  -- | Run the given mod finalizers.
  RunModFinalizers :: InboxId -- ^ announce mailbox for related 'remoteTHCall's
                   -> RemoteRef (IORef QState)
                   -> [RemoteRef (TH.Q ())]
                   -> Message (QResult ())

  -- | Remote interface to GHC.Internal.Heap.getClosureData. This is used by
  -- the GHCi debugger to inspect values in the heap for :print and
  -- type reconstruction.
  GetClosure
    :: HValueRef
    -> Message (Heap.GenClosure HValueRef)

  -- | Remote interface to GHC.InfoProv.whereFrom. This is used by
  -- the GHCi debugger to inspect the provenance of thunks for :print.
  WhereFrom
    :: HValueRef
    -> Message (Maybe InfoProv)

  -- | Evaluate something. This is used to support :force in GHCi.
  Seq
    :: HValueRef
    -> Message (RemoteRef ThreadId)

  -- | Resume forcing a free variable in a breakpoint (#2950)
  ResumeSeq
    :: RemoteRef ThreadId
    -> Message ()

  WaitThreadResult
    :: RemoteRef ThreadId
    -> Message (EvalStatus [HValueRef])

  WaitAnyThreadBreak
    :: Message EvalBreak

  WaitThreadResultOrAnyThreadBreak
    :: RemoteRef ThreadId
    -> Message (EvalStatus [HValueRef])

  -- | User-defined request encoded as a tag/payload pair.  This is left
  -- uninterpreted by GHC and is meant for GHC API applications to be able to supply
  -- their own interpreter which understands additional commands.
  CustomMessage :: Word8 -> ByteString -> Message ByteString

deriving instance Show (Message a)

-- | Used to dynamically create a data constructor's info table at
-- run-time.
data ConInfoTable = ConInfoTable {
  conItblTablesNextToCode :: !Bool, -- ^ TABLES_NEXT_TO_CODE
  conItblPtrs :: !Int,              -- ^ ptr words
  conItblNPtrs :: !Int,             -- ^ non-ptr words
  conItblConTag :: !Int,            -- ^ constr tag
  conItblPtrTag :: !Int,            -- ^ pointer tag
  conItblDescr :: !ByteString       -- ^ constructor desccription
}
  deriving (Generic, Show)

instance Binary ConInfoTable

instance NFData ConInfoTable

-- | Template Haskell return values
data QResult a
  = QDone a
    -- ^ RunTH finished successfully; return value follows
  | QException String
    -- ^ RunTH threw an exception
  | QFail String
    -- ^ RunTH called 'fail'
  deriving (Generic, Show)

instance Binary a => Binary (QResult a)


-- | Messages sent back to GHC from GHCi.TH, to implement the methods
-- of 'Quasi'.  For an overview of how TH works with Remote GHCi, see
-- Note [Remote Template Haskell] in GHCi.TH.
data THMessage a where
  NewName :: String -> THMessage (THResult TH.Name)
  Report :: Bool -> String -> THMessage (THResult ())
  LookupName :: Bool -> String -> THMessage (THResult (Maybe TH.Name))
  Reify :: TH.Name -> THMessage (THResult TH.Info)
  ReifyFixity :: TH.Name -> THMessage (THResult (Maybe TH.Fixity))
  ReifyType :: TH.Name -> THMessage (THResult TH.Type)
  ReifyInstances :: TH.Name -> [TH.Type] -> THMessage (THResult [TH.Dec])
  ReifyRoles :: TH.Name -> THMessage (THResult [TH.Role])
  ReifyAnnotations :: TH.AnnLookup -> TypeRep
    -> THMessage (THResult [ByteString])
  ReifyModule :: TH.Module -> THMessage (THResult TH.ModuleInfo)
  ReifyConStrictness :: TH.Name -> THMessage (THResult [TH.DecidedStrictness])

  GetPackageRoot :: THMessage (THResult FilePath)
  AddDependentFile :: FilePath -> THMessage (THResult ())
  AddDependentDirectory :: FilePath -> THMessage (THResult ())
  AddTempFile :: String -> THMessage (THResult FilePath)
  AddModFinalizer :: RemoteRef (TH.Q ()) -> THMessage (THResult ())
  AddCorePlugin :: String -> THMessage (THResult ())
  AddTopDecls :: [TH.Dec] -> THMessage (THResult ())
  AddForeignFilePath :: ForeignSrcLang -> FilePath -> THMessage (THResult ())
  IsExtEnabled :: Extension -> THMessage (THResult Bool)
  ExtsEnabled :: THMessage (THResult [Extension])
  PutDoc :: TH.DocLoc -> String -> THMessage (THResult ())
  GetDoc :: TH.DocLoc -> THMessage (THResult (Maybe String))

  StartRecover :: THMessage ()
  EndRecover :: Bool -> THMessage ()
  FailIfErrs :: THMessage (THResult ())

  -- | Indicates that this RunTH is finished, and the next message
  -- will be the result of RunTH (a QResult).
  RunTHDone :: THMessage ()

deriving instance Show (THMessage a)

data THMsg = forall a . (Binary a, Show a) => THMsg (THMessage a)

getTHMessage :: Get THMsg
getTHMessage = do
  b <- getWord8
  case b of
    0  -> THMsg <$> NewName <$> get
    1  -> THMsg <$> (Report <$> get <*> get)
    2  -> THMsg <$> (LookupName <$> get <*> get)
    3  -> THMsg <$> Reify <$> get
    4  -> THMsg <$> ReifyFixity <$> get
    5  -> THMsg <$> (ReifyInstances <$> get <*> get)
    6  -> THMsg <$> ReifyRoles <$> get
    7  -> THMsg <$> (ReifyAnnotations <$> get <*> get)
    8  -> THMsg <$> ReifyModule <$> get
    9  -> THMsg <$> ReifyConStrictness <$> get
    10 -> THMsg <$> AddDependentFile <$> get
    11 -> THMsg <$> AddTempFile <$> get
    12 -> THMsg <$> AddTopDecls <$> get
    13 -> THMsg <$> (IsExtEnabled <$> get)
    14 -> THMsg <$> return ExtsEnabled
    15 -> THMsg <$> return StartRecover
    16 -> THMsg <$> EndRecover <$> get
    17 -> THMsg <$> return FailIfErrs
    18 -> return (THMsg RunTHDone)
    19 -> THMsg <$> AddModFinalizer <$> get
    20 -> THMsg <$> (AddForeignFilePath <$> get <*> get)
    21 -> THMsg <$> AddCorePlugin <$> get
    22 -> THMsg <$> ReifyType <$> get
    23 -> THMsg <$> (PutDoc <$> get <*> get)
    24 -> THMsg <$> GetDoc <$> get
    25 -> THMsg <$> return GetPackageRoot
    26 -> THMsg <$> AddDependentDirectory <$> get
    n -> error ("getTHMessage: unknown message " ++ show n)

putTHMessage :: THMessage a -> Put
putTHMessage m = case m of
  NewName a                   -> putWord8 0  >> put a
  Report a b                  -> putWord8 1  >> put a >> put b
  LookupName a b              -> putWord8 2  >> put a >> put b
  Reify a                     -> putWord8 3  >> put a
  ReifyFixity a               -> putWord8 4  >> put a
  ReifyInstances a b          -> putWord8 5  >> put a >> put b
  ReifyRoles a                -> putWord8 6  >> put a
  ReifyAnnotations a b        -> putWord8 7  >> put a >> put b
  ReifyModule a               -> putWord8 8  >> put a
  ReifyConStrictness a        -> putWord8 9  >> put a
  AddDependentFile a          -> putWord8 10 >> put a
  AddTempFile a               -> putWord8 11 >> put a
  AddTopDecls a               -> putWord8 12 >> put a
  IsExtEnabled a              -> putWord8 13 >> put a
  ExtsEnabled                 -> putWord8 14
  StartRecover                -> putWord8 15
  EndRecover a                -> putWord8 16 >> put a
  FailIfErrs                  -> putWord8 17
  RunTHDone                   -> putWord8 18
  AddModFinalizer a           -> putWord8 19 >> put a
  AddForeignFilePath lang a   -> putWord8 20 >> put lang >> put a
  AddCorePlugin a             -> putWord8 21 >> put a
  ReifyType a                 -> putWord8 22 >> put a
  PutDoc l s                  -> putWord8 23 >> put l >> put s
  GetDoc l                    -> putWord8 24 >> put l
  GetPackageRoot              -> putWord8 25
  AddDependentDirectory a     -> putWord8 26 >> put a

data EvalOpts
  = EvalOpts
  { useSandboxThread :: Bool
  , singleStep :: Bool
  , stepOut :: Bool
  , breakOnException :: Bool
  , breakOnError :: Bool
  , isolateThreadBreaks :: !Bool
  -- ^ If @isolateThreadBreaks == True@, any breakpoint hit by the thread
  -- forked~[1] to execute this expression is only seen by a caller observing
  -- this thread's result explicitly. That is, this thread's 'EvalStatus' can
  -- be read by 'WaitThreadResult', or 'WaitThreadResultOrAnyThreadBreak', but
  -- will be ignored by 'WaitAnyThreadBreak'.
  --
  -- [1] a thread is forked to execute the expression by default, unless
  -- @-fno-ghci-sandbox@ which sets @useSandboxThread = False@
  }
  deriving (Generic, Show)

instance Binary EvalOpts

-- | We can pass simple expressions to EvalStmt, consisting of values
-- and application.  This allows us to wrap the statement to be
-- executed in another function, which is used by GHCi to implement
-- :set args and :set prog.  It might be worthwhile to extend this
-- little language in the future.
data EvalExpr a
  = EvalThis a
  | EvalApp (EvalExpr a) (EvalExpr a)
  deriving (Generic, Show)

instance Binary a => Binary (EvalExpr a)

type EvalStatus a = EvalStatus_ a a

data EvalStatus_ a b
  = EvalComplete Word64 (EvalResult a)
  | EvalPaused EvalBreak
  deriving (Generic, Show)

pattern EvalBreak :: HValueRef -> (Maybe EvalBreakpoint) -> (RemoteRef ThreadId) -> (RemotePtr CostCentreStack) -> EvalStatus_ a b
pattern EvalBreak a b c d = EvalPaused (EvalBreak_ a b c d)
{-# COMPLETE EvalComplete, EvalBreak #-}

data EvalBreak
  = EvalBreak_
       HValueRef{- AP_STACK -}
       (Maybe EvalBreakpoint)
       (RemoteRef ThreadId)
       (RemotePtr CostCentreStack) -- Cost centre stack
  deriving (Generic, Show)

instance Binary EvalBreak
instance Binary a => Binary (EvalStatus_ a b)

data EvalBreakpoint = EvalBreakpoint
  { eb_info_mod      :: !BS.ShortByteString -- ^ Breakpoint info module
  , eb_info_mod_unit :: BS.ShortByteString -- ^ Breakpoint tick module unit id
  , eb_info_index    :: Int    -- ^ Breakpoint info index
  }
  deriving (Generic, Show)

instance Binary EvalBreakpoint

data EvalResult a
  = EvalException SerializableException
  | EvalSuccess a
  deriving (Generic, Show)

instance Binary a => Binary (EvalResult a)

-- | A dummy type that tags the pointer to a breakpoint's @ModuleName@, because
-- that type isn't available here.
data BreakModule

-- | A dummy type that tags the pointer to a breakpoint's @UnitId@, because
-- that type isn't available here.
data BreakUnitId

-- | A dummy type that tags pointers returned by 'LoadDLLs'.
data LoadedDLL

data THResult a
  = THException String
  | THComplete a
  deriving (Generic, Show)

instance Binary a => Binary (THResult a)

data THResultType = THExp | THPat | THType | THDec | THAnnWrapper
  deriving (Enum, Show, Generic)

instance Binary THResultType

-- | The server-side Template Haskell state.  This is created by the
-- StartTH message.  A new one is created per module that GHC
-- typechecks.
data QState = QState
  { qsMap      :: Map TypeRep Dynamic
       -- ^ persistent data between splices in a module
  , qsLocation :: Maybe TH.Loc
       -- ^ location for current splice, if any
  , qsInboxId  :: InboxId
      -- ^ the mailbox expecting requests for each RunTH-like run
  , qsPipe     :: Pipe
       -- ^ pipe to communicate with GHC
  }
instance Show QState where show _ = "<QState>"

-- Orphan instances of Binary for Ptr / FunPtr by conversion to Word64.
-- This is to support Binary StgInfoTable which includes these.
instance Binary (Ptr a) where
  put p = put (fromIntegral (ptrToWordPtr p) :: Word64)
  get = (wordPtrToPtr . fromIntegral) <$> (get :: Get Word64)

instance Binary (FunPtr a) where
  put = put . castFunPtrToPtr
  get = castPtrToFunPtr <$> get

#if MIN_VERSION_GLASGOW_HASKELL(9,12,2,20250919)
instance Binary Heap.HalfWord where
  put x = put (fromIntegral x :: Word32)
  get = fromIntegral <$> (get :: Get Word32)
#endif

-- Binary instances to support the GetClosure message
instance Binary Heap.StgTSOProfInfo
instance Binary Heap.CostCentreStack
instance Binary Heap.CostCentre
instance Binary Heap.IndexTable
instance Binary Heap.WhatNext
instance Binary Heap.WhyBlocked
instance Binary Heap.TsoFlags

instance Binary Heap.StgInfoTable
instance Binary Heap.ClosureType
instance Binary Heap.PrimType
instance Binary a => Binary (Heap.GenClosure a)
instance Binary InfoProv where
#if MIN_VERSION_base(4,20,0)
  get = InfoProv <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  put (InfoProv x1 x2 x3 x4 x5 x6 x7 x8)
    = put x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6 >> put x7 >> put x8
#else
  get = InfoProv <$> get <*> get <*> get <*> get <*> get <*> get <*> get
  put (InfoProv x1 x2 x3 x4 x5 x6 x7) = put x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6 >> put x7
#endif

data Msg = forall a . (Binary a, Show a) => Msg (Message a)

getMessage :: Get Msg
getMessage = do
    b <- getWord8
    case b of
      0  -> Msg <$> return Shutdown
      1  -> Msg <$> return InitLinker
      2  -> Msg <$> LookupSymbol <$> get
      3  -> Msg <$> LookupClosure <$> get
      4  -> Msg <$> LoadDLLs <$> get
      5  -> Msg <$> LoadArchive <$> get
      6  -> Msg <$> LoadObj <$> get
      7  -> Msg <$> UnloadObj <$> get
      8  -> Msg <$> AddLibrarySearchPath <$> get
      9  -> Msg <$> RemoveLibrarySearchPath <$> get
      10 -> Msg <$> return ResolveObjs
      11 -> Msg <$> FindSystemLibrary <$> get
      12 -> Msg <$> (CreateBCOs . concatMap (runGet get)) <$> (get :: Get [LB.ByteString])
                    -- See Note [Parallelize CreateBCOs serialization]
      13 -> Msg <$> FreeHValueRefs <$> get
      14 -> Msg <$> MallocData <$> get
      15 -> Msg <$> MallocStrings <$> get
      16 -> Msg <$> (PrepFFI <$> get <*> get)
      17 -> Msg <$> FreeFFI <$> get
      18 -> Msg <$> MkConInfoTable <$> get
      19 -> Msg <$> (EvalStmt <$> get <*> get)
      20 -> Msg <$> (ResumeStmt <$> get <*> get)
      21 -> Msg <$> (AbandonStmt <$> get)
      22 -> Msg <$> (EvalString <$> get)
      23 -> Msg <$> (EvalStringToString <$> get <*> get)
      24 -> Msg <$> (EvalIO <$> get)
      25 -> Msg <$> (MkCostCentres <$> get <*> get)
      26 -> Msg <$> (CostCentreStackInfo <$> get)
      27 -> Msg <$> (NewBreakArray <$> get)
      28 -> Msg <$> (SetupBreakpoint <$> get <*> get <*> get)
      29 -> Msg <$> (BreakpointStatus <$> get <*> get)
      30 -> Msg <$> (GetBreakpointVar <$> get <*> get)
      31 -> Msg <$> return StartTH
      32 -> Msg <$> (RunModFinalizers <$> get <*> get <*> get)
      33 -> Msg <$> (AddSptEntry <$> get <*> get)
      34 -> Msg <$> (RunTH <$> get <*> get <*> get <*> get <*> get)
      35 -> Msg <$> (GetClosure <$> get)
      36 -> Msg <$> (Seq <$> get)
      37 -> Msg <$> return RtsRevertCAFs
      38 -> Msg <$> (ResumeSeq <$> get)
      39 -> Msg <$> (LookupSymbolInDLL <$> get <*> get)
      40 -> Msg <$> (WhereFrom <$> get)
      41 -> Msg <$> (AddHpcModule <$> get <*> get <*> get <*> get)
      42 -> Msg <$> (CustomMessage <$> get <*> get)
      43 -> Msg <$> (WaitThreadResult <$> get)
      44 -> Msg <$> return WaitAnyThreadBreak
      45 -> Msg <$> (WaitThreadResultOrAnyThreadBreak <$> get)
      _  -> error $ "Unknown Message code " ++ (show b)

putMessage :: Message a -> Put
putMessage m = case m of
  Shutdown                    -> putWord8 0
  InitLinker                  -> putWord8 1
  LookupSymbol str            -> putWord8 2  >> put str
  LookupClosure str           -> putWord8 3  >> put str
  LoadDLLs strs               -> putWord8 4  >> put strs
  LoadArchive str             -> putWord8 5  >> put str
  LoadObj str                 -> putWord8 6  >> put str
  UnloadObj str               -> putWord8 7  >> put str
  AddLibrarySearchPath str    -> putWord8 8  >> put str
  RemoveLibrarySearchPath ptr -> putWord8 9  >> put ptr
  ResolveObjs                 -> putWord8 10
  FindSystemLibrary str       -> putWord8 11 >> put str
  CreateBCOs bco              -> putWord8 12 >> put (serializeBCOs bco)
                              -- See Note [Parallelize CreateBCOs serialization]
  FreeHValueRefs val          -> putWord8 13 >> put val
  MallocData bs               -> putWord8 14 >> put bs
  MallocStrings bss           -> putWord8 15 >> put bss
  PrepFFI args res            -> putWord8 16 >> put args >> put res
  FreeFFI p                   -> putWord8 17 >> put p
  MkConInfoTable itbl         -> putWord8 18 >> put itbl
  EvalStmt opts val           -> putWord8 19 >> put opts >> put val
  ResumeStmt opts val         -> putWord8 20 >> put opts >> put val
  AbandonStmt val             -> putWord8 21 >> put val
  EvalString val              -> putWord8 22 >> put val
  EvalStringToString str val  -> putWord8 23 >> put str >> put val
  EvalIO val                  -> putWord8 24 >> put val
  MkCostCentres mod ccs       -> putWord8 25 >> put mod >> put ccs
  CostCentreStackInfo ptr     -> putWord8 26 >> put ptr
  NewBreakArray sz            -> putWord8 27 >> put sz
  SetupBreakpoint arr ix cnt  -> putWord8 28 >> put arr >> put ix >> put cnt
  BreakpointStatus arr ix     -> putWord8 29 >> put arr >> put ix
  GetBreakpointVar a b        -> putWord8 30 >> put a >> put b
  StartTH                     -> putWord8 31
  RunModFinalizers a b c      -> putWord8 32 >> put a >> put b >> put c
  AddSptEntry a b             -> putWord8 33 >> put a >> put b
  RunTH ii st q loc ty        -> putWord8 34 >> put ii >> put st >> put q >> put loc >> put ty
  GetClosure a                -> putWord8 35 >> put a
  Seq a                       -> putWord8 36 >> put a
  RtsRevertCAFs               -> putWord8 37
  ResumeSeq a                 -> putWord8 38 >> put a
  LookupSymbolInDLL dll str   -> putWord8 39 >> put dll >> put str
  WhereFrom a                 -> putWord8 40 >> put a
  AddHpcModule m n h ticks    -> putWord8 41 >> put m >> put n >> put h >> put ticks
  CustomMessage tag payload   -> putWord8 42 >> put tag >> put payload
  WaitThreadResult tid        -> putWord8 43 >> put tid
  WaitAnyThreadBreak          -> putWord8 44
  WaitThreadResultOrAnyThreadBreak tid
                              -> putWord8 45 >> put tid

{-
Note [Parallelize CreateBCOs serialization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Serializing ResolvedBCO is expensive, so we do it in parallel.
We split the list [ResolvedBCO] into chunks of length <= 100,
and serialize every chunk in parallel, getting a [LB.ByteString]
where every bytestring corresponds to a single chunk (multiple ResolvedBCOs).

Previously, we stored [LB.ByteString] in the Message object, but that
incurs unneccessary serialization with the internal interpreter (#23919).
-}

serializeBCOs :: [ResolvedBCO] -> [LB.ByteString]
serializeBCOs rbcos = parMap doChunk (chunkList 100 rbcos)
 where
  -- make sure we force the whole lazy ByteString
  doChunk c = pseq (LB.length bs) bs
    where bs = runPut (put c)

  -- We don't have the parallel package, so roll our own simple parMap
  parMap _ [] = []
  parMap f (x:xs) = fx `par` (fxs `pseq` (fx : fxs))
    where fx = f x; fxs = parMap f xs

  chunkList :: Int -> [a] -> [[a]]
  chunkList _ [] = []
  chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs

-- | Send a 'Message' request to and wait for a reply from the interpreter
-- (blocks on the calling side, doesn't necessarily block the interpreter)
remoteCall :: Binary a => Pipe -> Message a -> IO a
remoteCall pipe msg = either throwIO (pure . runGet get)
                  =<< remoteAsyncReq pipe (putMessage msg) Nothing

-- | Handle a single 'Message' request on the interpreter and reply to the host
handleRemoteCall :: Pipe
                 -> (Msg -> IO Msg) {-^ Msg hook -}
                 -> (forall a. (Show a, Binary a) => WriteReply a %1 -> (IO () -> IO ()) -> Message a -> IO r)
                 -> IO r
handleRemoteCall pipe hook k =
  handleAsyncReq pipe getMessage Nothing \(WriteReply writer) sa msg0 -> IO \s0 ->
    case hook msg0 |#> s0 of -- unIO for linearity checking
      (# s1, Msg msg #) -> k (WriteReply \x -> writer (fmap put x)) sa msg |#> s1

-- | Send a 'THMessage' request to the host and wait for a reply from the host
-- to the interpreter.
--
-- A 'THMessage' request from the interpreter is always associated with a
-- 'RunTH'-like run. Since the host may have many in-flight TH runs, we always
-- want a specific host 'InboxId' (which is announced in the 'RunTH' request)
remoteTHCall :: Binary a => Pipe -> InboxId -> THMessage a -> IO a
remoteTHCall pipe ii msg = either throwIO (pure . runGet get)
                       =<< remoteAsyncReq pipe (putTHMessage msg) (Just ii)

-- | Handle a 'THMessage' request on the host and reply to the interpreter.
-- Symmterically to 'remoteTHCall', we always want to specify the 'InboxId' for
-- RunTH "reverse requests"
handleRemoteTHCall
  :: Pipe -> InboxId -> (forall a. (Show a, Binary a) => (IO () -> IO ()) -> THMessage a -> IO a) -> IO ()
handleRemoteTHCall pipe ii k =
  handleAsyncReq pipe getTHMessage (Just ii) \writer sa (THMsg msg) -> IO \s0 ->
    case k sa msg |#> s0 of -- unIO for linearity checking
      (# s1, a #) -> writeReply writer (put a) |#> s1


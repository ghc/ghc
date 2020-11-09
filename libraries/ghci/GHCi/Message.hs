{-# LANGUAGE GADTs, DeriveGeneric, StandaloneDeriving, ScopedTypeVariables,
    GeneralizedNewtypeDeriving, ExistentialQuantification, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}

-- |
-- Remote GHCi message types and serialization.
--
-- For details on Remote GHCi, see Note [Remote GHCi] in
-- compiler/GHC/Runtime/Interpreter.hs.
--
module GHCi.Message
  ( Message(..), Msg(..)
  , THMessage(..), THMsg(..)
  , QResult(..)
  , EvalStatus_(..), EvalStatus, EvalResult(..), EvalOpts(..), EvalExpr(..)
  , SerializableException(..)
  , toSerializableException, fromSerializableException
  , THResult(..), THResultType(..)
  , ResumeContext(..)
  , QState(..)
  , getMessage, putMessage, getTHMessage, putTHMessage
  , Pipe(..), remoteCall, remoteTHCall, readPipe, writePipe
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHCi.RemoteTypes
import GHCi.FFI
import GHCi.TH.Binary () -- For Binary instances
import GHCi.BreakArray

import GHC.LanguageExtensions
import GHC.Exts.Heap
import GHC.ForeignSrcLang
import GHC.Fingerprint
import Control.Concurrent
import Control.Exception
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic
import Data.Typeable (TypeRep)
import Data.IORef
import Data.Map (Map)
import Foreign
import GHC.Generics
import GHC.Stack.CCS
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import System.Exit
import System.IO
import System.IO.Error

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
  LookupSymbol :: String -> Message (Maybe (RemotePtr ()))
  LookupClosure :: String -> Message (Maybe HValueRef)
  LoadDLL :: String -> Message (Maybe String)
  LoadArchive :: String -> Message () -- error?
  LoadObj :: String -> Message () -- error?
  UnloadObj :: String -> Message () -- error?
  AddLibrarySearchPath :: String -> Message (RemotePtr ())
  RemoveLibrarySearchPath :: RemotePtr () -> Message Bool
  ResolveObjs :: Message Bool
  FindSystemLibrary :: String -> Message (Maybe String)

  -- Interpreter -------------------------------------------

  -- | Create a set of BCO objects, and return HValueRefs to them
  -- Note: Each ByteString contains a Binary-encoded [ResolvedBCO], not
  -- a ResolvedBCO. The list is to allow us to serialise the ResolvedBCOs
  -- in parallel. See @createBCOs@ in compiler/GHC/Runtime/Interpreter.hs.
  CreateBCOs :: [LB.ByteString] -> Message [HValueRef]

  -- | Release 'HValueRef's
  FreeHValueRefs :: [HValueRef] -> Message ()

  -- | Add entries to the Static Pointer Table
  AddSptEntry :: Fingerprint -> HValueRef -> Message ()

  -- | Malloc some data and return a 'RemotePtr' to it
  MallocData :: ByteString -> Message (RemotePtr ())
  MallocStrings :: [ByteString] -> Message [RemotePtr ()]

  -- | Calls 'GHCi.FFI.prepareForeignCall'
  PrepFFI :: FFIConv -> [FFIType] -> FFIType -> Message (RemotePtr C_ffi_cif)

  -- | Free data previously created by 'PrepFFI'
  FreeFFI :: RemotePtr C_ffi_cif -> Message ()

  -- | Create an info table for a constructor
  MkConInfoTable
   :: Bool    -- TABLES_NEXT_TO_CODE
   -> Int     -- ptr words
   -> Int     -- non-ptr words
   -> Int     -- constr tag
   -> Int     -- pointer tag
   -> ByteString -- constructor desccription
   -> Message (RemotePtr StgInfoTable)

  -- | Evaluate a statement
  EvalStmt
    :: EvalOpts
    -> EvalExpr HValueRef {- IO [a] -}
    -> Message (EvalStatus [HValueRef]) {- [a] -}

  -- | Resume evaluation of a statement after a breakpoint
  ResumeStmt
   :: EvalOpts
   -> RemoteRef (ResumeContext [HValueRef])
   -> Message (EvalStatus [HValueRef])

  -- | Abandon evaluation of a statement after a breakpoint
  AbandonStmt
   :: RemoteRef (ResumeContext [HValueRef])
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
   :: String     -- module, RemotePtr so it can be shared
   -> [(String,String)] -- (name, SrcSpan)
   -> Message [RemotePtr CostCentre]

  -- | Show a 'CostCentreStack' as a @[String]@
  CostCentreStackInfo
   :: RemotePtr CostCentreStack
   -> Message [String]

  -- | Create a new array of breakpoint flags
  NewBreakArray
   :: Int                               -- size
   -> Message (RemoteRef BreakArray)

  -- | Enable a breakpoint
  EnableBreakpoint
   :: RemoteRef BreakArray
   -> Int                               -- index
   -> Bool                              -- on or off
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
   :: RemoteRef (IORef QState)
   -> HValueRef {- e.g. TH.Q TH.Exp -}
   -> THResultType
   -> Maybe TH.Loc
   -> Message (QResult ByteString)

  -- | Run the given mod finalizers.
  RunModFinalizers :: RemoteRef (IORef QState)
                   -> [RemoteRef (TH.Q ())]
                   -> Message (QResult ())

  -- | Remote interface to GHC.Exts.Heap.getClosureData. This is used by
  -- the GHCi debugger to inspect values in the heap for :print and
  -- type reconstruction.
  GetClosure
    :: HValueRef
    -> Message (GenClosure HValueRef)

  -- | Evaluate something. This is used to support :force in GHCi.
  Seq
    :: HValueRef
    -> Message (EvalStatus ())

  -- | Resume forcing a free variable in a breakpoint (#2950)
  ResumeSeq
    :: RemoteRef (ResumeContext ())
    -> Message (EvalStatus ())

deriving instance Show (Message a)


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

  AddDependentFile :: FilePath -> THMessage (THResult ())
  AddTempFile :: String -> THMessage (THResult FilePath)
  AddModFinalizer :: RemoteRef (TH.Q ()) -> THMessage (THResult ())
  AddCorePlugin :: String -> THMessage (THResult ())
  AddTopDecls :: [TH.Dec] -> THMessage (THResult ())
  AddForeignFilePath :: ForeignSrcLang -> FilePath -> THMessage (THResult ())
  IsExtEnabled :: Extension -> THMessage (THResult Bool)
  ExtsEnabled :: THMessage (THResult [Extension])

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


data EvalOpts = EvalOpts
  { useSandboxThread :: Bool
  , singleStep :: Bool
  , breakOnException :: Bool
  , breakOnError :: Bool
  }
  deriving (Generic, Show)

instance Binary EvalOpts

data ResumeContext a = ResumeContext
  { resumeBreakMVar :: MVar ()
  , resumeStatusMVar :: MVar (EvalStatus a)
  , resumeThreadId :: ThreadId
  }

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
  | EvalBreak Bool
       HValueRef{- AP_STACK -}
       Int {- break index -}
       Int {- uniq of ModuleName -}
       (RemoteRef (ResumeContext b))
       (RemotePtr CostCentreStack) -- Cost centre stack
  deriving (Generic, Show)

instance Binary a => Binary (EvalStatus_ a b)

data EvalResult a
  = EvalException SerializableException
  | EvalSuccess a
  deriving (Generic, Show)

instance Binary a => Binary (EvalResult a)

-- SomeException can't be serialized because it contains dynamic
-- types.  However, we do very limited things with the exceptions that
-- are thrown by interpreted computations:
--
-- * We print them, e.g. "*** Exception: <something>"
-- * UserInterrupt has a special meaning
-- * In ghc -e, exitWith should exit with the appropriate exit code
--
-- So all we need to do is distinguish UserInterrupt and ExitCode, and
-- all other exceptions can be represented by their 'show' string.
--
data SerializableException
  = EUserInterrupt
  | EExitCode ExitCode
  | EOtherException String
  deriving (Generic, Show)

toSerializableException :: SomeException -> SerializableException
toSerializableException ex
  | Just UserInterrupt <- fromException ex  = EUserInterrupt
  | Just (ec::ExitCode) <- fromException ex = (EExitCode ec)
  | otherwise = EOtherException (show (ex :: SomeException))

fromSerializableException :: SerializableException -> SomeException
fromSerializableException EUserInterrupt = toException UserInterrupt
fromSerializableException (EExitCode c) = toException c
fromSerializableException (EOtherException str) = toException (ErrorCall str)

instance Binary ExitCode
instance Binary SerializableException

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
  { qsMap        :: Map TypeRep Dynamic
       -- ^ persistent data between splices in a module
  , qsLocation   :: Maybe TH.Loc
       -- ^ location for current splice, if any
  , qsPipe :: Pipe
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

-- Binary instances to support the GetClosure message
instance Binary StgInfoTable
instance Binary ClosureType
instance Binary PrimType
instance Binary a => Binary (GenClosure a)

data Msg = forall a . (Binary a, Show a) => Msg (Message a)

getMessage :: Get Msg
getMessage = do
    b <- getWord8
    case b of
      0  -> Msg <$> return Shutdown
      1  -> Msg <$> return InitLinker
      2  -> Msg <$> LookupSymbol <$> get
      3  -> Msg <$> LookupClosure <$> get
      4  -> Msg <$> LoadDLL <$> get
      5  -> Msg <$> LoadArchive <$> get
      6  -> Msg <$> LoadObj <$> get
      7  -> Msg <$> UnloadObj <$> get
      8  -> Msg <$> AddLibrarySearchPath <$> get
      9  -> Msg <$> RemoveLibrarySearchPath <$> get
      10 -> Msg <$> return ResolveObjs
      11 -> Msg <$> FindSystemLibrary <$> get
      12 -> Msg <$> CreateBCOs <$> get
      13 -> Msg <$> FreeHValueRefs <$> get
      14 -> Msg <$> MallocData <$> get
      15 -> Msg <$> MallocStrings <$> get
      16 -> Msg <$> (PrepFFI <$> get <*> get <*> get)
      17 -> Msg <$> FreeFFI <$> get
      18 -> Msg <$> (MkConInfoTable <$> get <*> get <*> get <*> get <*> get <*> get)
      19 -> Msg <$> (EvalStmt <$> get <*> get)
      20 -> Msg <$> (ResumeStmt <$> get <*> get)
      21 -> Msg <$> (AbandonStmt <$> get)
      22 -> Msg <$> (EvalString <$> get)
      23 -> Msg <$> (EvalStringToString <$> get <*> get)
      24 -> Msg <$> (EvalIO <$> get)
      25 -> Msg <$> (MkCostCentres <$> get <*> get)
      26 -> Msg <$> (CostCentreStackInfo <$> get)
      27 -> Msg <$> (NewBreakArray <$> get)
      28 -> Msg <$> (EnableBreakpoint <$> get <*> get <*> get)
      29 -> Msg <$> (BreakpointStatus <$> get <*> get)
      30 -> Msg <$> (GetBreakpointVar <$> get <*> get)
      31 -> Msg <$> return StartTH
      32 -> Msg <$> (RunModFinalizers <$> get <*> get)
      33 -> Msg <$> (AddSptEntry <$> get <*> get)
      34 -> Msg <$> (RunTH <$> get <*> get <*> get <*> get)
      35 -> Msg <$> (GetClosure <$> get)
      36 -> Msg <$> (Seq <$> get)
      37 -> Msg <$> return RtsRevertCAFs
      38 -> Msg <$> (ResumeSeq <$> get)
      _  -> error $ "Unknown Message code " ++ (show b)

putMessage :: Message a -> Put
putMessage m = case m of
  Shutdown                    -> putWord8 0
  InitLinker                  -> putWord8 1
  LookupSymbol str            -> putWord8 2  >> put str
  LookupClosure str           -> putWord8 3  >> put str
  LoadDLL str                 -> putWord8 4  >> put str
  LoadArchive str             -> putWord8 5  >> put str
  LoadObj str                 -> putWord8 6  >> put str
  UnloadObj str               -> putWord8 7  >> put str
  AddLibrarySearchPath str    -> putWord8 8  >> put str
  RemoveLibrarySearchPath ptr -> putWord8 9  >> put ptr
  ResolveObjs                 -> putWord8 10
  FindSystemLibrary str       -> putWord8 11 >> put str
  CreateBCOs bco              -> putWord8 12 >> put bco
  FreeHValueRefs val          -> putWord8 13 >> put val
  MallocData bs               -> putWord8 14 >> put bs
  MallocStrings bss           -> putWord8 15 >> put bss
  PrepFFI conv args res       -> putWord8 16 >> put conv >> put args >> put res
  FreeFFI p                   -> putWord8 17 >> put p
  MkConInfoTable tc p n t pt d -> putWord8 18 >> put tc >> put p >> put n >> put t >> put pt >> put d
  EvalStmt opts val           -> putWord8 19 >> put opts >> put val
  ResumeStmt opts val         -> putWord8 20 >> put opts >> put val
  AbandonStmt val             -> putWord8 21 >> put val
  EvalString val              -> putWord8 22 >> put val
  EvalStringToString str val  -> putWord8 23 >> put str >> put val
  EvalIO val                  -> putWord8 24 >> put val
  MkCostCentres mod ccs       -> putWord8 25 >> put mod >> put ccs
  CostCentreStackInfo ptr     -> putWord8 26 >> put ptr
  NewBreakArray sz            -> putWord8 27 >> put sz
  EnableBreakpoint arr ix b   -> putWord8 28 >> put arr >> put ix >> put b
  BreakpointStatus arr ix     -> putWord8 29 >> put arr >> put ix
  GetBreakpointVar a b        -> putWord8 30 >> put a >> put b
  StartTH                     -> putWord8 31
  RunModFinalizers a b        -> putWord8 32 >> put a >> put b
  AddSptEntry a b             -> putWord8 33 >> put a >> put b
  RunTH st q loc ty           -> putWord8 34 >> put st >> put q >> put loc >> put ty
  GetClosure a                -> putWord8 35 >> put a
  Seq a                       -> putWord8 36 >> put a
  RtsRevertCAFs               -> putWord8 37
  ResumeSeq a                 -> putWord8 38 >> put a

-- -----------------------------------------------------------------------------
-- Reading/writing messages

data Pipe = Pipe
  { pipeRead :: Handle
  , pipeWrite ::  Handle
  , pipeLeftovers :: IORef (Maybe ByteString)
  }

remoteCall :: Binary a => Pipe -> Message a -> IO a
remoteCall pipe msg = do
  writePipe pipe (putMessage msg)
  readPipe pipe get

remoteTHCall :: Binary a => Pipe -> THMessage a -> IO a
remoteTHCall pipe msg = do
  writePipe pipe (putTHMessage msg)
  readPipe pipe get

writePipe :: Pipe -> Put -> IO ()
writePipe Pipe{..} put
  | LB.null bs = return ()
  | otherwise  = do
    LB.hPut pipeWrite bs
    hFlush pipeWrite
 where
  bs = runPut put

readPipe :: Pipe -> Get a -> IO a
readPipe Pipe{..} get = do
  leftovers <- readIORef pipeLeftovers
  m <- getBin pipeRead get leftovers
  case m of
    Nothing -> throw $
      mkIOError eofErrorType "GHCi.Message.remoteCall" (Just pipeRead) Nothing
    Just (result, new_leftovers) -> do
      writeIORef pipeLeftovers new_leftovers
      return result

getBin
  :: Handle -> Get a -> Maybe ByteString
  -> IO (Maybe (a, Maybe ByteString))

getBin h get leftover = go leftover (runGetIncremental get)
 where
   go Nothing (Done leftover _ msg) =
     return (Just (msg, if B.null leftover then Nothing else Just leftover))
   go _ Done{} = throwIO (ErrorCall "getBin: Done with leftovers")
   go (Just leftover) (Partial fun) = do
     go Nothing (fun (Just leftover))
   go Nothing (Partial fun) = do
     -- putStrLn "before hGetSome"
     b <- B.hGetSome h (32*1024)
     -- printf "hGetSome: %d\n" (B.length b)
     if B.null b
        then return Nothing
        else go Nothing (fun (Just b))
   go _lft (Fail _rest _off str) =
     throwIO (ErrorCall ("getBin: " ++ str))

{-# LANGUAGE CPP, LambdaCase, BangPatterns, MagicHash, TupleSections, ScopedTypeVariables, DeriveDataTypeable #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI #-}
#endif

{- |
     Evaluate Template Haskell splices on node.js
 -}

module GHCJS.Prim.TH.Eval (
#ifdef ghcjs_HOST_OS
         runTHServer
#endif
       ) where

#ifdef ghcjs_HOST_OS

import           Prelude

import           GHCJS.Prim.TH.Serialized
import           GHCJS.Prim.TH.Types

import           Control.Applicative
import qualified Control.Exception        as E
import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.IO.Class

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Unsafe   as BU
import           Data.Data
import           Data.Dynamic
import           Data.Int
import           Data.IORef
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Monoid              ((<>))
import           Data.Typeable
import           Data.Word

import           Foreign.C
import           Foreign.Ptr

import           GHC.Prim
import           GHC.Exts
import           GHC.Desugar
import           GHC.Fingerprint.Type

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.IO

import           Unsafe.Coerce

data QState = QState { qsMap        :: Map TypeRep Dynamic    -- ^ persistent data between splices in a module
                     , qsFinalizers :: [TH.Q ()]              -- ^ registered finalizers (in reverse order)
                     , qsLocation   :: Maybe TH.Loc           -- ^ location for current splice, if any
                     }
instance Show QState where show _ = "<QState>"

initQState :: QState
initQState = QState M.empty [] Nothing

runModFinalizers :: GHCJSQ ()
runModFinalizers = go =<< getState
  where
    go s | (f:ff) <- qsFinalizers s =
      putState (s { qsFinalizers = ff}) >> TH.runQ f >> getState >>= go
    go _ = return ()

data GHCJSQ a = GHCJSQ { runGHCJSQ :: QState -> IO (a, QState) }

data GHCJSQException = GHCJSQException QState (Maybe Int) String
  deriving (Show, Typeable)

instance E.Exception GHCJSQException

instance Functor GHCJSQ where
  fmap f (GHCJSQ s) = GHCJSQ $ fmap (\(x,s') -> (f x,s')) . s

instance Applicative GHCJSQ where
  f <*> a = GHCJSQ $ \s ->
    do (f',s')  <- runGHCJSQ f s
       (a',s'') <- runGHCJSQ a s'
       return (f' a', s'')
  pure x = GHCJSQ (\s -> return (x,s))

instance Monad GHCJSQ where
  m >>= f = GHCJSQ $ \s ->
    do (m', s')  <- runGHCJSQ m s
       (a,  s'') <- runGHCJSQ (f m') s'
       return (a, s'')
  return    = pure

instance MonadFail GHCJSQ where
  fail err = GHCJSQ $ \s -> E.throw (GHCJSQException s Nothing err)

instance MonadIO GHCJSQ where
  liftIO m = GHCJSQ $ \s -> fmap (,s) m

getState :: GHCJSQ QState
getState = GHCJSQ $ \s -> return (s,s)

putState :: QState -> GHCJSQ ()
putState s = GHCJSQ $ \_ -> return ((),s)

noLoc :: TH.Loc
noLoc = TH.Loc "<no file>" "<no package>" "<no module>" (0,0) (0,0)

instance TH.Quasi GHCJSQ where
  qNewName str = do
    NewName' name <- sendRequestQ (NewName str)
    return name
  qReport isError msg = do
    Report' <- sendRequestQ (Report isError msg)
    return ()
  qRecover (GHCJSQ h) (GHCJSQ a) = GHCJSQ $ \s -> do
    let r :: Bool -> IO ()
        r b = do EndRecover' <- sendRequest (EndRecover b)
                 return ()
    StartRecover' <- sendRequest StartRecover
    (a s >>= \s' -> r False >> return s') `E.catch`
      \(GHCJSQException s' _ _) -> r True >> h s
  qLookupName isType occ = do
    LookupName' name <- sendRequestQ (LookupName isType occ)
    return name
  qReify name = do
    Reify' info <- sendRequestQ (Reify name)
    return info
  qReifyInstances name tys = do
    ReifyInstances' decls <- sendRequestQ (ReifyInstances name tys)
    return decls
  qReifyRoles name = do
    ReifyRoles' roles <- sendRequestQ (ReifyRoles name)
    return roles
  qReifyAnnotations lookup = do
    ReifyAnnotations' payloads <- sendRequestQ (ReifyAnnotations lookup)
    return (convertAnnPayloads payloads)
  qReifyModule m = do
    ReifyModule' mi <- sendRequestQ (ReifyModule m)
    return mi
  qReifyFixity m = do
    ReifyFixity' mi <- sendRequestQ (ReifyFixity m)
    return mi
  qReifyConStrictness name = do
    ReifyConStrictness' ss <- sendRequestQ (ReifyConStrictness name)
    return ss
  qAddForeignFilePath lang contents = do
    AddForeignFilePath' <- sendRequestQ (AddForeignFilePath lang contents)
    return ()
  qIsExtEnabled ext = do
    IsExtEnabled' b <- sendRequestQ (IsExtEnabled ext)
    return b
  qExtsEnabled = do
    ExtsEnabled' exts <- sendRequestQ ExtsEnabled
    return exts
  qLocation = fromMaybe noLoc . qsLocation <$> getState
  qRunIO m = GHCJSQ $ \s -> fmap (,s) m
  qAddDependentFile file = do
    AddDependentFile' <- sendRequestQ (AddDependentFile file)
    return ()
  qAddTempFile xs = do
    AddTempFile' path <- sendRequestQ (AddTempFile xs)
    return path
  qAddTopDecls decls = do
    AddTopDecls' <- sendRequestQ (AddTopDecls decls)
    return ()
  qAddModFinalizer fin = GHCJSQ $ \s ->
    return ((), s { qsFinalizers = fin : qsFinalizers s })
  qGetQ = GHCJSQ $ \s ->
    let lookup :: forall a. Typeable a => Map TypeRep Dynamic -> Maybe a
        lookup m = fromDynamic =<< M.lookup (typeOf (undefined::a)) m
    in return (lookup (qsMap s), s)
  qPutQ k = GHCJSQ $ \s ->
    return ((), s { qsMap = M.insert (typeOf k) (toDyn k) (qsMap s) })
  qAddCorePlugin plugin = do
    AddCorePlugin' <- sendRequestQ (AddCorePlugin plugin)
    return ()

makeAnnPayload :: forall a. Data a => a -> ByteString
makeAnnPayload x =
  let Fingerprint w1 w2 = typeRepFingerprint (typeOf (undefined :: a))
      fp = runPut (putWord64be w1 >> putWord64be w2)
  in  BL.toStrict $ fp <> BL.pack (serializeWithData x)

convertAnnPayloads :: forall a. Data a => [ByteString] -> [a]
convertAnnPayloads bs = catMaybes (map convert bs)
  where
    Fingerprint w1 w2 = typeRepFingerprint (typeOf (undefined :: a))
    getFp b = runGet ((,) <$> getWord64be <*> getWord64be) $ BL.fromStrict (B.take 16 b)
    convert b | (bw1,bw2) <- getFp b, bw1 == w1, bw2 == w2 =
                  Just (deserializeWithData . B.unpack . B.drop 16 $ b)
              | otherwise = Nothing

-- | the Template Haskell server
runTHServer :: IO ()
runTHServer = do
--  msgs <- newIORef []
  void (runGHCJSQ server initQState) `E.catches`
    [ E.Handler $ \(GHCJSQException _ mn msg) ->
         void . sendRequest $ maybe (QFail msg) QCompilerException mn
    , E.Handler $ \(E.SomeException e) ->
         void (sendRequest $ QUserException (show e))
    ]
  where
    server = TH.qRunIO awaitMessage >>= \case
      RunTH t code loc -> do
        a <- TH.qRunIO (loadCode code)
        runTH t a loc
        server
      FinishTH endProcess -> do
        runModFinalizers
        mu <- TH.qRunIO $ js_getMemoryUsage
        TH.qRunIO $ sendResult (FinishTH' mu)
        when (not endProcess) server
      _ -> error "runTHServer: unexpected message type"

{-# NOINLINE runTH #-}
runTH :: THResultType -> Any -> Maybe TH.Loc -> GHCJSQ ()
runTH rt obj = \mb_loc -> obj `seq` do
  s0 <- getState
  putState $ s0 { qsLocation = mb_loc }
  res <- case rt of
           THExp        -> runTHCode (unsafeCoerce obj :: TH.Q TH.Exp)
           THPat        -> runTHCode (unsafeCoerce obj :: TH.Q TH.Pat)
           THType       -> runTHCode (unsafeCoerce obj :: TH.Q TH.Type)
           THDec        -> runTHCode (unsafeCoerce obj :: TH.Q [TH.Dec])
           THAnnWrapper -> case unsafeCoerce obj of
                             AnnotationWrapper x -> return (makeAnnPayload x)
  s1 <- getState
  TH.qRunIO (sendResult $ RunTH' res)
  putState $ s1 { qsLocation = Nothing }

{-# NOINLINE runTHCode #-}
runTHCode :: Binary a => TH.Q a -> GHCJSQ ByteString
runTHCode c = BL.toStrict . runPut . put <$> TH.runQ c

{-# NOINLINE loadCode #-}
loadCode :: ByteString -> IO Any
loadCode bs = do
  p <- fromBs bs
  unsafeCoerce <$> js_loadCode p (B.length bs)

awaitMessage :: IO Message
awaitMessage = fmap (runGet get . BL.fromStrict) . toBs =<< js_awaitMessage

-- | send result back
sendResult :: Message -> IO ()
sendResult msg = do
  let bs = BL.toStrict $ runPut (put msg)
  p <- fromBs bs
  js_sendMessage p (B.length bs)

-- | send a request and wait for the response
sendRequest :: Message -> IO Message
sendRequest msg = do
  let bs = BL.toStrict $ runPut (put msg)
  p <- fromBs bs
  fmap (runGet get . BL.fromStrict) . toBs =<< js_sendRequest p (B.length bs)

-- | send a request and wait for the response
--   a CompilerException' response is converted to a GHCJSQException which
--   can be handled with recover.
sendRequestQ :: Message -> GHCJSQ Message
sendRequestQ msg = TH.qRunIO (sendRequest msg) >>= \case
   QCompilerException' n msg -> GHCJSQ $
     \s -> E.throw (GHCJSQException s (Just n) msg)
   response                 -> return response

foreign import javascript interruptible "h$TH.sendRequest($1_1,$1_2,$2,$c);"
  js_sendRequest :: Ptr Word8 -> Int -> IO (Ptr Word8)

foreign import javascript interruptible "h$TH.sendMessage($1_1,$1_2,$2,0,$c);"
  js_sendMessage :: Ptr Word8 -> Int -> IO ()

foreign import javascript interruptible "h$TH.awaitMessage(0,$c);"
  js_awaitMessage :: IO (Ptr Word8)

foreign import javascript unsafe "h$TH.bufSize($1_1, $1_2)"
  js_bufSize :: Ptr Word8 -> IO Int

-- | actually returns the heap object to be evaluated
foreign import javascript unsafe "h$TH.loadCode($1_1,$1_2,$2)"
  js_loadCode :: Ptr Word8 -> Int -> IO Double

foreign import javascript unsafe "$r = h$TH.getMemoryUsage();"
  js_getMemoryUsage :: IO Int

-- | only safe in JS
fromBs :: ByteString -> IO (Ptr Word8)
fromBs bs = BU.unsafeUseAsCString bs (return . castPtr)

-- | build a ByteString that uses the whole buffer, only works in JS
toBs :: Ptr Word8 -> IO ByteString
toBs p = do
  l <- js_bufSize p
  BU.unsafePackCStringLen (castPtr p, l)

#endif

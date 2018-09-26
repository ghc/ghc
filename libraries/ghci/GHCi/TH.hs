{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, DeriveGeneric,
    TupleSections, RecordWildCards, InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Running TH splices
--
module GHCi.TH
  ( startTH
  , runModFinalizerRefs
  , runTH
  , GHCiQException(..)
  ) where

{- Note [Remote Template Haskell]

Here is an overview of how TH works with -fexternal-interpreter.

Initialisation
~~~~~~~~~~~~~~

GHC sends a StartTH message to the server (see TcSplice.getTHState):

   StartTH :: Message (RemoteRef (IORef QState))

The server creates an initial QState object, makes an IORef to it, and
returns a RemoteRef to this to GHC. (see GHCi.TH.startTH below).

This happens once per module, the first time we need to run a TH
splice.  The reference that GHC gets back is kept in
tcg_th_remote_state in the TcGblEnv, and passed to each RunTH call
that follows.


For each splice
~~~~~~~~~~~~~~~

1. GHC compiles a splice to byte code, and sends it to the server: in
   a CreateBCOs message:

   CreateBCOs :: [LB.ByteString] -> Message [HValueRef]

2. The server creates the real byte-code objects in its heap, and
   returns HValueRefs to GHC.  HValueRef is the same as RemoteRef
   HValue.

3. GHC sends a RunTH message to the server:

  RunTH
   :: RemoteRef (IORef QState)
        -- The state returned by StartTH in step1
   -> HValueRef
        -- The HValueRef we got in step 4, points to the code for the splice
   -> THResultType
        -- Tells us what kind of splice this is (decl, expr, type, etc.)
   -> Maybe TH.Loc
        -- Source location
   -> Message (QResult ByteString)
        -- Eventually it will return a QResult back to GHC.  The
        -- ByteString here is the (encoded) result of the splice.

4. The server runs the splice code.

5. Each time the splice code calls a method of the Quasi class, such
   as qReify, a message is sent from the server to GHC.  These
   messages are defined by the THMessage type.  GHC responds with the
   result of the request, e.g. in the case of qReify it would be the
   TH.Info for the requested entity.

6. When the splice has been fully evaluated, the server sends
   RunTHDone back to GHC.  This tells GHC that the server has finished
   sending THMessages and will send the QResult next.

8. The server then sends a QResult back to GHC, which is notionally
   the response to the original RunTH message.  The QResult indicates
   whether the splice succeeded, failed, or threw an exception.


After typechecking
~~~~~~~~~~~~~~~~~~

GHC sends a FinishTH message to the server (see TcSplice.finishTH).
The server runs any finalizers that were added by addModuleFinalizer.


Other Notes on TH / Remote GHCi

  * Note [Remote GHCi] in compiler/ghci/GHCi.hs
  * Note [External GHCi pointers] in compiler/ghci/GHCi.hs
  * Note [TH recover with -fexternal-interpreter] in
    compiler/typecheck/TcSplice.hs
-}

import GHCi.Message
import GHCi.RemoteTypes
import GHC.Serialized

import Control.Exception
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Data
import Data.Dynamic
import Data.Either
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import GHC.Desugar
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import Unsafe.Coerce

-- | Create a new instance of 'QState'
initQState :: Pipe -> QState
initQState p = QState M.empty Nothing p

-- | The monad in which we run TH computations on the server
newtype GHCiQ a = GHCiQ { runGHCiQ :: QState -> IO (a, QState) }

-- | The exception thrown by "fail" in the GHCiQ monad
data GHCiQException = GHCiQException QState String
  deriving Show

instance Exception GHCiQException

instance Functor GHCiQ where
  fmap f (GHCiQ s) = GHCiQ $ fmap (\(x,s') -> (f x,s')) . s

instance Applicative GHCiQ where
  f <*> a = GHCiQ $ \s ->
    do (f',s')  <- runGHCiQ f s
       (a',s'') <- runGHCiQ a s'
       return (f' a', s'')
  pure x = GHCiQ (\s -> return (x,s))

instance Monad GHCiQ where
  m >>= f = GHCiQ $ \s ->
    do (m', s')  <- runGHCiQ m s
       (a,  s'') <- runGHCiQ (f m') s'
       return (a, s'')
  fail = Fail.fail

instance Fail.MonadFail GHCiQ where
  fail err  = GHCiQ $ \s -> throwIO (GHCiQException s err)

getState :: GHCiQ QState
getState = GHCiQ $ \s -> return (s,s)

noLoc :: TH.Loc
noLoc = TH.Loc "<no file>" "<no package>" "<no module>" (0,0) (0,0)

-- | Send a 'THMessage' to GHC and return the result.
ghcCmd :: Binary a => THMessage (THResult a) -> GHCiQ a
ghcCmd m = GHCiQ $ \s -> do
  r <- remoteTHCall (qsPipe s) m
  case r of
    THException str -> throwIO (GHCiQException s str)
    THComplete res -> return (res, s)

instance MonadIO GHCiQ where
  liftIO m = GHCiQ $ \s -> fmap (,s) m

instance TH.Quasi GHCiQ where
  qNewName str = ghcCmd (NewName str)
  qReport isError msg = ghcCmd (Report isError msg)

  -- See Note [TH recover with -fexternal-interpreter] in TcSplice
  qRecover (GHCiQ h) a = GHCiQ $ \s -> mask $ \unmask -> do
    remoteTHCall (qsPipe s) StartRecover
    e <- try $ unmask $ runGHCiQ (a <* ghcCmd FailIfErrs) s
    remoteTHCall (qsPipe s) (EndRecover (isLeft e))
    case e of
      Left GHCiQException{} -> h s
      Right r -> return r
  qLookupName isType occ = ghcCmd (LookupName isType occ)
  qReify name = ghcCmd (Reify name)
  qReifyFixity name = ghcCmd (ReifyFixity name)
  qReifyInstances name tys = ghcCmd (ReifyInstances name tys)
  qReifyRoles name = ghcCmd (ReifyRoles name)

  -- To reify annotations, we send GHC the AnnLookup and also the
  -- TypeRep of the thing we're looking for, to avoid needing to
  -- serialize irrelevant annotations.
  qReifyAnnotations :: forall a . Data a => TH.AnnLookup -> GHCiQ [a]
  qReifyAnnotations lookup =
    map (deserializeWithData . B.unpack) <$>
      ghcCmd (ReifyAnnotations lookup typerep)
    where typerep = typeOf (undefined :: a)

  qReifyModule m = ghcCmd (ReifyModule m)
  qReifyConStrictness name = ghcCmd (ReifyConStrictness name)
  qLocation = fromMaybe noLoc . qsLocation <$> getState
  qAddDependentFile file = ghcCmd (AddDependentFile file)
  qAddTempFile suffix = ghcCmd (AddTempFile suffix)
  qAddTopDecls decls = ghcCmd (AddTopDecls decls)
  qAddForeignFilePath lang fp = ghcCmd (AddForeignFilePath lang fp)
  qAddModFinalizer fin = GHCiQ (\s -> mkRemoteRef fin >>= return . (, s)) >>=
                         ghcCmd . AddModFinalizer
  qAddCorePlugin str = ghcCmd (AddCorePlugin str)
  qGetQ = GHCiQ $ \s ->
    let lookup :: forall a. Typeable a => Map TypeRep Dynamic -> Maybe a
        lookup m = fromDynamic =<< M.lookup (typeOf (undefined::a)) m
    in return (lookup (qsMap s), s)
  qPutQ k = GHCiQ $ \s ->
    return ((), s { qsMap = M.insert (typeOf k) (toDyn k) (qsMap s) })
  qIsExtEnabled x = ghcCmd (IsExtEnabled x)
  qExtsEnabled = ghcCmd ExtsEnabled

-- | The implementation of the 'StartTH' message: create
-- a new IORef QState, and return a RemoteRef to it.
startTH :: IO (RemoteRef (IORef QState))
startTH = do
  r <- newIORef (initQState (error "startTH: no pipe"))
  mkRemoteRef r

-- | Runs the mod finalizers.
--
-- The references must be created on the caller process.
runModFinalizerRefs :: Pipe -> RemoteRef (IORef QState)
                    -> [RemoteRef (TH.Q ())]
                    -> IO ()
runModFinalizerRefs pipe rstate qrefs = do
  qs <- mapM localRef qrefs
  qstateref <- localRef rstate
  qstate <- readIORef qstateref
  _ <- runGHCiQ (TH.runQ $ sequence_ qs) qstate { qsPipe = pipe }
  return ()

-- | The implementation of the 'RunTH' message
runTH
  :: Pipe
  -> RemoteRef (IORef QState)
      -- ^ The TH state, created by 'startTH'
  -> HValueRef
      -- ^ The splice to run
  -> THResultType
      -- ^ What kind of splice it is
  -> Maybe TH.Loc
      -- ^ The source location
  -> IO ByteString
      -- ^ Returns an (encoded) result that depends on the THResultType

runTH pipe rstate rhv ty mb_loc = do
  hv <- localRef rhv
  case ty of
    THExp -> runTHQ pipe rstate mb_loc (unsafeCoerce hv :: TH.Q TH.Exp)
    THPat -> runTHQ pipe rstate mb_loc (unsafeCoerce hv :: TH.Q TH.Pat)
    THType -> runTHQ pipe rstate mb_loc (unsafeCoerce hv :: TH.Q TH.Type)
    THDec -> runTHQ pipe rstate mb_loc (unsafeCoerce hv :: TH.Q [TH.Dec])
    THAnnWrapper -> do
      hv <- unsafeCoerce <$> localRef rhv
      case hv :: AnnotationWrapper of
        AnnotationWrapper thing -> return $!
          LB.toStrict (runPut (put (toSerialized serializeWithData thing)))

-- | Run a Q computation.
runTHQ
  :: Binary a => Pipe -> RemoteRef (IORef QState) -> Maybe TH.Loc -> TH.Q a
  -> IO ByteString
runTHQ pipe@Pipe{..} rstate mb_loc ghciq = do
  qstateref <- localRef rstate
  qstate <- readIORef qstateref
  let st = qstate { qsLocation = mb_loc, qsPipe = pipe }
  (r,new_state) <- runGHCiQ (TH.runQ ghciq) st
  writeIORef qstateref new_state
  return $! LB.toStrict (runPut (put r))

{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, DeriveGeneric,
    TupleSections, RecordWildCards, InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Running TH splices
--
module GHCi.TH (startTH, finishTH, runTH, GHCiQException(..)) where

import GHCi.Message
import GHCi.RemoteTypes
import GHC.Serialized

import Control.Exception
import qualified Control.Monad.Fail as Fail
import Data.Binary
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Data
import Data.Dynamic
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import GHC.Desugar
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import Unsafe.Coerce

initQState :: Pipe -> QState
initQState p = QState M.empty [] Nothing p

runModFinalizers :: GHCiQ ()
runModFinalizers = go =<< getState
  where
    go s | (f:ff) <- qsFinalizers s = do
      putState (s { qsFinalizers = ff}) >> TH.runQ f >> getState >>= go
    go _ = return ()

newtype GHCiQ a = GHCiQ { runGHCiQ :: QState -> IO (a, QState) }

data GHCiQException = GHCiQException QState String
  deriving (Show, Typeable)

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

putState :: QState -> GHCiQ ()
putState s = GHCiQ $ \_ -> return ((),s)

noLoc :: TH.Loc
noLoc = TH.Loc "<no file>" "<no package>" "<no module>" (0,0) (0,0)

ghcCmd :: Binary a => Message (THResult a) -> GHCiQ a
ghcCmd m = GHCiQ $ \s -> do
  r <- remoteCall (qsPipe s) m
  case r of
    THException str -> throwIO (GHCiQException s str)
    THComplete res -> return (res, s)

instance TH.Quasi GHCiQ where
  qNewName str = ghcCmd (NewName str)
  qReport isError msg = ghcCmd (Report isError msg)

  -- See Note [TH recover with -fexternal-interpreter] in TcSplice
  qRecover (GHCiQ h) (GHCiQ a) = GHCiQ $ \s -> (do
    remoteCall (qsPipe s) StartRecover
    (r, s') <- a s
    remoteCall (qsPipe s) (EndRecover False)
    return (r,s'))
      `catch`
       \GHCiQException{} -> remoteCall (qsPipe s) (EndRecover True) >> h s
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
  qRunIO m = GHCiQ $ \s -> fmap (,s) m
  qAddDependentFile file = ghcCmd (AddDependentFile file)
  qAddTopDecls decls = ghcCmd (AddTopDecls decls)
  qAddModFinalizer fin = GHCiQ $ \s ->
    return ((), s { qsFinalizers = fin : qsFinalizers s })
  qGetQ = GHCiQ $ \s ->
    let lookup :: forall a. Typeable a => Map TypeRep Dynamic -> Maybe a
        lookup m = fromDynamic =<< M.lookup (typeOf (undefined::a)) m
    in return (lookup (qsMap s), s)
  qPutQ k = GHCiQ $ \s ->
    return ((), s { qsMap = M.insert (typeOf k) (toDyn k) (qsMap s) })
  qIsExtEnabled x = ghcCmd (IsExtEnabled x)
  qExtsEnabled = ghcCmd ExtsEnabled

startTH :: IO (RemoteRef (IORef QState))
startTH = do
  r <- newIORef (initQState (error "startTH: no pipe"))
  mkRemoteRef r

finishTH :: Pipe -> RemoteRef (IORef QState) -> IO ()
finishTH pipe rstate = do
  qstateref <- localRef rstate
  qstate <- readIORef qstateref
  _ <- runGHCiQ runModFinalizers qstate { qsPipe = pipe }
  return ()

runTH
  :: Pipe -> RemoteRef (IORef QState) -> HValueRef
  -> THResultType
  -> Maybe TH.Loc
  -> IO ByteString
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

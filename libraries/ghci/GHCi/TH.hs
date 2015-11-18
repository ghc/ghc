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

data QState = QState
  { qsMap        :: Map TypeRep Dynamic
       -- ^ persistent data between splices in a module
  , qsFinalizers :: [TH.Q ()]
       -- ^ registered finalizers (in reverse order)
  , qsLocation   :: Maybe TH.Loc
       -- ^ location for current splice, if any
  , qsPipe :: Pipe
       -- ^ pipe to communicate with GHC
  }
instance Show QState where show _ = "<QState>"

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
  return    = pure
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
  qRecover = undefined
{-
  qRecover (GHCiQ h) (GHCiQ a) = GHCiQ $ \s -> do
    let r :: Bool -> IO ()
        r b = do EndRecover' <- sendRequest (EndRecover b)
                 return ()
    StartRecover' <- sendRequest StartRecover
    (a s >>= \s' -> r False >> return s') `E.catch`
      \(GHCiQException s' _ _) -> r True >> h s
-}
  qLookupName isType occ = ghcCmd (LookupName isType occ)
  qReify name = ghcCmd (Reify name)
  qReifyFixity name = ghcCmd (ReifyFixity name)
  qReifyInstances name tys = ghcCmd (ReifyInstances name tys)
  qReifyRoles name = ghcCmd (ReifyRoles name)

  -- To reify annotations, we send GHC the AnnLookup and also the TypeRep of the
  -- thing we're looking for, to avoid needing to serialize irrelevant annotations.
  qReifyAnnotations :: forall a . Data a => TH.AnnLookup -> GHCiQ [a]
  qReifyAnnotations lookup =
    map (deserializeWithData . B.unpack) <$> ghcCmd (ReifyAnnotations lookup typerep)
    where typerep = typeOf (undefined :: a)

  qReifyModule m = ghcCmd (ReifyModule m)
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

startTH :: IO HValueRef
startTH = do
  r <- newIORef (initQState (error "startTH: no pipe"))
  mkHValueRef (unsafeCoerce r)

finishTH :: Pipe -> HValueRef -> IO ()
finishTH pipe rstate = do
  qstateref <- unsafeCoerce <$> localHValueRef rstate
  qstate <- readIORef qstateref
  _ <- runGHCiQ runModFinalizers qstate { qsPipe = pipe }
  freeHValueRef rstate
  return ()

runTH
  :: Pipe -> HValueRef -> HValueRef
  -> THResultType
  -> Maybe TH.Loc
  -> IO ByteString
runTH pipe rstate rhv ty mb_loc = do
  hv <- localHValueRef rhv
  case ty of
    THExp -> runTHQ pipe rstate mb_loc (unsafeCoerce hv :: TH.Q TH.Exp)
    THPat -> runTHQ pipe rstate mb_loc (unsafeCoerce hv :: TH.Q TH.Pat)
    THType -> runTHQ pipe rstate mb_loc (unsafeCoerce hv :: TH.Q TH.Type)
    THDec -> runTHQ pipe rstate mb_loc (unsafeCoerce hv :: TH.Q [TH.Dec])
    THAnnWrapper -> do
      hv <- unsafeCoerce <$> localHValueRef rhv
      case hv :: AnnotationWrapper of
        AnnotationWrapper thing ->
          return $! LB.toStrict (runPut (put (toSerialized serializeWithData thing)))

runTHQ :: Binary a => Pipe -> HValueRef -> Maybe TH.Loc -> TH.Q a
       -> IO ByteString
runTHQ pipe@Pipe{..} rstate mb_loc ghciq = do
  qstateref <- unsafeCoerce <$> localHValueRef rstate
  qstate <- readIORef qstateref
  let st = qstate { qsLocation = mb_loc, qsPipe = pipe }
  (r,new_state) <- runGHCiQ (TH.runQ ghciq) st
  writeIORef qstateref new_state
  return $! LB.toStrict (runPut (put r))

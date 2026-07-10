
-- | The endpoints on the server
module Development.Shake.Internal.History.Cloud(
    Cloud, newCloud, addCloud, lookupCloud
    ) where

import Development.Shake.Internal.Value
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.History.Types
import Development.Shake.Internal.History.Network
import Development.Shake.Internal.History.Server
import Development.Shake.Internal.History.Bloom
import Control.Concurrent.Extra
import System.Time.Extra
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import General.Fence
import qualified Data.HashMap.Strict as Map
import Data.Typeable
import Data.Either.Extra
import General.Binary
import General.Extra
import General.Wait


type Initial = Map.HashMap Key (Ver, [Key], Bloom [BS_Identity])

data Cloud = Cloud Server (Locked () -> IO ()) (Fence Locked Initial)


newLaterFence :: (Locked () -> IO ()) -> Seconds -> a -> IO a -> IO (Fence Locked a)
newLaterFence relock maxTime def act = do
    fence <- newFence
    forkFinally (timeout maxTime act) $ \res -> relock $ signalFence fence $ case res of
        Right (Just v) -> v
        _ -> def
    pure fence

laterFence :: MonadIO m => Fence m a -> Wait m a
laterFence fence = do
    res <- liftIO $ testFence fence
    case res of
        Just v -> pure v
        Nothing -> Later $ waitFence fence


newCloud :: (Locked () -> IO ()) -> Map.HashMap TypeRep (BinaryOp Key) -> Ver -> [(TypeRep, Ver)] -> [String] -> Maybe (IO Cloud)
newCloud relock binop globalVer ruleVer urls = flip fmap (if null urls then Nothing else connect $ last urls) $ \conn -> do
    conn <- conn
    server <- newServer conn binop globalVer
    fence <- newLaterFence relock 10 Map.empty $ do
        xs <- serverAllKeys server ruleVer
        pure $ Map.fromList [(k,(v,ds,test)) | (k,v,ds,test) <- xs]
    pure $ Cloud server relock fence


addCloud :: Cloud -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
addCloud (Cloud server _ _) x1 x2 x3 x4 x5 x6 = void $ forkIO $ serverUpload server x1 x2 x3 x4 x5 x6


lookupCloud :: Cloud -> (Key -> Wait Locked (Maybe BS_Identity)) -> Key -> Ver -> Ver -> Wait Locked (Maybe (BS_Store, [[Key]], IO ()))
lookupCloud (Cloud server relock initial) ask key builtinVer userVer = runMaybeT $ do
    mp <- lift $ laterFence initial
    Just (ver, deps, bloom)<- pure $ Map.lookup key mp
    unless (ver == userVer) $ fail ""
    Right vs <- lift $ firstLeftWaitUnordered (fmap (maybeToEither ()) . ask) deps
    unless (bloomTest bloom vs) $ fail ""
    fence <- liftIO $ newLaterFence relock 10 mempty $ serverOneKey server key builtinVer userVer $ zip deps vs
    tree <- lift $ laterFence fence
    f [deps] tree
    where
        f :: [[Key]] -> BuildTree Key -> MaybeT (Wait Locked) (BS_Store, [[Key]], IO ())
        f ks (Done store xs) = pure (store, reverse ks, serverDownloadFiles server key xs)
        f ks (Depend deps trees) = do
            Right vs <- lift $ firstLeftWaitUnordered (fmap (maybeToEither ()) . ask) deps
            Just tree<- pure $ lookup vs trees
            f (deps:ks) tree

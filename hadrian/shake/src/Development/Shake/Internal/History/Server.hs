
-- | The endpoints on the cloud server
module Development.Shake.Internal.History.Server(
    Server, BuildTree(..),
    newServer,
    serverAllKeys, serverOneKey, serverDownloadFiles,
    serverUpload
    ) where

import Development.Shake.Internal.History.Bloom
import Development.Shake.Internal.History.Serialise
import Development.Shake.Internal.Value
import General.Binary
import General.Extra
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.History.Types
import Development.Shake.Internal.History.Network
import Data.Typeable


data Server = Server Conn (Map.HashMap TypeRep (BinaryOp Key)) Ver

newServer :: Conn -> Map.HashMap TypeRep (BinaryOp Key) -> Ver -> IO Server
newServer a b c = pure $ Server a b c

serverAllKeys :: Server -> [(TypeRep, Ver)] -> IO [(Key, Ver, [Key], Bloom [BS_Identity])]
serverAllKeys (Server conn key ver) typs = do
    res <- post conn "allkeys/v1" $ LBS.fromChunks [runBuilder $ putEx $ withTypeReps $ SendAllKeys ver typs]
    let RecvAllKeys ans = withoutKeys key $ getEx $ BS.concat $ LBS.toChunks res
    pure ans

serverOneKey :: Server -> Key -> Ver -> Ver -> [(Key, BS_Identity)] -> IO (BuildTree Key)
serverOneKey _ _ _ _ _ = pure $ Depend [] []


serverDownloadFiles :: Server -> Key -> [(FilePath, FileSize, FileHash)] -> IO ()
serverDownloadFiles _ _ _ = fail "Failed to download the files"


serverUpload :: Server -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
serverUpload _ key _ _ _ _ _  = print ("SERVER", "Uploading key", key)

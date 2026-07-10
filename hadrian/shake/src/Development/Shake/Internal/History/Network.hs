{-# LANGUAGE CPP #-}

-- | The network operations available
module Development.Shake.Internal.History.Network(
    Conn, connect, post
    ) where

#ifdef NETWORK
import Network.HTTP
import Network.URI
import Data.List
import Data.Maybe
#endif

import qualified Data.ByteString.Lazy as LBS


newtype Conn = Conn String

connect :: String -> Maybe (IO Conn)
post :: Conn -> String -> LBS.ByteString -> IO LBS.ByteString


#ifndef NETWORK

connect _ = Nothing
post (Conn _) _ _ = fail "impossible to get here"

#else

connect x = Just $ pure $ Conn $ x ++ ['/' | not $ "/" `isSuffixOf` x]

post (Conn prefix) url send = do
    let request = Request
            {rqURI = parseURI_ $ prefix ++ url
            ,rqMethod = POST
            ,rqHeaders = [Header HdrContentType "application/octet-stream", Header HdrContentLength $ show $ LBS.length send]
            ,rqBody = send}
    response <- simpleHTTP request
    case response of
        Left e -> fail $ "Network.post, failed: " ++ show e
        Right v | rspCode v /= (2,0,0) -> fail $ "Network.post, failed: " ++ show (rspCode v)
                | otherwise -> pure $ rspBody v


parseURI_ :: String -> URI
parseURI_ x = fromMaybe (error $ "Failed to parse URI, " ++ x) $ parseURI x

#endif

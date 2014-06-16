{-# LANGUAGE Arrows, FunctionalDependencies, FlexibleContexts, 
             MultiParamTypeClasses, RecordWildCards  #-}

module T5045 where

import Control.Arrow

class (Control.Arrow.Arrow a') => ArrowAddReader r a a' | a -> a' where
  elimReader :: a (e, s) b -> a' (e, (r, s)) b

newtype ByteString = FakeByteString String

pathInfo :: Monad m => m String
pathInfo = undefined

requestMethod :: Monad m => m String
requestMethod = undefined

getInputsFPS :: Monad m => m [(String, ByteString)]  
getInputsFPS = undefined

class HTTPRequest r s | r -> s where
    httpGetPath :: r -> String
    httpSetPath :: r -> String -> r
    httpGetMethod :: r -> String
    httpGetInputs :: r -> [(String, s)]

data CGIDispatch = CGIDispatch {
    dispatchPath :: String,
    dispatchMethod :: String,
    dispatchInputs :: [(String, ByteString)]  }

instance HTTPRequest CGIDispatch ByteString where
    httpGetPath = dispatchPath
    httpSetPath r s = r { dispatchPath = s }
    httpGetMethod = dispatchMethod
    httpGetInputs = dispatchInputs

runDispatch :: (Arrow a, ArrowAddReader CGIDispatch a a', Monad m) => a b  c -> m (a' b c)  
runDispatch a = do
    dispatchPath <- pathInfo
    dispatchMethod <- requestMethod
    dispatchInputs <- getInputsFPS
    return $ proc b -> (| elimReader (a -< b) |) CGIDispatch { .. }  

module T20200AgdaDatatypes where

import T20200AgdaBase
import T20200AgdaPretty

reportSDoc :: IO Doc -> IO ()
reportSDoc d = render <$> d

getConstructorData :: HasConstInfo m => QName -> m Definition
getConstructorData = getConstInfo

getConType :: QName -> IO a
getConType t = do
  _ <- reportSDoc $ prettyTCM t
  return undefined

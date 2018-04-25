{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

import qualified Data.ByteString as BS
import Type.Reflection
import Data.Binary
import GHCi.TH.Binary ()

import GHC.Exts
import Data.Kind
import Data.Proxy

testRoundtrip :: Typeable a => TypeRep a -> IO ()
testRoundtrip rep
  | rep /= rep' = putStrLn $ "bad: " ++ show rep ++ " /= " ++ show rep'
  | otherwise   = putStrLn $ "good: " ++ show rep
  where
    rep' = decode (encode rep)

main :: IO ()
main = do
    testRoundtrip (typeRep :: TypeRep Int)
    testRoundtrip (typeRep :: TypeRep Int#)
    testRoundtrip (typeRep :: TypeRep IO)
    testRoundtrip (typeRep :: TypeRep Maybe)
    testRoundtrip (typeRep :: TypeRep TYPE)
    testRoundtrip (typeRep :: TypeRep RuntimeRep)
    testRoundtrip (typeRep :: TypeRep 'IntRep)
    testRoundtrip (typeRep :: TypeRep (->))
    testRoundtrip (typeRep :: TypeRep (Proxy Int))
    testRoundtrip (typeRep :: TypeRep (Proxy Int#))
    testRoundtrip (typeRep :: TypeRep Type)
    testRoundtrip (typeRep :: TypeRep (Int -> Int))
    testRoundtrip (typeRep :: TypeRep 5)
    testRoundtrip (typeRep :: TypeRep "hello world")
    testRoundtrip (typeRep :: TypeRep ('Just 5))

{-# LANGUAGE TypeApplications, DataKinds #-}
module Main where
import Data.IORef
import Data.Proxy
import NativeAlternatingBindersProvider

data ShowMarker = ShowMarker

instance Show ShowMarker where
  show _ = "selected"

main :: IO ()
main = do
  ref <- newIORef True
  b <- readIORef ref
  let m = Just False
      badB :: Bool
      badB = error "unused Bool"
      badM :: Maybe Bool
      badM = error "unused Maybe"
  print (case keepAfter b @Bool m (Reading 21) of Reading n -> n)
  print (case keepAfter badB @Bool badM (Reading 13) of Reading n -> n)
  print (case keepExplicit b @Bool m (Reading 8) of Reading n -> n)
  print (case keepAfter b m (Reading 5) of Reading n -> n)
  print (case keepAfter badB badM (Reading 14) of Reading n -> n)
  print (case keepImplicit b (Just False) (Reading 6) of Reading n -> n)
  print (case forwardKind True @'False Marker of Marker -> (1 :: Int))
  print (case forwardKind False @'EQ Marker of Marker -> (2 :: Int))
  print (case sameSpelling b @'LT Proxy of Proxy -> (10 :: Int))
  print (case twice b @Bool m @Ordering (Just LT) (Reading 3) of (Reading n, Proxy) -> n)
  print (case twice b m (Just LT) (Reading 4) of (Reading n, Proxy) -> n)
  print (case plainFirst () @Bool m (Reading 34) of Reading n -> n)
  showIndex <- newIORef (error "unused Show index" :: Maybe ShowMarker) >>= readIORef
  print (showAfter badB showIndex (error "unused Show argument") (Reading 51))

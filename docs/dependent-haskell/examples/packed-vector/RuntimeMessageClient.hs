{-# LANGUAGE GADTs, ImpredicativeTypes, NoExperimentalDependentTypes, TypeFamilies #-}

module RuntimeMessageClient (main, observeMessage) where

import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy (..))
import GHC.Sigma (Sigma (Pair), first)
import RuntimeMessage (Allowed, Format (Configured), Message, Payload (..), Permit (..), make)

-- | Pack the mode and a payload created at that mode.
pack :: Bool -> Message
pack flag = Pair flag (make flag)

-- | Inspect the stored witness without forcing the payload.
mode :: Message -> Bool
mode = first

-- | The stored witness selects which payload constructor can occur.
readMessage :: Message -> (Bool, String)
readMessage (Pair False (Number n)) = (False, show n)
readMessage (Pair True (Text text)) = (True, text)

-- | Use a global instance, a matched dictionary and a lazy payload.
observeMessage :: Bool -> ((Bool, String), (Bool, String), Bool)
observeMessage flag =
    ( readDefault (Pair flag (make flag))
    , readWith Permit (Proxy :: Proxy Bool) (not flag)
    , defaultMode (Pair flag (error "unused message payload"))
    )
  where
    readDefault :: Configured Bool -> (Bool, String)
    readDefault = readMessage

    readWith :: Permit tag -> Proxy tag -> Bool -> (Bool, String)
    readWith Permit key stored = readPermitted key (Pair stored (make stored))

    readPermitted :: Allowed tag => Proxy tag -> Configured [tag] -> (Bool, String)
    readPermitted _ = readMessage

    defaultMode :: Configured Bool -> Bool
    defaultMode = first

main :: IO ()
main = do
    flag <- readIORef =<< newIORef True
    print (readMessage (pack flag), readMessage (pack (not flag)),
           mode (Pair False (error "unused payload")))

{-# LANGUAGE CPP, DataKinds, GADTs, ImpredicativeTypes, NoExperimentalDependentTypes, TypeFamilies #-}
module AssociatedPayloadSumClient (observePayload) where
import Data.Proxy (Proxy (Proxy))
import GHC.Sigma (Sigma (Pair))
import AssociatedPayloadSum (Allowed, Permit (Permit), PayloadFormat (Packet), makeGlobal, makePermitted)

readGlobal :: Packet Bool -> (Bool, String)
readGlobal (Pair False n) = (False, show (n :: Int))
readGlobal (Pair True c) = (True, [c])

globalTag :: Packet Bool -> Bool
globalTag (Pair tag _) = tag

readPermitted :: Allowed a => Proxy a -> Packet [a] -> (Bool, String)
readPermitted _ (Pair False n) = (False, show (n :: Int))
readPermitted _ (Pair True c) = (True, [c])

permittedTag :: Allowed a => Proxy a -> Packet [a] -> Bool
permittedTag _ (Pair tag _) = tag

observeGiven :: Permit a -> Proxy a -> Bool -> ((Bool, String), Bool)
observeGiven Permit key tag =
    ( readPermitted key (Pair tag (makePermitted key tag))
    , permittedTag key (Pair tag (error "forced permitted associated payload"))
    )

#ifdef WRONG_PAYLOAD
wrong :: (Bool, String)
wrong = readGlobal (Pair False 'w')
#endif

#ifdef MISSING_DOMAIN
missing :: Bool
missing = permittedTag (Proxy :: Proxy Int) (Pair True (error "missing permit"))
#endif

#ifdef MISSING_TYPE_DOMAIN
missingTypeDomain :: Packet [Int] -> Bool
missingTypeDomain _ = False
#endif

-- | Read selected payloads and read both tags with undefined payloads.
observePayload :: Bool -> ((Bool, String), Bool, ((Bool, String), Bool))
observePayload tag =
    ( readGlobal (Pair tag (makeGlobal tag))
    , globalTag (Pair tag (error "forced global associated payload"))
    , observeGiven Permit (Proxy :: Proxy Bool) (not tag)
    )

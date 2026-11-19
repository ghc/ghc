{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import NativeSimplifierProvider (choose, keep, Tagged(..), keepTagged, one, toList, typeOnly, Nat(..), observeLengths, observeDemand, lazyLengths, lazyPartial, impossible, scopeAnnotations, sample, observePacked, driveSame, scopeLet, scopeKnown)

main :: IO ()
main = do
    let runtime value = newIORef value >>= readIORef
    yes <- runtime True
    no <- runtime False
    exceptional <- runtime (error "retained-index" :: Bool)
    print (toList (keep yes (choose yes)), toList (keep no (choose no)))
    case keepTagged exceptional (Tagged one) of Tagged xs -> print (toList xs)
    result <- try (evaluate (toList (choose exceptional)))
    case result of
      Left e | "retained-index" `isInfixOf` displayException (e :: ErrorCall) ->
          putStrLn "choose bottom: forced"
      _ -> fail "Unexpected demand of choose"
    print (typeOnly yes, typeOnly exceptional)
    zero <- runtime Z
    successor <- runtime (S Z)
    two <- runtime (S (S Z))
    unread <- runtime (error "length" :: Nat)
    print (observeLengths two successor)
    print (map (uncurry observeDemand) [(zero,zero),(zero,two),(two,zero),(two,successor)])
    print ("unused lengths", lazyLengths unread unread)
    print ("partial application", lazyPartial unread)
    demand <- try (evaluate (observeDemand two unread))
    case demand of
      Left e | "length" `isInfixOf` displayException (e :: ErrorCall) ->
          putStrLn "nested second length: forced"
      _ -> fail "Unexpected demand of nested second length"
    print (impossible (choose False))
    print (scopeAnnotations yes, scopeAnnotations exceptional)
    print (observePacked sample)
    driveSame exceptional >>= print
    print (scopeLet, scopeKnown exceptional)

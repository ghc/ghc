{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, NoExperimentalDependentTypes #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Proxy (Proxy (..))
import GHC.Sigma (Sigma (Pair), first)
import NativeClosedFamilySumChain (Loop, OpenLoop, OpenPackage, Outer)
import NativeClosedFamilySumProvider (KindCapture, Pack, PackViaAlias, Tagged (..))

closed :: Pack False Int
closed = Pair False (FalseTag True)

captured :: Pack True Char
#ifdef BAD_TAG
captured = Pair False (TrueTag 'x')
#else
captured = Pair True (TrueTag 'x')
#endif

lazyPayload :: Pack True Char
lazyPayload = Pair False (error "unused payload" :: Tagged False Char)

kindCapture :: KindCapture True Bool False
kindCapture = Pair True (Proxy :: Proxy True, Proxy :: Proxy False)

size :: Pack True Char -> Bool
size (Pair tag _) = tag

sizeViaAlias :: PackViaAlias True Char -> Bool
sizeViaAlias (Pair tag _) = tag

sizeKind :: KindCapture True Bool False -> Bool
sizeKind (Pair tag ~(_, _)) = tag

sizeOuter :: Outer True Char -> Bool
sizeOuter (Pair tag _) = tag

#ifdef BAD_PATTERN
lazyPackage :: Pack True Char -> Bool
lazyPackage ~(Pair tag _) = tag

stuckPackage :: Outer select Char -> Bool
stuckPackage (Pair tag _) = tag

newtype WrappedPackage = WrappedPackage (Pack True Char)

wrappedPackage :: WrappedPackage -> Bool
wrappedPackage (Pair tag _) = tag
#endif

#ifdef BAD_RECURSION
loopPackage :: Loop Char -> Bool
loopPackage (Pair tag _) = tag
#endif

sizeOpen :: OpenPackage Bool -> Bool
sizeOpen (Pair tag _) = tag

#ifdef BAD_OPEN_PATTERN
stuckOpen :: OpenPackage Char -> Bool
stuckOpen (Pair tag _) = tag

wrappedOpen :: OpenPackage Int -> Bool
wrappedOpen (Pair tag _) = tag
#endif

#ifdef BAD_OPEN_RECURSION
loopOpen :: OpenLoop Char -> Bool
loopOpen (Pair tag _) = tag
#endif

main :: IO ()
main = do
    outer <- try (evaluate (sizeOuter (error "outer package"))) :: IO (Either ErrorCall Bool)
    open <- try (evaluate (sizeOpen (error "open package"))) :: IO (Either ErrorCall Bool)
    case (outer, open) of
        (Left _, Left _) ->
            print (first closed, size captured, size lazyPayload, first kindCapture,
                   sizeViaAlias (Pair False (error "alias payload")),
                   sizeKind (Pair True (error "lazy tuple payload")),
                   sizeOuter (Pair False (error "chained payload")),
                   sizeOpen (Pair True (error "open payload")))
        _ -> fail "package pattern left the outer constructor unevaluated"

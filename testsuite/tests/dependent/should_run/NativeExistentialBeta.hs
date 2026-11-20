{-# LANGUAGE ExperimentalDependentTypes, GADTs, RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Kind (Type)
import Data.List (isInfixOf)
import GHC.Exists

data Box (a :: Type) = Box Int
type Hidden = exists (a :: Type) | Box a

{-# NOINLINE add #-}
add :: forall a. Box a -> Int -> Int
add (Box value) increment = value + increment

capture :: forall a. Box a -> Int -> (Int, Int)
capture outer original =
    case (Pack (type a) outer :: Hidden) of
      Pack (type witness) inner ->
        let value :: Box witness -> Int
            value (Box original) = original
        in (add @witness inner original, value inner)

discard :: forall a. Box a -> Bool
discard outer = case (Pack (type a) outer :: Hidden) of
    Pack (type witness) _ -> True

strict :: forall a. Box a -> Int
strict outer = case (Pack (type a) outer :: Hidden) of
    Pack (type witness) inner -> add @witness inner 0

main :: IO ()
main = do
    forced <- try (evaluate (strict @Int (error "forced payload"))) :: IO (Either ErrorCall Int)
    print (capture @Int (Box 37) 5,
           discard @Bool (error "discarded payload"),
           either (isInfixOf "forced payload" . displayException) (const False) forced)

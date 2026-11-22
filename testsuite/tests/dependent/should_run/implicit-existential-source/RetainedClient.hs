{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module RetainedClient where

import Control.Monad (unless)
import Data.IORef (newIORef, readIORef)
import RetainedDot (Choice(Choice), HiddenChoice, HiddenConstant, Value(Value), forget, observe, retained)

-- | Choose the erased payload type before packing the runtime flag.
choose :: Bool -> a -> HiddenChoice a
choose flag value = Choice flag (Value value)

-- | Hide the flag and leave the complete payload unevaluated.
chooseBottom :: Bool -> HiddenChoice Char
chooseBottom flag = Choice flag (error "forced choice payload")

-- | Use an erased payload type whose synonym discards the hidden flag.
chooseConstant :: Bool -> HiddenConstant
chooseConstant flag = Choice flag (Value (Just 'q'))

-- | Read the payload only when its stored flag is true.
readChoice :: Choice a b -> Maybe a
readChoice (Choice True (Value value)) = Just value
readChoice (Choice False _) = Nothing

main :: IO ()
main = do
    yes <- newIORef True >>= readIORef
    no <- newIORef False >>= readIORef
    unless (readChoice (choose yes 'x') == Just 'x' && readChoice (chooseBottom no) == Nothing &&
            show (readChoice (chooseConstant yes)) == "Just (Just 'q')") $
        fail "expected to open the runtime choices"
    print (map (observe . forget . retained) [True, False])

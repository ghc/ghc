{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main (main) where
import Provider
import System.Environment (getArgs)

data Normal = Normal Bool
data Invert = Invert Bool
data Ignore = Ignore
data Unobserved = Unobserved

instance Choose Normal where choose (Normal x) = x
instance Choose Invert where choose (Invert x) = not x
instance Choose Ignore where choose _ = True
instance Choose Unobserved where choose _ = error "the method was forced"

main :: IO ()
main = do
    args <- getArgs
    let input = args == ["true"]
    print (observe (Normal input))
    print (observeBoth (Normal input) (Invert input))
    print (observeGround input)
    print (observe (error "the ignored operand was forced" :: Ignore))
    print (ignorePayload Unobserved)

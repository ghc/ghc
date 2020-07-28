module Main where

main :: IO ()
main = mapM_ tryTest tests
    where
        tryTest :: (String, Bool) -> IO ()
        tryTest (name, test) = if test then pure () else putStrLn $ "Could not verify " <> name
        tests :: [(String, Bool)]
        tests = [ ("Associativity of addition", associativityOfPlus)
                , ("Associativity of multiplication", associativityOfTimes)
                , ("Additive inverse", additiveInverse)
                , ("Commutativity of addition", commutativityOfPlus)
                , ("Distributivity of * with respect to +", distributivity)
                ]

associativityOfPlus :: Bool
associativityOfPlus = (() + ()) + () == () + (() + ())

associativityOfTimes :: Bool
associativityOfTimes = (() * ()) * () == () * (() * ())

additiveInverse :: Bool
additiveInverse = () + (-()) == fromInteger 1

commutativityOfPlus :: Bool
commutativityOfPlus = () + () == () + ()

multiplicativeIdentity :: Bool
multiplicativeIdentity = first && second
    where
        first  = () * fromInteger 1 == ()
        second = fromInteger 1 * () == ()

distributivity :: Bool
distributivity = first && second
    where
        first = () * (() + ()) == (() * ()) + (() * ())
        second = (() + ()) * () == (() * ()) + (() * ())

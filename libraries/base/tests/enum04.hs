{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception

-- enumFrom on basic numeric types should be strict
-- (possibly a bug in the Haskell Report: it specifies that
-- these ops should be strict in the section on Enum, but the
-- sample code in the Prelude doesn't agree, at least for
-- Float and Double).

main = do 
  catch	(evaluate [error "" :: Int ..] >> return ())     (\(e::SomeException) -> putStrLn "ok1")
  catch	(evaluate [error "" :: Integer ..] >> return ()) (\(e::SomeException) -> putStrLn "ok2")
  catch	(evaluate [error "" :: Float ..] >> return ())   (\(e::SomeException) -> putStrLn "ok3")
  catch	(evaluate [error "" :: Double ..] >> return ())  (\(e::SomeException) -> putStrLn "ok4")

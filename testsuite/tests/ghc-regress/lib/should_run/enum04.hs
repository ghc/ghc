import Control.Exception
import Prelude hiding (catch)

main = do 
  catch	(evaluate [error "" :: Int ..] >> return ())     (\_ -> putStrLn "ok1")
  catch	(evaluate [error "" :: Integer ..] >> return ()) (\_ -> putStrLn "ok2")
  catch	(evaluate [error "" :: Float ..] >> return ())   (\_ -> putStrLn "ok3")
  catch	(evaluate [error "" :: Double ..] >> return ())  (\_ -> putStrLn "ok4")

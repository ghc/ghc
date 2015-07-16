{-# OPTIONS_GHC -fdefer-type-errors -fno-warn-deferred-type-errors #-}

import Control.Exception

a :: Int
a = 'a'

main :: IO ()
main = do
  catch (evaluate a)
        (\e -> do let err = show (e :: TypeError)
                  putStrLn ("As expected, TypeError: " ++ err)
                  return "")
  catch (evaluate a)
        (\e -> do let err = show (e :: ErrorCall)
                  putStrLn ("Something went horribly wrong: " ++ err)
                  return "")

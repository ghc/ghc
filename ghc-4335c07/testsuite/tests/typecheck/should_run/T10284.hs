{-# OPTIONS_GHC -fdefer-type-errors -fno-warn-deferred-type-errors #-}

import Control.Exception
import Data.Functor

a :: Int
a = 'a'

main :: IO ()
main = do
  catch (void $ evaluate a)
        (\e -> do let err = show (e :: TypeError)
                  putStrLn ("As expected, TypeError: " ++ err))
  catch (void $ evaluate a)
        (\e -> do let err = show (e :: ErrorCall)
                  putStrLn ("Something went horribly wrong: " ++ err))

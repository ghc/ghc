{-# LANGUAGE ExistentialQuantification, BangPatterns #-}
{-# OPTIONS_GHC -O #-}

module Main (main) where

import Data.IORef (newIORef, readIORef)

data Step s = Done
            | Skip !s
            | Yield !Char !s

data Stream = forall s. Stream (s -> Step s) !s !Int

unstreamList :: Stream -> [Char]
unstreamList (Stream next s0 _) = unfold s0
    where unfold !s = case next s of
                        Done       -> []
                        Skip s'    -> unfold s'
                        Yield x s' -> x : unfold s'
{-# INLINE [0] unstreamList #-}

appendS :: Stream -> Stream -> Stream
appendS (Stream next s len) _ = Stream next s len
{-# INLINE [0] appendS #-}

justifyLeftI :: Int -> Int -> Stream
justifyLeftI k u =
  let
       next Nothing = next (Just 0)
       next (Just n)
           | n < k       = Yield 'a' (Just (n+1))
           | otherwise   = Done
       {-# INLINE next #-}

     in Stream next Nothing (max k u)
{-# INLINE [0] justifyLeftI #-}

prettyPrintLogStats :: Int -> [String]
prettyPrintLogStats rawResults = map fromRow columns
  where
    columns :: [Int]
    columns = map (\_ -> 0) [rawResults]

    moduleLen, lineLen :: Int
    (moduleLen, lineLen) = foldr (\_ (_,_) -> (5, 2)) (0, 0) columns

    fromRow :: Int -> String
    fromRow x = unstreamList (justifyLeftI moduleLen x `appendS` justifyLeftI lineLen x)

main :: IO ()
main = do
    timingsRef <- newIORef 0
    timings <- readIORef timingsRef
    putStrLn $ concat $ prettyPrintLogStats timings

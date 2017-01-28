-- | Utilities to debug "GHC.HeapView".
module GHC.HeapView.Debug where

import GHC.HeapView
import Text.Printf
import System.IO
import Control.Monad
import System.Mem
import Data.Maybe
import Data.Char
import Data.IORef

-- | This function walks the heap referenced by the argument, printing the
-- \"path\", i.e. the pointer indices from the initial to the current closure
-- and the closure itself. When the runtime crashes, the problem is likely
-- related to one of the earlier steps.
walkHeap
    :: Bool -- ^ Whether to check for cycles
    -> Bool -- ^ Whether to GC in every step
    -> Box -- ^ The closure to investigate
    -> IO ()
walkHeap slow check x = do
    seenRef <- newIORef []
    go seenRef [] x
 where
    go seenRef prefix b = do
        _ <- printf "At %s:\n" (show prefix)
        seen <- readIORef seenRef
        previous <- if check then findM (areBoxesEqual b . fst) seen else return Nothing
        case previous of
            Just (_,p') -> printf "Seen at %s.\n" (show p')
            Nothing -> do
                hFlush stdout
                c <- getBoxedClosureData b
                putStrLn (ppClosure (\_ box -> show box) 0 c)
                when slow performGC
                isCC <- isCharCons c
                unless isCC $ do
                    modifyIORef seenRef ((b,prefix):)
                    forM_ (zip [(0::Int)..] (allPtrs c)) $ \(n,box) ->
                        go seenRef (prefix ++ [n]) box

walkPrefix :: [Int] -> a -> IO Box
walkPrefix is v = go is (asBox v)
  where
    go [] a = return a
    go (x:xs) a = do
        c <- getBoxedClosureData a
        walkPrefix xs (allPtrs c !! x)


findM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findM _p [] = return Nothing
findM p (x:xs) = do
    b <- p x
    if b then return (Just x) else findM p xs

isCharCons :: GenClosure Box -> IO Bool
isCharCons c | Just (h,_) <- isCons c = (isJust . isChar) <$> getBoxedClosureData h
isCharCons _ = return False

isCons :: GenClosure b -> Maybe (b, b)
isCons (ConsClosure { name = ":", dataArgs = [], ptrArgs = [h,t]}) = Just (h,t)
isCons _ = Nothing

isChar :: GenClosure b -> Maybe Char
isChar (ConsClosure { name = "C#", dataArgs = [ch], ptrArgs = []}) = Just (chr (fromIntegral ch))
isChar _ = Nothing

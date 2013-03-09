{- The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   Written by Tom Pledger, 13 Nov 2006. modified by Don Stewart
   Updated for chameneos-redux by Spencer Janssen, 27 Nov 2007
   Modified by Péter Diviánszky, 19 May 2010
   Modified by Louis Wasserman, 14 June 2010

   Should be compiled with -O2 -threaded -fvia-c -optc-O3 and run with +RTS -N<number of cores>.
   -}

import LwConc.Substrate
import ConcurrentList
import MVarList
import Control.Monad
import Data.Char
import Data.IORef
import System.Environment
import System.IO
-- import GHC.Conc
import Foreign hiding (complement)

newtype Color = C Int deriving (Storable,Enum)

#define Y (C 2)
#define R (C 1)
#define B (C 0)

instance Show Color where
  show Y = "yellow"
  show R = "red"
  show B = "blue"

complement :: Color -> Color -> Color
complement !a !b = case a of
    B -> case b of R -> Y; B -> B; _ -> R
    R -> case b of B -> Y; R -> R; _ -> B
    Y -> case b of B -> R; Y -> Y; _ -> B

type Chameneous = Ptr Color
data MP = Nobody !Int | Somebody !Int !Chameneous !(MVar Chameneous)

arrive :: MVar MP -> MVar (Int, Int) -> Chameneous -> IO ()
arrive !mpv !finish !ch = do
    waker <- newEmptyMVar
    hole1 <- newIORef undefined
    hole2 <- newIORef undefined
    let inc x = (fromEnum (ch == x) +)
        go !t !b = do
            w <- takeMVarWithHole mpv hole1
            case w of
                Nobody 0
                  -> do
                      putMVar mpv w
                      putMVar finish (t, b)
                Nobody q -> do
                    putMVar mpv $ Somebody q ch waker
                    ch' <- takeMVarWithHole waker hole2
                    go (t+1) $ inc ch' b
                Somebody q ch' waker' -> do
                    c  <- peek ch
                    c' <- peek ch'
                    let !c'' = complement c c'
                    poke ch  c''
                    poke ch' c''
                    putMVar waker' ch
                    let !q' = q-1
                    putMVar mpv $ Nobody q'
                    go (t+1) $ inc ch' b
    go 0 0

showN = unwords . map ((digits !!) . digitToInt) . show

digits = words "zero one two three four five six seven eight nine"

run :: Int -> Int -> [Color] -> IO (IO ())
run n cpu cs = do
  fs    <- replicateM (length cs) newEmptyMVar
  mpv   <- newMVar (Nobody n)
  hole <- newIORef undefined
  withArrayLen cs $ \ n cols -> do
    zipWithM_ ((forkOn cpu .) . arrive mpv) fs (take n (iterate (`advancePtr` 1) cols))
    return $ do
      putStrLn . map toLower . unwords . ([]:) . map show $ cs
      ns    <- mapM (\m -> takeMVarWithHole m hole) fs
      putStr . map toLower . unlines $ [unwords [show n, showN b] | (n, b) <- ns]
      putStrLn . (" "++) . showN . sum . map fst $ ns
      putStrLn ""

initSched = do
  newSchedFastUserLevelWakeup
  n <- getNumCapabilities
  replicateM_ (n-1) newCapability

main = do
    initSched
    putStrLn . map toLower . unlines $
        [unwords [show a, "+", show b, "->", show $ complement a b]
            | a <- [B..Y], b <- [B..Y]]

    n <- readIO . head =<< getArgs
    actions <- zipWithM (run n) [0..] [[B..Y],[B,R,Y,R,Y,B,R,Y,R,B]]
    sequence_ actions

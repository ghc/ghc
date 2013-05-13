{- The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   Written by Tom Pledger, 13 Nov 2006. modified by Don Stewart
   Updated for chameneos-redux by Spencer Janssen, 27 Nov 2007
   Modified by Péter Diviánszky, 19 May 2010
   Modified by Louis Wasserman, 14 June 2010

   Should be compiled with -O2 -threaded -fvia-c -optc-O3 and run with +RTS
   -N<number of cores>.

   XXX KC: The user of withArrayLen is unsafe. We obtain pointers to
   addresses inside the array but not the byte array itself. This is a
   recipie for disaster. See
   http://hackage.haskell.org/trac/ghc/ticket/7012. Solution?
   -}

import LwConc.Substrate
import FairShare
-- import LwConc.RunQueue
-- import ConcurrentList
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
  show (C v) = error ("show: impossible " ++ show v)

complement :: Color -> Color -> Color
complement !a !b = case a of
    B -> case b of R -> Y; B -> B; _ -> R
    R -> case b of B -> Y; R -> R; _ -> B
    Y -> case b of B -> R; Y -> Y; _ -> B
    C v -> error ("complement: impossible " ++ show v)

type Chameneous = Ptr Color
data MP = Nobody !Int | Somebody !Int !Chameneous !(MVar Chameneous)

arrive :: MVar MP -> MVar (Int, Int) -> Chameneous -> IO ()
arrive !mpv !finish !ch = do
    sc <- getSContIO
    !waker <- newEmptyMVar
    !hole1 <- newIORef undefined
    !hole2 <- newIORef undefined
    !tk <- atomically $ newResumeToken
    let inc x = (fromEnum (ch == x) +)
        go !t !b = do
            -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
            w <- takeMVarWithHole mpv hole1 tk
            -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
            case w of
                Nobody 0 -> do
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    putMVar mpv w tk
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    putMVar finish (t, b) tk
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    return ()
                Nobody q -> do
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    putMVar mpv (Somebody q ch waker) tk
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    ch' <- takeMVarWithHole waker hole2 tk
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    go (t+1) $ inc ch' b
                Somebody q ch' waker' -> do
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    c  <- peek ch
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    c' <- peek ch'
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    let !c'' = complement c c'
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    poke ch  c''
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    poke ch' c''
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    let !q' = q-1
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    putMVar waker' ch tk
                    -- peek ch >>= debugPrint . (\s -> show sc ++ " " ++ show s ++ " " ++ show ch)
                    putMVar mpv (Nobody q') tk
                    go (t+1) $ inc ch' b
    go 0 0

showN = unwords . map ((digits !!) . digitToInt) . show

digits = words "zero one two three four five six seven eight nine"

run :: Int -> Int -> [Color] -> IO (IO ())
run n cpu cs = do
  fs    <- replicateM (length cs) newEmptyMVar
  mpv   <- newMVar (Nobody n)
  hole  <- newIORef undefined
  tk    <- atomically $ newResumeToken
  withArrayLen cs $ \ n cols -> do
    zipWithM_ ((forkOn cpu .) . arrive mpv) fs (take n (iterate (`advancePtr` 1) cols))
    return $ do
      putStrLn . map toLower . unwords . ([]:) . map show $ cs
      ns    <- mapM (\m -> takeMVarWithHole m hole tk) fs
      putStr . map toLower . unlines $ [unwords [show n, showN b] | (n, b) <- ns]
      putStrLn . (" "++) . showN . sum . map fst $ ns
      putStrLn ""

initSched = do
  newSched
  n <- getNumCapabilities
  replicateM_ (n-1) newCapability

main = do
    initSched
    putStrLn . map toLower . unlines $
        [unwords [show a, "+", show b, "->", show $ complement a b]
            | a <- [B..Y], b <- [B..Y]]

    n <- readIO . head =<< getArgs
    actions <- zipWithM (run n) [0..] [[B..Y], [B,R,Y,R,Y,B,R,Y,R,B]]
    sequence_ actions

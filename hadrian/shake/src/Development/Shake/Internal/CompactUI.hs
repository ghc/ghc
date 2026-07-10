
-- | Provide a Buck/Bazel style UI.
module Development.Shake.Internal.CompactUI(
    compactUI
    ) where

import Development.Shake.Internal.CmdOption
import Development.Shake.Internal.Options
import Development.Shake.Internal.Progress

import System.Time.Extra
import General.Extra
import Control.Exception
import General.Thread
import General.EscCodes
import Data.IORef.Extra
import Control.Monad.Extra


data S = S
    {sOutput :: [String] -- ^ Messages that haven't yet been printed, in reverse.
    ,sProgress :: String -- ^ Last progress message.
    ,sTraces :: [Maybe (String, String, Seconds)] -- ^ the traced items, in the order we display them
    ,sUnwind :: Int -- ^ Number of lines we used last time around
    }

emptyS = S [] "Starting..." [] 0

addOutput pri msg s = s{sOutput = msg : sOutput s}
addProgress x s = s{sProgress = x}

addTrace key msg start time s
    | start = s{sTraces = insert (key,msg,time) $ sTraces s}
    | otherwise = s{sTraces = remove (\(a,b,_) -> a == key && b == msg) $ sTraces s}
    where
        insert v (Nothing:xs) = Just v:xs
        insert v (x:xs) = x : insert v xs
        insert v [] = [Just v]

        remove f (Just x:xs) | f x = Nothing:xs
        remove f (x:xs) = x : remove f xs
        remove f [] = []


display :: Seconds -> S -> (S, String)
display time s = (s{sOutput=[], sUnwind=length post}, escCursorUp (sUnwind s) ++ unlines (map pad $ pre ++ post))
    where
        pre = sOutput s
        post = "" : (escForeground Green ++ "Status: " ++ sProgress s ++ escNormal) : map f (sTraces s)

        pad x = x ++ escClearLine
        f Nothing = " *"
        f (Just (k,m,t)) = " * " ++ k ++ " (" ++ g (time - t) m ++ ")"

        g i m | showDurationSecs i == "0s" = m
              | i < 10 = s
              | otherwise = escForeground (if i > 20 then Red else Yellow) ++ s ++ escNormal
            where s = m ++ " " ++ showDurationSecs i


-- | Run a compact UI, with the ShakeOptions modifier, combined with
compactUI :: ShakeOptions -> IO (ShakeOptions, IO ())
compactUI opts = do
    unlessM checkEscCodes $
        putStrLn "Your terminal does not appear to support escape codes, --compact mode may not work"
    ref <- newIORef emptyS
    let tweak = atomicModifyIORef_ ref
    time <- offsetTime
    opts <- pure $ opts
        {shakeTrace = \a b c -> do t <- time; tweak (addTrace a b c t)
        ,shakeOutput = \a b -> tweak (addOutput a b)
        ,shakeProgress = \x -> void $ progressDisplay 1 (tweak . addProgress) x `withThreadsBoth` shakeProgress opts x
        ,shakeCommandOptions = [EchoStdout False, EchoStderr False] ++ shakeCommandOptions opts
        ,shakeVerbosity = Error
        }
    let tick = do t <- time; mask_ $ putStr =<< atomicModifyIORef ref (display t)
    pure (opts, forever (tick >> sleep 0.4) `finally` tick)

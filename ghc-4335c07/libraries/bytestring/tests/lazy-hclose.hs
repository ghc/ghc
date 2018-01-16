import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Monad
import System.Directory
import System.Mem
import System.IO

import Data.ByteString.Internal
import Foreign.ForeignPtr

main = do
    writeFile "a" "x"

    ------------------------------------------------------------------------
    -- readFile tests

    print "Testing resource leaks for Strict.readFile"
    forM_ [1..n] $ const $ do
         r <- S.readFile "a"
         S.writeFile "b" (S8.pack "abc")
         renameFile "b" "a"

    print "Testing resource leaks for Lazy.readFile"
    forM_ [1..n] $ const $ do
         r <- L.readFile "a"
         L.length r `seq` return ()      -- force the input, and done with 'r' now.
         L.writeFile "b" (L8.pack "abc") -- but we still need the finalizers to run
         renameFile "b" "a"

    -- manage the resources explicitly.
    print "Testing resource leaks when converting lazy to strict"
    forM_ [1..n] $ const $ do
         let release c = finalizeForeignPtr fp where (fp,_,_) = toForeignPtr c
         r <- L.readFile "a"
         mapM_ release (L.toChunks r) -- should close it.
         L.writeFile "b" (L8.pack "abc")
         renameFile "b" "a"

    ------------------------------------------------------------------------
    -- hGetContents tests

    -- works now
    print "Testing strict hGetContents"
    forM_ [1..n] $ const $ do
         h <- openFile "a" ReadMode
         r <- S.hGetContents h -- should be strict, and hClosed.
         S.last r `seq` return ()
         S.writeFile "b" (S8.pack "abc")
         renameFile "b" "a"

    -- works now
    print "Testing lazy hGetContents"
    forM_ [1..n] $ const $ do
         h <- openFile "a" ReadMode
         r <- L.hGetContents h -- should be strict, and hClosed.
         L.last r `seq` return ()
         L.writeFile "b" (L8.pack "abc")
         renameFile "b" "a"


n = 1000

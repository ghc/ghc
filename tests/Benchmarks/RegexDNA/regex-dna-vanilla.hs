--
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- contributed by Sergei Matusevich 2007
-- modified by Tim Newsham
-- modified by Stephen Blackheath 2009, v1.0
-- mostly redone by Louis Wasserman, 2010

-- Compile command: ghc --make -O2 -threaded regex.hs -o regex
-- Run command:     ./regex +RTS -N4 -qm -qw -H250M       (quad core)
--                  ./regex +RTS -H250M               (single core)
-- 
-- Note for future reference: with GHC HEAD as of 6/13/10,
-- works something like 3x as fast as GHC 6.12.1.  Reevaluate
-- in future! -LW


import Control.Concurrent
import Control.Parallel.Strategies
import Control.Monad
import GHC.Conc
import Foreign
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString          -- requires haskell-regex-pcre-builtin
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Array.Base
import Data.List
import Data.Word
import Data.ByteString.Unsafe

subs = [
    ("B", "(c|g|t)"),
    ("D", "(a|g|t)"),
    ("H", "(a|c|t)"),
    ("K", "(g|t)"),
    ("M", "(a|c)"),
    ("N", "(a|c|g|t)"),
    ("R", "(a|g)"),
    ("S", "(c|g)"),
    ("V", "(a|c|g)"),
    ("W", "(a|t)"),
    ("Y", "(c|t)")]

main = do
  file <- B.getContents
  let variants = map (\ x -> (x, makeRegex x)) [
	"agggtaaa|tttaccct",
	"[cgt]gggtaaa|tttaccc[acg]",
	"a[act]ggtaaa|tttacc[agt]t",
	"ag[act]gtaaa|tttac[agt]ct",
	"agg[act]taaa|ttta[agt]cct",
	"aggg[acg]aaa|ttt[cgt]ccct",
	"agggt[cgt]aa|tt[acg]accct",
	"agggta[cgt]a|t[acg]taccct",
	"agggtaa[cgt]|[acg]ttaccct" ]  :: [(String, Regex)]
  let [s1,s2,s3] = map (B.concat . tail) $
                groupBy notHeader $ B.split (BI.c2w '\n') file
      showVars :: (String, Regex) -> String
      showVars (rx,r) = let m2 = match r s2; m3 = match r s3 :: Int in
      	m2 `par` m3 `seq` (rx ++ ' ' : show (m2 + m3))
      results = map showVars variants ++ [
                  "",
                  show $ B.length file,
                  show $ B.length s1 + B.length s2 + B.length s3]
  store <- newEmptyMVar
  let chunks = fragment 20000 s2  -- break into chunks to parallelize, which
                                 -- is possible as our regexes are 1 char long
  s1 `seq` s2 `seq` s3 `seq` (variants `using` parList (evalTuple2 r0 rseq)) `par`
  	forkIO (parallel (map substituteAll chunks) >>= putMVar store)
  	-- do regex substitutions
  mapM_ putStrLn (results `using` parList rdeepseq)
  chunks' <- takeMVar store
  print $ B.length s1 + B.length s3 + chunks'
  where notHeader _ s = B.null s || B.head s /= (BI.c2w '>')

-- Drop in replacement for sequence
parallel :: [IO Int] -> IO Int
parallel actions = do
    vars <- mapM (\ action -> do
        var <- newEmptyMVar
        forkIO $ do
            answer <- action
            putMVar var $! answer
        return var) actions
    foldM (\ !x v -> liftM (+x) (takeMVar v)) 0 vars

fragment :: Int -> B.ByteString -> [B.ByteString]
fragment chunkSize bs = if B.null bs then [] else
    case B.splitAt chunkSize bs of
    	(start, rem) -> start : fragment chunkSize rem

-- Precompile regexes
subRegexes :: [(Regex, B.ByteString)]
subRegexes = flip map subs $ \(pattern, sub) ->
    (makeRegex pattern :: Regex, B.pack (map BI.c2w sub))

extend :: B.ByteString -> IO B.ByteString
extend src = do
	destFP <- BI.mallocByteString (B.length src * 3)
	copyBS src destFP

copyBS :: B.ByteString -> ForeignPtr Word8 -> IO B.ByteString
copyBS (BI.PS srcFP srcOff srcLen) destFP = withForeignPtr srcFP $ \ src0 ->
	withForeignPtr destFP $ \ dest0 -> do
	  copyArray dest0 (src0 +! srcOff) srcLen
	  return (BI.PS destFP 0 srcLen)

substituteAll :: B.ByteString -> IO Int
substituteAll !txt@(BI.PS srcFP srcOff srcLen) = allocaArray (B.length txt * 3) $ \ destP -> do
    destFP <- newForeignPtr_ destP
    withForeignPtr srcFP $ \ srcP -> copyArray destP (srcP `advancePtr` srcOff) srcLen
    let dest = BI.PS destFP 0 srcLen
    allocaArray (B.length txt * 3) $ \ tmp -> do
    	tmpF <- newForeignPtr_ tmp
    	foldM (\ !n sub -> do
       		n' <- substitute_ tmp (BI.PS destFP 0 n) sub
       		copyArray destP tmp n'
       		return n') srcLen subRegexes

(+!) = advancePtr

substitute_ :: Ptr Word8 -> B.ByteString -> (Regex, B.ByteString) -> IO Int
substitute_ !p xs@(BI.PS fp0 i0 l0) (regex, BI.PS fpSub iSub lSub) =
  withForeignPtr fp0 $ \ p00 -> let p0 = p00 +! i0 in withForeignPtr fpSub $ \ pSub -> do
	len <- do
		let go !i !j = do
		      match <- execute regex (unsafeDrop i xs)
		      case match of
			Right (Just arr) -> do
			    let !(!off, !len) = arr `unsafeAt` 0
			    copyArray (p +! j) (p0 +! i) off
			    copyArray (p +! (j + off)) (pSub +! iSub) lSub
			    go (i + off + len) (j + off + lSub)
			_ -> copyArray (p +! j) (p0 +! i) (l0 - i) >> return (j + l0 - i)
		go 0 0
	return len -- destFP now points to the substituted string

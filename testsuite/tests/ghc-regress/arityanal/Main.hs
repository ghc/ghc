{-# OPTIONS -fglasgow-exts -cpp #-}

-- Optimisation problem.  There are two missed opportunities for optimisation in alex_scan_tkn, below.

module Main (main) where

import Data.Char ( ord )
import Control.Monad.ST
import Control.Monad (when)
import Data.STRef
import GHC.ST
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif
alex_base :: AlexAddr
alex_base = AlexA# "\xf8\xff\xfd\xff\x02\x00\x4c\x00"#

alex_table :: AlexAddr
alex_table = AlexA# "\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

alex_check :: AlexAddr
alex_check = AlexA# "\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\x27\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\x2d\x00\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_deflt :: AlexAddr
alex_deflt = AlexA# "\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_accept = listArray (0::Int,3) [[],[],[(AlexAcc 0 (alex_action_0) Nothing Nothing)],[(AlexAcc 1 (alex_action_1) Nothing Nothing)]]
word (_,_,input) len = return (take len input)

scanner str = runAlex str $ do
  let loop i = do tok <- alexScan;
                   if tok == "stopped." || tok == "error."
                         then return i
                         else do let i' = i+1 in i' `seq` loop i'
  loop 0

alexEOF (_,_,"")   = return "stopped."
alexEOF (_,_,rest) = return "error."

main = do
 s <- getContents
 print (scanner s)
alex_action_0 = skip
alex_action_1 = word
-- {-# LINE 1 "GenericTemplate.hs" #-}
--
------------------------------------------------------------------------
-----
-- ALEX TEMPLATE
--
-- (c) Chris Dornan and Simon Marlow 2003

--
------------------------------------------------------------------------
-----
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
         deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

--
------------------------------------------------------------------------
-----
-- The Alex monad
--
-- Compile with -funbox-strict-fields for best results!

data AlexState s = AlexState {
         alex_pos :: !(STRef s AlexPosn),-- position at current input location
         alex_inp :: !(STRef s String),  -- the current input
         alex_chr :: !(STRef s Char),    -- the character before the input
         alex_scd :: !(STRef s Int)      -- the current startcode
    }

type AlexInput = (AlexPosn,Char,String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

runAlex :: String -> Alex a -> a
runAlex input (Alex f)
   = runST (do
       inp_r <- newSTRef input
       chr_r <- newSTRef '\n'
       pos_r <- newSTRef alexStartPos
       scd_r <- newSTRef 0
       f (AlexState {alex_pos = pos_r,
                      alex_inp = inp_r,
                      alex_chr = chr_r,
                      alex_scd = scd_r}))

--TODO include error support
newtype Alex a = Alex { unAlex :: forall s. AlexState s -> ST s a }

instance Monad Alex where
  (Alex m) >>= k  = Alex (\s -> m s >>= \a -> unAlex (k a) s)
  return a = Alex (\s -> return a)

alexGetChar :: Alex (Maybe Char)
alexGetChar = Alex (\st@AlexState{ alex_inp=inp_r,
                                     alex_chr=chr_r,
                                     alex_pos=pos_r } -> do
  inp <- readSTRef inp_r
  pos <- readSTRef pos_r
  case inp of
     []    -> return Nothing
     (c:s) -> do  writeSTRef inp_r s
                  writeSTRef chr_r c
                  let p' = alexMove pos c
                  p' `seq` writeSTRef pos_r p'
                  return (Just c)
  )

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex (\s@AlexState{alex_pos=pos_r,alex_chr=chr_r,alex_inp=inp_r} -> do
     inp <- readSTRef inp_r
     chr <- readSTRef chr_r
     pos <- readSTRef pos_r
     return (pos,chr,inp)
 )

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,chr,inp)
 = Alex (\s@AlexState{alex_pos=pos_r,alex_chr=chr_r,alex_inp=inp_r} -> do
    writeSTRef inp_r inp
    writeSTRef pos_r pos
    writeSTRef chr_r chr
  )

alexGetStartCode :: Alex Int
alexGetStartCode = Alex (\s@AlexState{alex_scd=scd_r} -> do
  readSTRef scd_r)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex (\s@AlexState{alex_scd=scd_r} -> do
  writeSTRef scd_r sc)

--
-----------------------------------------------------------------------------
-- Useful token actions


-- just ignore this token and scan another one
skip input len = alexScan

-- ignore this token, but set the start code to a new value
begin code input len = do alexSetStartCode code; alexScan

-- perform an action for this token, and set the start code to a new value
(token `andBegin` code) input len = do alexSetStartCode code; token input len

--
-----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

-- {-# LINE 144 "GenericTemplate.hs" #-}

data AlexAddr = AlexA# Addr#

{-# INLINE alexIndexShortOffAddr #-}
alexIndexShortOffAddr (AlexA# arr) off =
#if __GLASGOW_HASKELL__ > 500
         narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
         intToInt16# i
#else
         (i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
         i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
         i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
         high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
         low  = int2Word# (ord# (indexCharOffAddr# arr off'))
         off' = off *# 2#


--
-----------------------------------------------------------------------------
-- Main lexing routines



-- alexScan :: some a . Alex a
alexScan = do
  (I# (startcode)) <- alexGetStartCode  -- the startcode is the initial state
  cur_input <- alexGetInput
  let c = alexInputPrevChar cur_input
  c `seq` do
  r <- alex_scan_tkn c 0# startcode AlexNone
  case r of
    AlexNone ->



         alexEOF cur_input
    AlexLastAcc k input len -> do



         alexSetInput input
         k cur_input len

-- {-# LINE 221 "GenericTemplate.hs" #-}


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn lc len (-1#) last_acc = return last_acc
alex_scan_tkn lc len s last_acc = do
  new_acc <- check_accs s
  c <- alexGetChar
  let {-# INLINE [0] join #-}
      -- This is a *hack*, the compiler doesn't eliminate the Maybe return
      -- from alexGetChar unless we extract this join point and inline
      -- it later.
      join c' =



         alex_scan_tkn lc
                 (len +# 1#) s' new_acc
         where
                 base   = alexIndexShortOffAddr alex_base s
                 (I# (ord_c)) = ord c'
                 offset = (base +# ord_c)
                 check  = alexIndexShortOffAddr alex_check offset

                 s' =
                      if (offset >=# 0#) && (check ==# ord_c)
                         then alexIndexShortOffAddr alex_table offset
                         else alexIndexShortOffAddr alex_deflt s
  case c of
    Nothing -> return new_acc    -- end of input
    Just c' -> join c'
   where
         -- OPTIMISATION PROBLEM.  We need to eta-expand
         -- check_accs and check_accs1.  This needs a simple
        -- one-shot analysis of some kind, but note that
        -- check_accs1 is recursive.
         check_accs s = check_accs1 (alex_accept `unsafeAt` (I# (s)))
         check_accs1 accs =
           case accs of
                 [] -> return last_acc
                 (AlexAcc _ a lctx rctx : rest) ->

                   case lctx of
                     Nothing  -> check_rctx
                     Just arr | arr!lc    -> check_rctx
                              | otherwise -> check_accs1 rest
                   where

                     ok = do inp <- alexGetInput
                             return (AlexLastAcc a inp (I# (len)))

                     check_rctx =
                         case rctx of
                            Nothing -> ok
                            Just (I# (sn)) -> do
                               inp <- alexGetInput
                               let c = alexInputPrevChar inp
                               c `seq` do
                               acc <- alex_scan_tkn c 0# sn AlexNone
                               alexSetInput inp
                               case acc of
                                 AlexNone      -> check_accs1 rest
                                 AlexLastAcc{} -> ok
                                 -- TODO: there's no need to find the longest
                                 -- match when checking the right context, just
                                 -- the first match will do.

data AlexLastAcc a = AlexNone | AlexLastAcc a !AlexInput !Int

data AlexAcc a = AlexAcc Int a (Maybe (Array Char Bool)) (Maybe Int)

{-# OPTIONS -fglasgow-exts -cpp #-}
-- parser produced by Happy Version 1.11

module Parser where

import Char
import GlaExts
import Array
import IO
import IOExts

data HappyAbsSyn 
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 (Int)

happyActOffsets :: Addr
happyActOffsets = A# "\x0a\x00\x0a\x00\x00\x00\xff\xff\x0a\x00\x0a\x00\x08\x00\x07\x00\x00\x00"#

happyGotoOffsets :: Addr
happyGotoOffsets = A# "\x06\x00\x00\x00\x00\x00\x00\x00\x05\x00\x04\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: Addr
happyDefActions = A# "\x00\x00\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\xfc\xff\xfd\xff"#

happyCheck :: Addr
happyCheck = A# "\xff\xff\x02\x00\x03\x00\x04\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x02\x00\x02\x00\x01\x00\xff\xff\xff\xff\xff\xff"#

happyTable :: Addr
happyTable = A# "\x00\x00\x05\x00\x06\x00\xff\xff\x06\x00\x07\x00\x03\x00\x00\x00\x00\x00\x00\x00\x05\x00\x03\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 3) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3)
	]

happy_n_terms = 5 :: Int
happy_n_nonterms = 1 :: Int

happyReduce_1 = happySpecReduce_1 0# happyReduction_1
happyReduction_1 _
	 =  HappyAbsSyn4
		 (1
	)

happyReduce_2 = happySpecReduce_3 0# happyReduction_2
happyReduction_2 _
	_
	_
	 =  HappyAbsSyn4
		 (2
	)

happyReduce_3 = happySpecReduce_3 0# happyReduction_3
happyReduction_3 _
	_
	_
	 =  HappyAbsSyn4
		 (3
	)

happyNewToken action sts stk [] =
	happyDoAction 4# (error "reading EOF!") action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	Tid -> cont 1#;
	Tgreater -> cont 2#;
	Tand -> cont 3#;
	}

happyThen = \m k -> k m
happyReturn = \a -> a
happyThen1 = happyThen
happyReturn1 = \a tks -> a

parser tks = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

data Token = Tid | Tgreater | Tand
             deriving Show

happyError = error "parse error"

lexer :: String -> [Token]
lexer = l
      where l ""         = []
            l ('\n':cs)  = l cs 
            l ('a':'n':'d':cs) = Tand : l cs            
            l (c:cs)    
                | isSpace c = l cs 
                | isAlpha c = let (_,rs) = span isAlpha (c:cs)
                              in Tid : l rs
            l ('>':cs)      = Tgreater : l cs
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id: Parser.hs,v 1.1 2002/01/31 13:46:38 simonmar Exp $













{-# LINE 27 "GenericTemplate.hs" #-}



data Happy_IntList = HappyCons Int# Happy_IntList































happyTrace string expr = unsafePerformIO $ do
    hPutStr stderr string
    return expr




infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = (happyTcHack j 
				                  (happyTcHack st))
					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= (happyTrace ("state: " ++ show (I# (st)) ++ 
		      ",\ttoken: " ++ show (I# (i)) ++
		      ",\taction: ")) $
	  case action of
		0#		  -> (happyTrace ("fail.\n")) $
				     happyFail i tk st
		-1# 	  -> (happyTrace ("accept.\n")) $
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> (happyTrace ("reduce (rule " ++ show rule
						 ++ ")")) $
				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> (happyTrace ("shift, enter state "
						 ++ show (I# (new_state))
						 ++ "\n")) $
				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st







indexShortOffAddr (A# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 153 "GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = happyGoto nt j tk st sts (fn v1 `HappyStk` stk')

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = happyGoto nt j tk st sts (fn v1 v2 `HappyStk` stk')

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = happyGoto nt j tk st sts (fn v1 v2 v3 `HappyStk` stk')

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk = happyGoto nt j tk st1 sts1 (fn stk)
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   (happyTrace (", goto state " ++ show (I# (new_state)) ++ "\n")) $
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (HappyErrorToken (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

{-# OPTIONS -fglasgow-exts #-}
{-
Bug report from Jon Mountjoy:

While playing with Happy I managed to generate a Haskell program
which compiles fine under ghc but not under Hugs.  I don't know which
one is the culprit....

In Hugs(January 1998), one gets

     ERROR "hugs.hs" (line 32): Unresolved top-level overloading
     *** Binding             : happyReduce_1
     *** Outstanding context : Functor b

where line 32 is the one marked -- ##

It compiles in ghc-3.00.  Changing very small things, like the
line marked ---**** to 
      action_0 (6) = happyShift action_0        ---****

then makes ghc produce a similar message:

   hugs.hs:37:
   Cannot resolve the ambiguous context (Functor a1Ab)
   `Functor a1Ab' arising from use of `reduction', at hugs.hs:37
-}

module ShouldSucceed where

data HappyAbsSyn t1 t2 t3
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn1 t1
	| HappyAbsSyn2 t2
	| HappyAbsSyn3 t3

-- action_0 (6) = happyShift action_3        --- *****
-- action_0 (1) = happyGoto action_1
-- action_0 (2) = happyGoto action_2
action_0 _ = happyFail

-- action_1 (7) = happyAccept
-- action_1 _ = happyFail

-- action_2 _ = happyReduce_1

-- action_3 (5) = happyShift action_4
-- action_3 _ = happyFail

-- action_4 (4) = happyShift action_6
-- action_4 (3) = happyGoto action_5
-- action_4 _ = happyFail

-- action_5 _ = happyReduce_2

-- action_6 _ = happyReduce_3

{-
happyReduce_1 = happySpecReduce_1 1 reduction where {    -- ##
  reduction
	(HappyAbsSyn2  happy_var_1)
	 =  HappyAbsSyn1
		 (\p -> let q = map (\(x,y) -> (x,y p)) happy_var_1 in  (10.1))
  ;
  reduction _  = notHappyAtAll }

happyReduce_2 = happySpecReduce_3 2 reduction where {
  reduction
	(HappyAbsSyn3  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn2
		 ([(happy_var_1,happy_var_3)]);
  reduction _ _ _  = notHappyAtAll }

happyReduce_3 = happySpecReduce_1 3 reduction where {
  reduction
	(HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn3
		 (\p -> happy_var_1);
  reduction _  = notHappyAtAll }
-}

happyNewToken action sts stk [] =
	action 7 7 (error "reading EOF!") (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 4;
	TokenEq -> cont 5;
	TokenVar happy_dollar_dollar -> cont 6;
	}

-- happyThen = \m k -> k m
-- happyReturn = \a tks -> a

myparser :: forall t. [Token] -> t -> Double
myparser = happyParse



happyError ::[Token] -> a
happyError _ = error "Parse error\n"

--Here are our tokens
data Token  = 
              TokenInt Int
            | TokenVar String
            | TokenEq
            deriving Show

main = print (myparser [] [])
-- $Id: tc095.hs,v 1.2 2005/05/16 12:53:22 simonpj Exp $

{-
	The stack is in the following order throughout the parse:

	i	current token number
	j	another copy of this to avoid messing with the stack
	tk	current token semantic value
	st	current state
	sts	state stack
	stk	semantic stack
-}

-----------------------------------------------------------------------------

happyParse = happyNewToken action_0 [] []

-- All this HappyState stuff is simply because we can't have recursive
-- types in Haskell without an intervening data structure.

data HappyState b c = HappyState
        (Int ->                         -- token number
         Int ->                         -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)

-----------------------------------------------------------------------------
-- Accepting the parse
{-

happyAccept j tk st sts [ HappyAbsSyn1 ans ] = happyReturn ans
happyAccept j tk st sts _                    = notHappyAtAll

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (-1) tk st sts stk@(HappyErrorToken i : _) =
--     _trace "shifting the error token" $
     new_state i i tk (HappyState new_state) (st:sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (st:sts) (HappyTerminal tk:stk)

-----------------------------------------------------------------------------
-- Reducing

-- happyReduce is specialised for the common cases.

-- don't allow reductions when we're in error recovery, because this can
-- lead to an infinite loop.
happySpecReduce_0 i fn (-1) tk _ sts stk
     = case sts of
	st@(HappyState action):sts -> action (-1) (-1) tk st sts stk
	_ -> happyError
happySpecReduce_0 i fn j tk st@(HappyState action) sts stk
     = action i j tk st (st:sts) (fn : stk)

happySpecReduce_1 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_1 i fn j tk _ sts@(st@(HappyState action):_) (v1:stk')
     = action i j tk st sts (fn v1 : stk')
happySpecReduce_1 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_2 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_2 i fn j tk _ (_:sts@(st@(HappyState action):_)) (v1:v2:stk')
     = action i j tk st sts (fn v1 v2 : stk')
happySpecReduce_2 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_3 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_3 i fn j tk _ (_:_:sts@(st@(HappyState action):_)) 
	(v1:v2:v3:stk')
     = action i j tk st sts (fn v1 v2 v3 : stk')
happySpecReduce_3 _ _ _ _ _ _ _
     = notHappyAtAll

happyReduce k i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happyReduce k i fn j tk st sts stk = action i j tk st' sts' (fn stk)
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)

happyMonadReduce k i c fn (-1) tk _ sts stk
      = case sts of
	     (st@(HappyState action):sts) -> action (-1) (-1) tk st sts stk
	     [] -> happyError
happyMonadReduce k i c fn j tk st sts stk =
	happyThen (fn stk) (\r -> action i j tk st' sts' (c r : stk'))
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)
	     stk' = drop (k::Int) stk
-}
-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto action j tk st = action j j tk (HappyState action)

-----------------------------------------------------------------------------
-- Error recovery (-1 is the error token)

-- fail if we are in recovery and no more states to discard
happyFail  (-1) tk st' [] stk = happyError

-- discard a state
happyFail  (-1) tk st' (st@(HappyState action):sts) stk =
--	_trace "discarding state" $
	action (-1) (-1) tk st sts stk

-- Enter error recovery: generate an error token,
-- 			 save the old token and carry on.

-- we push the error token on the stack in anticipation of a shift,
-- and also because this is a convenient place to store the saved token.

happyFail  i tk st@(HappyState action) sts stk =
--	_trace "entering error recovery" $
	action (-1) (-1) tk st sts (HappyErrorToken i : stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-- end of Happy Template.

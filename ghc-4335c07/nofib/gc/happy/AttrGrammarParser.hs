module AttrGrammarParser (agParser) where
import ParseMonad
import AttrGrammar

-- parser produced by Happy Version 1.16

data HappyAbsSyn 
	= HappyTerminal AgToken
	| HappyErrorToken Int
	| HappyAbsSyn4 ([AgRule])
	| HappyAbsSyn6 (AgRule)
	| HappyAbsSyn7 ([AgToken])

type HappyReduction m = 
	   Int 
	-> (AgToken)
	-> HappyState (AgToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (AgToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47 :: () => Int -> HappyReduction (P)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23 :: () => HappyReduction (P)

action_0 (13) = happyShift action_4
action_0 (14) = happyShift action_5
action_0 (15) = happyShift action_6
action_0 (16) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_4

action_1 (13) = happyShift action_4
action_1 (14) = happyShift action_5
action_1 (15) = happyShift action_6
action_1 (16) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (11) = happyShift action_19
action_3 _ = happyReduce_3

action_4 (9) = happyShift action_13
action_4 (12) = happyShift action_14
action_4 (14) = happyShift action_15
action_4 (15) = happyShift action_16
action_4 (16) = happyShift action_17
action_4 (17) = happyShift action_18
action_4 (7) = happyGoto action_12
action_4 _ = happyReduce_15

action_5 (12) = happyShift action_11
action_5 _ = happyFail

action_6 (12) = happyShift action_10
action_6 _ = happyFail

action_7 (12) = happyShift action_9
action_7 _ = happyFail

action_8 (18) = happyAccept
action_8 _ = happyFail

action_9 (9) = happyShift action_13
action_9 (12) = happyShift action_14
action_9 (14) = happyShift action_15
action_9 (15) = happyShift action_16
action_9 (16) = happyShift action_17
action_9 (17) = happyShift action_18
action_9 (7) = happyGoto action_36
action_9 _ = happyReduce_15

action_10 (9) = happyShift action_13
action_10 (12) = happyShift action_14
action_10 (14) = happyShift action_15
action_10 (15) = happyShift action_16
action_10 (16) = happyShift action_17
action_10 (17) = happyShift action_18
action_10 (7) = happyGoto action_35
action_10 _ = happyReduce_15

action_11 (9) = happyShift action_13
action_11 (12) = happyShift action_14
action_11 (14) = happyShift action_15
action_11 (15) = happyShift action_16
action_11 (16) = happyShift action_17
action_11 (17) = happyShift action_18
action_11 (7) = happyGoto action_34
action_11 _ = happyReduce_15

action_12 _ = happyReduce_8

action_13 (9) = happyShift action_27
action_13 (11) = happyShift action_28
action_13 (12) = happyShift action_29
action_13 (14) = happyShift action_30
action_13 (15) = happyShift action_31
action_13 (16) = happyShift action_32
action_13 (17) = happyShift action_33
action_13 (8) = happyGoto action_26
action_13 _ = happyReduce_23

action_14 (9) = happyShift action_13
action_14 (12) = happyShift action_14
action_14 (14) = happyShift action_15
action_14 (15) = happyShift action_16
action_14 (16) = happyShift action_17
action_14 (17) = happyShift action_18
action_14 (7) = happyGoto action_25
action_14 _ = happyReduce_15

action_15 (9) = happyShift action_13
action_15 (12) = happyShift action_14
action_15 (14) = happyShift action_15
action_15 (15) = happyShift action_16
action_15 (16) = happyShift action_17
action_15 (17) = happyShift action_18
action_15 (7) = happyGoto action_24
action_15 _ = happyReduce_15

action_16 (9) = happyShift action_13
action_16 (12) = happyShift action_14
action_16 (14) = happyShift action_15
action_16 (15) = happyShift action_16
action_16 (16) = happyShift action_17
action_16 (17) = happyShift action_18
action_16 (7) = happyGoto action_23
action_16 _ = happyReduce_15

action_17 (9) = happyShift action_13
action_17 (12) = happyShift action_14
action_17 (14) = happyShift action_15
action_17 (15) = happyShift action_16
action_17 (16) = happyShift action_17
action_17 (17) = happyShift action_18
action_17 (7) = happyGoto action_22
action_17 _ = happyReduce_15

action_18 (9) = happyShift action_13
action_18 (12) = happyShift action_14
action_18 (14) = happyShift action_15
action_18 (15) = happyShift action_16
action_18 (16) = happyShift action_17
action_18 (17) = happyShift action_18
action_18 (7) = happyGoto action_21
action_18 _ = happyReduce_15

action_19 (13) = happyShift action_4
action_19 (14) = happyShift action_5
action_19 (15) = happyShift action_6
action_19 (16) = happyShift action_7
action_19 (5) = happyGoto action_20
action_19 (6) = happyGoto action_3
action_19 _ = happyReduce_4

action_20 _ = happyReduce_2

action_21 _ = happyReduce_14

action_22 _ = happyReduce_13

action_23 _ = happyReduce_12

action_24 _ = happyReduce_11

action_25 _ = happyReduce_10

action_26 (10) = happyShift action_44
action_26 _ = happyFail

action_27 (9) = happyShift action_27
action_27 (11) = happyShift action_28
action_27 (12) = happyShift action_29
action_27 (14) = happyShift action_30
action_27 (15) = happyShift action_31
action_27 (16) = happyShift action_32
action_27 (17) = happyShift action_33
action_27 (8) = happyGoto action_43
action_27 _ = happyReduce_23

action_28 (9) = happyShift action_27
action_28 (11) = happyShift action_28
action_28 (12) = happyShift action_29
action_28 (14) = happyShift action_30
action_28 (15) = happyShift action_31
action_28 (16) = happyShift action_32
action_28 (17) = happyShift action_33
action_28 (8) = happyGoto action_42
action_28 _ = happyReduce_23

action_29 (9) = happyShift action_27
action_29 (11) = happyShift action_28
action_29 (12) = happyShift action_29
action_29 (14) = happyShift action_30
action_29 (15) = happyShift action_31
action_29 (16) = happyShift action_32
action_29 (17) = happyShift action_33
action_29 (8) = happyGoto action_41
action_29 _ = happyReduce_23

action_30 (9) = happyShift action_27
action_30 (11) = happyShift action_28
action_30 (12) = happyShift action_29
action_30 (14) = happyShift action_30
action_30 (15) = happyShift action_31
action_30 (16) = happyShift action_32
action_30 (17) = happyShift action_33
action_30 (8) = happyGoto action_40
action_30 _ = happyReduce_23

action_31 (9) = happyShift action_27
action_31 (11) = happyShift action_28
action_31 (12) = happyShift action_29
action_31 (14) = happyShift action_30
action_31 (15) = happyShift action_31
action_31 (16) = happyShift action_32
action_31 (17) = happyShift action_33
action_31 (8) = happyGoto action_39
action_31 _ = happyReduce_23

action_32 (9) = happyShift action_13
action_32 (12) = happyShift action_14
action_32 (14) = happyShift action_15
action_32 (15) = happyShift action_16
action_32 (16) = happyShift action_17
action_32 (17) = happyShift action_18
action_32 (7) = happyGoto action_38
action_32 _ = happyReduce_15

action_33 (9) = happyShift action_27
action_33 (11) = happyShift action_28
action_33 (12) = happyShift action_29
action_33 (14) = happyShift action_30
action_33 (15) = happyShift action_31
action_33 (16) = happyShift action_32
action_33 (17) = happyShift action_33
action_33 (8) = happyGoto action_37
action_33 _ = happyReduce_23

action_34 _ = happyReduce_5

action_35 _ = happyReduce_6

action_36 _ = happyReduce_7

action_37 _ = happyReduce_22

action_38 _ = happyReduce_21

action_39 _ = happyReduce_20

action_40 _ = happyReduce_19

action_41 _ = happyReduce_17

action_42 _ = happyReduce_18

action_43 (10) = happyShift action_46
action_43 _ = happyFail

action_44 (9) = happyShift action_13
action_44 (12) = happyShift action_14
action_44 (14) = happyShift action_15
action_44 (15) = happyShift action_16
action_44 (16) = happyShift action_17
action_44 (17) = happyShift action_18
action_44 (7) = happyGoto action_45
action_44 _ = happyReduce_15

action_45 _ = happyReduce_9

action_46 (9) = happyShift action_27
action_46 (11) = happyShift action_28
action_46 (12) = happyShift action_29
action_46 (14) = happyShift action_30
action_46 (15) = happyShift action_31
action_46 (16) = happyShift action_32
action_46 (17) = happyShift action_33
action_46 (8) = happyGoto action_47
action_46 _ = happyReduce_23

action_47 _ = happyReduce_16

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : []
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  5 happyReduction_4
happyReduction_4  =  HappyAbsSyn4
		 ([]
	)

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (SelfAssign (selfRefVal happy_var_1) happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (SubAssign (subRefVal happy_var_1) happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (RightmostAssign (rightRefVal happy_var_1) happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Conditional happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 7 happyReduction_9
happyReduction_9 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ([happy_var_1] ++ happy_var_2 ++ [happy_var_3] ++ happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_2  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  7 happyReduction_15
happyReduction_15  =  HappyAbsSyn7
		 ([]
	)

happyReduce_16 = happyReduce 4 8 happyReduction_16
happyReduction_16 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ([happy_var_1] ++ happy_var_2 ++ [happy_var_3] ++ happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  8 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  8 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  8 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  8 happyReduction_20
happyReduction_20 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  8 happyReduction_21
happyReduction_21 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  8 happyReduction_22
happyReduction_22 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  8 happyReduction_23
happyReduction_23  =  HappyAbsSyn7
		 ([]
	)

happyNewToken action sts stk
	= agLexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	AgTok_EOF -> action 18 18 tk (HappyState action) sts stk;
	AgTok_LBrace -> cont 9;
	AgTok_RBrace -> cont 10;
	AgTok_Semicolon -> cont 11;
	AgTok_Eq -> cont 12;
	AgTok_Where -> cont 13;
	AgTok_SelfRef _ -> cont 14;
	AgTok_SubRef _ -> cont 15;
	AgTok_RightmostRef _ -> cont 16;
	AgTok_Unknown _ -> cont 17;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => AgToken -> P a
happyError' tk = (\token -> happyError) tk

agParser = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: P a
happyError = fail ("Parse error\n")
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "GenericTemplate.hs" #-}








{-# LINE 49 "GenericTemplate.hs" #-}

{-# LINE 59 "GenericTemplate.hs" #-}

{-# LINE 68 "GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "GenericTemplate.hs" #-}
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

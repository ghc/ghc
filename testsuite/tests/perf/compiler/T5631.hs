{-# OPTIONS_GHC -w #-}
import Control.Monad(when)
import System.Exit

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 t51 t52 t53 t54 t55 t56 t57 t58 t59 t60 t61
	= HappyTerminal (Char)
	| HappyErrorToken Int
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44
	| HappyAbsSyn45 t45
	| HappyAbsSyn46 t46
	| HappyAbsSyn47 t47
	| HappyAbsSyn48 t48
	| HappyAbsSyn49 t49
	| HappyAbsSyn50 t50
	| HappyAbsSyn51 t51
	| HappyAbsSyn52 t52
	| HappyAbsSyn53 t53
	| HappyAbsSyn54 t54
	| HappyAbsSyn55 t55
	| HappyAbsSyn56 t56
	| HappyAbsSyn57 t57
	| HappyAbsSyn58 t58
	| HappyAbsSyn59 t59
	| HappyAbsSyn60 t60
	| HappyAbsSyn61 t61

action_0 (62) = happyShift action_22
action_0 (14) = happyGoto action_66
action_0 (17) = happyGoto action_12
action_0 (18) = happyGoto action_13
action_0 (19) = happyGoto action_14
action_0 (31) = happyGoto action_15
action_0 (32) = happyGoto action_16
action_0 (33) = happyGoto action_17
action_0 (35) = happyGoto action_18
action_0 (43) = happyGoto action_19
action_0 (44) = happyGoto action_20
action_0 (45) = happyGoto action_21
action_0 _ = happyFail

action_1 (62) = happyShift action_22
action_1 (15) = happyGoto action_63
action_1 (34) = happyGoto action_64
action_1 (35) = happyGoto action_65
action_1 _ = happyReduce_32

action_2 (62) = happyShift action_62
action_2 (16) = happyGoto action_56
action_2 (25) = happyGoto action_57
action_2 (36) = happyGoto action_58
action_2 (46) = happyGoto action_59
action_2 (53) = happyGoto action_60
action_2 (60) = happyGoto action_61
action_2 _ = happyReduce_35

action_3 (62) = happyShift action_22
action_3 (17) = happyGoto action_55
action_3 (18) = happyGoto action_13
action_3 (19) = happyGoto action_14
action_3 (32) = happyGoto action_16
action_3 (33) = happyGoto action_17
action_3 (35) = happyGoto action_18
action_3 (44) = happyGoto action_20
action_3 (45) = happyGoto action_21
action_3 _ = happyFail

action_4 (62) = happyShift action_22
action_4 (18) = happyGoto action_54
action_4 (19) = happyGoto action_14
action_4 (33) = happyGoto action_17
action_4 (35) = happyGoto action_18
action_4 (45) = happyGoto action_21
action_4 _ = happyFail

action_5 (62) = happyShift action_22
action_5 (19) = happyGoto action_53
action_5 (35) = happyGoto action_18
action_5 _ = happyFail

action_6 (62) = happyShift action_52
action_6 (20) = happyGoto action_47
action_6 (26) = happyGoto action_48
action_6 (38) = happyGoto action_49
action_6 (47) = happyGoto action_50
action_6 (55) = happyGoto action_51
action_6 _ = happyFail

action_7 (62) = happyShift action_46
action_7 (21) = happyGoto action_41
action_7 (27) = happyGoto action_42
action_7 (39) = happyGoto action_43
action_7 (48) = happyGoto action_44
action_7 (56) = happyGoto action_45
action_7 _ = happyFail

action_8 (62) = happyShift action_40
action_8 (22) = happyGoto action_35
action_8 (28) = happyGoto action_36
action_8 (40) = happyGoto action_37
action_8 (49) = happyGoto action_38
action_8 (57) = happyGoto action_39
action_8 _ = happyFail

action_9 (62) = happyShift action_34
action_9 (23) = happyGoto action_29
action_9 (29) = happyGoto action_30
action_9 (41) = happyGoto action_31
action_9 (50) = happyGoto action_32
action_9 (58) = happyGoto action_33
action_9 _ = happyFail

action_10 (62) = happyShift action_28
action_10 (24) = happyGoto action_23
action_10 (30) = happyGoto action_24
action_10 (42) = happyGoto action_25
action_10 (51) = happyGoto action_26
action_10 (59) = happyGoto action_27
action_10 _ = happyFail

action_11 (62) = happyShift action_22
action_11 (17) = happyGoto action_12
action_11 (18) = happyGoto action_13
action_11 (19) = happyGoto action_14
action_11 (31) = happyGoto action_15
action_11 (32) = happyGoto action_16
action_11 (33) = happyGoto action_17
action_11 (35) = happyGoto action_18
action_11 (43) = happyGoto action_19
action_11 (44) = happyGoto action_20
action_11 (45) = happyGoto action_21
action_11 _ = happyFail

action_12 _ = happyReduce_43

action_13 _ = happyReduce_45

action_14 _ = happyReduce_47

action_15 _ = happyReduce_11

action_16 _ = happyReduce_14

action_17 _ = happyReduce_15

action_18 _ = happyReduce_16

action_19 (62) = happyShift action_22
action_19 (17) = happyGoto action_86
action_19 (18) = happyGoto action_13
action_19 (19) = happyGoto action_14
action_19 (32) = happyGoto action_16
action_19 (33) = happyGoto action_17
action_19 (35) = happyGoto action_18
action_19 (44) = happyGoto action_20
action_19 (45) = happyGoto action_21
action_19 _ = happyReduce_28

action_20 (62) = happyShift action_22
action_20 (18) = happyGoto action_85
action_20 (19) = happyGoto action_14
action_20 (33) = happyGoto action_17
action_20 (35) = happyGoto action_18
action_20 (45) = happyGoto action_21
action_20 _ = happyReduce_29

action_21 (62) = happyShift action_22
action_21 (19) = happyGoto action_84
action_21 (35) = happyGoto action_18
action_21 _ = happyReduce_30

action_22 (63) = happyShift action_83
action_22 (37) = happyGoto action_79
action_22 (52) = happyGoto action_80
action_22 (54) = happyGoto action_81
action_22 (61) = happyGoto action_82
action_22 _ = happyReduce_37

action_23 (64) = happyAccept
action_23 _ = happyFail

action_24 _ = happyReduce_21

action_25 _ = happyReduce_27

action_26 _ = happyReduce_66

action_27 (62) = happyShift action_28
action_27 (51) = happyGoto action_78
action_27 _ = happyReduce_42

action_28 (62) = happyShift action_22
action_28 (19) = happyGoto action_77
action_28 (35) = happyGoto action_18
action_28 _ = happyFail

action_29 (64) = happyAccept
action_29 _ = happyFail

action_30 _ = happyReduce_20

action_31 _ = happyReduce_26

action_32 _ = happyReduce_64

action_33 (62) = happyShift action_34
action_33 (50) = happyGoto action_76
action_33 _ = happyReduce_41

action_34 (62) = happyShift action_22
action_34 (18) = happyGoto action_75
action_34 (19) = happyGoto action_14
action_34 (33) = happyGoto action_17
action_34 (35) = happyGoto action_18
action_34 (45) = happyGoto action_21
action_34 _ = happyFail

action_35 (64) = happyAccept
action_35 _ = happyFail

action_36 _ = happyReduce_19

action_37 _ = happyReduce_25

action_38 _ = happyReduce_62

action_39 (62) = happyShift action_40
action_39 (49) = happyGoto action_74
action_39 _ = happyReduce_40

action_40 (62) = happyShift action_22
action_40 (17) = happyGoto action_73
action_40 (18) = happyGoto action_13
action_40 (19) = happyGoto action_14
action_40 (32) = happyGoto action_16
action_40 (33) = happyGoto action_17
action_40 (35) = happyGoto action_18
action_40 (44) = happyGoto action_20
action_40 (45) = happyGoto action_21
action_40 _ = happyFail

action_41 (64) = happyAccept
action_41 _ = happyFail

action_42 _ = happyReduce_18

action_43 _ = happyReduce_24

action_44 _ = happyReduce_60

action_45 (62) = happyShift action_46
action_45 (48) = happyGoto action_72
action_45 _ = happyReduce_39

action_46 (62) = happyShift action_62
action_46 (16) = happyGoto action_71
action_46 (25) = happyGoto action_57
action_46 (36) = happyGoto action_58
action_46 (46) = happyGoto action_59
action_46 (53) = happyGoto action_60
action_46 (60) = happyGoto action_61
action_46 _ = happyReduce_35

action_47 (64) = happyAccept
action_47 _ = happyFail

action_48 _ = happyReduce_17

action_49 _ = happyReduce_23

action_50 _ = happyReduce_58

action_51 (62) = happyShift action_52
action_51 (47) = happyGoto action_70
action_51 _ = happyReduce_38

action_52 (62) = happyShift action_22
action_52 (15) = happyGoto action_69
action_52 (34) = happyGoto action_64
action_52 (35) = happyGoto action_65
action_52 _ = happyReduce_32

action_53 (64) = happyAccept
action_53 _ = happyFail

action_54 (64) = happyAccept
action_54 _ = happyFail

action_55 (64) = happyAccept
action_55 _ = happyFail

action_56 (64) = happyAccept
action_56 _ = happyFail

action_57 _ = happyReduce_13

action_58 _ = happyReduce_22

action_59 _ = happyReduce_68

action_60 _ = happyReduce_34

action_61 (62) = happyShift action_62
action_61 (46) = happyGoto action_68
action_61 _ = happyReduce_56

action_62 (63) = happyShift action_67
action_62 _ = happyFail

action_63 (64) = happyAccept
action_63 _ = happyFail

action_64 _ = happyReduce_12

action_65 _ = happyReduce_31

action_66 (64) = happyAccept
action_66 _ = happyFail

action_67 _ = happyReduce_49

action_68 _ = happyReduce_69

action_69 _ = happyReduce_50

action_70 _ = happyReduce_59

action_71 _ = happyReduce_51

action_72 _ = happyReduce_61

action_73 _ = happyReduce_52

action_74 _ = happyReduce_63

action_75 _ = happyReduce_53

action_76 _ = happyReduce_65

action_77 _ = happyReduce_54

action_78 _ = happyReduce_67

action_79 _ = happyReduce_33

action_80 _ = happyReduce_70

action_81 _ = happyReduce_36

action_82 (63) = happyShift action_83
action_82 (52) = happyGoto action_88
action_82 _ = happyReduce_57

action_83 (62) = happyShift action_87
action_83 _ = happyFail

action_84 _ = happyReduce_48

action_85 _ = happyReduce_46

action_86 _ = happyReduce_44

action_87 _ = happyReduce_55

action_88 _ = happyReduce_71

happyReduce_11 = happySpecReduce_1  14 happyReduction_11
happyReduction_11 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  15 happyReduction_12
happyReduction_12 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  18 happyReduction_15
happyReduction_15 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  19 happyReduction_16
happyReduction_16 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  20 happyReduction_17
happyReduction_17 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  21 happyReduction_18
happyReduction_18 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  22 happyReduction_19
happyReduction_19 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  23 happyReduction_20
happyReduction_20 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  24 happyReduction_21
happyReduction_21 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  25 happyReduction_22
happyReduction_22 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  26 happyReduction_23
happyReduction_23 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  27 happyReduction_24
happyReduction_24 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  28 happyReduction_25
happyReduction_25 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  29 happyReduction_26
happyReduction_26 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  30 happyReduction_27
happyReduction_27 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  31 happyReduction_28
happyReduction_28 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn31
		 (reverse happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  32 happyReduction_29
happyReduction_29 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn32
		 (reverse happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  33 happyReduction_30
happyReduction_30 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn33
		 (reverse happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  34 happyReduction_31
happyReduction_31 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  34 happyReduction_32
happyReduction_32  =  HappyAbsSyn34
		 ([]
	)

happyReduce_33 = happySpecReduce_2  35 happyReduction_33
happyReduction_33 (HappyAbsSyn37  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1 : happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  36 happyReduction_34
happyReduction_34 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  36 happyReduction_35
happyReduction_35  =  HappyAbsSyn36
		 ([]
	)

happyReduce_36 = happySpecReduce_1  37 happyReduction_36
happyReduction_36 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  37 happyReduction_37
happyReduction_37  =  HappyAbsSyn37
		 ([]
	)

happyReduce_38 = happySpecReduce_1  38 happyReduction_38
happyReduction_38 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn38
		 (reverse happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  39 happyReduction_39
happyReduction_39 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn39
		 (reverse happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  40 happyReduction_40
happyReduction_40 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn40
		 (reverse happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  41 happyReduction_41
happyReduction_41 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn41
		 (reverse happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  42 happyReduction_42
happyReduction_42 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn42
		 (reverse happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  43 happyReduction_43
happyReduction_43 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn43
		 ([happy_var_1]
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  43 happyReduction_44
happyReduction_44 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_2 : happy_var_1
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  44 happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn44
		 ([happy_var_1]
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  44 happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_2 : happy_var_1
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  45 happyReduction_47
happyReduction_47 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn45
		 ([happy_var_1]
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  45 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_2 : happy_var_1
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  46 happyReduction_49
happyReduction_49 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  47 happyReduction_50
happyReduction_50 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_1
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  48 happyReduction_51
happyReduction_51 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  49 happyReduction_52
happyReduction_52 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  50 happyReduction_53
happyReduction_53 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  51 happyReduction_54
happyReduction_54 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  52 happyReduction_55
happyReduction_55 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn52
		 (happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  53 happyReduction_56
happyReduction_56 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn53
		 (reverse happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  54 happyReduction_57
happyReduction_57 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn54
		 (reverse happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  55 happyReduction_58
happyReduction_58 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn55
		 ([happy_var_1]
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  55 happyReduction_59
happyReduction_59 (HappyAbsSyn47  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_2 : happy_var_1
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  56 happyReduction_60
happyReduction_60 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn56
		 ([happy_var_1]
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  56 happyReduction_61
happyReduction_61 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_2 : happy_var_1
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  57 happyReduction_62
happyReduction_62 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn57
		 ([happy_var_1]
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  57 happyReduction_63
happyReduction_63 (HappyAbsSyn49  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (happy_var_2 : happy_var_1
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  58 happyReduction_64
happyReduction_64 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn58
		 ([happy_var_1]
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  58 happyReduction_65
happyReduction_65 (HappyAbsSyn50  happy_var_2)
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_2 : happy_var_1
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  59 happyReduction_66
happyReduction_66 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn59
		 ([happy_var_1]
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  59 happyReduction_67
happyReduction_67 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn59
		 (happy_var_2 : happy_var_1
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  60 happyReduction_68
happyReduction_68 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn60
		 ([happy_var_1]
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  60 happyReduction_69
happyReduction_69 (HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_2 : happy_var_1
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  61 happyReduction_70
happyReduction_70 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn61
		 ([happy_var_1]
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  61 happyReduction_71
happyReduction_71 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn61
		 (happy_var_2 : happy_var_1
	)
happyReduction_71 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 64 64 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	'a' -> cont 62;
	'b' -> cont 63;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Maybe a -> (a -> Maybe b) -> Maybe b
happyThen = ((>>=))
happyReturn :: () => a -> Maybe a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Maybe a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Char)] -> Maybe a
happyError' = happyError

test0 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

test1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

test2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

test3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

test4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

test5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

test6 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

test7 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

test8 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

test9 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

test10 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError _  = Nothing

tests         = [ test1 ""      == Just ""
                , test1 "a"     == Just "a"
                , test1 "ab"    == Nothing
                , test1 "aba"   == Just "aa"
                , test1 "abab"  == Nothing

                , test2 ""      == Just ""
                , test2 "a"     == Nothing
                , test2 "ab"    == Just "a"
                , test2 "aba"   == Nothing
                , test2 "abab"  == Just "aa"
                ]

main        = do let failed = filter (not . snd) (zip [0..] tests)
                 when (not (null failed)) $
                   do putStrLn ("Failed tests: " ++ show (map fst failed))
                      exitFailure
                 putStrLn "Tests passed."
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

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

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
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

notHappyAtAll :: a
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

{-# LINE 311 "templates/GenericTemplate.hs" #-}
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

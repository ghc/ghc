-- parser produced by Happy Version 1.13

module Funcs_Parser_Lazy ( runParser ) where

import Funcs_Lexer

import Data_Lazy

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18

action_0 (41) = happyShift action_6
action_0 (42) = happyShift action_7
action_0 (43) = happyShift action_8
action_0 (44) = happyShift action_9
action_0 (51) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (18) = happyGoto action_5
action_0 _ = happyReduce_3

action_1 (41) = happyShift action_6
action_1 (42) = happyShift action_7
action_1 (43) = happyShift action_8
action_1 (44) = happyShift action_9
action_1 (51) = happyShift action_10
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (18) = happyGoto action_5
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (19) = happyShift action_14
action_3 _ = happyFail

action_4 (51) = happyShift action_10
action_4 (18) = happyGoto action_13
action_4 _ = happyFail

action_5 (20) = happyShift action_12
action_5 _ = happyFail

action_6 _ = happyReduce_7

action_7 _ = happyReduce_8

action_8 _ = happyReduce_9

action_9 _ = happyReduce_10

action_10 _ = happyReduce_51

action_11 (54) = happyAccept
action_11 _ = happyFail

action_12 (41) = happyShift action_6
action_12 (42) = happyShift action_7
action_12 (43) = happyShift action_8
action_12 (44) = happyShift action_9
action_12 (7) = happyGoto action_17
action_12 (8) = happyGoto action_18
action_12 (9) = happyGoto action_19
action_12 (10) = happyGoto action_20
action_12 _ = happyReduce_11

action_13 (22) = happyShift action_16
action_13 _ = happyReduce_4

action_14 (41) = happyShift action_6
action_14 (42) = happyShift action_7
action_14 (43) = happyShift action_8
action_14 (44) = happyShift action_9
action_14 (51) = happyShift action_10
action_14 (5) = happyGoto action_15
action_14 (6) = happyGoto action_3
action_14 (7) = happyGoto action_4
action_14 (18) = happyGoto action_5
action_14 _ = happyReduce_3

action_15 _ = happyReduce_2

action_16 (52) = happyShift action_24
action_16 _ = happyFail

action_17 (51) = happyShift action_10
action_17 (18) = happyGoto action_23
action_17 _ = happyFail

action_18 (21) = happyShift action_22
action_18 _ = happyFail

action_19 _ = happyReduce_12

action_20 (27) = happyShift action_21
action_20 _ = happyReduce_14

action_21 (41) = happyShift action_6
action_21 (42) = happyShift action_7
action_21 (43) = happyShift action_8
action_21 (44) = happyShift action_9
action_21 (7) = happyGoto action_17
action_21 (9) = happyGoto action_27
action_21 (10) = happyGoto action_20
action_21 _ = happyFail

action_22 (26) = happyShift action_26
action_22 _ = happyFail

action_23 _ = happyReduce_15

action_24 (23) = happyShift action_25
action_24 _ = happyFail

action_25 _ = happyReduce_5

action_26 (41) = happyShift action_6
action_26 (42) = happyShift action_7
action_26 (43) = happyShift action_8
action_26 (44) = happyShift action_9
action_26 (7) = happyGoto action_28
action_26 _ = happyFail

action_27 _ = happyReduce_13

action_28 (24) = happyShift action_29
action_28 _ = happyFail

action_29 (41) = happyShift action_6
action_29 (42) = happyShift action_7
action_29 (43) = happyShift action_8
action_29 (44) = happyShift action_9
action_29 (45) = happyShift action_35
action_29 (48) = happyShift action_36
action_29 (49) = happyShift action_37
action_29 (50) = happyShift action_38
action_29 (51) = happyShift action_10
action_29 (7) = happyGoto action_30
action_29 (11) = happyGoto action_31
action_29 (12) = happyGoto action_32
action_29 (15) = happyGoto action_33
action_29 (18) = happyGoto action_34
action_29 _ = happyReduce_17

action_30 (51) = happyShift action_10
action_30 (18) = happyGoto action_59
action_30 _ = happyFail

action_31 (25) = happyShift action_58
action_31 _ = happyFail

action_32 (19) = happyShift action_57
action_32 _ = happyFail

action_33 (31) = happyShift action_56
action_33 _ = happyFail

action_34 (20) = happyShift action_53
action_34 (22) = happyShift action_54
action_34 (31) = happyShift action_55
action_34 _ = happyFail

action_35 (20) = happyShift action_43
action_35 (34) = happyShift action_44
action_35 (38) = happyShift action_45
action_35 (39) = happyShift action_46
action_35 (40) = happyShift action_47
action_35 (51) = happyShift action_10
action_35 (52) = happyShift action_48
action_35 (53) = happyShift action_49
action_35 (13) = happyGoto action_52
action_35 (14) = happyGoto action_40
action_35 (15) = happyGoto action_41
action_35 (18) = happyGoto action_42
action_35 _ = happyFail

action_36 (20) = happyShift action_43
action_36 (34) = happyShift action_44
action_36 (38) = happyShift action_45
action_36 (39) = happyShift action_46
action_36 (40) = happyShift action_47
action_36 (51) = happyShift action_10
action_36 (52) = happyShift action_48
action_36 (53) = happyShift action_49
action_36 (13) = happyGoto action_51
action_36 (14) = happyGoto action_40
action_36 (15) = happyGoto action_41
action_36 (18) = happyGoto action_42
action_36 _ = happyFail

action_37 (51) = happyShift action_10
action_37 (18) = happyGoto action_50
action_37 _ = happyFail

action_38 (20) = happyShift action_43
action_38 (34) = happyShift action_44
action_38 (38) = happyShift action_45
action_38 (39) = happyShift action_46
action_38 (40) = happyShift action_47
action_38 (51) = happyShift action_10
action_38 (52) = happyShift action_48
action_38 (53) = happyShift action_49
action_38 (13) = happyGoto action_39
action_38 (14) = happyGoto action_40
action_38 (15) = happyGoto action_41
action_38 (18) = happyGoto action_42
action_38 _ = happyFail

action_39 (28) = happyShift action_67
action_39 (29) = happyShift action_68
action_39 (30) = happyShift action_69
action_39 (32) = happyShift action_70
action_39 (33) = happyShift action_71
action_39 (34) = happyShift action_72
action_39 (35) = happyShift action_73
action_39 (36) = happyShift action_74
action_39 (37) = happyShift action_75
action_39 _ = happyReduce_25

action_40 _ = happyReduce_26

action_41 _ = happyReduce_42

action_42 (20) = happyShift action_81
action_42 (22) = happyShift action_54
action_42 _ = happyReduce_43

action_43 (20) = happyShift action_43
action_43 (34) = happyShift action_44
action_43 (38) = happyShift action_45
action_43 (39) = happyShift action_46
action_43 (40) = happyShift action_47
action_43 (51) = happyShift action_10
action_43 (52) = happyShift action_48
action_43 (53) = happyShift action_49
action_43 (13) = happyGoto action_80
action_43 (14) = happyGoto action_40
action_43 (15) = happyGoto action_41
action_43 (18) = happyGoto action_42
action_43 _ = happyFail

action_44 (20) = happyShift action_43
action_44 (34) = happyShift action_44
action_44 (38) = happyShift action_45
action_44 (39) = happyShift action_46
action_44 (40) = happyShift action_47
action_44 (51) = happyShift action_10
action_44 (52) = happyShift action_48
action_44 (53) = happyShift action_49
action_44 (13) = happyGoto action_79
action_44 (14) = happyGoto action_40
action_44 (15) = happyGoto action_41
action_44 (18) = happyGoto action_42
action_44 _ = happyFail

action_45 (20) = happyShift action_43
action_45 (34) = happyShift action_44
action_45 (38) = happyShift action_45
action_45 (39) = happyShift action_46
action_45 (40) = happyShift action_47
action_45 (51) = happyShift action_10
action_45 (52) = happyShift action_48
action_45 (53) = happyShift action_49
action_45 (13) = happyGoto action_78
action_45 (14) = happyGoto action_40
action_45 (15) = happyGoto action_41
action_45 (18) = happyGoto action_42
action_45 _ = happyFail

action_46 _ = happyReduce_40

action_47 _ = happyReduce_41

action_48 _ = happyReduce_38

action_49 _ = happyReduce_39

action_50 _ = happyReduce_24

action_51 (24) = happyShift action_77
action_51 (28) = happyShift action_67
action_51 (29) = happyShift action_68
action_51 (30) = happyShift action_69
action_51 (32) = happyShift action_70
action_51 (33) = happyShift action_71
action_51 (34) = happyShift action_72
action_51 (35) = happyShift action_73
action_51 (36) = happyShift action_74
action_51 (37) = happyShift action_75
action_51 _ = happyFail

action_52 (28) = happyShift action_67
action_52 (29) = happyShift action_68
action_52 (30) = happyShift action_69
action_52 (32) = happyShift action_70
action_52 (33) = happyShift action_71
action_52 (34) = happyShift action_72
action_52 (35) = happyShift action_73
action_52 (36) = happyShift action_74
action_52 (37) = happyShift action_75
action_52 (46) = happyShift action_76
action_52 _ = happyFail

action_53 (20) = happyShift action_43
action_53 (34) = happyShift action_44
action_53 (38) = happyShift action_45
action_53 (39) = happyShift action_46
action_53 (40) = happyShift action_47
action_53 (51) = happyShift action_10
action_53 (52) = happyShift action_48
action_53 (53) = happyShift action_49
action_53 (13) = happyGoto action_64
action_53 (14) = happyGoto action_40
action_53 (15) = happyGoto action_41
action_53 (16) = happyGoto action_65
action_53 (17) = happyGoto action_66
action_53 (18) = happyGoto action_42
action_53 _ = happyReduce_47

action_54 (20) = happyShift action_43
action_54 (34) = happyShift action_44
action_54 (38) = happyShift action_45
action_54 (39) = happyShift action_46
action_54 (40) = happyShift action_47
action_54 (51) = happyShift action_10
action_54 (52) = happyShift action_48
action_54 (53) = happyShift action_49
action_54 (13) = happyGoto action_63
action_54 (14) = happyGoto action_40
action_54 (15) = happyGoto action_41
action_54 (18) = happyGoto action_42
action_54 _ = happyFail

action_55 (20) = happyShift action_43
action_55 (34) = happyShift action_44
action_55 (38) = happyShift action_45
action_55 (39) = happyShift action_46
action_55 (40) = happyShift action_47
action_55 (51) = happyShift action_10
action_55 (52) = happyShift action_48
action_55 (53) = happyShift action_49
action_55 (13) = happyGoto action_62
action_55 (14) = happyGoto action_40
action_55 (15) = happyGoto action_41
action_55 (18) = happyGoto action_42
action_55 _ = happyFail

action_56 (20) = happyShift action_43
action_56 (34) = happyShift action_44
action_56 (38) = happyShift action_45
action_56 (39) = happyShift action_46
action_56 (40) = happyShift action_47
action_56 (51) = happyShift action_10
action_56 (52) = happyShift action_48
action_56 (53) = happyShift action_49
action_56 (13) = happyGoto action_61
action_56 (14) = happyGoto action_40
action_56 (15) = happyGoto action_41
action_56 (18) = happyGoto action_42
action_56 _ = happyFail

action_57 (41) = happyShift action_6
action_57 (42) = happyShift action_7
action_57 (43) = happyShift action_8
action_57 (44) = happyShift action_9
action_57 (45) = happyShift action_35
action_57 (48) = happyShift action_36
action_57 (49) = happyShift action_37
action_57 (50) = happyShift action_38
action_57 (51) = happyShift action_10
action_57 (7) = happyGoto action_30
action_57 (11) = happyGoto action_60
action_57 (12) = happyGoto action_32
action_57 (15) = happyGoto action_33
action_57 (18) = happyGoto action_34
action_57 _ = happyReduce_17

action_58 _ = happyReduce_6

action_59 _ = happyReduce_18

action_60 _ = happyReduce_16

action_61 (28) = happyShift action_67
action_61 (29) = happyShift action_68
action_61 (30) = happyShift action_69
action_61 (32) = happyShift action_70
action_61 (33) = happyShift action_71
action_61 (34) = happyShift action_72
action_61 (35) = happyShift action_73
action_61 (36) = happyShift action_74
action_61 (37) = happyShift action_75
action_61 _ = happyReduce_20

action_62 (28) = happyShift action_67
action_62 (29) = happyShift action_68
action_62 (30) = happyShift action_69
action_62 (32) = happyShift action_70
action_62 (33) = happyShift action_71
action_62 (34) = happyShift action_72
action_62 (35) = happyShift action_73
action_62 (36) = happyShift action_74
action_62 (37) = happyShift action_75
action_62 _ = happyReduce_19

action_63 (23) = happyShift action_97
action_63 (28) = happyShift action_67
action_63 (29) = happyShift action_68
action_63 (30) = happyShift action_69
action_63 (32) = happyShift action_70
action_63 (33) = happyShift action_71
action_63 (34) = happyShift action_72
action_63 (35) = happyShift action_73
action_63 (36) = happyShift action_74
action_63 (37) = happyShift action_75
action_63 _ = happyFail

action_64 (27) = happyShift action_96
action_64 (28) = happyShift action_67
action_64 (29) = happyShift action_68
action_64 (30) = happyShift action_69
action_64 (32) = happyShift action_70
action_64 (33) = happyShift action_71
action_64 (34) = happyShift action_72
action_64 (35) = happyShift action_73
action_64 (36) = happyShift action_74
action_64 (37) = happyShift action_75
action_64 _ = happyReduce_50

action_65 (21) = happyShift action_95
action_65 _ = happyFail

action_66 _ = happyReduce_48

action_67 (20) = happyShift action_43
action_67 (34) = happyShift action_44
action_67 (38) = happyShift action_45
action_67 (39) = happyShift action_46
action_67 (40) = happyShift action_47
action_67 (51) = happyShift action_10
action_67 (52) = happyShift action_48
action_67 (53) = happyShift action_49
action_67 (13) = happyGoto action_94
action_67 (14) = happyGoto action_40
action_67 (15) = happyGoto action_41
action_67 (18) = happyGoto action_42
action_67 _ = happyFail

action_68 (20) = happyShift action_43
action_68 (34) = happyShift action_44
action_68 (38) = happyShift action_45
action_68 (39) = happyShift action_46
action_68 (40) = happyShift action_47
action_68 (51) = happyShift action_10
action_68 (52) = happyShift action_48
action_68 (53) = happyShift action_49
action_68 (13) = happyGoto action_93
action_68 (14) = happyGoto action_40
action_68 (15) = happyGoto action_41
action_68 (18) = happyGoto action_42
action_68 _ = happyFail

action_69 (20) = happyShift action_43
action_69 (34) = happyShift action_44
action_69 (38) = happyShift action_45
action_69 (39) = happyShift action_46
action_69 (40) = happyShift action_47
action_69 (51) = happyShift action_10
action_69 (52) = happyShift action_48
action_69 (53) = happyShift action_49
action_69 (13) = happyGoto action_92
action_69 (14) = happyGoto action_40
action_69 (15) = happyGoto action_41
action_69 (18) = happyGoto action_42
action_69 _ = happyFail

action_70 (20) = happyShift action_43
action_70 (34) = happyShift action_44
action_70 (38) = happyShift action_45
action_70 (39) = happyShift action_46
action_70 (40) = happyShift action_47
action_70 (51) = happyShift action_10
action_70 (52) = happyShift action_48
action_70 (53) = happyShift action_49
action_70 (13) = happyGoto action_91
action_70 (14) = happyGoto action_40
action_70 (15) = happyGoto action_41
action_70 (18) = happyGoto action_42
action_70 _ = happyFail

action_71 (20) = happyShift action_43
action_71 (34) = happyShift action_44
action_71 (38) = happyShift action_45
action_71 (39) = happyShift action_46
action_71 (40) = happyShift action_47
action_71 (51) = happyShift action_10
action_71 (52) = happyShift action_48
action_71 (53) = happyShift action_49
action_71 (13) = happyGoto action_90
action_71 (14) = happyGoto action_40
action_71 (15) = happyGoto action_41
action_71 (18) = happyGoto action_42
action_71 _ = happyFail

action_72 (20) = happyShift action_43
action_72 (34) = happyShift action_44
action_72 (38) = happyShift action_45
action_72 (39) = happyShift action_46
action_72 (40) = happyShift action_47
action_72 (51) = happyShift action_10
action_72 (52) = happyShift action_48
action_72 (53) = happyShift action_49
action_72 (13) = happyGoto action_89
action_72 (14) = happyGoto action_40
action_72 (15) = happyGoto action_41
action_72 (18) = happyGoto action_42
action_72 _ = happyFail

action_73 (20) = happyShift action_43
action_73 (34) = happyShift action_44
action_73 (38) = happyShift action_45
action_73 (39) = happyShift action_46
action_73 (40) = happyShift action_47
action_73 (51) = happyShift action_10
action_73 (52) = happyShift action_48
action_73 (53) = happyShift action_49
action_73 (13) = happyGoto action_88
action_73 (14) = happyGoto action_40
action_73 (15) = happyGoto action_41
action_73 (18) = happyGoto action_42
action_73 _ = happyFail

action_74 (20) = happyShift action_43
action_74 (34) = happyShift action_44
action_74 (38) = happyShift action_45
action_74 (39) = happyShift action_46
action_74 (40) = happyShift action_47
action_74 (51) = happyShift action_10
action_74 (52) = happyShift action_48
action_74 (53) = happyShift action_49
action_74 (13) = happyGoto action_87
action_74 (14) = happyGoto action_40
action_74 (15) = happyGoto action_41
action_74 (18) = happyGoto action_42
action_74 _ = happyFail

action_75 (20) = happyShift action_43
action_75 (34) = happyShift action_44
action_75 (38) = happyShift action_45
action_75 (39) = happyShift action_46
action_75 (40) = happyShift action_47
action_75 (51) = happyShift action_10
action_75 (52) = happyShift action_48
action_75 (53) = happyShift action_49
action_75 (13) = happyGoto action_86
action_75 (14) = happyGoto action_40
action_75 (15) = happyGoto action_41
action_75 (18) = happyGoto action_42
action_75 _ = happyFail

action_76 (24) = happyShift action_85
action_76 _ = happyFail

action_77 (41) = happyShift action_6
action_77 (42) = happyShift action_7
action_77 (43) = happyShift action_8
action_77 (44) = happyShift action_9
action_77 (45) = happyShift action_35
action_77 (48) = happyShift action_36
action_77 (49) = happyShift action_37
action_77 (50) = happyShift action_38
action_77 (51) = happyShift action_10
action_77 (7) = happyGoto action_30
action_77 (11) = happyGoto action_84
action_77 (12) = happyGoto action_32
action_77 (15) = happyGoto action_33
action_77 (18) = happyGoto action_34
action_77 _ = happyReduce_17

action_78 (28) = happyShift action_67
action_78 (29) = happyShift action_68
action_78 (30) = happyShift action_69
action_78 (32) = happyShift action_70
action_78 (33) = happyShift action_71
action_78 (34) = happyShift action_72
action_78 (35) = happyShift action_73
action_78 (36) = happyShift action_74
action_78 (37) = happyShift action_75
action_78 _ = happyReduce_36

action_79 (28) = happyShift action_67
action_79 (29) = happyShift action_68
action_79 (30) = happyShift action_69
action_79 (32) = happyShift action_70
action_79 (33) = happyShift action_71
action_79 (34) = happyShift action_72
action_79 (35) = happyShift action_73
action_79 (36) = happyShift action_74
action_79 (37) = happyShift action_75
action_79 _ = happyReduce_37

action_80 (21) = happyShift action_83
action_80 (28) = happyShift action_67
action_80 (29) = happyShift action_68
action_80 (30) = happyShift action_69
action_80 (32) = happyShift action_70
action_80 (33) = happyShift action_71
action_80 (34) = happyShift action_72
action_80 (35) = happyShift action_73
action_80 (36) = happyShift action_74
action_80 (37) = happyShift action_75
action_80 _ = happyFail

action_81 (20) = happyShift action_43
action_81 (34) = happyShift action_44
action_81 (38) = happyShift action_45
action_81 (39) = happyShift action_46
action_81 (40) = happyShift action_47
action_81 (51) = happyShift action_10
action_81 (52) = happyShift action_48
action_81 (53) = happyShift action_49
action_81 (13) = happyGoto action_64
action_81 (14) = happyGoto action_40
action_81 (15) = happyGoto action_41
action_81 (17) = happyGoto action_82
action_81 (18) = happyGoto action_42
action_81 _ = happyFail

action_82 (21) = happyShift action_101
action_82 _ = happyFail

action_83 _ = happyReduce_45

action_84 (25) = happyShift action_100
action_84 _ = happyFail

action_85 (41) = happyShift action_6
action_85 (42) = happyShift action_7
action_85 (43) = happyShift action_8
action_85 (44) = happyShift action_9
action_85 (45) = happyShift action_35
action_85 (48) = happyShift action_36
action_85 (49) = happyShift action_37
action_85 (50) = happyShift action_38
action_85 (51) = happyShift action_10
action_85 (7) = happyGoto action_30
action_85 (11) = happyGoto action_99
action_85 (12) = happyGoto action_32
action_85 (15) = happyGoto action_33
action_85 (18) = happyGoto action_34
action_85 _ = happyReduce_17

action_86 (28) = happyShift action_67
action_86 (29) = happyShift action_68
action_86 (30) = happyShift action_69
action_86 (32) = happyShift action_70
action_86 (33) = happyShift action_71
action_86 (34) = happyShift action_72
action_86 (35) = happyShift action_73
action_86 (36) = happyShift action_74
action_86 (37) = happyShift action_75
action_86 _ = happyReduce_35

action_87 (28) = happyShift action_67
action_87 (29) = happyShift action_68
action_87 (30) = happyShift action_69
action_87 (32) = happyShift action_70
action_87 (33) = happyShift action_71
action_87 (34) = happyShift action_72
action_87 (35) = happyShift action_73
action_87 (36) = happyShift action_74
action_87 (37) = happyShift action_75
action_87 _ = happyReduce_34

action_88 (28) = happyShift action_67
action_88 (29) = happyShift action_68
action_88 (30) = happyShift action_69
action_88 (32) = happyShift action_70
action_88 (33) = happyShift action_71
action_88 (34) = happyShift action_72
action_88 (35) = happyShift action_73
action_88 (36) = happyShift action_74
action_88 (37) = happyShift action_75
action_88 _ = happyReduce_30

action_89 (28) = happyShift action_67
action_89 (29) = happyShift action_68
action_89 (30) = happyShift action_69
action_89 (32) = happyShift action_70
action_89 (33) = happyShift action_71
action_89 (34) = happyShift action_72
action_89 (35) = happyShift action_73
action_89 (36) = happyShift action_74
action_89 (37) = happyShift action_75
action_89 _ = happyReduce_28

action_90 (28) = happyShift action_67
action_90 (29) = happyShift action_68
action_90 (30) = happyShift action_69
action_90 (32) = happyShift action_70
action_90 (33) = happyShift action_71
action_90 (34) = happyShift action_72
action_90 (35) = happyShift action_73
action_90 (36) = happyShift action_74
action_90 (37) = happyShift action_75
action_90 _ = happyReduce_29

action_91 (28) = happyShift action_67
action_91 (29) = happyShift action_68
action_91 (30) = happyShift action_69
action_91 (32) = happyShift action_70
action_91 (33) = happyShift action_71
action_91 (34) = happyShift action_72
action_91 (35) = happyShift action_73
action_91 (36) = happyShift action_74
action_91 (37) = happyShift action_75
action_91 _ = happyReduce_27

action_92 (28) = happyShift action_67
action_92 (29) = happyShift action_68
action_92 (30) = happyShift action_69
action_92 (32) = happyShift action_70
action_92 (33) = happyShift action_71
action_92 (34) = happyShift action_72
action_92 (35) = happyShift action_73
action_92 (36) = happyShift action_74
action_92 (37) = happyShift action_75
action_92 _ = happyReduce_32

action_93 (28) = happyShift action_67
action_93 (29) = happyShift action_68
action_93 (30) = happyShift action_69
action_93 (32) = happyShift action_70
action_93 (33) = happyShift action_71
action_93 (34) = happyShift action_72
action_93 (35) = happyShift action_73
action_93 (36) = happyShift action_74
action_93 (37) = happyShift action_75
action_93 _ = happyReduce_31

action_94 (28) = happyShift action_67
action_94 (29) = happyShift action_68
action_94 (30) = happyShift action_69
action_94 (32) = happyShift action_70
action_94 (33) = happyShift action_71
action_94 (34) = happyShift action_72
action_94 (35) = happyShift action_73
action_94 (36) = happyShift action_74
action_94 (37) = happyShift action_75
action_94 _ = happyReduce_33

action_95 _ = happyReduce_21

action_96 (20) = happyShift action_43
action_96 (34) = happyShift action_44
action_96 (38) = happyShift action_45
action_96 (39) = happyShift action_46
action_96 (40) = happyShift action_47
action_96 (51) = happyShift action_10
action_96 (52) = happyShift action_48
action_96 (53) = happyShift action_49
action_96 (13) = happyGoto action_64
action_96 (14) = happyGoto action_40
action_96 (15) = happyGoto action_41
action_96 (17) = happyGoto action_98
action_96 (18) = happyGoto action_42
action_96 _ = happyFail

action_97 _ = happyReduce_46

action_98 _ = happyReduce_49

action_99 (25) = happyShift action_102
action_99 _ = happyFail

action_100 _ = happyReduce_22

action_101 _ = happyReduce_44

action_102 (47) = happyShift action_103
action_102 _ = happyFail

action_103 (24) = happyShift action_104
action_103 _ = happyFail

action_104 (41) = happyShift action_6
action_104 (42) = happyShift action_7
action_104 (43) = happyShift action_8
action_104 (44) = happyShift action_9
action_104 (45) = happyShift action_35
action_104 (48) = happyShift action_36
action_104 (49) = happyShift action_37
action_104 (50) = happyShift action_38
action_104 (51) = happyShift action_10
action_104 (7) = happyGoto action_30
action_104 (11) = happyGoto action_105
action_104 (12) = happyGoto action_32
action_104 (15) = happyGoto action_33
action_104 (18) = happyGoto action_34
action_104 _ = happyReduce_17

action_105 (25) = happyShift action_106
action_105 _ = happyFail

action_106 _ = happyReduce_23

happyReduce_1 = happySpecReduce_1 4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (C_RootProd_1 happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3 5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (C_Defs2_1 happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0 5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 (C_NoDefs_1
	)

happyReduce_4 = happySpecReduce_2 6 happyReduction_4
happyReduction_4 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (C_Vardecl_1 happy_var_1 happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 5 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyTerminal (TintVal happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C_Arraydecl_1 happy_var_1 happy_var_2 (toInteger happy_var_4)
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 9 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (C_Declfunc_1 happy_var_6 happy_var_1 happy_var_3 happy_var_8
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1 7 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn7
		 (C_Inttype_1
	)

happyReduce_8 = happySpecReduce_1 7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (C_Realtype_1
	)

happyReduce_9 = happySpecReduce_1 7 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (C_Booltype_1
	)

happyReduce_10 = happySpecReduce_1 7 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (C_Chartype_1
	)

happyReduce_11 = happySpecReduce_0 8 happyReduction_11
happyReduction_11  =  HappyAbsSyn8
		 (C_Emptyformpars_1
	)

happyReduce_12 = happySpecReduce_1 8 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3 9 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (C_Lstformpars_1 happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1 9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (C_Lstformpars_1 happy_var_1 C_Emptyformpars_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2 10 happyReduction_15
happyReduction_15 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn10
		 (C_Declformpar_1 happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3 11 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (C_Lststats_1 happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0 11 happyReduction_17
happyReduction_17  =  HappyAbsSyn11
		 (C_Emptystat_1
	)

happyReduce_18 = happySpecReduce_2 12 happyReduction_18
happyReduction_18 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn12
		 (C_LocalDecl_1 happy_var_1 happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3 12 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn12
		 (C_Assign_1 happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3 12 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn12
		 (C_ArrAssign_1 happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 12 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (C_Funccall_1 happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 12 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (C_While_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 10 12 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (C_If_t_e_1 happy_var_2 happy_var_5 happy_var_9
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_2 12 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (C_Input_1 happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2 12 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (C_Print_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1 13 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (C_Factor_1 happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3 13 happyReduction_27
happyReduction_27 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_AddExp_1 happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3 13 happyReduction_28
happyReduction_28 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_SubExp_1 happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3 13 happyReduction_29
happyReduction_29 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_MulExp_1 happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3 13 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_DivExp_1 happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3 13 happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_AndExp_1 happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3 13 happyReduction_32
happyReduction_32 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_OrExp_1 happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3 13 happyReduction_33
happyReduction_33 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_EqExp_1 happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3 13 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_GTExp_1 happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3 13 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (C_LTExp_1 happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2 13 happyReduction_36
happyReduction_36 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (C_NotExp_1 happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2 13 happyReduction_37
happyReduction_37 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (C_MinExp_1 happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1 14 happyReduction_38
happyReduction_38 (HappyTerminal (TintVal happy_var_1))
	 =  HappyAbsSyn14
		 (C_IntConst_1 (toInteger happy_var_1)
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1 14 happyReduction_39
happyReduction_39 (HappyTerminal (TrealVal happy_var_1))
	 =  HappyAbsSyn14
		 (C_RealConst_1 happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1 14 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn14
		 (C_BoolConst_1 True
	)

happyReduce_41 = happySpecReduce_1 14 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn14
		 (C_BoolConst_1 False
	)

happyReduce_42 = happySpecReduce_1 14 happyReduction_42
happyReduction_42 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (C_ArrayConst_1 happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1 14 happyReduction_43
happyReduction_43 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn14
		 (C_CNIdent_1 happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 14 happyReduction_44
happyReduction_44 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (C_Funcinv_1 happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_3 14 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (C_Expr_1 happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happyReduce 4 15 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (C_ArrayInd_1 happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_0 16 happyReduction_47
happyReduction_47  =  HappyAbsSyn16
		 (C_Emptyactpars_1
	)

happyReduce_48 = happySpecReduce_1 16 happyReduction_48
happyReduction_48 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3 17 happyReduction_49
happyReduction_49 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn17
		 (C_Lstactpars_1 happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1 17 happyReduction_50
happyReduction_50 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn17
		 (C_Lstactpars_1 happy_var_1 C_Emptyactpars_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1 18 happyReduction_51
happyReduction_51 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn18
		 (C_Ident_1 happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 54 54 (error "reading EOF!") (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tsemicolon -> cont 19;
	TopenB -> cont 20;
	TcloseB -> cont 21;
	TopenSB -> cont 22;
	TcloseSB -> cont 23;
	TopenCB -> cont 24;
	TcloseCB -> cont 25;
	Ttowdots -> cont 26;
	Tcomma -> cont 27;
	Tequal -> cont 28;
	Tand -> cont 29;
	Tor -> cont 30;
	Tassing -> cont 31;
	Tadd -> cont 32;
	Tmul -> cont 33;
	Tsub -> cont 34;
	Tdiv -> cont 35;
	Tgt -> cont 36;
	Tlt -> cont 37;
	Tnot -> cont 38;
	Ttrue -> cont 39;
	Tfalse -> cont 40;
	Tint -> cont 41;
	Treal -> cont 42;
	Tbool -> cont 43;
	Tchar -> cont 44;
	Tif -> cont 45;
	Tthen -> cont 46;
	Telse -> cont 47;
	Twhile -> cont 48;
	Tinput -> cont 49;
	Tprint -> cont 50;
	TIdent happy_dollar_dollar -> cont 51;
	TintVal happy_dollar_dollar -> cont 52;
	TrealVal happy_dollar_dollar -> cont 53;
	_ -> happyError tks
	}

happyThen = \m k -> k m
happyReturn = \a -> a
happyThen1 = happyThen
happyReturn1 = \a tks -> a

parser tks = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq

-- happyError :: [Token] -> a
happyError _ = error ("Parse error in line \n")



-- runParser :: String -> Error
runParser = parser . lexer


testParser :: String -> IO()
testParser filename 
 = do s <- readFile filename
      let tks = lexer s
      putStr (show tks)
      let ast = parser tks
      putStr (show ast)
      return () -- (scanner inp)
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id: Funcs_Parser_Lazy.hs,v 1.1 2002/11/19 17:00:02 simonpj Exp $

{-# LINE 15 "GenericTemplate.hs" #-}






















































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = 

					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 150 "GenericTemplate.hs" #-}


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
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError


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

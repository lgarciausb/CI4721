{-# OPTIONS_GHC -w #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Parser.Parser where 

import Parser.Lexer
import Parser.LexerDefinitions
import Data.Text (Text)
import Data.Text qualified as T
import Control.Lens ((&))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 (PTypes AlexPosn)
	| HappyAbsSyn8 ([(Text,PTypes AlexPosn,AlexPosn)])
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
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,420) ([0,18432,0,8,0,0,8,0,0,0,17856,4112,34,12,0,0,0,0,0,4096,16384,0,0,0,0,0,0,0,288,8192,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,32,0,0,0,0,1,0,0,0,0,0,0,0,576,16384,0,0,8192,1,32,0,0,0,0,0,0,8192,0,65472,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4464,33796,8,3,47104,520,1090,384,0,1116,8449,49154,0,36864,0,16,0,0,272,4096,0,0,35712,8224,68,24,49152,4165,8720,3072,0,0,0,1,0,0,0,0,0,0,128,32768,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,32768,8331,17440,6144,0,0,0,2,0,0,32,128,0,0,0,8,65532,0,0,16,65024,127,0,128,0,0,0,0,8,65408,31,0,0,0,0,0,35712,8224,68,24,49152,4165,8720,3072,0,8928,2056,17,6,28672,1041,2180,768,0,2232,16898,32772,1,23552,260,545,192,0,33326,4224,24577,0,5888,16449,136,48,32768,8331,17440,6144,0,17856,4112,34,12,57344,2082,4360,1536,0,4464,33796,8,3,47104,520,1090,384,0,1116,8449,49154,0,0,0,0,0,0,0,3072,0,0,0,1,0,0,0,18,512,0,0,0,0,0,0,0,64,64,0,0,512,0,0,0,0,0,2,0,0,0,32768,6143,0,0,0,65472,15,0,0,57344,7,0,0,0,1008,0,0,0,63488,1,0,0,0,252,0,0,0,32256,0,0,0,0,63,0,0,0,2048,0,0,0,0,4,0,0,0,512,0,0,0,0,1,0,0,0,480,0,0,0,61440,0,0,2232,16898,32772,1,0,0,0,0,0,0,0,0,0,18176,0,8,0,32768,8331,17440,6144,0,0,0,0,0,0,64,0,0,0,4464,33796,8,3,0,1,65024,127,0,0,0,0,0,0,4,0,0,0,4096,0,0,0,9088,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,128,0,0,17856,4880,34,12,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,2,0,0,16384,0,0,0,0,0,61440,1023,0,8192,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,144,4096,0,0,0,0,0,0,0,64,0,0,0,0,4,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,1116,8497,49154,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,49152,4165,8720,3072,0,2272,0,1,0,0,0,0,0,0,2232,16994,32772,1,23552,260,545,192,0,0,32768,8191,0,0,0,0,0,32768,1059,1024,0,0,32768,0,65520,3,57344,34850,4361,1536,0,2048,0,0,0,0,0,0,0,0,512,0,0,0,32768,0,0,0,0,16663,34880,12288,0,0,1,65504,7,49152,4165,8735,3072,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,57344,34850,4367,1536,0,0,0,0,0,47104,57864,1091,384,0,0,0,0,0,0,1,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","ixs","lvaluable","args","records","mRecords","T0","T","loop_a0","loop_a","loop_as","pattern","patterns","a0","as","actions","optionalByRef","e","moreArgs","fun_args","function_def","function_defs","char","string","number","atom","'['","']'","'{'","'}'","'('","')'","','","':'","'=>'","':='","match","with","type","';'","for","while","continue","break","new","by","reference","'***'","'|'","identifier","'.'","EOF","'+'","'-'","'*'","'/'","'^'","'%'","'<'","'>'","'!='","'=='","'>='","'<='","'||'","'&&'","'~'","'&'","%eof"]
        bit_start = st Prelude.* 71
        bit_end = (st Prelude.+ 1) Prelude.* 71
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..70]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (28) = happyShift action_7
action_0 (31) = happyShift action_8
action_0 (52) = happyShift action_9
action_0 (9) = happyGoto action_3
action_0 (10) = happyGoto action_4
action_0 (23) = happyGoto action_5
action_0 (24) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (29) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (25) = happyShift action_17
action_2 (26) = happyShift action_18
action_2 (27) = happyShift action_19
action_2 (29) = happyShift action_20
action_2 (33) = happyShift action_21
action_2 (39) = happyShift action_22
action_2 (47) = happyShift action_23
action_2 (52) = happyShift action_24
action_2 (56) = happyShift action_25
action_2 (69) = happyShift action_26
action_2 (70) = happyShift action_27
action_2 (5) = happyGoto action_15
action_2 (20) = happyGoto action_16
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_14

action_4 (33) = happyShift action_13
action_4 (51) = happyShift action_14
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_70

action_6 (28) = happyShift action_7
action_6 (31) = happyShift action_8
action_6 (52) = happyShift action_9
action_6 (71) = happyAccept
action_6 (9) = happyGoto action_3
action_6 (10) = happyGoto action_4
action_6 (23) = happyGoto action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_11

action_8 (52) = happyShift action_11
action_8 (7) = happyGoto action_10
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_12

action_10 (32) = happyShift action_59
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (36) = happyShift action_58
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_69

action_13 (28) = happyShift action_7
action_13 (31) = happyShift action_8
action_13 (52) = happyShift action_9
action_13 (9) = happyGoto action_3
action_13 (10) = happyGoto action_56
action_13 (22) = happyGoto action_57
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (28) = happyShift action_7
action_14 (31) = happyShift action_8
action_14 (52) = happyShift action_9
action_14 (9) = happyGoto action_55
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_39

action_16 (30) = happyShift action_40
action_16 (55) = happyShift action_41
action_16 (56) = happyShift action_42
action_16 (57) = happyShift action_43
action_16 (58) = happyShift action_44
action_16 (59) = happyShift action_45
action_16 (60) = happyShift action_46
action_16 (61) = happyShift action_47
action_16 (62) = happyShift action_48
action_16 (63) = happyShift action_49
action_16 (64) = happyShift action_50
action_16 (65) = happyShift action_51
action_16 (66) = happyShift action_52
action_16 (67) = happyShift action_53
action_16 (68) = happyShift action_54
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_62

action_18 _ = happyReduce_61

action_19 _ = happyReduce_60

action_20 (25) = happyShift action_17
action_20 (26) = happyShift action_18
action_20 (27) = happyShift action_19
action_20 (29) = happyShift action_20
action_20 (33) = happyShift action_21
action_20 (39) = happyShift action_22
action_20 (47) = happyShift action_23
action_20 (52) = happyShift action_24
action_20 (56) = happyShift action_25
action_20 (69) = happyShift action_26
action_20 (70) = happyShift action_27
action_20 (5) = happyGoto action_15
action_20 (6) = happyGoto action_38
action_20 (20) = happyGoto action_39
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (25) = happyShift action_17
action_21 (26) = happyShift action_18
action_21 (27) = happyShift action_19
action_21 (29) = happyShift action_20
action_21 (33) = happyShift action_21
action_21 (39) = happyShift action_22
action_21 (47) = happyShift action_23
action_21 (52) = happyShift action_24
action_21 (56) = happyShift action_25
action_21 (69) = happyShift action_26
action_21 (70) = happyShift action_27
action_21 (5) = happyGoto action_15
action_21 (20) = happyGoto action_37
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (25) = happyShift action_17
action_22 (26) = happyShift action_18
action_22 (27) = happyShift action_19
action_22 (29) = happyShift action_20
action_22 (33) = happyShift action_21
action_22 (39) = happyShift action_22
action_22 (47) = happyShift action_23
action_22 (52) = happyShift action_24
action_22 (56) = happyShift action_25
action_22 (69) = happyShift action_26
action_22 (70) = happyShift action_27
action_22 (5) = happyGoto action_15
action_22 (20) = happyGoto action_36
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (28) = happyShift action_7
action_23 (31) = happyShift action_8
action_23 (52) = happyShift action_9
action_23 (9) = happyGoto action_3
action_23 (10) = happyGoto action_35
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (29) = happyShift action_2
action_24 (33) = happyShift action_33
action_24 (53) = happyShift action_34
action_24 (4) = happyGoto action_32
action_24 _ = happyReduce_5

action_25 (25) = happyShift action_17
action_25 (26) = happyShift action_18
action_25 (27) = happyShift action_19
action_25 (29) = happyShift action_20
action_25 (33) = happyShift action_21
action_25 (39) = happyShift action_22
action_25 (47) = happyShift action_23
action_25 (52) = happyShift action_24
action_25 (56) = happyShift action_25
action_25 (69) = happyShift action_26
action_25 (70) = happyShift action_27
action_25 (5) = happyGoto action_15
action_25 (20) = happyGoto action_31
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (25) = happyShift action_17
action_26 (26) = happyShift action_18
action_26 (27) = happyShift action_19
action_26 (29) = happyShift action_20
action_26 (33) = happyShift action_21
action_26 (39) = happyShift action_22
action_26 (47) = happyShift action_23
action_26 (52) = happyShift action_24
action_26 (56) = happyShift action_25
action_26 (69) = happyShift action_26
action_26 (70) = happyShift action_27
action_26 (5) = happyGoto action_15
action_26 (20) = happyGoto action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (52) = happyShift action_29
action_27 (5) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_41

action_29 (29) = happyShift action_2
action_29 (53) = happyShift action_34
action_29 (4) = happyGoto action_32
action_29 _ = happyReduce_5

action_30 _ = happyReduce_57

action_31 _ = happyReduce_42

action_32 (29) = happyShift action_84
action_32 _ = happyReduce_3

action_33 (25) = happyShift action_17
action_33 (26) = happyShift action_18
action_33 (27) = happyShift action_19
action_33 (29) = happyShift action_20
action_33 (33) = happyShift action_21
action_33 (39) = happyShift action_22
action_33 (47) = happyShift action_23
action_33 (52) = happyShift action_24
action_33 (56) = happyShift action_25
action_33 (69) = happyShift action_26
action_33 (70) = happyShift action_27
action_33 (5) = happyGoto action_15
action_33 (6) = happyGoto action_83
action_33 (20) = happyGoto action_39
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (52) = happyShift action_29
action_34 (5) = happyGoto action_82
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (33) = happyShift action_81
action_35 (51) = happyShift action_14
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (40) = happyShift action_80
action_36 (55) = happyShift action_41
action_36 (56) = happyShift action_42
action_36 (57) = happyShift action_43
action_36 (58) = happyShift action_44
action_36 (59) = happyShift action_45
action_36 (60) = happyShift action_46
action_36 (61) = happyShift action_47
action_36 (62) = happyShift action_48
action_36 (63) = happyShift action_49
action_36 (64) = happyShift action_50
action_36 (65) = happyShift action_51
action_36 (66) = happyShift action_52
action_36 (67) = happyShift action_53
action_36 (68) = happyShift action_54
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (34) = happyShift action_79
action_37 (55) = happyShift action_41
action_37 (56) = happyShift action_42
action_37 (57) = happyShift action_43
action_37 (58) = happyShift action_44
action_37 (59) = happyShift action_45
action_37 (60) = happyShift action_46
action_37 (61) = happyShift action_47
action_37 (62) = happyShift action_48
action_37 (63) = happyShift action_49
action_37 (64) = happyShift action_50
action_37 (65) = happyShift action_51
action_37 (66) = happyShift action_52
action_37 (67) = happyShift action_53
action_37 (68) = happyShift action_54
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (30) = happyShift action_78
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (35) = happyShift action_77
action_39 (55) = happyShift action_41
action_39 (56) = happyShift action_42
action_39 (57) = happyShift action_43
action_39 (58) = happyShift action_44
action_39 (59) = happyShift action_45
action_39 (60) = happyShift action_46
action_39 (61) = happyShift action_47
action_39 (62) = happyShift action_48
action_39 (63) = happyShift action_49
action_39 (64) = happyShift action_50
action_39 (65) = happyShift action_51
action_39 (66) = happyShift action_52
action_39 (67) = happyShift action_53
action_39 (68) = happyShift action_54
action_39 _ = happyReduce_6

action_40 _ = happyReduce_1

action_41 (25) = happyShift action_17
action_41 (26) = happyShift action_18
action_41 (27) = happyShift action_19
action_41 (29) = happyShift action_20
action_41 (33) = happyShift action_21
action_41 (39) = happyShift action_22
action_41 (47) = happyShift action_23
action_41 (52) = happyShift action_24
action_41 (56) = happyShift action_25
action_41 (69) = happyShift action_26
action_41 (70) = happyShift action_27
action_41 (5) = happyGoto action_15
action_41 (20) = happyGoto action_76
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (25) = happyShift action_17
action_42 (26) = happyShift action_18
action_42 (27) = happyShift action_19
action_42 (29) = happyShift action_20
action_42 (33) = happyShift action_21
action_42 (39) = happyShift action_22
action_42 (47) = happyShift action_23
action_42 (52) = happyShift action_24
action_42 (56) = happyShift action_25
action_42 (69) = happyShift action_26
action_42 (70) = happyShift action_27
action_42 (5) = happyGoto action_15
action_42 (20) = happyGoto action_75
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (25) = happyShift action_17
action_43 (26) = happyShift action_18
action_43 (27) = happyShift action_19
action_43 (29) = happyShift action_20
action_43 (33) = happyShift action_21
action_43 (39) = happyShift action_22
action_43 (47) = happyShift action_23
action_43 (52) = happyShift action_24
action_43 (56) = happyShift action_25
action_43 (69) = happyShift action_26
action_43 (70) = happyShift action_27
action_43 (5) = happyGoto action_15
action_43 (20) = happyGoto action_74
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (25) = happyShift action_17
action_44 (26) = happyShift action_18
action_44 (27) = happyShift action_19
action_44 (29) = happyShift action_20
action_44 (33) = happyShift action_21
action_44 (39) = happyShift action_22
action_44 (47) = happyShift action_23
action_44 (52) = happyShift action_24
action_44 (56) = happyShift action_25
action_44 (69) = happyShift action_26
action_44 (70) = happyShift action_27
action_44 (5) = happyGoto action_15
action_44 (20) = happyGoto action_73
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (25) = happyShift action_17
action_45 (26) = happyShift action_18
action_45 (27) = happyShift action_19
action_45 (29) = happyShift action_20
action_45 (33) = happyShift action_21
action_45 (39) = happyShift action_22
action_45 (47) = happyShift action_23
action_45 (52) = happyShift action_24
action_45 (56) = happyShift action_25
action_45 (69) = happyShift action_26
action_45 (70) = happyShift action_27
action_45 (5) = happyGoto action_15
action_45 (20) = happyGoto action_72
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (25) = happyShift action_17
action_46 (26) = happyShift action_18
action_46 (27) = happyShift action_19
action_46 (29) = happyShift action_20
action_46 (33) = happyShift action_21
action_46 (39) = happyShift action_22
action_46 (47) = happyShift action_23
action_46 (52) = happyShift action_24
action_46 (56) = happyShift action_25
action_46 (69) = happyShift action_26
action_46 (70) = happyShift action_27
action_46 (5) = happyGoto action_15
action_46 (20) = happyGoto action_71
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (25) = happyShift action_17
action_47 (26) = happyShift action_18
action_47 (27) = happyShift action_19
action_47 (29) = happyShift action_20
action_47 (33) = happyShift action_21
action_47 (39) = happyShift action_22
action_47 (47) = happyShift action_23
action_47 (52) = happyShift action_24
action_47 (56) = happyShift action_25
action_47 (69) = happyShift action_26
action_47 (70) = happyShift action_27
action_47 (5) = happyGoto action_15
action_47 (20) = happyGoto action_70
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (25) = happyShift action_17
action_48 (26) = happyShift action_18
action_48 (27) = happyShift action_19
action_48 (29) = happyShift action_20
action_48 (33) = happyShift action_21
action_48 (39) = happyShift action_22
action_48 (47) = happyShift action_23
action_48 (52) = happyShift action_24
action_48 (56) = happyShift action_25
action_48 (69) = happyShift action_26
action_48 (70) = happyShift action_27
action_48 (5) = happyGoto action_15
action_48 (20) = happyGoto action_69
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (25) = happyShift action_17
action_49 (26) = happyShift action_18
action_49 (27) = happyShift action_19
action_49 (29) = happyShift action_20
action_49 (33) = happyShift action_21
action_49 (39) = happyShift action_22
action_49 (47) = happyShift action_23
action_49 (52) = happyShift action_24
action_49 (56) = happyShift action_25
action_49 (69) = happyShift action_26
action_49 (70) = happyShift action_27
action_49 (5) = happyGoto action_15
action_49 (20) = happyGoto action_68
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (25) = happyShift action_17
action_50 (26) = happyShift action_18
action_50 (27) = happyShift action_19
action_50 (29) = happyShift action_20
action_50 (33) = happyShift action_21
action_50 (39) = happyShift action_22
action_50 (47) = happyShift action_23
action_50 (52) = happyShift action_24
action_50 (56) = happyShift action_25
action_50 (69) = happyShift action_26
action_50 (70) = happyShift action_27
action_50 (5) = happyGoto action_15
action_50 (20) = happyGoto action_67
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (25) = happyShift action_17
action_51 (26) = happyShift action_18
action_51 (27) = happyShift action_19
action_51 (29) = happyShift action_20
action_51 (33) = happyShift action_21
action_51 (39) = happyShift action_22
action_51 (47) = happyShift action_23
action_51 (52) = happyShift action_24
action_51 (56) = happyShift action_25
action_51 (69) = happyShift action_26
action_51 (70) = happyShift action_27
action_51 (5) = happyGoto action_15
action_51 (20) = happyGoto action_66
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (25) = happyShift action_17
action_52 (26) = happyShift action_18
action_52 (27) = happyShift action_19
action_52 (29) = happyShift action_20
action_52 (33) = happyShift action_21
action_52 (39) = happyShift action_22
action_52 (47) = happyShift action_23
action_52 (52) = happyShift action_24
action_52 (56) = happyShift action_25
action_52 (69) = happyShift action_26
action_52 (70) = happyShift action_27
action_52 (5) = happyGoto action_15
action_52 (20) = happyGoto action_65
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (25) = happyShift action_17
action_53 (26) = happyShift action_18
action_53 (27) = happyShift action_19
action_53 (29) = happyShift action_20
action_53 (33) = happyShift action_21
action_53 (39) = happyShift action_22
action_53 (47) = happyShift action_23
action_53 (52) = happyShift action_24
action_53 (56) = happyShift action_25
action_53 (69) = happyShift action_26
action_53 (70) = happyShift action_27
action_53 (5) = happyGoto action_15
action_53 (20) = happyGoto action_64
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (25) = happyShift action_17
action_54 (26) = happyShift action_18
action_54 (27) = happyShift action_19
action_54 (29) = happyShift action_20
action_54 (33) = happyShift action_21
action_54 (39) = happyShift action_22
action_54 (47) = happyShift action_23
action_54 (52) = happyShift action_24
action_54 (56) = happyShift action_25
action_54 (69) = happyShift action_26
action_54 (70) = happyShift action_27
action_54 (5) = happyGoto action_15
action_54 (20) = happyGoto action_63
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_15

action_56 (51) = happyShift action_14
action_56 (52) = happyShift action_62
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (34) = happyShift action_61
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (28) = happyShift action_7
action_58 (31) = happyShift action_8
action_58 (52) = happyShift action_9
action_58 (9) = happyGoto action_3
action_58 (10) = happyGoto action_60
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_13

action_60 (35) = happyShift action_100
action_60 (51) = happyShift action_14
action_60 (8) = happyGoto action_99
action_60 _ = happyReduce_9

action_61 (31) = happyShift action_98
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (48) = happyShift action_97
action_62 (19) = happyGoto action_96
action_62 _ = happyReduce_38

action_63 (55) = happyShift action_41
action_63 (56) = happyShift action_42
action_63 (57) = happyShift action_43
action_63 (58) = happyShift action_44
action_63 (59) = happyShift action_45
action_63 (60) = happyShift action_46
action_63 (61) = happyShift action_47
action_63 (62) = happyShift action_48
action_63 (63) = happyShift action_49
action_63 (64) = happyShift action_50
action_63 (65) = happyShift action_51
action_63 (66) = happyShift action_52
action_63 (68) = happyShift action_54
action_63 _ = happyReduce_56

action_64 (55) = happyShift action_41
action_64 (56) = happyShift action_42
action_64 (57) = happyShift action_43
action_64 (58) = happyShift action_44
action_64 (59) = happyShift action_45
action_64 (60) = happyShift action_46
action_64 (61) = happyShift action_47
action_64 (62) = happyShift action_48
action_64 (63) = happyShift action_49
action_64 (64) = happyShift action_50
action_64 (65) = happyShift action_51
action_64 (66) = happyShift action_52
action_64 (67) = happyShift action_53
action_64 (68) = happyShift action_54
action_64 _ = happyReduce_55

action_65 (55) = happyShift action_41
action_65 (56) = happyShift action_42
action_65 (57) = happyShift action_43
action_65 (58) = happyShift action_44
action_65 (59) = happyShift action_45
action_65 (60) = happyShift action_46
action_65 _ = happyReduce_54

action_66 (55) = happyShift action_41
action_66 (56) = happyShift action_42
action_66 (57) = happyShift action_43
action_66 (58) = happyShift action_44
action_66 (59) = happyShift action_45
action_66 (60) = happyShift action_46
action_66 _ = happyReduce_53

action_67 (55) = happyShift action_41
action_67 (56) = happyShift action_42
action_67 (57) = happyShift action_43
action_67 (58) = happyShift action_44
action_67 (59) = happyShift action_45
action_67 (60) = happyShift action_46
action_67 _ = happyReduce_52

action_68 (55) = happyShift action_41
action_68 (56) = happyShift action_42
action_68 (57) = happyShift action_43
action_68 (58) = happyShift action_44
action_68 (59) = happyShift action_45
action_68 (60) = happyShift action_46
action_68 _ = happyReduce_51

action_69 (55) = happyShift action_41
action_69 (56) = happyShift action_42
action_69 (57) = happyShift action_43
action_69 (58) = happyShift action_44
action_69 (59) = happyShift action_45
action_69 (60) = happyShift action_46
action_69 _ = happyReduce_50

action_70 (55) = happyShift action_41
action_70 (56) = happyShift action_42
action_70 (57) = happyShift action_43
action_70 (58) = happyShift action_44
action_70 (59) = happyShift action_45
action_70 (60) = happyShift action_46
action_70 _ = happyReduce_49

action_71 (59) = happyShift action_45
action_71 _ = happyReduce_47

action_72 (59) = happyShift action_45
action_72 _ = happyReduce_46

action_73 (59) = happyShift action_45
action_73 _ = happyReduce_45

action_74 (59) = happyShift action_45
action_74 _ = happyReduce_44

action_75 (57) = happyShift action_43
action_75 (58) = happyShift action_44
action_75 (59) = happyShift action_45
action_75 (60) = happyShift action_46
action_75 _ = happyReduce_48

action_76 (57) = happyShift action_43
action_76 (58) = happyShift action_44
action_76 (59) = happyShift action_45
action_76 (60) = happyShift action_46
action_76 _ = happyReduce_43

action_77 (25) = happyShift action_17
action_77 (26) = happyShift action_18
action_77 (27) = happyShift action_19
action_77 (29) = happyShift action_20
action_77 (33) = happyShift action_21
action_77 (39) = happyShift action_22
action_77 (47) = happyShift action_23
action_77 (52) = happyShift action_24
action_77 (56) = happyShift action_25
action_77 (69) = happyShift action_26
action_77 (70) = happyShift action_27
action_77 (5) = happyGoto action_15
action_77 (6) = happyGoto action_95
action_77 (20) = happyGoto action_39
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_59

action_79 _ = happyReduce_58

action_80 (25) = happyShift action_90
action_80 (26) = happyShift action_91
action_80 (27) = happyShift action_92
action_80 (31) = happyShift action_93
action_80 (52) = happyShift action_94
action_80 (14) = happyGoto action_88
action_80 (15) = happyGoto action_89
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (25) = happyShift action_17
action_81 (26) = happyShift action_18
action_81 (27) = happyShift action_19
action_81 (29) = happyShift action_20
action_81 (33) = happyShift action_21
action_81 (39) = happyShift action_22
action_81 (47) = happyShift action_23
action_81 (52) = happyShift action_24
action_81 (56) = happyShift action_25
action_81 (69) = happyShift action_26
action_81 (70) = happyShift action_27
action_81 (5) = happyGoto action_15
action_81 (6) = happyGoto action_87
action_81 (20) = happyGoto action_39
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_4

action_83 (34) = happyShift action_86
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (25) = happyShift action_17
action_84 (26) = happyShift action_18
action_84 (27) = happyShift action_19
action_84 (29) = happyShift action_20
action_84 (33) = happyShift action_21
action_84 (39) = happyShift action_22
action_84 (47) = happyShift action_23
action_84 (52) = happyShift action_24
action_84 (56) = happyShift action_25
action_84 (69) = happyShift action_26
action_84 (70) = happyShift action_27
action_84 (5) = happyGoto action_15
action_84 (20) = happyGoto action_85
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (30) = happyShift action_117
action_85 (55) = happyShift action_41
action_85 (56) = happyShift action_42
action_85 (57) = happyShift action_43
action_85 (58) = happyShift action_44
action_85 (59) = happyShift action_45
action_85 (60) = happyShift action_46
action_85 (61) = happyShift action_47
action_85 (62) = happyShift action_48
action_85 (63) = happyShift action_49
action_85 (64) = happyShift action_50
action_85 (65) = happyShift action_51
action_85 (66) = happyShift action_52
action_85 (67) = happyShift action_53
action_85 (68) = happyShift action_54
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_64

action_87 (34) = happyShift action_116
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (37) = happyShift action_115
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (25) = happyShift action_90
action_89 (26) = happyShift action_91
action_89 (27) = happyShift action_92
action_89 (31) = happyShift action_93
action_89 (52) = happyShift action_94
action_89 (14) = happyGoto action_114
action_89 _ = happyReduce_63

action_90 _ = happyReduce_23

action_91 _ = happyReduce_24

action_92 _ = happyReduce_22

action_93 (52) = happyShift action_11
action_93 (7) = happyGoto action_113
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (48) = happyShift action_97
action_94 (19) = happyGoto action_112
action_94 _ = happyReduce_38

action_95 _ = happyReduce_7

action_96 (35) = happyShift action_111
action_96 (21) = happyGoto action_110
action_96 _ = happyReduce_66

action_97 (49) = happyShift action_109
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (25) = happyShift action_17
action_98 (26) = happyShift action_18
action_98 (27) = happyShift action_19
action_98 (29) = happyShift action_20
action_98 (33) = happyShift action_21
action_98 (39) = happyShift action_22
action_98 (43) = happyShift action_107
action_98 (44) = happyShift action_108
action_98 (47) = happyShift action_23
action_98 (52) = happyShift action_24
action_98 (56) = happyShift action_25
action_98 (69) = happyShift action_26
action_98 (70) = happyShift action_27
action_98 (5) = happyGoto action_102
action_98 (16) = happyGoto action_103
action_98 (17) = happyGoto action_104
action_98 (18) = happyGoto action_105
action_98 (20) = happyGoto action_106
action_98 _ = happyReduce_36

action_99 _ = happyReduce_8

action_100 (52) = happyShift action_11
action_100 (7) = happyGoto action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_10

action_102 (38) = happyShift action_126
action_102 _ = happyReduce_39

action_103 _ = happyReduce_34

action_104 (42) = happyShift action_125
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (32) = happyShift action_124
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (55) = happyShift action_41
action_106 (56) = happyShift action_42
action_106 (57) = happyShift action_43
action_106 (58) = happyShift action_44
action_106 (59) = happyShift action_45
action_106 (60) = happyShift action_46
action_106 (61) = happyShift action_47
action_106 (62) = happyShift action_48
action_106 (63) = happyShift action_49
action_106 (64) = happyShift action_50
action_106 (65) = happyShift action_51
action_106 (66) = happyShift action_52
action_106 (67) = happyShift action_53
action_106 (68) = happyShift action_54
action_106 _ = happyReduce_29

action_107 (33) = happyShift action_123
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (33) = happyShift action_122
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_37

action_110 _ = happyReduce_67

action_111 (28) = happyShift action_7
action_111 (31) = happyShift action_8
action_111 (52) = happyShift action_9
action_111 (9) = happyGoto action_3
action_111 (10) = happyGoto action_56
action_111 (22) = happyGoto action_121
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_25

action_113 (32) = happyShift action_120
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (37) = happyShift action_119
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (31) = happyShift action_118
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_40

action_117 _ = happyReduce_2

action_118 (25) = happyShift action_17
action_118 (26) = happyShift action_18
action_118 (27) = happyShift action_19
action_118 (29) = happyShift action_20
action_118 (33) = happyShift action_21
action_118 (39) = happyShift action_22
action_118 (43) = happyShift action_107
action_118 (44) = happyShift action_108
action_118 (47) = happyShift action_23
action_118 (52) = happyShift action_24
action_118 (56) = happyShift action_25
action_118 (69) = happyShift action_26
action_118 (70) = happyShift action_27
action_118 (5) = happyGoto action_102
action_118 (16) = happyGoto action_103
action_118 (17) = happyGoto action_104
action_118 (18) = happyGoto action_132
action_118 (20) = happyGoto action_106
action_118 _ = happyReduce_36

action_119 (31) = happyShift action_131
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_26

action_121 _ = happyReduce_65

action_122 (25) = happyShift action_17
action_122 (26) = happyShift action_18
action_122 (27) = happyShift action_19
action_122 (29) = happyShift action_20
action_122 (33) = happyShift action_21
action_122 (39) = happyShift action_22
action_122 (47) = happyShift action_23
action_122 (52) = happyShift action_24
action_122 (56) = happyShift action_25
action_122 (69) = happyShift action_26
action_122 (70) = happyShift action_27
action_122 (5) = happyGoto action_15
action_122 (20) = happyGoto action_130
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (25) = happyShift action_90
action_123 (26) = happyShift action_91
action_123 (27) = happyShift action_92
action_123 (31) = happyShift action_93
action_123 (52) = happyShift action_94
action_123 (14) = happyGoto action_88
action_123 (15) = happyGoto action_129
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_68

action_125 (25) = happyShift action_17
action_125 (26) = happyShift action_18
action_125 (27) = happyShift action_19
action_125 (29) = happyShift action_20
action_125 (33) = happyShift action_21
action_125 (39) = happyShift action_22
action_125 (43) = happyShift action_107
action_125 (44) = happyShift action_108
action_125 (47) = happyShift action_23
action_125 (52) = happyShift action_24
action_125 (56) = happyShift action_25
action_125 (69) = happyShift action_26
action_125 (70) = happyShift action_27
action_125 (5) = happyGoto action_102
action_125 (16) = happyGoto action_128
action_125 (20) = happyGoto action_106
action_125 _ = happyReduce_35

action_126 (25) = happyShift action_17
action_126 (26) = happyShift action_18
action_126 (27) = happyShift action_19
action_126 (29) = happyShift action_20
action_126 (33) = happyShift action_21
action_126 (39) = happyShift action_22
action_126 (47) = happyShift action_23
action_126 (52) = happyShift action_24
action_126 (56) = happyShift action_25
action_126 (69) = happyShift action_26
action_126 (70) = happyShift action_27
action_126 (5) = happyGoto action_15
action_126 (20) = happyGoto action_127
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (55) = happyShift action_41
action_127 (56) = happyShift action_42
action_127 (57) = happyShift action_43
action_127 (58) = happyShift action_44
action_127 (59) = happyShift action_45
action_127 (60) = happyShift action_46
action_127 (61) = happyShift action_47
action_127 (62) = happyShift action_48
action_127 (63) = happyShift action_49
action_127 (64) = happyShift action_50
action_127 (65) = happyShift action_51
action_127 (66) = happyShift action_52
action_127 (67) = happyShift action_53
action_127 (68) = happyShift action_54
action_127 _ = happyReduce_32

action_128 _ = happyReduce_33

action_129 (25) = happyShift action_90
action_129 (26) = happyShift action_91
action_129 (27) = happyShift action_92
action_129 (31) = happyShift action_93
action_129 (36) = happyShift action_136
action_129 (52) = happyShift action_94
action_129 (14) = happyGoto action_114
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (34) = happyShift action_135
action_130 (55) = happyShift action_41
action_130 (56) = happyShift action_42
action_130 (57) = happyShift action_43
action_130 (58) = happyShift action_44
action_130 (59) = happyShift action_45
action_130 (60) = happyShift action_46
action_130 (61) = happyShift action_47
action_130 (62) = happyShift action_48
action_130 (63) = happyShift action_49
action_130 (64) = happyShift action_50
action_130 (65) = happyShift action_51
action_130 (66) = happyShift action_52
action_130 (67) = happyShift action_53
action_130 (68) = happyShift action_54
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (25) = happyShift action_17
action_131 (26) = happyShift action_18
action_131 (27) = happyShift action_19
action_131 (29) = happyShift action_20
action_131 (33) = happyShift action_21
action_131 (39) = happyShift action_22
action_131 (43) = happyShift action_107
action_131 (44) = happyShift action_108
action_131 (47) = happyShift action_23
action_131 (52) = happyShift action_24
action_131 (56) = happyShift action_25
action_131 (69) = happyShift action_26
action_131 (70) = happyShift action_27
action_131 (5) = happyGoto action_102
action_131 (16) = happyGoto action_103
action_131 (17) = happyGoto action_104
action_131 (18) = happyGoto action_134
action_131 (20) = happyGoto action_106
action_131 _ = happyReduce_36

action_132 (32) = happyShift action_133
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_27

action_134 (32) = happyShift action_139
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (31) = happyShift action_138
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (25) = happyShift action_17
action_136 (26) = happyShift action_18
action_136 (27) = happyShift action_19
action_136 (29) = happyShift action_20
action_136 (33) = happyShift action_21
action_136 (39) = happyShift action_22
action_136 (47) = happyShift action_23
action_136 (52) = happyShift action_24
action_136 (56) = happyShift action_25
action_136 (69) = happyShift action_26
action_136 (70) = happyShift action_27
action_136 (5) = happyGoto action_15
action_136 (20) = happyGoto action_137
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (34) = happyShift action_146
action_137 (55) = happyShift action_41
action_137 (56) = happyShift action_42
action_137 (57) = happyShift action_43
action_137 (58) = happyShift action_44
action_137 (59) = happyShift action_45
action_137 (60) = happyShift action_46
action_137 (61) = happyShift action_47
action_137 (62) = happyShift action_48
action_137 (63) = happyShift action_49
action_137 (64) = happyShift action_50
action_137 (65) = happyShift action_51
action_137 (66) = happyShift action_52
action_137 (67) = happyShift action_53
action_137 (68) = happyShift action_54
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (25) = happyShift action_17
action_138 (26) = happyShift action_18
action_138 (27) = happyShift action_19
action_138 (29) = happyShift action_20
action_138 (33) = happyShift action_21
action_138 (39) = happyShift action_22
action_138 (43) = happyShift action_107
action_138 (44) = happyShift action_108
action_138 (45) = happyShift action_144
action_138 (46) = happyShift action_145
action_138 (47) = happyShift action_23
action_138 (52) = happyShift action_24
action_138 (56) = happyShift action_25
action_138 (69) = happyShift action_26
action_138 (70) = happyShift action_27
action_138 (5) = happyGoto action_102
action_138 (11) = happyGoto action_140
action_138 (12) = happyGoto action_141
action_138 (13) = happyGoto action_142
action_138 (16) = happyGoto action_143
action_138 (20) = happyGoto action_106
action_138 _ = happyFail (happyExpListPerState 138)

action_139 _ = happyReduce_28

action_140 _ = happyReduce_20

action_141 (42) = happyShift action_149
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (32) = happyShift action_148
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_18

action_144 _ = happyReduce_17

action_145 _ = happyReduce_16

action_146 (31) = happyShift action_147
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (25) = happyShift action_17
action_147 (26) = happyShift action_18
action_147 (27) = happyShift action_19
action_147 (29) = happyShift action_20
action_147 (33) = happyShift action_21
action_147 (39) = happyShift action_22
action_147 (43) = happyShift action_107
action_147 (44) = happyShift action_108
action_147 (45) = happyShift action_144
action_147 (46) = happyShift action_145
action_147 (47) = happyShift action_23
action_147 (52) = happyShift action_24
action_147 (56) = happyShift action_25
action_147 (69) = happyShift action_26
action_147 (70) = happyShift action_27
action_147 (5) = happyGoto action_102
action_147 (11) = happyGoto action_140
action_147 (12) = happyGoto action_141
action_147 (13) = happyGoto action_151
action_147 (16) = happyGoto action_143
action_147 (20) = happyGoto action_106
action_147 _ = happyFail (happyExpListPerState 147)

action_148 _ = happyReduce_31

action_149 (25) = happyShift action_17
action_149 (26) = happyShift action_18
action_149 (27) = happyShift action_19
action_149 (29) = happyShift action_20
action_149 (33) = happyShift action_21
action_149 (39) = happyShift action_22
action_149 (43) = happyShift action_107
action_149 (44) = happyShift action_108
action_149 (45) = happyShift action_144
action_149 (46) = happyShift action_145
action_149 (47) = happyShift action_23
action_149 (52) = happyShift action_24
action_149 (56) = happyShift action_25
action_149 (69) = happyShift action_26
action_149 (70) = happyShift action_27
action_149 (5) = happyGoto action_102
action_149 (11) = happyGoto action_150
action_149 (16) = happyGoto action_143
action_149 (20) = happyGoto action_106
action_149 _ = happyReduce_21

action_150 _ = happyReduce_19

action_151 (32) = happyShift action_152
action_151 _ = happyFail (happyExpListPerState 151)

action_152 _ = happyReduce_30

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	_
	_
	 =  HappyAbsSyn4
		 (undefined
	)

happyReduce_2 = happyReduce 4 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (undefined
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 _
	_
	 =  HappyAbsSyn5
		 (undefined
	)

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 _
	_
	_
	 =  HappyAbsSyn5
		 (undefined
	)

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn5
		 (undefined
	)

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn6
		 (undefined
	)

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 _
	_
	_
	 =  HappyAbsSyn6
		 (undefined
	)

happyReduce_8 = happyReduce 4 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((\(LIdentifier t p) ty rs -> PRecord (t,ty,p) rs p) happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_0  8 happyReduction_9
happyReduction_9  =  HappyAbsSyn8
		 ([]
	)

happyReduce_10 = happySpecReduce_2  8 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 ((\(PRecord r rs _) -> r:rs) happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LAtom t p) -> PAtom t p
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LIdentifier t p) -> PId t p
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (PUnion happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn11
		 (undefined
	)

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn11
		 (undefined
	)

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn11
		 (undefined
	)

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 _
	_
	_
	 =  HappyAbsSyn12
		 (undefined
	)

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn12
		 (undefined
	)

happyReduce_21 = happySpecReduce_2  13 happyReduction_21
happyReduction_21 _
	_
	 =  HappyAbsSyn13
		 (undefined
	)

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn14
		 (undefined
	)

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn14
		 (undefined
	)

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn14
		 (undefined
	)

happyReduce_25 = happySpecReduce_2  14 happyReduction_25
happyReduction_25 _
	_
	 =  HappyAbsSyn14
		 (undefined
	)

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 _
	_
	_
	 =  HappyAbsSyn14
		 (undefined
	)

happyReduce_27 = happyReduce 5 15 happyReduction_27
happyReduction_27 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (undefined
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 15 happyReduction_28
happyReduction_28 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (undefined
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn16
		 (undefined
	)

happyReduce_30 = happyReduce 9 16 happyReduction_30
happyReduction_30 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (undefined
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 7 16 happyReduction_31
happyReduction_31 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (undefined
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 _
	_
	_
	 =  HappyAbsSyn16
		 (undefined
	)

happyReduce_33 = happySpecReduce_3  17 happyReduction_33
happyReduction_33 _
	_
	_
	 =  HappyAbsSyn17
		 (undefined
	)

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn17
		 (undefined
	)

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 _
	_
	 =  HappyAbsSyn18
		 (undefined
	)

happyReduce_36 = happySpecReduce_0  18 happyReduction_36
happyReduction_36  =  HappyAbsSyn18
		 (undefined
	)

happyReduce_37 = happySpecReduce_2  19 happyReduction_37
happyReduction_37 _
	_
	 =  HappyAbsSyn19
		 (undefined
	)

happyReduce_38 = happySpecReduce_0  19 happyReduction_38
happyReduction_38  =  HappyAbsSyn19
		 (undefined
	)

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_40 = happyReduce 5 20 happyReduction_40
happyReduction_40 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (undefined
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_2  20 happyReduction_41
happyReduction_41 _
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_42 = happySpecReduce_2  20 happyReduction_42
happyReduction_42 _
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_43 = happySpecReduce_3  20 happyReduction_43
happyReduction_43 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_44 = happySpecReduce_3  20 happyReduction_44
happyReduction_44 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_45 = happySpecReduce_3  20 happyReduction_45
happyReduction_45 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_46 = happySpecReduce_3  20 happyReduction_46
happyReduction_46 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_47 = happySpecReduce_3  20 happyReduction_47
happyReduction_47 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_48 = happySpecReduce_3  20 happyReduction_48
happyReduction_48 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_49 = happySpecReduce_3  20 happyReduction_49
happyReduction_49 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_50 = happySpecReduce_3  20 happyReduction_50
happyReduction_50 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_51 = happySpecReduce_3  20 happyReduction_51
happyReduction_51 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_52 = happySpecReduce_3  20 happyReduction_52
happyReduction_52 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_53 = happySpecReduce_3  20 happyReduction_53
happyReduction_53 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_54 = happySpecReduce_3  20 happyReduction_54
happyReduction_54 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_55 = happySpecReduce_3  20 happyReduction_55
happyReduction_55 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_56 = happySpecReduce_3  20 happyReduction_56
happyReduction_56 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_57 = happySpecReduce_2  20 happyReduction_57
happyReduction_57 _
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_58 = happySpecReduce_3  20 happyReduction_58
happyReduction_58 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_59 = happySpecReduce_3  20 happyReduction_59
happyReduction_59 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_60 = happySpecReduce_1  20 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_61 = happySpecReduce_1  20 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_62 = happySpecReduce_1  20 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_63 = happyReduce 4 20 happyReduction_63
happyReduction_63 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (undefined
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 4 20 happyReduction_64
happyReduction_64 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (undefined
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_2  21 happyReduction_65
happyReduction_65 _
	_
	 =  HappyAbsSyn21
		 (undefined
	)

happyReduce_66 = happySpecReduce_0  21 happyReduction_66
happyReduction_66  =  HappyAbsSyn21
		 (undefined
	)

happyReduce_67 = happyReduce 4 22 happyReduction_67
happyReduction_67 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (undefined
	) `HappyStk` happyRest

happyReduce_68 = happyReduce 7 23 happyReduction_68
happyReduction_68 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (undefined
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_2  24 happyReduction_69
happyReduction_69 _
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_70 = happySpecReduce_1  24 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn24
		 (undefined
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	LEOF -> action 71 71 tk (HappyState action) sts stk;
	LChar _ _ -> cont 25;
	LString _ _ -> cont 26;
	LNumber _ _ -> cont 27;
	LAtom _ _ -> cont 28;
	LOBckt _ -> cont 29;
	LCBckt _ -> cont 30;
	LOBrc _ -> cont 31;
	LCBrc _ -> cont 32;
	LOParen _ -> cont 33;
	LCParen _ -> cont 34;
	LComma _ -> cont 35;
	LColon _ -> cont 36;
	LFatArrow _ -> cont 37;
	LAssign _ -> cont 38;
	LMatch _ -> cont 39;
	LWith _ -> cont 40;
	LType _ -> cont 41;
	LSemiColon _ -> cont 42;
	LFor _ -> cont 43;
	LWhile _ -> cont 44;
	LContinue _ -> cont 45;
	LBreak _ -> cont 46;
	LNew _ -> cont 47;
	LBy _ -> cont 48;
	LReference _ -> cont 49;
	LOp _ _ -> cont 50;
	LVBar _ -> cont 51;
	LIdentifier _ _ -> cont 52;
	LDot _ -> cont 53;
	LEOF -> cont 54;
	LOp "+" _ -> cont 55;
	LOp "-" _ -> cont 56;
	LOp "*" _ -> cont 57;
	LOp "/" _ -> cont 58;
	LOp "^" _ -> cont 59;
	LOp "%" _ -> cont 60;
	LOp "<" _ -> cont 61;
	LOp ">" _ -> cont 62;
	LOp "!=" _ -> cont 63;
	LOp "==" _ -> cont 64;
	LOp ">=" _ -> cont 65;
	LOp "<=" _ -> cont 66;
	LOp "||" _ -> cont 67;
	LOp "&&" _ -> cont 68;
	LOp "~"  _ -> cont 69;
	LOp "&" _ -> cont 70;
	_ -> happyError' (tk, [])
	})

happyError_ explist 71 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (>>=)
happyReturn :: () => a -> Alex a
happyReturn = (pure)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parse = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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

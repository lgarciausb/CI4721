{-# OPTIONS_GHC -w #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
module Parser.Parser where 

import Parser.Lexer
import Parser.LexerDefinitions
import Data.Text (Text)
import Data.Text qualified as T
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
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
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,381) ([0,4096,0,0,0,0,16,0,0,0,5888,16449,136,80,0,16,0,0,0,5888,16449,136,80,0,32,49152,4095,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16663,34880,20480,0,5888,16449,136,80,0,16663,34880,20480,0,18432,0,8,0,0,0,0,0,0,5888,16449,136,80,0,16663,34880,20480,0,0,0,8,0,0,0,0,0,0,4096,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,4,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,128,65472,15,0,512,49152,4095,0,8192,0,0,0,0,1024,49152,4095,0,0,0,0,0,0,16663,34880,20480,0,5888,16449,136,80,0,16663,34880,20480,0,5888,16449,136,80,0,16663,34880,20480,0,5888,16449,136,80,0,16663,34880,20480,0,5888,16449,136,80,0,16663,34880,20480,0,5888,16449,136,80,0,16663,34880,20480,0,5888,16449,136,80,0,16663,34880,20480,0,5888,16449,136,80,0,32,49152,4095,0,0,0,0,0,0,0,49152,3071,0,0,0,65472,15,0,0,49152,15,0,0,0,4032,0,0,0,49152,15,0,0,0,4032,0,0,0,49152,15,0,0,0,4032,0,0,0,0,4,0,0,0,1024,0,0,0,0,4,0,0,0,1024,0,0,0,0,15,0,0,0,3840,0,0,16663,34880,20480,0,0,0,0,0,0,0,0,0,0,18176,0,8,0,0,128,0,0,0,0,8,0,0,0,16663,34880,20480,0,18432,0,8,0,0,16,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,18432,0,8,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,32768,0,0,0,0,64,0,0,0,0,4,4,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,5888,19521,136,80,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,2,0,0,32768,0,0,0,0,0,49152,4095,0,0,1,0,0,0,256,0,0,0,5888,16449,24,80,0,0,0,0,0,0,0,65472,15,0,16663,34880,20480,0,5888,16449,136,80,0,71,2048,0,0,18176,0,8,0,0,0,0,0,0,5888,16449,136,80,0,0,49152,4095,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,512,49152,4095,0,0,4,65472,15,0,64,0,0,0,5888,16449,136,80,0,512,49152,4095,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,512,0,0,0,0,2,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","ixs","lvaluable","args","records","mRecords","T0","T","moreArgs","fun_args","loop_a","loop_as","optionalByRef","pattern","function_return","patterns","a0","as","actions","function_def","function_defs","e","char","string","number","atom","'['","']'","'{'","'}'","'('","')'","','","':'","'=>'","':='","match","with","type","';'","for","while","continue","break","new","by","reference","'***'","'|'","identifier","'.'","EOF","'+'","'-'","'*'","'/'","'^'","'%'","'<'","'>'","'!='","'=='","'>='","'<='","'||'","'&&'","'~'","NEG","'&'","%eof"]
        bit_start = st Prelude.* 72
        bit_end = (st Prelude.+ 1) Prelude.* 72
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..71]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (29) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (29) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (25) = happyShift action_6
action_2 (26) = happyShift action_7
action_2 (27) = happyShift action_8
action_2 (29) = happyShift action_9
action_2 (33) = happyShift action_10
action_2 (39) = happyShift action_11
action_2 (47) = happyShift action_12
action_2 (52) = happyShift action_13
action_2 (56) = happyShift action_14
action_2 (69) = happyShift action_15
action_2 (71) = happyShift action_16
action_2 (24) = happyGoto action_5
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (29) = happyShift action_4
action_3 (72) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (25) = happyShift action_6
action_4 (26) = happyShift action_7
action_4 (27) = happyShift action_8
action_4 (29) = happyShift action_9
action_4 (33) = happyShift action_10
action_4 (39) = happyShift action_11
action_4 (47) = happyShift action_12
action_4 (52) = happyShift action_13
action_4 (56) = happyShift action_14
action_4 (69) = happyShift action_15
action_4 (71) = happyShift action_16
action_4 (24) = happyGoto action_45
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (30) = happyShift action_30
action_5 (55) = happyShift action_31
action_5 (56) = happyShift action_32
action_5 (57) = happyShift action_33
action_5 (58) = happyShift action_34
action_5 (59) = happyShift action_35
action_5 (60) = happyShift action_36
action_5 (61) = happyShift action_37
action_5 (62) = happyShift action_38
action_5 (63) = happyShift action_39
action_5 (64) = happyShift action_40
action_5 (65) = happyShift action_41
action_5 (66) = happyShift action_42
action_5 (67) = happyShift action_43
action_5 (68) = happyShift action_44
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_69

action_7 _ = happyReduce_68

action_8 _ = happyReduce_67

action_9 (25) = happyShift action_6
action_9 (26) = happyShift action_7
action_9 (27) = happyShift action_8
action_9 (29) = happyShift action_9
action_9 (33) = happyShift action_10
action_9 (39) = happyShift action_11
action_9 (47) = happyShift action_12
action_9 (52) = happyShift action_13
action_9 (56) = happyShift action_14
action_9 (69) = happyShift action_15
action_9 (71) = happyShift action_16
action_9 (6) = happyGoto action_28
action_9 (24) = happyGoto action_29
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (25) = happyShift action_6
action_10 (26) = happyShift action_7
action_10 (27) = happyShift action_8
action_10 (29) = happyShift action_9
action_10 (33) = happyShift action_10
action_10 (39) = happyShift action_11
action_10 (47) = happyShift action_12
action_10 (52) = happyShift action_13
action_10 (56) = happyShift action_14
action_10 (69) = happyShift action_15
action_10 (71) = happyShift action_16
action_10 (24) = happyGoto action_27
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (25) = happyShift action_6
action_11 (26) = happyShift action_7
action_11 (27) = happyShift action_8
action_11 (29) = happyShift action_9
action_11 (33) = happyShift action_10
action_11 (39) = happyShift action_11
action_11 (47) = happyShift action_12
action_11 (52) = happyShift action_13
action_11 (56) = happyShift action_14
action_11 (69) = happyShift action_15
action_11 (71) = happyShift action_16
action_11 (24) = happyGoto action_26
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (28) = happyShift action_23
action_12 (31) = happyShift action_24
action_12 (52) = happyShift action_25
action_12 (9) = happyGoto action_21
action_12 (10) = happyGoto action_22
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_46

action_14 (25) = happyShift action_6
action_14 (26) = happyShift action_7
action_14 (27) = happyShift action_8
action_14 (29) = happyShift action_9
action_14 (33) = happyShift action_10
action_14 (39) = happyShift action_11
action_14 (47) = happyShift action_12
action_14 (52) = happyShift action_13
action_14 (56) = happyShift action_14
action_14 (69) = happyShift action_15
action_14 (71) = happyShift action_16
action_14 (24) = happyGoto action_20
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (25) = happyShift action_6
action_15 (26) = happyShift action_7
action_15 (27) = happyShift action_8
action_15 (29) = happyShift action_9
action_15 (33) = happyShift action_10
action_15 (39) = happyShift action_11
action_15 (47) = happyShift action_12
action_15 (52) = happyShift action_13
action_15 (56) = happyShift action_14
action_15 (69) = happyShift action_15
action_15 (71) = happyShift action_16
action_15 (24) = happyGoto action_19
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (52) = happyShift action_18
action_16 (5) = happyGoto action_17
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_48

action_18 (29) = happyShift action_2
action_18 (53) = happyShift action_70
action_18 (4) = happyGoto action_69
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_64

action_20 _ = happyReduce_49

action_21 _ = happyReduce_13

action_22 (33) = happyShift action_67
action_22 (51) = happyShift action_68
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_10

action_24 (52) = happyShift action_66
action_24 (7) = happyGoto action_65
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_11

action_26 (40) = happyShift action_64
action_26 (55) = happyShift action_31
action_26 (56) = happyShift action_32
action_26 (57) = happyShift action_33
action_26 (58) = happyShift action_34
action_26 (59) = happyShift action_35
action_26 (60) = happyShift action_36
action_26 (61) = happyShift action_37
action_26 (62) = happyShift action_38
action_26 (63) = happyShift action_39
action_26 (64) = happyShift action_40
action_26 (65) = happyShift action_41
action_26 (66) = happyShift action_42
action_26 (67) = happyShift action_43
action_26 (68) = happyShift action_44
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (34) = happyShift action_63
action_27 (55) = happyShift action_31
action_27 (56) = happyShift action_32
action_27 (57) = happyShift action_33
action_27 (58) = happyShift action_34
action_27 (59) = happyShift action_35
action_27 (60) = happyShift action_36
action_27 (61) = happyShift action_37
action_27 (62) = happyShift action_38
action_27 (63) = happyShift action_39
action_27 (64) = happyShift action_40
action_27 (65) = happyShift action_41
action_27 (66) = happyShift action_42
action_27 (67) = happyShift action_43
action_27 (68) = happyShift action_44
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (30) = happyShift action_62
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (35) = happyShift action_61
action_29 (55) = happyShift action_31
action_29 (56) = happyShift action_32
action_29 (57) = happyShift action_33
action_29 (58) = happyShift action_34
action_29 (59) = happyShift action_35
action_29 (60) = happyShift action_36
action_29 (61) = happyShift action_37
action_29 (62) = happyShift action_38
action_29 (63) = happyShift action_39
action_29 (64) = happyShift action_40
action_29 (65) = happyShift action_41
action_29 (66) = happyShift action_42
action_29 (67) = happyShift action_43
action_29 (68) = happyShift action_44
action_29 _ = happyReduce_5

action_30 _ = happyReduce_1

action_31 (25) = happyShift action_6
action_31 (26) = happyShift action_7
action_31 (27) = happyShift action_8
action_31 (29) = happyShift action_9
action_31 (33) = happyShift action_10
action_31 (39) = happyShift action_11
action_31 (47) = happyShift action_12
action_31 (52) = happyShift action_13
action_31 (56) = happyShift action_14
action_31 (69) = happyShift action_15
action_31 (71) = happyShift action_16
action_31 (24) = happyGoto action_60
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (25) = happyShift action_6
action_32 (26) = happyShift action_7
action_32 (27) = happyShift action_8
action_32 (29) = happyShift action_9
action_32 (33) = happyShift action_10
action_32 (39) = happyShift action_11
action_32 (47) = happyShift action_12
action_32 (52) = happyShift action_13
action_32 (56) = happyShift action_14
action_32 (69) = happyShift action_15
action_32 (71) = happyShift action_16
action_32 (24) = happyGoto action_59
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (25) = happyShift action_6
action_33 (26) = happyShift action_7
action_33 (27) = happyShift action_8
action_33 (29) = happyShift action_9
action_33 (33) = happyShift action_10
action_33 (39) = happyShift action_11
action_33 (47) = happyShift action_12
action_33 (52) = happyShift action_13
action_33 (56) = happyShift action_14
action_33 (69) = happyShift action_15
action_33 (71) = happyShift action_16
action_33 (24) = happyGoto action_58
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (25) = happyShift action_6
action_34 (26) = happyShift action_7
action_34 (27) = happyShift action_8
action_34 (29) = happyShift action_9
action_34 (33) = happyShift action_10
action_34 (39) = happyShift action_11
action_34 (47) = happyShift action_12
action_34 (52) = happyShift action_13
action_34 (56) = happyShift action_14
action_34 (69) = happyShift action_15
action_34 (71) = happyShift action_16
action_34 (24) = happyGoto action_57
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (25) = happyShift action_6
action_35 (26) = happyShift action_7
action_35 (27) = happyShift action_8
action_35 (29) = happyShift action_9
action_35 (33) = happyShift action_10
action_35 (39) = happyShift action_11
action_35 (47) = happyShift action_12
action_35 (52) = happyShift action_13
action_35 (56) = happyShift action_14
action_35 (69) = happyShift action_15
action_35 (71) = happyShift action_16
action_35 (24) = happyGoto action_56
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (25) = happyShift action_6
action_36 (26) = happyShift action_7
action_36 (27) = happyShift action_8
action_36 (29) = happyShift action_9
action_36 (33) = happyShift action_10
action_36 (39) = happyShift action_11
action_36 (47) = happyShift action_12
action_36 (52) = happyShift action_13
action_36 (56) = happyShift action_14
action_36 (69) = happyShift action_15
action_36 (71) = happyShift action_16
action_36 (24) = happyGoto action_55
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (25) = happyShift action_6
action_37 (26) = happyShift action_7
action_37 (27) = happyShift action_8
action_37 (29) = happyShift action_9
action_37 (33) = happyShift action_10
action_37 (39) = happyShift action_11
action_37 (47) = happyShift action_12
action_37 (52) = happyShift action_13
action_37 (56) = happyShift action_14
action_37 (69) = happyShift action_15
action_37 (71) = happyShift action_16
action_37 (24) = happyGoto action_54
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (25) = happyShift action_6
action_38 (26) = happyShift action_7
action_38 (27) = happyShift action_8
action_38 (29) = happyShift action_9
action_38 (33) = happyShift action_10
action_38 (39) = happyShift action_11
action_38 (47) = happyShift action_12
action_38 (52) = happyShift action_13
action_38 (56) = happyShift action_14
action_38 (69) = happyShift action_15
action_38 (71) = happyShift action_16
action_38 (24) = happyGoto action_53
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (25) = happyShift action_6
action_39 (26) = happyShift action_7
action_39 (27) = happyShift action_8
action_39 (29) = happyShift action_9
action_39 (33) = happyShift action_10
action_39 (39) = happyShift action_11
action_39 (47) = happyShift action_12
action_39 (52) = happyShift action_13
action_39 (56) = happyShift action_14
action_39 (69) = happyShift action_15
action_39 (71) = happyShift action_16
action_39 (24) = happyGoto action_52
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (25) = happyShift action_6
action_40 (26) = happyShift action_7
action_40 (27) = happyShift action_8
action_40 (29) = happyShift action_9
action_40 (33) = happyShift action_10
action_40 (39) = happyShift action_11
action_40 (47) = happyShift action_12
action_40 (52) = happyShift action_13
action_40 (56) = happyShift action_14
action_40 (69) = happyShift action_15
action_40 (71) = happyShift action_16
action_40 (24) = happyGoto action_51
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (25) = happyShift action_6
action_41 (26) = happyShift action_7
action_41 (27) = happyShift action_8
action_41 (29) = happyShift action_9
action_41 (33) = happyShift action_10
action_41 (39) = happyShift action_11
action_41 (47) = happyShift action_12
action_41 (52) = happyShift action_13
action_41 (56) = happyShift action_14
action_41 (69) = happyShift action_15
action_41 (71) = happyShift action_16
action_41 (24) = happyGoto action_50
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (25) = happyShift action_6
action_42 (26) = happyShift action_7
action_42 (27) = happyShift action_8
action_42 (29) = happyShift action_9
action_42 (33) = happyShift action_10
action_42 (39) = happyShift action_11
action_42 (47) = happyShift action_12
action_42 (52) = happyShift action_13
action_42 (56) = happyShift action_14
action_42 (69) = happyShift action_15
action_42 (71) = happyShift action_16
action_42 (24) = happyGoto action_49
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (25) = happyShift action_6
action_43 (26) = happyShift action_7
action_43 (27) = happyShift action_8
action_43 (29) = happyShift action_9
action_43 (33) = happyShift action_10
action_43 (39) = happyShift action_11
action_43 (47) = happyShift action_12
action_43 (52) = happyShift action_13
action_43 (56) = happyShift action_14
action_43 (69) = happyShift action_15
action_43 (71) = happyShift action_16
action_43 (24) = happyGoto action_48
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (25) = happyShift action_6
action_44 (26) = happyShift action_7
action_44 (27) = happyShift action_8
action_44 (29) = happyShift action_9
action_44 (33) = happyShift action_10
action_44 (39) = happyShift action_11
action_44 (47) = happyShift action_12
action_44 (52) = happyShift action_13
action_44 (56) = happyShift action_14
action_44 (69) = happyShift action_15
action_44 (71) = happyShift action_16
action_44 (24) = happyGoto action_47
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (30) = happyShift action_46
action_45 (55) = happyShift action_31
action_45 (56) = happyShift action_32
action_45 (57) = happyShift action_33
action_45 (58) = happyShift action_34
action_45 (59) = happyShift action_35
action_45 (60) = happyShift action_36
action_45 (61) = happyShift action_37
action_45 (62) = happyShift action_38
action_45 (63) = happyShift action_39
action_45 (64) = happyShift action_40
action_45 (65) = happyShift action_41
action_45 (66) = happyShift action_42
action_45 (67) = happyShift action_43
action_45 (68) = happyShift action_44
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_2

action_47 (55) = happyShift action_31
action_47 (56) = happyShift action_32
action_47 (57) = happyShift action_33
action_47 (58) = happyShift action_34
action_47 (59) = happyShift action_35
action_47 (60) = happyShift action_36
action_47 (61) = happyShift action_37
action_47 (62) = happyShift action_38
action_47 (63) = happyShift action_39
action_47 (64) = happyShift action_40
action_47 (65) = happyShift action_41
action_47 (66) = happyShift action_42
action_47 (68) = happyShift action_44
action_47 _ = happyReduce_63

action_48 (55) = happyShift action_31
action_48 (56) = happyShift action_32
action_48 (57) = happyShift action_33
action_48 (58) = happyShift action_34
action_48 (59) = happyShift action_35
action_48 (60) = happyShift action_36
action_48 (61) = happyShift action_37
action_48 (62) = happyShift action_38
action_48 (63) = happyShift action_39
action_48 (64) = happyShift action_40
action_48 (65) = happyShift action_41
action_48 (66) = happyShift action_42
action_48 (67) = happyShift action_43
action_48 (68) = happyShift action_44
action_48 _ = happyReduce_62

action_49 (55) = happyShift action_31
action_49 (56) = happyShift action_32
action_49 (57) = happyShift action_33
action_49 (58) = happyShift action_34
action_49 (59) = happyShift action_35
action_49 (60) = happyShift action_36
action_49 _ = happyReduce_61

action_50 (55) = happyShift action_31
action_50 (56) = happyShift action_32
action_50 (57) = happyShift action_33
action_50 (58) = happyShift action_34
action_50 (59) = happyShift action_35
action_50 (60) = happyShift action_36
action_50 _ = happyReduce_60

action_51 (55) = happyShift action_31
action_51 (56) = happyShift action_32
action_51 (57) = happyShift action_33
action_51 (58) = happyShift action_34
action_51 (59) = happyShift action_35
action_51 (60) = happyShift action_36
action_51 _ = happyReduce_59

action_52 (55) = happyShift action_31
action_52 (56) = happyShift action_32
action_52 (57) = happyShift action_33
action_52 (58) = happyShift action_34
action_52 (59) = happyShift action_35
action_52 (60) = happyShift action_36
action_52 _ = happyReduce_58

action_53 (55) = happyShift action_31
action_53 (56) = happyShift action_32
action_53 (57) = happyShift action_33
action_53 (58) = happyShift action_34
action_53 (59) = happyShift action_35
action_53 (60) = happyShift action_36
action_53 _ = happyReduce_57

action_54 (55) = happyShift action_31
action_54 (56) = happyShift action_32
action_54 (57) = happyShift action_33
action_54 (58) = happyShift action_34
action_54 (59) = happyShift action_35
action_54 (60) = happyShift action_36
action_54 _ = happyReduce_56

action_55 (59) = happyShift action_35
action_55 _ = happyReduce_54

action_56 (59) = happyShift action_35
action_56 _ = happyReduce_53

action_57 (59) = happyShift action_35
action_57 _ = happyReduce_52

action_58 (59) = happyShift action_35
action_58 _ = happyReduce_51

action_59 (57) = happyShift action_33
action_59 (58) = happyShift action_34
action_59 (59) = happyShift action_35
action_59 (60) = happyShift action_36
action_59 _ = happyReduce_55

action_60 (57) = happyShift action_33
action_60 (58) = happyShift action_34
action_60 (59) = happyShift action_35
action_60 (60) = happyShift action_36
action_60 _ = happyReduce_50

action_61 (25) = happyShift action_6
action_61 (26) = happyShift action_7
action_61 (27) = happyShift action_8
action_61 (29) = happyShift action_9
action_61 (33) = happyShift action_10
action_61 (39) = happyShift action_11
action_61 (47) = happyShift action_12
action_61 (52) = happyShift action_13
action_61 (56) = happyShift action_14
action_61 (69) = happyShift action_15
action_61 (71) = happyShift action_16
action_61 (6) = happyGoto action_83
action_61 (24) = happyGoto action_29
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_66

action_63 _ = happyReduce_65

action_64 (25) = happyShift action_78
action_64 (26) = happyShift action_79
action_64 (27) = happyShift action_80
action_64 (31) = happyShift action_81
action_64 (52) = happyShift action_82
action_64 (16) = happyGoto action_76
action_64 (18) = happyGoto action_77
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (32) = happyShift action_75
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (36) = happyShift action_74
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (25) = happyShift action_6
action_67 (26) = happyShift action_7
action_67 (27) = happyShift action_8
action_67 (29) = happyShift action_9
action_67 (33) = happyShift action_10
action_67 (39) = happyShift action_11
action_67 (47) = happyShift action_12
action_67 (52) = happyShift action_13
action_67 (56) = happyShift action_14
action_67 (69) = happyShift action_15
action_67 (71) = happyShift action_16
action_67 (6) = happyGoto action_73
action_67 (24) = happyGoto action_29
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (28) = happyShift action_23
action_68 (31) = happyShift action_24
action_68 (52) = happyShift action_25
action_68 (9) = happyGoto action_72
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (29) = happyShift action_4
action_69 _ = happyReduce_3

action_70 (52) = happyShift action_18
action_70 (5) = happyGoto action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_4

action_72 _ = happyReduce_14

action_73 (34) = happyShift action_89
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (28) = happyShift action_23
action_74 (31) = happyShift action_24
action_74 (52) = happyShift action_25
action_74 (9) = happyGoto action_21
action_74 (10) = happyGoto action_88
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_12

action_76 (37) = happyShift action_87
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_70

action_78 (37) = happyReduce_28
action_78 _ = happyReduce_28

action_79 _ = happyReduce_27

action_80 _ = happyReduce_25

action_81 (52) = happyShift action_66
action_81 (7) = happyGoto action_86
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (48) = happyShift action_85
action_82 (15) = happyGoto action_84
action_82 _ = happyReduce_24

action_83 _ = happyReduce_6

action_84 _ = happyReduce_29

action_85 (49) = happyShift action_94
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (32) = happyShift action_93
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (31) = happyShift action_92
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (35) = happyShift action_91
action_88 (51) = happyShift action_68
action_88 (8) = happyGoto action_90
action_88 _ = happyReduce_8

action_89 _ = happyReduce_47

action_90 _ = happyReduce_7

action_91 (52) = happyShift action_66
action_91 (7) = happyGoto action_103
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (25) = happyShift action_6
action_92 (26) = happyShift action_7
action_92 (27) = happyShift action_8
action_92 (29) = happyShift action_9
action_92 (33) = happyShift action_10
action_92 (39) = happyShift action_11
action_92 (43) = happyShift action_100
action_92 (44) = happyShift action_101
action_92 (47) = happyShift action_12
action_92 (52) = happyShift action_102
action_92 (56) = happyShift action_14
action_92 (69) = happyShift action_15
action_92 (71) = happyShift action_16
action_92 (5) = happyGoto action_95
action_92 (19) = happyGoto action_96
action_92 (20) = happyGoto action_97
action_92 (21) = happyGoto action_98
action_92 (24) = happyGoto action_99
action_92 _ = happyReduce_42

action_93 _ = happyReduce_30

action_94 _ = happyReduce_23

action_95 (38) = happyShift action_110
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_40

action_97 (42) = happyShift action_109
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (32) = happyShift action_108
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (55) = happyShift action_31
action_99 (56) = happyShift action_32
action_99 (57) = happyShift action_33
action_99 (58) = happyShift action_34
action_99 (59) = happyShift action_35
action_99 (60) = happyShift action_36
action_99 (61) = happyShift action_37
action_99 (62) = happyShift action_38
action_99 (63) = happyShift action_39
action_99 (64) = happyShift action_40
action_99 (65) = happyShift action_41
action_99 (66) = happyShift action_42
action_99 (67) = happyShift action_43
action_99 (68) = happyShift action_44
action_99 _ = happyReduce_37

action_100 (33) = happyShift action_107
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (33) = happyShift action_106
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (25) = happyShift action_6
action_102 (26) = happyShift action_7
action_102 (27) = happyShift action_8
action_102 (29) = happyShift action_105
action_102 (33) = happyShift action_10
action_102 (39) = happyShift action_11
action_102 (47) = happyShift action_12
action_102 (52) = happyShift action_13
action_102 (53) = happyShift action_70
action_102 (56) = happyShift action_14
action_102 (69) = happyShift action_15
action_102 (71) = happyShift action_16
action_102 (4) = happyGoto action_69
action_102 (24) = happyGoto action_104
action_102 _ = happyReduce_46

action_103 _ = happyReduce_9

action_104 (55) = happyShift action_31
action_104 (56) = happyShift action_32
action_104 (57) = happyShift action_33
action_104 (58) = happyShift action_34
action_104 (59) = happyShift action_35
action_104 (60) = happyShift action_36
action_104 (61) = happyShift action_37
action_104 (62) = happyShift action_38
action_104 (63) = happyShift action_39
action_104 (64) = happyShift action_40
action_104 (65) = happyShift action_41
action_104 (66) = happyShift action_42
action_104 (67) = happyShift action_43
action_104 (68) = happyShift action_44
action_104 _ = happyReduce_38

action_105 (25) = happyShift action_6
action_105 (26) = happyShift action_7
action_105 (27) = happyShift action_8
action_105 (29) = happyShift action_9
action_105 (33) = happyShift action_10
action_105 (39) = happyShift action_11
action_105 (47) = happyShift action_12
action_105 (52) = happyShift action_13
action_105 (56) = happyShift action_14
action_105 (69) = happyShift action_15
action_105 (71) = happyShift action_16
action_105 (6) = happyGoto action_28
action_105 (24) = happyGoto action_116
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (25) = happyShift action_6
action_106 (26) = happyShift action_7
action_106 (27) = happyShift action_8
action_106 (29) = happyShift action_9
action_106 (33) = happyShift action_10
action_106 (39) = happyShift action_11
action_106 (47) = happyShift action_12
action_106 (52) = happyShift action_13
action_106 (56) = happyShift action_14
action_106 (69) = happyShift action_15
action_106 (71) = happyShift action_16
action_106 (24) = happyGoto action_115
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (25) = happyShift action_78
action_107 (26) = happyShift action_79
action_107 (27) = happyShift action_80
action_107 (31) = happyShift action_81
action_107 (52) = happyShift action_82
action_107 (16) = happyGoto action_76
action_107 (18) = happyGoto action_114
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (25) = happyShift action_78
action_108 (26) = happyShift action_79
action_108 (27) = happyShift action_80
action_108 (31) = happyShift action_81
action_108 (52) = happyShift action_82
action_108 (16) = happyGoto action_76
action_108 (18) = happyGoto action_113
action_108 _ = happyReduce_32

action_109 (25) = happyShift action_6
action_109 (26) = happyShift action_7
action_109 (27) = happyShift action_8
action_109 (29) = happyShift action_9
action_109 (33) = happyShift action_10
action_109 (39) = happyShift action_11
action_109 (43) = happyShift action_100
action_109 (44) = happyShift action_101
action_109 (47) = happyShift action_12
action_109 (52) = happyShift action_102
action_109 (56) = happyShift action_14
action_109 (69) = happyShift action_15
action_109 (71) = happyShift action_16
action_109 (5) = happyGoto action_95
action_109 (19) = happyGoto action_112
action_109 (24) = happyGoto action_99
action_109 _ = happyReduce_41

action_110 (25) = happyShift action_6
action_110 (26) = happyShift action_7
action_110 (27) = happyShift action_8
action_110 (29) = happyShift action_9
action_110 (33) = happyShift action_10
action_110 (39) = happyShift action_11
action_110 (47) = happyShift action_12
action_110 (52) = happyShift action_13
action_110 (56) = happyShift action_14
action_110 (69) = happyShift action_15
action_110 (71) = happyShift action_16
action_110 (24) = happyGoto action_111
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (55) = happyShift action_31
action_111 (56) = happyShift action_32
action_111 (57) = happyShift action_33
action_111 (58) = happyShift action_34
action_111 (59) = happyShift action_35
action_111 (60) = happyShift action_36
action_111 (61) = happyShift action_37
action_111 (62) = happyShift action_38
action_111 (63) = happyShift action_39
action_111 (64) = happyShift action_40
action_111 (65) = happyShift action_41
action_111 (66) = happyShift action_42
action_111 (67) = happyShift action_43
action_111 (68) = happyShift action_44
action_111 _ = happyReduce_34

action_112 _ = happyReduce_39

action_113 _ = happyReduce_33

action_114 (36) = happyShift action_118
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (34) = happyShift action_117
action_115 (55) = happyShift action_31
action_115 (56) = happyShift action_32
action_115 (57) = happyShift action_33
action_115 (58) = happyShift action_34
action_115 (59) = happyShift action_35
action_115 (60) = happyShift action_36
action_115 (61) = happyShift action_37
action_115 (62) = happyShift action_38
action_115 (63) = happyShift action_39
action_115 (64) = happyShift action_40
action_115 (65) = happyShift action_41
action_115 (66) = happyShift action_42
action_115 (67) = happyShift action_43
action_115 (68) = happyShift action_44
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (30) = happyShift action_30
action_116 (35) = happyShift action_61
action_116 (55) = happyShift action_31
action_116 (56) = happyShift action_32
action_116 (57) = happyShift action_33
action_116 (58) = happyShift action_34
action_116 (59) = happyShift action_35
action_116 (60) = happyShift action_36
action_116 (61) = happyShift action_37
action_116 (62) = happyShift action_38
action_116 (63) = happyShift action_39
action_116 (64) = happyShift action_40
action_116 (65) = happyShift action_41
action_116 (66) = happyShift action_42
action_116 (67) = happyShift action_43
action_116 (68) = happyShift action_44
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (31) = happyShift action_120
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (25) = happyShift action_6
action_118 (26) = happyShift action_7
action_118 (27) = happyShift action_8
action_118 (29) = happyShift action_9
action_118 (33) = happyShift action_10
action_118 (39) = happyShift action_11
action_118 (47) = happyShift action_12
action_118 (52) = happyShift action_13
action_118 (56) = happyShift action_14
action_118 (69) = happyShift action_15
action_118 (71) = happyShift action_16
action_118 (24) = happyGoto action_119
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (34) = happyShift action_126
action_119 (55) = happyShift action_31
action_119 (56) = happyShift action_32
action_119 (57) = happyShift action_33
action_119 (58) = happyShift action_34
action_119 (59) = happyShift action_35
action_119 (60) = happyShift action_36
action_119 (61) = happyShift action_37
action_119 (62) = happyShift action_38
action_119 (63) = happyShift action_39
action_119 (64) = happyShift action_40
action_119 (65) = happyShift action_41
action_119 (66) = happyShift action_42
action_119 (67) = happyShift action_43
action_119 (68) = happyShift action_44
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (25) = happyShift action_6
action_120 (26) = happyShift action_7
action_120 (27) = happyShift action_8
action_120 (29) = happyShift action_9
action_120 (32) = happyReduce_42
action_120 (33) = happyShift action_10
action_120 (39) = happyShift action_11
action_120 (43) = happyShift action_100
action_120 (44) = happyShift action_101
action_120 (45) = happyShift action_124
action_120 (46) = happyShift action_125
action_120 (47) = happyShift action_12
action_120 (52) = happyShift action_102
action_120 (56) = happyShift action_14
action_120 (69) = happyShift action_15
action_120 (71) = happyShift action_16
action_120 (5) = happyGoto action_95
action_120 (13) = happyGoto action_121
action_120 (14) = happyGoto action_122
action_120 (19) = happyGoto action_96
action_120 (20) = happyGoto action_97
action_120 (21) = happyGoto action_123
action_120 (24) = happyGoto action_99
action_120 _ = happyReduce_42

action_121 _ = happyReduce_22

action_122 (32) = happyShift action_131
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (25) = happyShift action_6
action_123 (26) = happyShift action_7
action_123 (27) = happyShift action_8
action_123 (29) = happyShift action_9
action_123 (32) = happyReduce_42
action_123 (33) = happyShift action_10
action_123 (39) = happyShift action_11
action_123 (43) = happyShift action_100
action_123 (44) = happyShift action_101
action_123 (45) = happyShift action_124
action_123 (46) = happyShift action_125
action_123 (47) = happyShift action_12
action_123 (52) = happyShift action_102
action_123 (56) = happyShift action_14
action_123 (69) = happyShift action_15
action_123 (71) = happyShift action_16
action_123 (5) = happyGoto action_95
action_123 (13) = happyGoto action_121
action_123 (14) = happyGoto action_130
action_123 (19) = happyGoto action_96
action_123 (20) = happyGoto action_97
action_123 (21) = happyGoto action_123
action_123 (24) = happyGoto action_99
action_123 _ = happyReduce_42

action_124 (42) = happyShift action_129
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (42) = happyShift action_128
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (31) = happyShift action_127
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (25) = happyShift action_6
action_127 (26) = happyShift action_7
action_127 (27) = happyShift action_8
action_127 (29) = happyShift action_9
action_127 (32) = happyReduce_42
action_127 (33) = happyShift action_10
action_127 (39) = happyShift action_11
action_127 (43) = happyShift action_100
action_127 (44) = happyShift action_101
action_127 (45) = happyShift action_124
action_127 (46) = happyShift action_125
action_127 (47) = happyShift action_12
action_127 (52) = happyShift action_102
action_127 (56) = happyShift action_14
action_127 (69) = happyShift action_15
action_127 (71) = happyShift action_16
action_127 (5) = happyGoto action_95
action_127 (13) = happyGoto action_121
action_127 (14) = happyGoto action_132
action_127 (19) = happyGoto action_96
action_127 (20) = happyGoto action_97
action_127 (21) = happyGoto action_123
action_127 (24) = happyGoto action_99
action_127 _ = happyReduce_42

action_128 _ = happyReduce_18

action_129 _ = happyReduce_19

action_130 _ = happyReduce_20

action_131 _ = happyReduce_36

action_132 (32) = happyShift action_133
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_35

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

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn6
		 (undefined
	)

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 _
	_
	_
	 =  HappyAbsSyn6
		 (undefined
	)

happyReduce_7 = happyReduce 4 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (undefined
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_0  8 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 (undefined
	)

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 _
	_
	 =  HappyAbsSyn8
		 (undefined
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 (undefined
	)

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn9
		 (undefined
	)

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 _
	_
	_
	 =  HappyAbsSyn9
		 (undefined
	)

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn10
		 (undefined
	)

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 _
	_
	_
	 =  HappyAbsSyn10
		 (undefined
	)

happyReduce_15 = happySpecReduce_2  11 happyReduction_15
happyReduction_15 _
	_
	 =  HappyAbsSyn11
		 (undefined
	)

happyReduce_16 = happySpecReduce_0  11 happyReduction_16
happyReduction_16  =  HappyAbsSyn11
		 (undefined
	)

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 _
	_
	_
	 =  HappyAbsSyn12
		 (undefined
	)

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 _
	_
	 =  HappyAbsSyn13
		 (undefined
	)

happyReduce_19 = happySpecReduce_2  13 happyReduction_19
happyReduction_19 _
	_
	 =  HappyAbsSyn13
		 (undefined
	)

happyReduce_20 = happySpecReduce_2  13 happyReduction_20
happyReduction_20 _
	_
	 =  HappyAbsSyn13
		 (undefined
	)

happyReduce_21 = happySpecReduce_0  14 happyReduction_21
happyReduction_21  =  HappyAbsSyn14
		 (undefined
	)

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn14
		 (undefined
	)

happyReduce_23 = happySpecReduce_2  15 happyReduction_23
happyReduction_23 _
	_
	 =  HappyAbsSyn15
		 (undefined
	)

happyReduce_24 = happySpecReduce_0  15 happyReduction_24
happyReduction_24  =  HappyAbsSyn15
		 (undefined
	)

happyReduce_25 = happySpecReduce_1  16 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn16
		 (undefined
	)

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn16
		 (undefined
	)

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn16
		 (undefined
	)

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn16
		 (undefined
	)

happyReduce_29 = happySpecReduce_2  16 happyReduction_29
happyReduction_29 _
	_
	 =  HappyAbsSyn16
		 (undefined
	)

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 _
	_
	_
	 =  HappyAbsSyn16
		 (undefined
	)

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn17
		 (undefined
	)

happyReduce_32 = happyReduce 5 18 happyReduction_32
happyReduction_32 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (undefined
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 6 18 happyReduction_33
happyReduction_33 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (undefined
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 _
	_
	_
	 =  HappyAbsSyn19
		 (undefined
	)

happyReduce_35 = happyReduce 9 19 happyReduction_35
happyReduction_35 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (undefined
	) `HappyStk` happyRest

happyReduce_36 = happyReduce 7 19 happyReduction_36
happyReduction_36 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (undefined
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_1  19 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn19
		 (undefined
	)

happyReduce_38 = happySpecReduce_2  19 happyReduction_38
happyReduction_38 _
	_
	 =  HappyAbsSyn19
		 (undefined
	)

happyReduce_39 = happySpecReduce_3  20 happyReduction_39
happyReduction_39 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_41 = happySpecReduce_2  21 happyReduction_41
happyReduction_41 _
	_
	 =  HappyAbsSyn21
		 (undefined
	)

happyReduce_42 = happySpecReduce_0  21 happyReduction_42
happyReduction_42  =  HappyAbsSyn21
		 (undefined
	)

happyReduce_43 = happyReduce 8 22 happyReduction_43
happyReduction_43 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (undefined
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_2  23 happyReduction_44
happyReduction_44 _
	_
	 =  HappyAbsSyn23
		 (undefined
	)

happyReduce_45 = happySpecReduce_1  23 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn23
		 (undefined
	)

happyReduce_46 = happySpecReduce_1  24 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_47 = happyReduce 5 24 happyReduction_47
happyReduction_47 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (undefined
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_2  24 happyReduction_48
happyReduction_48 _
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_49 = happySpecReduce_2  24 happyReduction_49
happyReduction_49 _
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_50 = happySpecReduce_3  24 happyReduction_50
happyReduction_50 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_51 = happySpecReduce_3  24 happyReduction_51
happyReduction_51 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_52 = happySpecReduce_3  24 happyReduction_52
happyReduction_52 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_53 = happySpecReduce_3  24 happyReduction_53
happyReduction_53 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_54 = happySpecReduce_3  24 happyReduction_54
happyReduction_54 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_55 = happySpecReduce_3  24 happyReduction_55
happyReduction_55 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_56 = happySpecReduce_3  24 happyReduction_56
happyReduction_56 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_57 = happySpecReduce_3  24 happyReduction_57
happyReduction_57 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_58 = happySpecReduce_3  24 happyReduction_58
happyReduction_58 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_59 = happySpecReduce_3  24 happyReduction_59
happyReduction_59 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_60 = happySpecReduce_3  24 happyReduction_60
happyReduction_60 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_61 = happySpecReduce_3  24 happyReduction_61
happyReduction_61 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_62 = happySpecReduce_3  24 happyReduction_62
happyReduction_62 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_63 = happySpecReduce_3  24 happyReduction_63
happyReduction_63 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_64 = happySpecReduce_2  24 happyReduction_64
happyReduction_64 _
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_65 = happySpecReduce_3  24 happyReduction_65
happyReduction_65 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_66 = happySpecReduce_3  24 happyReduction_66
happyReduction_66 _
	_
	_
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_67 = happySpecReduce_1  24 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_68 = happySpecReduce_1  24 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_69 = happySpecReduce_1  24 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn24
		 (undefined
	)

happyReduce_70 = happyReduce 4 24 happyReduction_70
happyReduction_70 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (undefined
	) `HappyStk` happyRest

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	LEOF -> action 72 72 tk (HappyState action) sts stk;
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
	LOp "-" _ -> cont 70;
	LOp "&" _ -> cont 71;
	_ -> happyError' (tk, [])
	})

happyError_ explist 72 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

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

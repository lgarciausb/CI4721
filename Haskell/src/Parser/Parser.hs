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

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 ([Expression AlexPosn])
	| HappyAbsSyn5 (LValuable AlexPosn)
	| HappyAbsSyn7 (RecordPattern AlexPosn)
	| HappyAbsSyn8 ([(Text,PTypes AlexPosn,AlexPosn)])
	| HappyAbsSyn9 (PTypes AlexPosn)
	| HappyAbsSyn11 (LoopAction AlexPosn)
	| HappyAbsSyn12 ([LoopAction AlexPosn])
	| HappyAbsSyn14 (Pattern AlexPosn)
	| HappyAbsSyn15 ([(Pattern AlexPosn,[Action AlexPosn])])
	| HappyAbsSyn16 (Action AlexPosn)
	| HappyAbsSyn17 ([Action AlexPosn])
	| HappyAbsSyn19 (Maybe (ByRef AlexPosn))
	| HappyAbsSyn20 (Expression AlexPosn)
	| HappyAbsSyn21 (FunArgs AlexPosn)
	| HappyAbsSyn22 (FunctionDef AlexPosn)
	| HappyAbsSyn23 ([FunctionDef AlexPosn])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

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
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

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
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,401) ([0,9216,0,4,0,0,2,0,0,0,2232,16898,32772,1,0,0,0,0,0,128,512,0,0,0,0,0,0,16384,2,64,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,1024,0,0,0,0,16,0,0,0,0,0,0,0,2304,0,1,0,16384,2,64,0,0,0,0,0,0,4096,0,65504,7,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,8331,17440,6144,0,8928,2056,17,6,47104,520,1090,384,0,144,4096,0,0,34816,0,8,0,57344,2082,4360,1536,0,2232,16898,32772,1,0,0,16,0,0,0,0,0,0,512,0,2,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,57344,2082,4360,1536,0,0,16384,0,0,0,2,8,0,0,16384,57344,2047,0,16384,0,65528,1,0,33,0,0,0,0,32768,8191,0,0,0,0,0,57344,2082,4360,1536,0,2232,16898,32772,1,11776,32898,272,96,32768,8331,17440,6144,0,8928,2056,17,6,47104,520,1090,384,0,33326,4224,24577,0,35712,8224,68,24,57344,2082,4360,1536,0,2232,16898,32772,1,11776,32898,272,96,32768,8331,17440,6144,0,8928,2056,17,6,47104,520,1090,384,0,0,0,0,0,0,0,6,0,0,64,0,0,0,576,16384,0,0,0,0,0,0,0,512,512,0,0,2048,0,0,0,0,0,4,0,0,0,32768,6143,0,0,0,65504,7,0,0,63488,1,0,0,0,126,0,0,0,8064,0,0,0,57344,7,0,0,0,504,0,0,0,32256,0,0,0,0,8,0,0,0,512,0,0,0,32768,0,0,0,0,32,0,0,0,7680,0,0,0,32768,7,0,0,0,0,0,47104,520,1090,384,0,0,0,0,0,9088,0,4,0,57344,2082,4360,1536,0,0,0,0,0,0,12,0,0,32768,8331,17440,6144,0,1024,0,65528,1,0,0,0,0,0,3072,0,0,0,0,8,0,0,57344,8,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,4,0,0,0,32768,8191,0,0,2,0,0,0,0,32,0,0,2232,16994,32772,1,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,256,0,0,0,16,0,0,0,0,0,32766,0,0,2,0,0,0,128,0,0,0,0,0,0,0,16384,2,64,0,0,0,0,0,0,16384,0,0,0,0,512,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,8928,2440,17,6,0,2,0,0,0,0,0,0,0,0,0,0,0,57344,2082,4360,1536,0,568,16384,0,0,0,0,0,0,32768,8331,17446,6144,0,8928,2056,17,6,0,0,65024,127,0,0,0,0,0,0,4,0,0,0,64,63488,511,0,2232,16994,32772,1,0,1,0,0,0,0,0,0,0,4096,0,0,0,0,2,0,0,0,33326,4224,24577,0,0,1,65504,7,57344,34850,4367,1536,0,0,0,0,0,0,0,0,0,0,0,1,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,2232,17378,32772,1,0,0,0,0,32768,8331,17470,6144,0,0,0,0,0,0,4,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","ixs","lvaluable","args","records","mRecords","T0","T","loop_a0","loop_a","loop_as","pattern","patterns","a0","as","actions","optionalByRef","e","fun_args","function_def","function_defs","char","string","number","atom","'['","']'","'{'","'}'","'('","')'","','","':'","'=>'","':='","match","with","type","';'","for","while","continue","break","new","by","reference","'***'","'|'","identifier","'.'","EOF","'+'","'-'","'*'","'/'","'^'","'%'","'<'","'>'","'!='","'=='","'>='","'<='","'||'","'&&'","'~'","'&'","%eof"]
        bit_start = st Prelude.* 70
        bit_end = (st Prelude.+ 1) Prelude.* 70
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..69]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (27) = happyShift action_7
action_0 (30) = happyShift action_8
action_0 (51) = happyShift action_9
action_0 (9) = happyGoto action_3
action_0 (10) = happyGoto action_4
action_0 (22) = happyGoto action_5
action_0 (23) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (28) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (24) = happyShift action_17
action_2 (25) = happyShift action_18
action_2 (26) = happyShift action_19
action_2 (28) = happyShift action_20
action_2 (32) = happyShift action_21
action_2 (38) = happyShift action_22
action_2 (46) = happyShift action_23
action_2 (51) = happyShift action_24
action_2 (55) = happyShift action_25
action_2 (68) = happyShift action_26
action_2 (69) = happyShift action_27
action_2 (5) = happyGoto action_15
action_2 (20) = happyGoto action_16
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_15

action_4 (32) = happyShift action_13
action_4 (50) = happyShift action_14
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_70

action_6 (27) = happyShift action_7
action_6 (30) = happyShift action_8
action_6 (51) = happyShift action_9
action_6 (70) = happyAccept
action_6 (9) = happyGoto action_3
action_6 (10) = happyGoto action_4
action_6 (22) = happyGoto action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_12

action_8 (51) = happyShift action_11
action_8 (7) = happyGoto action_10
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_13

action_10 (31) = happyShift action_59
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (35) = happyShift action_58
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_69

action_13 (27) = happyShift action_7
action_13 (30) = happyShift action_8
action_13 (51) = happyShift action_9
action_13 (9) = happyGoto action_3
action_13 (10) = happyGoto action_56
action_13 (21) = happyGoto action_57
action_13 _ = happyReduce_67

action_14 (27) = happyShift action_7
action_14 (30) = happyShift action_8
action_14 (51) = happyShift action_9
action_14 (9) = happyGoto action_55
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_40

action_16 (29) = happyShift action_40
action_16 (54) = happyShift action_41
action_16 (55) = happyShift action_42
action_16 (56) = happyShift action_43
action_16 (57) = happyShift action_44
action_16 (58) = happyShift action_45
action_16 (59) = happyShift action_46
action_16 (60) = happyShift action_47
action_16 (61) = happyShift action_48
action_16 (62) = happyShift action_49
action_16 (63) = happyShift action_50
action_16 (64) = happyShift action_51
action_16 (65) = happyShift action_52
action_16 (66) = happyShift action_53
action_16 (67) = happyShift action_54
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_63

action_18 _ = happyReduce_62

action_19 _ = happyReduce_61

action_20 (24) = happyShift action_17
action_20 (25) = happyShift action_18
action_20 (26) = happyShift action_19
action_20 (28) = happyShift action_20
action_20 (32) = happyShift action_21
action_20 (38) = happyShift action_22
action_20 (46) = happyShift action_23
action_20 (51) = happyShift action_24
action_20 (55) = happyShift action_25
action_20 (68) = happyShift action_26
action_20 (69) = happyShift action_27
action_20 (5) = happyGoto action_15
action_20 (6) = happyGoto action_38
action_20 (20) = happyGoto action_39
action_20 _ = happyReduce_8

action_21 (24) = happyShift action_17
action_21 (25) = happyShift action_18
action_21 (26) = happyShift action_19
action_21 (28) = happyShift action_20
action_21 (32) = happyShift action_21
action_21 (38) = happyShift action_22
action_21 (46) = happyShift action_23
action_21 (51) = happyShift action_24
action_21 (55) = happyShift action_25
action_21 (68) = happyShift action_26
action_21 (69) = happyShift action_27
action_21 (5) = happyGoto action_15
action_21 (20) = happyGoto action_37
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (24) = happyShift action_17
action_22 (25) = happyShift action_18
action_22 (26) = happyShift action_19
action_22 (28) = happyShift action_20
action_22 (32) = happyShift action_21
action_22 (38) = happyShift action_22
action_22 (46) = happyShift action_23
action_22 (51) = happyShift action_24
action_22 (55) = happyShift action_25
action_22 (68) = happyShift action_26
action_22 (69) = happyShift action_27
action_22 (5) = happyGoto action_15
action_22 (20) = happyGoto action_36
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (27) = happyShift action_7
action_23 (30) = happyShift action_8
action_23 (51) = happyShift action_9
action_23 (9) = happyGoto action_3
action_23 (10) = happyGoto action_35
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (28) = happyShift action_2
action_24 (32) = happyShift action_33
action_24 (52) = happyShift action_34
action_24 (4) = happyGoto action_32
action_24 _ = happyReduce_5

action_25 (24) = happyShift action_17
action_25 (25) = happyShift action_18
action_25 (26) = happyShift action_19
action_25 (28) = happyShift action_20
action_25 (32) = happyShift action_21
action_25 (38) = happyShift action_22
action_25 (46) = happyShift action_23
action_25 (51) = happyShift action_24
action_25 (55) = happyShift action_25
action_25 (68) = happyShift action_26
action_25 (69) = happyShift action_27
action_25 (5) = happyGoto action_15
action_25 (20) = happyGoto action_31
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (24) = happyShift action_17
action_26 (25) = happyShift action_18
action_26 (26) = happyShift action_19
action_26 (28) = happyShift action_20
action_26 (32) = happyShift action_21
action_26 (38) = happyShift action_22
action_26 (46) = happyShift action_23
action_26 (51) = happyShift action_24
action_26 (55) = happyShift action_25
action_26 (68) = happyShift action_26
action_26 (69) = happyShift action_27
action_26 (5) = happyGoto action_15
action_26 (20) = happyGoto action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (51) = happyShift action_29
action_27 (5) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_42

action_29 (28) = happyShift action_2
action_29 (52) = happyShift action_34
action_29 (4) = happyGoto action_32
action_29 _ = happyReduce_5

action_30 _ = happyReduce_58

action_31 _ = happyReduce_43

action_32 (28) = happyShift action_84
action_32 _ = happyReduce_3

action_33 (24) = happyShift action_17
action_33 (25) = happyShift action_18
action_33 (26) = happyShift action_19
action_33 (28) = happyShift action_20
action_33 (32) = happyShift action_21
action_33 (38) = happyShift action_22
action_33 (46) = happyShift action_23
action_33 (51) = happyShift action_24
action_33 (55) = happyShift action_25
action_33 (68) = happyShift action_26
action_33 (69) = happyShift action_27
action_33 (5) = happyGoto action_15
action_33 (6) = happyGoto action_83
action_33 (20) = happyGoto action_39
action_33 _ = happyReduce_8

action_34 (51) = happyShift action_29
action_34 (5) = happyGoto action_82
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (32) = happyShift action_81
action_35 (50) = happyShift action_14
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (39) = happyShift action_80
action_36 (54) = happyShift action_41
action_36 (55) = happyShift action_42
action_36 (56) = happyShift action_43
action_36 (57) = happyShift action_44
action_36 (58) = happyShift action_45
action_36 (59) = happyShift action_46
action_36 (60) = happyShift action_47
action_36 (61) = happyShift action_48
action_36 (62) = happyShift action_49
action_36 (63) = happyShift action_50
action_36 (64) = happyShift action_51
action_36 (65) = happyShift action_52
action_36 (66) = happyShift action_53
action_36 (67) = happyShift action_54
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (33) = happyShift action_79
action_37 (54) = happyShift action_41
action_37 (55) = happyShift action_42
action_37 (56) = happyShift action_43
action_37 (57) = happyShift action_44
action_37 (58) = happyShift action_45
action_37 (59) = happyShift action_46
action_37 (60) = happyShift action_47
action_37 (61) = happyShift action_48
action_37 (62) = happyShift action_49
action_37 (63) = happyShift action_50
action_37 (64) = happyShift action_51
action_37 (65) = happyShift action_52
action_37 (66) = happyShift action_53
action_37 (67) = happyShift action_54
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (29) = happyShift action_77
action_38 (34) = happyShift action_78
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (54) = happyShift action_41
action_39 (55) = happyShift action_42
action_39 (56) = happyShift action_43
action_39 (57) = happyShift action_44
action_39 (58) = happyShift action_45
action_39 (59) = happyShift action_46
action_39 (60) = happyShift action_47
action_39 (61) = happyShift action_48
action_39 (62) = happyShift action_49
action_39 (63) = happyShift action_50
action_39 (64) = happyShift action_51
action_39 (65) = happyShift action_52
action_39 (66) = happyShift action_53
action_39 (67) = happyShift action_54
action_39 _ = happyReduce_6

action_40 _ = happyReduce_1

action_41 (24) = happyShift action_17
action_41 (25) = happyShift action_18
action_41 (26) = happyShift action_19
action_41 (28) = happyShift action_20
action_41 (32) = happyShift action_21
action_41 (38) = happyShift action_22
action_41 (46) = happyShift action_23
action_41 (51) = happyShift action_24
action_41 (55) = happyShift action_25
action_41 (68) = happyShift action_26
action_41 (69) = happyShift action_27
action_41 (5) = happyGoto action_15
action_41 (20) = happyGoto action_76
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (24) = happyShift action_17
action_42 (25) = happyShift action_18
action_42 (26) = happyShift action_19
action_42 (28) = happyShift action_20
action_42 (32) = happyShift action_21
action_42 (38) = happyShift action_22
action_42 (46) = happyShift action_23
action_42 (51) = happyShift action_24
action_42 (55) = happyShift action_25
action_42 (68) = happyShift action_26
action_42 (69) = happyShift action_27
action_42 (5) = happyGoto action_15
action_42 (20) = happyGoto action_75
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (24) = happyShift action_17
action_43 (25) = happyShift action_18
action_43 (26) = happyShift action_19
action_43 (28) = happyShift action_20
action_43 (32) = happyShift action_21
action_43 (38) = happyShift action_22
action_43 (46) = happyShift action_23
action_43 (51) = happyShift action_24
action_43 (55) = happyShift action_25
action_43 (68) = happyShift action_26
action_43 (69) = happyShift action_27
action_43 (5) = happyGoto action_15
action_43 (20) = happyGoto action_74
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (24) = happyShift action_17
action_44 (25) = happyShift action_18
action_44 (26) = happyShift action_19
action_44 (28) = happyShift action_20
action_44 (32) = happyShift action_21
action_44 (38) = happyShift action_22
action_44 (46) = happyShift action_23
action_44 (51) = happyShift action_24
action_44 (55) = happyShift action_25
action_44 (68) = happyShift action_26
action_44 (69) = happyShift action_27
action_44 (5) = happyGoto action_15
action_44 (20) = happyGoto action_73
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (24) = happyShift action_17
action_45 (25) = happyShift action_18
action_45 (26) = happyShift action_19
action_45 (28) = happyShift action_20
action_45 (32) = happyShift action_21
action_45 (38) = happyShift action_22
action_45 (46) = happyShift action_23
action_45 (51) = happyShift action_24
action_45 (55) = happyShift action_25
action_45 (68) = happyShift action_26
action_45 (69) = happyShift action_27
action_45 (5) = happyGoto action_15
action_45 (20) = happyGoto action_72
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (24) = happyShift action_17
action_46 (25) = happyShift action_18
action_46 (26) = happyShift action_19
action_46 (28) = happyShift action_20
action_46 (32) = happyShift action_21
action_46 (38) = happyShift action_22
action_46 (46) = happyShift action_23
action_46 (51) = happyShift action_24
action_46 (55) = happyShift action_25
action_46 (68) = happyShift action_26
action_46 (69) = happyShift action_27
action_46 (5) = happyGoto action_15
action_46 (20) = happyGoto action_71
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (24) = happyShift action_17
action_47 (25) = happyShift action_18
action_47 (26) = happyShift action_19
action_47 (28) = happyShift action_20
action_47 (32) = happyShift action_21
action_47 (38) = happyShift action_22
action_47 (46) = happyShift action_23
action_47 (51) = happyShift action_24
action_47 (55) = happyShift action_25
action_47 (68) = happyShift action_26
action_47 (69) = happyShift action_27
action_47 (5) = happyGoto action_15
action_47 (20) = happyGoto action_70
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (24) = happyShift action_17
action_48 (25) = happyShift action_18
action_48 (26) = happyShift action_19
action_48 (28) = happyShift action_20
action_48 (32) = happyShift action_21
action_48 (38) = happyShift action_22
action_48 (46) = happyShift action_23
action_48 (51) = happyShift action_24
action_48 (55) = happyShift action_25
action_48 (68) = happyShift action_26
action_48 (69) = happyShift action_27
action_48 (5) = happyGoto action_15
action_48 (20) = happyGoto action_69
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (24) = happyShift action_17
action_49 (25) = happyShift action_18
action_49 (26) = happyShift action_19
action_49 (28) = happyShift action_20
action_49 (32) = happyShift action_21
action_49 (38) = happyShift action_22
action_49 (46) = happyShift action_23
action_49 (51) = happyShift action_24
action_49 (55) = happyShift action_25
action_49 (68) = happyShift action_26
action_49 (69) = happyShift action_27
action_49 (5) = happyGoto action_15
action_49 (20) = happyGoto action_68
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (24) = happyShift action_17
action_50 (25) = happyShift action_18
action_50 (26) = happyShift action_19
action_50 (28) = happyShift action_20
action_50 (32) = happyShift action_21
action_50 (38) = happyShift action_22
action_50 (46) = happyShift action_23
action_50 (51) = happyShift action_24
action_50 (55) = happyShift action_25
action_50 (68) = happyShift action_26
action_50 (69) = happyShift action_27
action_50 (5) = happyGoto action_15
action_50 (20) = happyGoto action_67
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (24) = happyShift action_17
action_51 (25) = happyShift action_18
action_51 (26) = happyShift action_19
action_51 (28) = happyShift action_20
action_51 (32) = happyShift action_21
action_51 (38) = happyShift action_22
action_51 (46) = happyShift action_23
action_51 (51) = happyShift action_24
action_51 (55) = happyShift action_25
action_51 (68) = happyShift action_26
action_51 (69) = happyShift action_27
action_51 (5) = happyGoto action_15
action_51 (20) = happyGoto action_66
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (24) = happyShift action_17
action_52 (25) = happyShift action_18
action_52 (26) = happyShift action_19
action_52 (28) = happyShift action_20
action_52 (32) = happyShift action_21
action_52 (38) = happyShift action_22
action_52 (46) = happyShift action_23
action_52 (51) = happyShift action_24
action_52 (55) = happyShift action_25
action_52 (68) = happyShift action_26
action_52 (69) = happyShift action_27
action_52 (5) = happyGoto action_15
action_52 (20) = happyGoto action_65
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (24) = happyShift action_17
action_53 (25) = happyShift action_18
action_53 (26) = happyShift action_19
action_53 (28) = happyShift action_20
action_53 (32) = happyShift action_21
action_53 (38) = happyShift action_22
action_53 (46) = happyShift action_23
action_53 (51) = happyShift action_24
action_53 (55) = happyShift action_25
action_53 (68) = happyShift action_26
action_53 (69) = happyShift action_27
action_53 (5) = happyGoto action_15
action_53 (20) = happyGoto action_64
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (24) = happyShift action_17
action_54 (25) = happyShift action_18
action_54 (26) = happyShift action_19
action_54 (28) = happyShift action_20
action_54 (32) = happyShift action_21
action_54 (38) = happyShift action_22
action_54 (46) = happyShift action_23
action_54 (51) = happyShift action_24
action_54 (55) = happyShift action_25
action_54 (68) = happyShift action_26
action_54 (69) = happyShift action_27
action_54 (5) = happyGoto action_15
action_54 (20) = happyGoto action_63
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_16

action_56 (50) = happyShift action_14
action_56 (51) = happyShift action_62
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (33) = happyShift action_61
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (27) = happyShift action_7
action_58 (30) = happyShift action_8
action_58 (51) = happyShift action_9
action_58 (9) = happyGoto action_3
action_58 (10) = happyGoto action_60
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_14

action_60 (34) = happyShift action_100
action_60 (50) = happyShift action_14
action_60 (8) = happyGoto action_99
action_60 _ = happyReduce_10

action_61 (30) = happyShift action_98
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (47) = happyShift action_97
action_62 (19) = happyGoto action_96
action_62 _ = happyReduce_39

action_63 (54) = happyShift action_41
action_63 (55) = happyShift action_42
action_63 (56) = happyShift action_43
action_63 (57) = happyShift action_44
action_63 (58) = happyShift action_45
action_63 (59) = happyShift action_46
action_63 (60) = happyShift action_47
action_63 (61) = happyShift action_48
action_63 (62) = happyShift action_49
action_63 (63) = happyShift action_50
action_63 (64) = happyShift action_51
action_63 (65) = happyShift action_52
action_63 (67) = happyShift action_54
action_63 _ = happyReduce_57

action_64 (54) = happyShift action_41
action_64 (55) = happyShift action_42
action_64 (56) = happyShift action_43
action_64 (57) = happyShift action_44
action_64 (58) = happyShift action_45
action_64 (59) = happyShift action_46
action_64 (60) = happyShift action_47
action_64 (61) = happyShift action_48
action_64 (62) = happyShift action_49
action_64 (63) = happyShift action_50
action_64 (64) = happyShift action_51
action_64 (65) = happyShift action_52
action_64 (66) = happyShift action_53
action_64 (67) = happyShift action_54
action_64 _ = happyReduce_56

action_65 (54) = happyShift action_41
action_65 (55) = happyShift action_42
action_65 (56) = happyShift action_43
action_65 (57) = happyShift action_44
action_65 (58) = happyShift action_45
action_65 (59) = happyShift action_46
action_65 _ = happyReduce_55

action_66 (54) = happyShift action_41
action_66 (55) = happyShift action_42
action_66 (56) = happyShift action_43
action_66 (57) = happyShift action_44
action_66 (58) = happyShift action_45
action_66 (59) = happyShift action_46
action_66 _ = happyReduce_54

action_67 (54) = happyShift action_41
action_67 (55) = happyShift action_42
action_67 (56) = happyShift action_43
action_67 (57) = happyShift action_44
action_67 (58) = happyShift action_45
action_67 (59) = happyShift action_46
action_67 _ = happyReduce_53

action_68 (54) = happyShift action_41
action_68 (55) = happyShift action_42
action_68 (56) = happyShift action_43
action_68 (57) = happyShift action_44
action_68 (58) = happyShift action_45
action_68 (59) = happyShift action_46
action_68 _ = happyReduce_52

action_69 (54) = happyShift action_41
action_69 (55) = happyShift action_42
action_69 (56) = happyShift action_43
action_69 (57) = happyShift action_44
action_69 (58) = happyShift action_45
action_69 (59) = happyShift action_46
action_69 _ = happyReduce_51

action_70 (54) = happyShift action_41
action_70 (55) = happyShift action_42
action_70 (56) = happyShift action_43
action_70 (57) = happyShift action_44
action_70 (58) = happyShift action_45
action_70 (59) = happyShift action_46
action_70 _ = happyReduce_50

action_71 (58) = happyShift action_45
action_71 _ = happyReduce_48

action_72 (58) = happyShift action_45
action_72 _ = happyReduce_47

action_73 (58) = happyShift action_45
action_73 _ = happyReduce_46

action_74 (58) = happyShift action_45
action_74 _ = happyReduce_45

action_75 (56) = happyShift action_43
action_75 (57) = happyShift action_44
action_75 (58) = happyShift action_45
action_75 (59) = happyShift action_46
action_75 _ = happyReduce_49

action_76 (56) = happyShift action_43
action_76 (57) = happyShift action_44
action_76 (58) = happyShift action_45
action_76 (59) = happyShift action_46
action_76 _ = happyReduce_44

action_77 _ = happyReduce_60

action_78 (24) = happyShift action_17
action_78 (25) = happyShift action_18
action_78 (26) = happyShift action_19
action_78 (28) = happyShift action_20
action_78 (32) = happyShift action_21
action_78 (38) = happyShift action_22
action_78 (46) = happyShift action_23
action_78 (51) = happyShift action_24
action_78 (55) = happyShift action_25
action_78 (68) = happyShift action_26
action_78 (69) = happyShift action_27
action_78 (5) = happyGoto action_15
action_78 (20) = happyGoto action_95
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_59

action_80 (24) = happyShift action_90
action_80 (25) = happyShift action_91
action_80 (26) = happyShift action_92
action_80 (30) = happyShift action_93
action_80 (51) = happyShift action_94
action_80 (14) = happyGoto action_88
action_80 (15) = happyGoto action_89
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (24) = happyShift action_17
action_81 (25) = happyShift action_18
action_81 (26) = happyShift action_19
action_81 (28) = happyShift action_20
action_81 (32) = happyShift action_21
action_81 (38) = happyShift action_22
action_81 (46) = happyShift action_23
action_81 (51) = happyShift action_24
action_81 (55) = happyShift action_25
action_81 (68) = happyShift action_26
action_81 (69) = happyShift action_27
action_81 (5) = happyGoto action_15
action_81 (6) = happyGoto action_87
action_81 (20) = happyGoto action_39
action_81 _ = happyReduce_8

action_82 _ = happyReduce_4

action_83 (33) = happyShift action_86
action_83 (34) = happyShift action_78
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (24) = happyShift action_17
action_84 (25) = happyShift action_18
action_84 (26) = happyShift action_19
action_84 (28) = happyShift action_20
action_84 (32) = happyShift action_21
action_84 (38) = happyShift action_22
action_84 (46) = happyShift action_23
action_84 (51) = happyShift action_24
action_84 (55) = happyShift action_25
action_84 (68) = happyShift action_26
action_84 (69) = happyShift action_27
action_84 (5) = happyGoto action_15
action_84 (20) = happyGoto action_85
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (29) = happyShift action_116
action_85 (54) = happyShift action_41
action_85 (55) = happyShift action_42
action_85 (56) = happyShift action_43
action_85 (57) = happyShift action_44
action_85 (58) = happyShift action_45
action_85 (59) = happyShift action_46
action_85 (60) = happyShift action_47
action_85 (61) = happyShift action_48
action_85 (62) = happyShift action_49
action_85 (63) = happyShift action_50
action_85 (64) = happyShift action_51
action_85 (65) = happyShift action_52
action_85 (66) = happyShift action_53
action_85 (67) = happyShift action_54
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_65

action_87 (33) = happyShift action_115
action_87 (34) = happyShift action_78
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (36) = happyShift action_114
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (24) = happyShift action_90
action_89 (25) = happyShift action_91
action_89 (26) = happyShift action_92
action_89 (30) = happyShift action_93
action_89 (51) = happyShift action_94
action_89 (14) = happyGoto action_113
action_89 _ = happyReduce_64

action_90 _ = happyReduce_24

action_91 _ = happyReduce_25

action_92 _ = happyReduce_23

action_93 (51) = happyShift action_11
action_93 (7) = happyGoto action_112
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (47) = happyShift action_97
action_94 (19) = happyGoto action_111
action_94 _ = happyReduce_39

action_95 (54) = happyShift action_41
action_95 (55) = happyShift action_42
action_95 (56) = happyShift action_43
action_95 (57) = happyShift action_44
action_95 (58) = happyShift action_45
action_95 (59) = happyShift action_46
action_95 (60) = happyShift action_47
action_95 (61) = happyShift action_48
action_95 (62) = happyShift action_49
action_95 (63) = happyShift action_50
action_95 (64) = happyShift action_51
action_95 (65) = happyShift action_52
action_95 (66) = happyShift action_53
action_95 (67) = happyShift action_54
action_95 _ = happyReduce_7

action_96 (34) = happyShift action_110
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (48) = happyShift action_109
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (24) = happyShift action_17
action_98 (25) = happyShift action_18
action_98 (26) = happyShift action_19
action_98 (28) = happyShift action_20
action_98 (32) = happyShift action_21
action_98 (38) = happyShift action_22
action_98 (42) = happyShift action_107
action_98 (43) = happyShift action_108
action_98 (46) = happyShift action_23
action_98 (51) = happyShift action_24
action_98 (55) = happyShift action_25
action_98 (68) = happyShift action_26
action_98 (69) = happyShift action_27
action_98 (5) = happyGoto action_102
action_98 (16) = happyGoto action_103
action_98 (17) = happyGoto action_104
action_98 (18) = happyGoto action_105
action_98 (20) = happyGoto action_106
action_98 _ = happyReduce_37

action_99 _ = happyReduce_9

action_100 (51) = happyShift action_11
action_100 (7) = happyGoto action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_11

action_102 (37) = happyShift action_125
action_102 _ = happyReduce_40

action_103 _ = happyReduce_35

action_104 (41) = happyShift action_124
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (31) = happyShift action_123
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (54) = happyShift action_41
action_106 (55) = happyShift action_42
action_106 (56) = happyShift action_43
action_106 (57) = happyShift action_44
action_106 (58) = happyShift action_45
action_106 (59) = happyShift action_46
action_106 (60) = happyShift action_47
action_106 (61) = happyShift action_48
action_106 (62) = happyShift action_49
action_106 (63) = happyShift action_50
action_106 (64) = happyShift action_51
action_106 (65) = happyShift action_52
action_106 (66) = happyShift action_53
action_106 (67) = happyShift action_54
action_106 _ = happyReduce_30

action_107 (32) = happyShift action_122
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (32) = happyShift action_121
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_38

action_110 (27) = happyShift action_7
action_110 (30) = happyShift action_8
action_110 (51) = happyShift action_9
action_110 (9) = happyGoto action_3
action_110 (10) = happyGoto action_56
action_110 (21) = happyGoto action_120
action_110 _ = happyReduce_67

action_111 _ = happyReduce_26

action_112 (31) = happyShift action_119
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (36) = happyShift action_118
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (30) = happyShift action_117
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_41

action_116 _ = happyReduce_2

action_117 (24) = happyShift action_17
action_117 (25) = happyShift action_18
action_117 (26) = happyShift action_19
action_117 (28) = happyShift action_20
action_117 (32) = happyShift action_21
action_117 (38) = happyShift action_22
action_117 (42) = happyShift action_107
action_117 (43) = happyShift action_108
action_117 (46) = happyShift action_23
action_117 (51) = happyShift action_24
action_117 (55) = happyShift action_25
action_117 (68) = happyShift action_26
action_117 (69) = happyShift action_27
action_117 (5) = happyGoto action_102
action_117 (16) = happyGoto action_103
action_117 (17) = happyGoto action_104
action_117 (18) = happyGoto action_131
action_117 (20) = happyGoto action_106
action_117 _ = happyReduce_37

action_118 (30) = happyShift action_130
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_27

action_120 _ = happyReduce_66

action_121 (24) = happyShift action_17
action_121 (25) = happyShift action_18
action_121 (26) = happyShift action_19
action_121 (28) = happyShift action_20
action_121 (32) = happyShift action_21
action_121 (38) = happyShift action_22
action_121 (46) = happyShift action_23
action_121 (51) = happyShift action_24
action_121 (55) = happyShift action_25
action_121 (68) = happyShift action_26
action_121 (69) = happyShift action_27
action_121 (5) = happyGoto action_15
action_121 (20) = happyGoto action_129
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (24) = happyShift action_90
action_122 (25) = happyShift action_91
action_122 (26) = happyShift action_92
action_122 (30) = happyShift action_93
action_122 (51) = happyShift action_94
action_122 (14) = happyGoto action_128
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_68

action_124 (24) = happyShift action_17
action_124 (25) = happyShift action_18
action_124 (26) = happyShift action_19
action_124 (28) = happyShift action_20
action_124 (32) = happyShift action_21
action_124 (38) = happyShift action_22
action_124 (42) = happyShift action_107
action_124 (43) = happyShift action_108
action_124 (46) = happyShift action_23
action_124 (51) = happyShift action_24
action_124 (55) = happyShift action_25
action_124 (68) = happyShift action_26
action_124 (69) = happyShift action_27
action_124 (5) = happyGoto action_102
action_124 (16) = happyGoto action_127
action_124 (20) = happyGoto action_106
action_124 _ = happyReduce_36

action_125 (24) = happyShift action_17
action_125 (25) = happyShift action_18
action_125 (26) = happyShift action_19
action_125 (28) = happyShift action_20
action_125 (32) = happyShift action_21
action_125 (38) = happyShift action_22
action_125 (46) = happyShift action_23
action_125 (51) = happyShift action_24
action_125 (55) = happyShift action_25
action_125 (68) = happyShift action_26
action_125 (69) = happyShift action_27
action_125 (5) = happyGoto action_15
action_125 (20) = happyGoto action_126
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (54) = happyShift action_41
action_126 (55) = happyShift action_42
action_126 (56) = happyShift action_43
action_126 (57) = happyShift action_44
action_126 (58) = happyShift action_45
action_126 (59) = happyShift action_46
action_126 (60) = happyShift action_47
action_126 (61) = happyShift action_48
action_126 (62) = happyShift action_49
action_126 (63) = happyShift action_50
action_126 (64) = happyShift action_51
action_126 (65) = happyShift action_52
action_126 (66) = happyShift action_53
action_126 (67) = happyShift action_54
action_126 _ = happyReduce_33

action_127 _ = happyReduce_34

action_128 (35) = happyShift action_135
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (33) = happyShift action_134
action_129 (54) = happyShift action_41
action_129 (55) = happyShift action_42
action_129 (56) = happyShift action_43
action_129 (57) = happyShift action_44
action_129 (58) = happyShift action_45
action_129 (59) = happyShift action_46
action_129 (60) = happyShift action_47
action_129 (61) = happyShift action_48
action_129 (62) = happyShift action_49
action_129 (63) = happyShift action_50
action_129 (64) = happyShift action_51
action_129 (65) = happyShift action_52
action_129 (66) = happyShift action_53
action_129 (67) = happyShift action_54
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (24) = happyShift action_17
action_130 (25) = happyShift action_18
action_130 (26) = happyShift action_19
action_130 (28) = happyShift action_20
action_130 (32) = happyShift action_21
action_130 (38) = happyShift action_22
action_130 (42) = happyShift action_107
action_130 (43) = happyShift action_108
action_130 (46) = happyShift action_23
action_130 (51) = happyShift action_24
action_130 (55) = happyShift action_25
action_130 (68) = happyShift action_26
action_130 (69) = happyShift action_27
action_130 (5) = happyGoto action_102
action_130 (16) = happyGoto action_103
action_130 (17) = happyGoto action_104
action_130 (18) = happyGoto action_133
action_130 (20) = happyGoto action_106
action_130 _ = happyReduce_37

action_131 (31) = happyShift action_132
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_28

action_133 (31) = happyShift action_138
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (30) = happyShift action_137
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (24) = happyShift action_17
action_135 (25) = happyShift action_18
action_135 (26) = happyShift action_19
action_135 (28) = happyShift action_20
action_135 (32) = happyShift action_21
action_135 (38) = happyShift action_22
action_135 (46) = happyShift action_23
action_135 (51) = happyShift action_24
action_135 (55) = happyShift action_25
action_135 (68) = happyShift action_26
action_135 (69) = happyShift action_27
action_135 (5) = happyGoto action_15
action_135 (20) = happyGoto action_136
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (33) = happyShift action_145
action_136 (54) = happyShift action_41
action_136 (55) = happyShift action_42
action_136 (56) = happyShift action_43
action_136 (57) = happyShift action_44
action_136 (58) = happyShift action_45
action_136 (59) = happyShift action_46
action_136 (60) = happyShift action_47
action_136 (61) = happyShift action_48
action_136 (62) = happyShift action_49
action_136 (63) = happyShift action_50
action_136 (64) = happyShift action_51
action_136 (65) = happyShift action_52
action_136 (66) = happyShift action_53
action_136 (67) = happyShift action_54
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (24) = happyShift action_17
action_137 (25) = happyShift action_18
action_137 (26) = happyShift action_19
action_137 (28) = happyShift action_20
action_137 (32) = happyShift action_21
action_137 (38) = happyShift action_22
action_137 (42) = happyShift action_107
action_137 (43) = happyShift action_108
action_137 (44) = happyShift action_143
action_137 (45) = happyShift action_144
action_137 (46) = happyShift action_23
action_137 (51) = happyShift action_24
action_137 (55) = happyShift action_25
action_137 (68) = happyShift action_26
action_137 (69) = happyShift action_27
action_137 (5) = happyGoto action_102
action_137 (11) = happyGoto action_139
action_137 (12) = happyGoto action_140
action_137 (13) = happyGoto action_141
action_137 (16) = happyGoto action_142
action_137 (20) = happyGoto action_106
action_137 _ = happyFail (happyExpListPerState 137)

action_138 _ = happyReduce_29

action_139 _ = happyReduce_21

action_140 (41) = happyShift action_148
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (31) = happyShift action_147
action_141 _ = happyFail (happyExpListPerState 141)

action_142 _ = happyReduce_19

action_143 _ = happyReduce_18

action_144 _ = happyReduce_17

action_145 (30) = happyShift action_146
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (24) = happyShift action_17
action_146 (25) = happyShift action_18
action_146 (26) = happyShift action_19
action_146 (28) = happyShift action_20
action_146 (32) = happyShift action_21
action_146 (38) = happyShift action_22
action_146 (42) = happyShift action_107
action_146 (43) = happyShift action_108
action_146 (44) = happyShift action_143
action_146 (45) = happyShift action_144
action_146 (46) = happyShift action_23
action_146 (51) = happyShift action_24
action_146 (55) = happyShift action_25
action_146 (68) = happyShift action_26
action_146 (69) = happyShift action_27
action_146 (5) = happyGoto action_102
action_146 (11) = happyGoto action_139
action_146 (12) = happyGoto action_140
action_146 (13) = happyGoto action_150
action_146 (16) = happyGoto action_142
action_146 (20) = happyGoto action_106
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_32

action_148 (24) = happyShift action_17
action_148 (25) = happyShift action_18
action_148 (26) = happyShift action_19
action_148 (28) = happyShift action_20
action_148 (32) = happyShift action_21
action_148 (38) = happyShift action_22
action_148 (42) = happyShift action_107
action_148 (43) = happyShift action_108
action_148 (44) = happyShift action_143
action_148 (45) = happyShift action_144
action_148 (46) = happyShift action_23
action_148 (51) = happyShift action_24
action_148 (55) = happyShift action_25
action_148 (68) = happyShift action_26
action_148 (69) = happyShift action_27
action_148 (5) = happyGoto action_102
action_148 (11) = happyGoto action_149
action_148 (16) = happyGoto action_142
action_148 (20) = happyGoto action_106
action_148 _ = happyReduce_22

action_149 _ = happyReduce_20

action_150 (31) = happyShift action_151
action_150 _ = happyFail (happyExpListPerState 150)

action_151 _ = happyReduce_31

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn4
		 ([happy_var_2]
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_1 <> [happy_var_3]
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 |> happy_var_1 |> \(LIdentifier t p) ixs -> PLIndexed t ixs p
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_3 |> happy_var_1 |> \(LIdentifier t p) l -> PLDot t l p
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 |> \(LIdentifier t p) -> PLId t p
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 <> [happy_var_3]
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  6 happyReduction_8
happyReduction_8  =  HappyAbsSyn4
		 ([]
	)

happyReduce_9 = happyReduce 4 7 happyReduction_9
happyReduction_9 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((\(LIdentifier t p) ty rs -> RecordPattern(t,ty,p) rs p) happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_0  8 happyReduction_10
happyReduction_10  =  HappyAbsSyn8
		 ([]
	)

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 ((\(RecordPattern r rs _) -> r:rs) happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LAtom t p) -> PAtom t p
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LIdentifier t p) -> PId t p
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (PRecord happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (PUnion happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 |> \(LBreak p) -> Break p
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 |> \(LContinue p) -> Continue p
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn11
		 (LAction happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 <> [happy_var_3]
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  13 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 |> \(LNumber t p) -> PaNumber t p
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 |> \(LChar t p) -> PaChar t p
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 |> \(LString t p) -> PaString t p
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  14 happyReduction_26
happyReduction_26 (HappyAbsSyn19  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2 |> happy_var_1 |> \(LIdentifier t p) mref -> PaId t mref p
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (PaPattern happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 5 15 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ([(happy_var_1,happy_var_4)]
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 6 15 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (happy_var_1 <> [(happy_var_2,happy_var_5)]
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_1  16 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn16
		 (AExpression happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 9 16 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (happy_var_8 |> happy_var_5 |> happy_var_3 |> happy_var_1 |>
                                                  \(LFor p) pt e as -> For pt e as p
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 7 16 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (happy_var_6 |> happy_var_3 |> happy_var_1 |> 
                                        \(LWhile p) e as -> While e as p
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_3  16 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn16
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  17 happyReduction_34
happyReduction_34 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 <> [happy_var_3]
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  17 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  18 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  18 happyReduction_37
happyReduction_37  =  HappyAbsSyn17
		 ([]
	)

happyReduce_38 = happySpecReduce_2  19 happyReduction_38
happyReduction_38 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 & \(LBy p) -> Just $ ByRef p
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  19 happyReduction_39
happyReduction_39  =  HappyAbsSyn19
		 (Nothing
	)

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_41 = happyReduce 5 20 happyReduction_41
happyReduction_41 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (undefined
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_2  20 happyReduction_42
happyReduction_42 _
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_43 = happySpecReduce_2  20 happyReduction_43
happyReduction_43 _
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

happyReduce_57 = happySpecReduce_3  20 happyReduction_57
happyReduction_57 _
	_
	_
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_58 = happySpecReduce_2  20 happyReduction_58
happyReduction_58 _
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

happyReduce_60 = happySpecReduce_3  20 happyReduction_60
happyReduction_60 _
	_
	_
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

happyReduce_63 = happySpecReduce_1  20 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn20
		 (undefined
	)

happyReduce_64 = happyReduce 4 20 happyReduction_64
happyReduction_64 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (undefined
	) `HappyStk` happyRest

happyReduce_65 = happyReduce 4 20 happyReduction_65
happyReduction_65 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (undefined
	) `HappyStk` happyRest

happyReduce_66 = happyReduce 5 21 happyReduction_66
happyReduction_66 ((HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (happy_var_5 |> happy_var_3 |> happy_var_2 |> happy_var_1 |>
                                                       \t (LIdentifier x _) mref fs 
                                                        -> FunArg t x mref (getPTypesInfo t) : fs
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_0  21 happyReduction_67
happyReduction_67  =  HappyAbsSyn21
		 ([]
	)

happyReduce_68 = happyReduce 7 22 happyReduction_68
happyReduction_68 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (happy_var_6 |> happy_var_3 |> happy_var_1 |>
                                                    \t fs as 
                                                      -> FunctionDef t fs as (getPTypesInfo t)
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_2  23 happyReduction_69
happyReduction_69 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 <> [happy_var_2]
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  23 happyReduction_70
happyReduction_70 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_70 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	LEOF -> action 70 70 tk (HappyState action) sts stk;
	LChar _ _ -> cont 24;
	LString _ _ -> cont 25;
	LNumber _ _ -> cont 26;
	LAtom _ _ -> cont 27;
	LOBckt _ -> cont 28;
	LCBckt _ -> cont 29;
	LOBrc _ -> cont 30;
	LCBrc _ -> cont 31;
	LOParen _ -> cont 32;
	LCParen _ -> cont 33;
	LComma _ -> cont 34;
	LColon _ -> cont 35;
	LFatArrow _ -> cont 36;
	LAssign _ -> cont 37;
	LMatch _ -> cont 38;
	LWith _ -> cont 39;
	LType _ -> cont 40;
	LSemiColon _ -> cont 41;
	LFor _ -> cont 42;
	LWhile _ -> cont 43;
	LContinue _ -> cont 44;
	LBreak _ -> cont 45;
	LNew _ -> cont 46;
	LBy _ -> cont 47;
	LReference _ -> cont 48;
	LOp _ _ -> cont 49;
	LVBar _ -> cont 50;
	LIdentifier _ _ -> cont 51;
	LDot _ -> cont 52;
	LEOF -> cont 53;
	LOp "+" _ -> cont 54;
	LOp "-" _ -> cont 55;
	LOp "*" _ -> cont 56;
	LOp "/" _ -> cont 57;
	LOp "^" _ -> cont 58;
	LOp "%" _ -> cont 59;
	LOp "<" _ -> cont 60;
	LOp ">" _ -> cont 61;
	LOp "!=" _ -> cont 62;
	LOp "==" _ -> cont 63;
	LOp ">=" _ -> cont 64;
	LOp "<=" _ -> cont 65;
	LOp "||" _ -> cont 66;
	LOp "&&" _ -> cont 67;
	LOp "~"  _ -> cont 68;
	LOp "&" _ -> cont 69;
	_ -> happyError' (tk, [])
	})

happyError_ explist 70 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

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

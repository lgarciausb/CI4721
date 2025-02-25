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
	| HappyAbsSyn22 ([FunArg AlexPosn])
	| HappyAbsSyn23 (FunctionDef AlexPosn)
	| HappyAbsSyn24 ([FunctionDef AlexPosn])

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
 action_151,
 action_152,
 action_153,
 action_154 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,406) ([0,18432,0,8,0,0,16,0,0,0,5888,16449,136,48,0,0,0,0,0,0,0,12,0,0,0,0,0,0,18432,0,8,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,32768,0,0,0,0,2048,0,0,0,0,0,0,0,0,72,2048,0,0,0,1,0,0,0,0,0,0,0,8192,0,65472,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5888,16449,136,48,0,16663,34880,12288,0,5888,16449,136,48,0,72,2048,0,0,4096,1,16,0,0,16663,34880,12288,0,5888,16449,136,48,0,16663,34880,12288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,5888,16449,136,48,0,0,2048,0,0,0,1,4,0,0,32768,49152,4095,0,0,2,65472,15,0,1056,0,0,0,0,0,65472,15,0,0,0,0,0,5888,16449,136,48,0,16663,34880,12288,0,5888,16449,136,48,0,16663,34880,12288,0,5888,16449,136,48,0,16663,34880,12288,0,5888,16449,136,48,0,16663,34880,12288,0,5888,16449,136,48,0,16663,34880,12288,0,5888,16449,136,48,0,16663,34880,12288,0,5888,16449,136,48,0,16663,34880,12288,0,18432,0,8,0,0,0,0,0,0,18432,0,8,0,0,0,0,0,0,0,4,4,0,0,0,3072,0,0,0,6,0,0,0,0,49152,3071,0,0,0,65472,15,0,0,49152,15,0,0,0,4032,0,0,0,49152,15,0,0,0,4032,0,0,0,49152,15,0,0,0,4032,0,0,0,0,4,0,0,0,1024,0,0,0,0,4,0,0,0,1024,0,0,0,0,15,0,0,0,3840,0,0,0,0,0,0,5888,16449,136,48,0,0,0,0,0,18176,0,8,0,0,16663,34880,12288,0,0,0,0,0,0,16,4096,0,0,0,6,0,0,0,16663,34880,12288,0,8192,0,65472,15,0,0,0,0,0,0,6,0,0,0,4096,0,0,0,18176,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,128,0,0,0,0,65472,15,0,64,0,0,0,18432,0,8,0,0,0,128,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,5888,19521,136,112,0,0,0,0,0,32768,0,0,0,0,4096,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,16663,34892,28672,0,16384,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,512,0,0,0,128,0,0,0,0,0,65472,15,0,256,0,0,0,0,1,0,0,0,16663,34880,12288,0,0,0,0,0,0,0,49152,4095,0,5888,16449,136,48,0,71,2048,0,0,0,0,0,0,0,16663,34892,28672,0,5888,16449,136,48,0,16663,34892,28672,0,32768,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,49152,4095,0,0,0,0,0,0,2048,0,0,0,0,2,65472,15,0,64,0,0,0,5888,16449,136,48,0,0,0,0,0,0,2,65472,15,0,16663,34940,28672,0,0,0,0,0,0,0,2,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,16663,34940,28672,0,0,0,0,0,0,16663,34940,28672,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","ixs","lvaluable","args","records","mRecords","T0","T","loop_a0","loop_a","loop_as","pattern","patterns","a0","as","actions","optionalByRef","e","fun_args","m_fun_args","function_def","function_defs","char","string","number","atom","'['","']'","'{'","'}'","'('","')'","','","':'","'=>'","':='","match","with","type","';'","for","while","continue","break","new","by","reference","'***'","'|'","identifier","'.'","EOF","'+'","'-'","'*'","'/'","'^'","'%'","'<'","'>'","'!='","'=='","'>='","'<='","'||'","'&&'","'~'","'&'","'return'","%eof"]
        bit_start = st Prelude.* 72
        bit_end = (st Prelude.+ 1) Prelude.* 72
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..71]
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

action_3 _ = happyReduce_15

action_4 (51) = happyShift action_13
action_4 (52) = happyShift action_14
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_73

action_6 (28) = happyShift action_7
action_6 (31) = happyShift action_8
action_6 (52) = happyShift action_9
action_6 (72) = happyAccept
action_6 (9) = happyGoto action_3
action_6 (10) = happyGoto action_4
action_6 (23) = happyGoto action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_12

action_8 (52) = happyShift action_11
action_8 (7) = happyGoto action_10
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_13

action_10 (32) = happyShift action_57
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (36) = happyShift action_56
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_72

action_13 (28) = happyShift action_7
action_13 (31) = happyShift action_8
action_13 (52) = happyShift action_9
action_13 (9) = happyGoto action_55
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (33) = happyShift action_54
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_41

action_16 (30) = happyShift action_39
action_16 (55) = happyShift action_40
action_16 (56) = happyShift action_41
action_16 (57) = happyShift action_42
action_16 (58) = happyShift action_43
action_16 (59) = happyShift action_44
action_16 (60) = happyShift action_45
action_16 (61) = happyShift action_46
action_16 (62) = happyShift action_47
action_16 (63) = happyShift action_48
action_16 (64) = happyShift action_49
action_16 (65) = happyShift action_50
action_16 (66) = happyShift action_51
action_16 (67) = happyShift action_52
action_16 (68) = happyShift action_53
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_64

action_18 _ = happyReduce_63

action_19 _ = happyReduce_62

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
action_20 (6) = happyGoto action_37
action_20 (20) = happyGoto action_38
action_20 _ = happyReduce_8

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
action_21 (20) = happyGoto action_36
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
action_22 (20) = happyGoto action_35
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (28) = happyShift action_7
action_23 (31) = happyShift action_8
action_23 (52) = happyShift action_9
action_23 (9) = happyGoto action_3
action_23 (10) = happyGoto action_34
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (29) = happyShift action_2
action_24 (33) = happyShift action_32
action_24 (53) = happyShift action_33
action_24 (4) = happyGoto action_31
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
action_25 (20) = happyGoto action_30
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
action_26 (20) = happyGoto action_29
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (25) = happyShift action_17
action_27 (26) = happyShift action_18
action_27 (27) = happyShift action_19
action_27 (29) = happyShift action_20
action_27 (33) = happyShift action_21
action_27 (39) = happyShift action_22
action_27 (47) = happyShift action_23
action_27 (52) = happyShift action_24
action_27 (56) = happyShift action_25
action_27 (69) = happyShift action_26
action_27 (70) = happyShift action_27
action_27 (5) = happyGoto action_15
action_27 (20) = happyGoto action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_43

action_29 _ = happyReduce_59

action_30 _ = happyReduce_44

action_31 (29) = happyShift action_83
action_31 _ = happyReduce_3

action_32 (25) = happyShift action_17
action_32 (26) = happyShift action_18
action_32 (27) = happyShift action_19
action_32 (29) = happyShift action_20
action_32 (33) = happyShift action_21
action_32 (39) = happyShift action_22
action_32 (47) = happyShift action_23
action_32 (52) = happyShift action_24
action_32 (56) = happyShift action_25
action_32 (69) = happyShift action_26
action_32 (70) = happyShift action_27
action_32 (5) = happyGoto action_15
action_32 (6) = happyGoto action_82
action_32 (20) = happyGoto action_38
action_32 _ = happyReduce_8

action_33 (52) = happyShift action_81
action_33 (5) = happyGoto action_80
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (33) = happyShift action_79
action_34 (51) = happyShift action_13
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (40) = happyShift action_78
action_35 (55) = happyShift action_40
action_35 (56) = happyShift action_41
action_35 (57) = happyShift action_42
action_35 (58) = happyShift action_43
action_35 (59) = happyShift action_44
action_35 (60) = happyShift action_45
action_35 (61) = happyShift action_46
action_35 (62) = happyShift action_47
action_35 (63) = happyShift action_48
action_35 (64) = happyShift action_49
action_35 (65) = happyShift action_50
action_35 (66) = happyShift action_51
action_35 (67) = happyShift action_52
action_35 (68) = happyShift action_53
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (34) = happyShift action_77
action_36 (55) = happyShift action_40
action_36 (56) = happyShift action_41
action_36 (57) = happyShift action_42
action_36 (58) = happyShift action_43
action_36 (59) = happyShift action_44
action_36 (60) = happyShift action_45
action_36 (61) = happyShift action_46
action_36 (62) = happyShift action_47
action_36 (63) = happyShift action_48
action_36 (64) = happyShift action_49
action_36 (65) = happyShift action_50
action_36 (66) = happyShift action_51
action_36 (67) = happyShift action_52
action_36 (68) = happyShift action_53
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (30) = happyShift action_75
action_37 (35) = happyShift action_76
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (55) = happyShift action_40
action_38 (56) = happyShift action_41
action_38 (57) = happyShift action_42
action_38 (58) = happyShift action_43
action_38 (59) = happyShift action_44
action_38 (60) = happyShift action_45
action_38 (61) = happyShift action_46
action_38 (62) = happyShift action_47
action_38 (63) = happyShift action_48
action_38 (64) = happyShift action_49
action_38 (65) = happyShift action_50
action_38 (66) = happyShift action_51
action_38 (67) = happyShift action_52
action_38 (68) = happyShift action_53
action_38 _ = happyReduce_6

action_39 _ = happyReduce_1

action_40 (25) = happyShift action_17
action_40 (26) = happyShift action_18
action_40 (27) = happyShift action_19
action_40 (29) = happyShift action_20
action_40 (33) = happyShift action_21
action_40 (39) = happyShift action_22
action_40 (47) = happyShift action_23
action_40 (52) = happyShift action_24
action_40 (56) = happyShift action_25
action_40 (69) = happyShift action_26
action_40 (70) = happyShift action_27
action_40 (5) = happyGoto action_15
action_40 (20) = happyGoto action_74
action_40 _ = happyFail (happyExpListPerState 40)

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
action_41 (20) = happyGoto action_73
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
action_42 (20) = happyGoto action_72
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
action_43 (20) = happyGoto action_71
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
action_44 (20) = happyGoto action_70
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
action_45 (20) = happyGoto action_69
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
action_46 (20) = happyGoto action_68
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
action_47 (20) = happyGoto action_67
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
action_48 (20) = happyGoto action_66
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
action_49 (20) = happyGoto action_65
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
action_50 (20) = happyGoto action_64
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
action_51 (20) = happyGoto action_63
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
action_52 (20) = happyGoto action_62
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
action_53 (20) = happyGoto action_61
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (28) = happyShift action_7
action_54 (31) = happyShift action_8
action_54 (52) = happyShift action_9
action_54 (9) = happyGoto action_3
action_54 (10) = happyGoto action_59
action_54 (21) = happyGoto action_60
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_16

action_56 (28) = happyShift action_7
action_56 (31) = happyShift action_8
action_56 (52) = happyShift action_9
action_56 (9) = happyGoto action_3
action_56 (10) = happyGoto action_58
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_14

action_58 (35) = happyShift action_99
action_58 (51) = happyShift action_13
action_58 (8) = happyGoto action_98
action_58 _ = happyReduce_10

action_59 (51) = happyShift action_13
action_59 (52) = happyShift action_97
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (34) = happyShift action_95
action_60 (35) = happyShift action_96
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (55) = happyShift action_40
action_61 (56) = happyShift action_41
action_61 (57) = happyShift action_42
action_61 (58) = happyShift action_43
action_61 (59) = happyShift action_44
action_61 (60) = happyShift action_45
action_61 (61) = happyShift action_46
action_61 (62) = happyShift action_47
action_61 (63) = happyShift action_48
action_61 (64) = happyShift action_49
action_61 (65) = happyShift action_50
action_61 (66) = happyShift action_51
action_61 (68) = happyShift action_53
action_61 _ = happyReduce_58

action_62 (55) = happyShift action_40
action_62 (56) = happyShift action_41
action_62 (57) = happyShift action_42
action_62 (58) = happyShift action_43
action_62 (59) = happyShift action_44
action_62 (60) = happyShift action_45
action_62 (61) = happyShift action_46
action_62 (62) = happyShift action_47
action_62 (63) = happyShift action_48
action_62 (64) = happyShift action_49
action_62 (65) = happyShift action_50
action_62 (66) = happyShift action_51
action_62 (67) = happyShift action_52
action_62 (68) = happyShift action_53
action_62 _ = happyReduce_57

action_63 (55) = happyShift action_40
action_63 (56) = happyShift action_41
action_63 (57) = happyShift action_42
action_63 (58) = happyShift action_43
action_63 (59) = happyShift action_44
action_63 (60) = happyShift action_45
action_63 _ = happyReduce_56

action_64 (55) = happyShift action_40
action_64 (56) = happyShift action_41
action_64 (57) = happyShift action_42
action_64 (58) = happyShift action_43
action_64 (59) = happyShift action_44
action_64 (60) = happyShift action_45
action_64 _ = happyReduce_55

action_65 (55) = happyShift action_40
action_65 (56) = happyShift action_41
action_65 (57) = happyShift action_42
action_65 (58) = happyShift action_43
action_65 (59) = happyShift action_44
action_65 (60) = happyShift action_45
action_65 _ = happyReduce_54

action_66 (55) = happyShift action_40
action_66 (56) = happyShift action_41
action_66 (57) = happyShift action_42
action_66 (58) = happyShift action_43
action_66 (59) = happyShift action_44
action_66 (60) = happyShift action_45
action_66 _ = happyReduce_53

action_67 (55) = happyShift action_40
action_67 (56) = happyShift action_41
action_67 (57) = happyShift action_42
action_67 (58) = happyShift action_43
action_67 (59) = happyShift action_44
action_67 (60) = happyShift action_45
action_67 _ = happyReduce_52

action_68 (55) = happyShift action_40
action_68 (56) = happyShift action_41
action_68 (57) = happyShift action_42
action_68 (58) = happyShift action_43
action_68 (59) = happyShift action_44
action_68 (60) = happyShift action_45
action_68 _ = happyReduce_51

action_69 (59) = happyShift action_44
action_69 _ = happyReduce_49

action_70 (59) = happyShift action_44
action_70 _ = happyReduce_48

action_71 (59) = happyShift action_44
action_71 _ = happyReduce_47

action_72 (59) = happyShift action_44
action_72 _ = happyReduce_46

action_73 (57) = happyShift action_42
action_73 (58) = happyShift action_43
action_73 (59) = happyShift action_44
action_73 (60) = happyShift action_45
action_73 _ = happyReduce_50

action_74 (57) = happyShift action_42
action_74 (58) = happyShift action_43
action_74 (59) = happyShift action_44
action_74 (60) = happyShift action_45
action_74 _ = happyReduce_45

action_75 _ = happyReduce_61

action_76 (25) = happyShift action_17
action_76 (26) = happyShift action_18
action_76 (27) = happyShift action_19
action_76 (29) = happyShift action_20
action_76 (33) = happyShift action_21
action_76 (39) = happyShift action_22
action_76 (47) = happyShift action_23
action_76 (52) = happyShift action_24
action_76 (56) = happyShift action_25
action_76 (69) = happyShift action_26
action_76 (70) = happyShift action_27
action_76 (5) = happyGoto action_15
action_76 (20) = happyGoto action_94
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_60

action_78 (25) = happyShift action_89
action_78 (26) = happyShift action_90
action_78 (27) = happyShift action_91
action_78 (31) = happyShift action_92
action_78 (52) = happyShift action_93
action_78 (14) = happyGoto action_87
action_78 (15) = happyGoto action_88
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (25) = happyShift action_17
action_79 (26) = happyShift action_18
action_79 (27) = happyShift action_19
action_79 (29) = happyShift action_20
action_79 (33) = happyShift action_21
action_79 (39) = happyShift action_22
action_79 (47) = happyShift action_23
action_79 (52) = happyShift action_24
action_79 (56) = happyShift action_25
action_79 (69) = happyShift action_26
action_79 (70) = happyShift action_27
action_79 (5) = happyGoto action_15
action_79 (6) = happyGoto action_86
action_79 (20) = happyGoto action_38
action_79 _ = happyReduce_8

action_80 _ = happyReduce_4

action_81 (29) = happyShift action_2
action_81 (53) = happyShift action_33
action_81 (4) = happyGoto action_31
action_81 _ = happyReduce_5

action_82 (34) = happyShift action_85
action_82 (35) = happyShift action_76
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (25) = happyShift action_17
action_83 (26) = happyShift action_18
action_83 (27) = happyShift action_19
action_83 (29) = happyShift action_20
action_83 (33) = happyShift action_21
action_83 (39) = happyShift action_22
action_83 (47) = happyShift action_23
action_83 (52) = happyShift action_24
action_83 (56) = happyShift action_25
action_83 (69) = happyShift action_26
action_83 (70) = happyShift action_27
action_83 (5) = happyGoto action_15
action_83 (20) = happyGoto action_84
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (30) = happyShift action_110
action_84 (55) = happyShift action_40
action_84 (56) = happyShift action_41
action_84 (57) = happyShift action_42
action_84 (58) = happyShift action_43
action_84 (59) = happyShift action_44
action_84 (60) = happyShift action_45
action_84 (61) = happyShift action_46
action_84 (62) = happyShift action_47
action_84 (63) = happyShift action_48
action_84 (64) = happyShift action_49
action_84 (65) = happyShift action_50
action_84 (66) = happyShift action_51
action_84 (67) = happyShift action_52
action_84 (68) = happyShift action_53
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_66

action_86 (34) = happyShift action_109
action_86 (35) = happyShift action_76
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (37) = happyShift action_108
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (25) = happyShift action_89
action_88 (26) = happyShift action_90
action_88 (27) = happyShift action_91
action_88 (31) = happyShift action_92
action_88 (52) = happyShift action_93
action_88 (14) = happyGoto action_107
action_88 _ = happyReduce_65

action_89 _ = happyReduce_24

action_90 _ = happyReduce_25

action_91 _ = happyReduce_23

action_92 (52) = happyShift action_11
action_92 (7) = happyGoto action_106
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (48) = happyShift action_102
action_93 (19) = happyGoto action_105
action_93 _ = happyReduce_40

action_94 (55) = happyShift action_40
action_94 (56) = happyShift action_41
action_94 (57) = happyShift action_42
action_94 (58) = happyShift action_43
action_94 (59) = happyShift action_44
action_94 (60) = happyShift action_45
action_94 (61) = happyShift action_46
action_94 (62) = happyShift action_47
action_94 (63) = happyShift action_48
action_94 (64) = happyShift action_49
action_94 (65) = happyShift action_50
action_94 (66) = happyShift action_51
action_94 (67) = happyShift action_52
action_94 (68) = happyShift action_53
action_94 _ = happyReduce_7

action_95 (31) = happyShift action_104
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (28) = happyShift action_7
action_96 (31) = happyShift action_8
action_96 (52) = happyShift action_9
action_96 (9) = happyGoto action_3
action_96 (10) = happyGoto action_59
action_96 (21) = happyGoto action_103
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (48) = happyShift action_102
action_97 (19) = happyGoto action_101
action_97 _ = happyReduce_40

action_98 _ = happyReduce_9

action_99 (52) = happyShift action_11
action_99 (7) = happyGoto action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_11

action_101 _ = happyReduce_68

action_102 (49) = happyShift action_122
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (35) = happyShift action_96
action_103 _ = happyReduce_67

action_104 (25) = happyShift action_17
action_104 (26) = happyShift action_18
action_104 (27) = happyShift action_19
action_104 (29) = happyShift action_20
action_104 (33) = happyShift action_21
action_104 (39) = happyShift action_22
action_104 (43) = happyShift action_119
action_104 (44) = happyShift action_120
action_104 (47) = happyShift action_23
action_104 (52) = happyShift action_24
action_104 (56) = happyShift action_25
action_104 (69) = happyShift action_26
action_104 (70) = happyShift action_27
action_104 (71) = happyShift action_121
action_104 (5) = happyGoto action_114
action_104 (16) = happyGoto action_115
action_104 (17) = happyGoto action_116
action_104 (18) = happyGoto action_117
action_104 (20) = happyGoto action_118
action_104 _ = happyReduce_38

action_105 _ = happyReduce_26

action_106 (32) = happyShift action_113
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (37) = happyShift action_112
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (31) = happyShift action_111
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_42

action_110 _ = happyReduce_2

action_111 (25) = happyShift action_17
action_111 (26) = happyShift action_18
action_111 (27) = happyShift action_19
action_111 (29) = happyShift action_20
action_111 (33) = happyShift action_21
action_111 (39) = happyShift action_22
action_111 (43) = happyShift action_119
action_111 (44) = happyShift action_120
action_111 (47) = happyShift action_23
action_111 (52) = happyShift action_24
action_111 (56) = happyShift action_25
action_111 (69) = happyShift action_26
action_111 (70) = happyShift action_27
action_111 (71) = happyShift action_121
action_111 (5) = happyGoto action_114
action_111 (16) = happyGoto action_115
action_111 (17) = happyGoto action_116
action_111 (18) = happyGoto action_130
action_111 (20) = happyGoto action_118
action_111 _ = happyReduce_38

action_112 (31) = happyShift action_129
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_27

action_114 (38) = happyShift action_128
action_114 _ = happyReduce_41

action_115 _ = happyReduce_36

action_116 (42) = happyShift action_127
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (32) = happyShift action_126
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (55) = happyShift action_40
action_118 (56) = happyShift action_41
action_118 (57) = happyShift action_42
action_118 (58) = happyShift action_43
action_118 (59) = happyShift action_44
action_118 (60) = happyShift action_45
action_118 (61) = happyShift action_46
action_118 (62) = happyShift action_47
action_118 (63) = happyShift action_48
action_118 (64) = happyShift action_49
action_118 (65) = happyShift action_50
action_118 (66) = happyShift action_51
action_118 (67) = happyShift action_52
action_118 (68) = happyShift action_53
action_118 _ = happyReduce_30

action_119 (33) = happyShift action_125
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (33) = happyShift action_124
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (25) = happyShift action_17
action_121 (26) = happyShift action_18
action_121 (27) = happyShift action_19
action_121 (29) = happyShift action_20
action_121 (33) = happyShift action_21
action_121 (39) = happyShift action_22
action_121 (47) = happyShift action_23
action_121 (52) = happyShift action_24
action_121 (56) = happyShift action_25
action_121 (69) = happyShift action_26
action_121 (70) = happyShift action_27
action_121 (5) = happyGoto action_15
action_121 (20) = happyGoto action_123
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_39

action_123 (55) = happyShift action_40
action_123 (56) = happyShift action_41
action_123 (57) = happyShift action_42
action_123 (58) = happyShift action_43
action_123 (59) = happyShift action_44
action_123 (60) = happyShift action_45
action_123 (61) = happyShift action_46
action_123 (62) = happyShift action_47
action_123 (63) = happyShift action_48
action_123 (64) = happyShift action_49
action_123 (65) = happyShift action_50
action_123 (66) = happyShift action_51
action_123 (67) = happyShift action_52
action_123 (68) = happyShift action_53
action_123 _ = happyReduce_34

action_124 (25) = happyShift action_17
action_124 (26) = happyShift action_18
action_124 (27) = happyShift action_19
action_124 (29) = happyShift action_20
action_124 (33) = happyShift action_21
action_124 (39) = happyShift action_22
action_124 (47) = happyShift action_23
action_124 (52) = happyShift action_24
action_124 (56) = happyShift action_25
action_124 (69) = happyShift action_26
action_124 (70) = happyShift action_27
action_124 (5) = happyGoto action_15
action_124 (20) = happyGoto action_136
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (25) = happyShift action_89
action_125 (26) = happyShift action_90
action_125 (27) = happyShift action_91
action_125 (31) = happyShift action_92
action_125 (52) = happyShift action_93
action_125 (14) = happyGoto action_135
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_71

action_127 (25) = happyShift action_17
action_127 (26) = happyShift action_18
action_127 (27) = happyShift action_19
action_127 (29) = happyShift action_20
action_127 (33) = happyShift action_21
action_127 (39) = happyShift action_22
action_127 (43) = happyShift action_119
action_127 (44) = happyShift action_120
action_127 (47) = happyShift action_23
action_127 (52) = happyShift action_24
action_127 (56) = happyShift action_25
action_127 (69) = happyShift action_26
action_127 (70) = happyShift action_27
action_127 (71) = happyShift action_121
action_127 (5) = happyGoto action_114
action_127 (16) = happyGoto action_134
action_127 (20) = happyGoto action_118
action_127 _ = happyReduce_37

action_128 (25) = happyShift action_17
action_128 (26) = happyShift action_18
action_128 (27) = happyShift action_19
action_128 (29) = happyShift action_20
action_128 (33) = happyShift action_21
action_128 (39) = happyShift action_22
action_128 (47) = happyShift action_23
action_128 (52) = happyShift action_24
action_128 (56) = happyShift action_25
action_128 (69) = happyShift action_26
action_128 (70) = happyShift action_27
action_128 (5) = happyGoto action_15
action_128 (20) = happyGoto action_133
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (25) = happyShift action_17
action_129 (26) = happyShift action_18
action_129 (27) = happyShift action_19
action_129 (29) = happyShift action_20
action_129 (33) = happyShift action_21
action_129 (39) = happyShift action_22
action_129 (43) = happyShift action_119
action_129 (44) = happyShift action_120
action_129 (47) = happyShift action_23
action_129 (52) = happyShift action_24
action_129 (56) = happyShift action_25
action_129 (69) = happyShift action_26
action_129 (70) = happyShift action_27
action_129 (71) = happyShift action_121
action_129 (5) = happyGoto action_114
action_129 (16) = happyGoto action_115
action_129 (17) = happyGoto action_116
action_129 (18) = happyGoto action_132
action_129 (20) = happyGoto action_118
action_129 _ = happyReduce_38

action_130 (32) = happyShift action_131
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_28

action_132 (32) = happyShift action_139
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (55) = happyShift action_40
action_133 (56) = happyShift action_41
action_133 (57) = happyShift action_42
action_133 (58) = happyShift action_43
action_133 (59) = happyShift action_44
action_133 (60) = happyShift action_45
action_133 (61) = happyShift action_46
action_133 (62) = happyShift action_47
action_133 (63) = happyShift action_48
action_133 (64) = happyShift action_49
action_133 (65) = happyShift action_50
action_133 (66) = happyShift action_51
action_133 (67) = happyShift action_52
action_133 (68) = happyShift action_53
action_133 _ = happyReduce_33

action_134 _ = happyReduce_35

action_135 (36) = happyShift action_138
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (34) = happyShift action_137
action_136 (55) = happyShift action_40
action_136 (56) = happyShift action_41
action_136 (57) = happyShift action_42
action_136 (58) = happyShift action_43
action_136 (59) = happyShift action_44
action_136 (60) = happyShift action_45
action_136 (61) = happyShift action_46
action_136 (62) = happyShift action_47
action_136 (63) = happyShift action_48
action_136 (64) = happyShift action_49
action_136 (65) = happyShift action_50
action_136 (66) = happyShift action_51
action_136 (67) = happyShift action_52
action_136 (68) = happyShift action_53
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (31) = happyShift action_141
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (25) = happyShift action_17
action_138 (26) = happyShift action_18
action_138 (27) = happyShift action_19
action_138 (29) = happyShift action_20
action_138 (33) = happyShift action_21
action_138 (39) = happyShift action_22
action_138 (47) = happyShift action_23
action_138 (52) = happyShift action_24
action_138 (56) = happyShift action_25
action_138 (69) = happyShift action_26
action_138 (70) = happyShift action_27
action_138 (5) = happyGoto action_15
action_138 (20) = happyGoto action_140
action_138 _ = happyFail (happyExpListPerState 138)

action_139 _ = happyReduce_29

action_140 (34) = happyShift action_148
action_140 (55) = happyShift action_40
action_140 (56) = happyShift action_41
action_140 (57) = happyShift action_42
action_140 (58) = happyShift action_43
action_140 (59) = happyShift action_44
action_140 (60) = happyShift action_45
action_140 (61) = happyShift action_46
action_140 (62) = happyShift action_47
action_140 (63) = happyShift action_48
action_140 (64) = happyShift action_49
action_140 (65) = happyShift action_50
action_140 (66) = happyShift action_51
action_140 (67) = happyShift action_52
action_140 (68) = happyShift action_53
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (25) = happyShift action_17
action_141 (26) = happyShift action_18
action_141 (27) = happyShift action_19
action_141 (29) = happyShift action_20
action_141 (33) = happyShift action_21
action_141 (39) = happyShift action_22
action_141 (43) = happyShift action_119
action_141 (44) = happyShift action_120
action_141 (45) = happyShift action_146
action_141 (46) = happyShift action_147
action_141 (47) = happyShift action_23
action_141 (52) = happyShift action_24
action_141 (56) = happyShift action_25
action_141 (69) = happyShift action_26
action_141 (70) = happyShift action_27
action_141 (71) = happyShift action_121
action_141 (5) = happyGoto action_114
action_141 (11) = happyGoto action_142
action_141 (12) = happyGoto action_143
action_141 (13) = happyGoto action_144
action_141 (16) = happyGoto action_145
action_141 (20) = happyGoto action_118
action_141 _ = happyFail (happyExpListPerState 141)

action_142 _ = happyReduce_21

action_143 (42) = happyShift action_151
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (32) = happyShift action_150
action_144 _ = happyFail (happyExpListPerState 144)

action_145 _ = happyReduce_19

action_146 _ = happyReduce_18

action_147 _ = happyReduce_17

action_148 (31) = happyShift action_149
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (25) = happyShift action_17
action_149 (26) = happyShift action_18
action_149 (27) = happyShift action_19
action_149 (29) = happyShift action_20
action_149 (33) = happyShift action_21
action_149 (39) = happyShift action_22
action_149 (43) = happyShift action_119
action_149 (44) = happyShift action_120
action_149 (45) = happyShift action_146
action_149 (46) = happyShift action_147
action_149 (47) = happyShift action_23
action_149 (52) = happyShift action_24
action_149 (56) = happyShift action_25
action_149 (69) = happyShift action_26
action_149 (70) = happyShift action_27
action_149 (71) = happyShift action_121
action_149 (5) = happyGoto action_114
action_149 (11) = happyGoto action_142
action_149 (12) = happyGoto action_143
action_149 (13) = happyGoto action_153
action_149 (16) = happyGoto action_145
action_149 (20) = happyGoto action_118
action_149 _ = happyFail (happyExpListPerState 149)

action_150 _ = happyReduce_32

action_151 (25) = happyShift action_17
action_151 (26) = happyShift action_18
action_151 (27) = happyShift action_19
action_151 (29) = happyShift action_20
action_151 (33) = happyShift action_21
action_151 (39) = happyShift action_22
action_151 (43) = happyShift action_119
action_151 (44) = happyShift action_120
action_151 (45) = happyShift action_146
action_151 (46) = happyShift action_147
action_151 (47) = happyShift action_23
action_151 (52) = happyShift action_24
action_151 (56) = happyShift action_25
action_151 (69) = happyShift action_26
action_151 (70) = happyShift action_27
action_151 (71) = happyShift action_121
action_151 (5) = happyGoto action_114
action_151 (11) = happyGoto action_152
action_151 (16) = happyGoto action_145
action_151 (20) = happyGoto action_118
action_151 _ = happyReduce_22

action_152 _ = happyReduce_20

action_153 (32) = happyShift action_154
action_153 _ = happyFail (happyExpListPerState 153)

action_154 _ = happyReduce_31

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

happyReduce_34 = happySpecReduce_2  16 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 |> \(LIdentifier _ p) -> Return happy_var_2 p
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  17 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 <> [happy_var_3]
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  17 happyReduction_36
happyReduction_36 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  18 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  18 happyReduction_38
happyReduction_38  =  HappyAbsSyn17
		 ([]
	)

happyReduce_39 = happySpecReduce_2  19 happyReduction_39
happyReduction_39 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 & \(LBy p) -> Just $ ByRef p
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_0  19 happyReduction_40
happyReduction_40  =  HappyAbsSyn19
		 (Nothing
	)

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn20
		 (ELValuable happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happyReduce 5 20 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_1 |> \(LNew p) -> New happy_var_2 happy_var_4 p
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_2  20 happyReduction_43
happyReduction_43 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LOp "&" p) -> Ref happy_var_2 p
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  20 happyReduction_44
happyReduction_44 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LOp "-" p) -> Neg happy_var_2 p
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  20 happyReduction_45
happyReduction_45 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Plus happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  20 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Times happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  20 happyReduction_47
happyReduction_47 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Divide happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  20 happyReduction_48
happyReduction_48 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Power happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  20 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Mod happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  20 happyReduction_50
happyReduction_50 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Minus happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  20 happyReduction_51
happyReduction_51 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ELT happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  20 happyReduction_52
happyReduction_52 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EGT happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  20 happyReduction_53
happyReduction_53 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (NotEq happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  20 happyReduction_54
happyReduction_54 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EEq happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  20 happyReduction_55
happyReduction_55 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EGTEq happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  20 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ELTEq happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  20 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Or happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  20 happyReduction_58
happyReduction_58 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (And happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  20 happyReduction_59
happyReduction_59 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LOp "~" p) -> Not happy_var_2 p
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  20 happyReduction_60
happyReduction_60 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  20 happyReduction_61
happyReduction_61 _
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LOBckt p) -> Arr happy_var_2 p
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  20 happyReduction_62
happyReduction_62 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LNumber t p) -> ENumber t p
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  20 happyReduction_63
happyReduction_63 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LString t p) -> EString t p
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  20 happyReduction_64
happyReduction_64 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LChar t p) -> EChar t p
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happyReduce 4 20 happyReduction_65
happyReduction_65 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_1 |> \(LMatch p) -> Match happy_var_2 happy_var_4 p
	) `HappyStk` happyRest

happyReduce_66 = happyReduce 4 20 happyReduction_66
happyReduction_66 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_1 |> \(LIdentifier t p) -> FApp t happy_var_3 p
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_3  21 happyReduction_67
happyReduction_67 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <> happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  21 happyReduction_68
happyReduction_68 (HappyAbsSyn19  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_3 |> happy_var_2 |> happy_var_1 |> \t (LIdentifier x _) mref -> [FunArg t x mref $ getPTypesInfo t]
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  22 happyReduction_69
happyReduction_69 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  22 happyReduction_70
happyReduction_70  =  HappyAbsSyn22
		 ([]
	)

happyReduce_71 = happyReduce 8 23 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (happy_var_7 |> happy_var_4 |> happy_var_2 |> happy_var_1 |>
                                                    \t (LIdentifier name _) fs as 
                                                      -> FunctionDef t name fs as (getPTypesInfo t)
	) `HappyStk` happyRest

happyReduce_72 = happySpecReduce_2  24 happyReduction_72
happyReduction_72 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 <> [happy_var_2]
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  24 happyReduction_73
happyReduction_73 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_73 _  = notHappyAtAll 

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
	LOp "&" _ -> cont 70;
	LIdentifier "return" _ -> cont 71;
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

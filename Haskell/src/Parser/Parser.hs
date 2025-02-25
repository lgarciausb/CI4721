{-# OPTIONS_GHC -w #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
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
	| HappyAbsSyn23 (Definition AlexPosn)
	| HappyAbsSyn24 ([Definition AlexPosn])

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
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,516) ([0,61440,159,2050,0,0,0,32,0,0,0,3840,33312,34944,12288,0,0,0,0,0,0,0,0,3072,0,0,0,0,0,0,0,61440,159,2050,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,2048,0,0,0,0,2048,0,0,0,0,0,0,0,0,16384,0,0,0,0,256,0,0,0,0,4096,0,0,0,61440,159,2048,0,0,0,0,0,0,0,61440,159,2048,0,0,0,512,0,0,0,0,0,0,0,0,0,64,49152,4095,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,61440,159,2048,0,0,0,544,4096,0,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,3840,33312,34944,12288,0,0,0,2048,0,0,0,512,1024,0,0,0,0,49153,4095,0,0,1024,49152,4095,0,0,2112,0,0,0,0,0,49152,4095,0,0,0,0,0,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,3840,33312,34944,12288,0,61440,159,2048,0,0,0,0,0,0,0,0,0,1024,32,0,61440,159,2048,0,0,0,0,0,0,0,61440,159,2048,0,0,0,0,1024,0,0,0,2048,1024,0,0,0,0,0,0,0,0,0,3072,0,0,0,2048,0,0,0,0,1024,0,0,0,0,0,49152,3071,0,0,0,49152,4095,0,0,0,49152,15,0,0,0,49152,15,0,0,0,49152,15,0,0,0,49152,15,0,0,0,49152,15,0,0,0,49152,15,0,0,0,0,4,0,0,0,0,4,0,0,0,0,4,0,0,0,0,4,0,0,0,0,15,0,0,0,0,15,0,0,0,0,0,0,3840,33312,34944,12288,0,0,0,0,0,0,3840,128,2048,0,0,3840,33312,34944,12288,0,0,0,0,0,0,0,32,4096,0,0,0,3072,0,0,0,3840,33312,34944,12288,0,0,64,49152,4095,0,0,0,0,0,0,0,3072,0,0,0,0,8192,0,0,0,3840,128,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,256,0,0,0,0,49152,4095,0,0,128,0,0,0,61440,159,2048,0,0,0,0,256,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,65280,33471,34968,28672,0,0,0,0,0,0,0,256,0,0,0,0,8192,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,65280,33471,34968,28672,0,0,128,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,3072,0,0,0,0,0,0,0,0,0,4,0,0,0,256,0,0,0,0,0,49152,4095,0,0,512,0,0,0,0,512,0,0,0,0,544,4096,0,0,3840,33316,34944,12288,0,0,0,0,0,0,0,0,49152,4095,0,0,0,0,0,0,3840,33312,34944,12288,0,3840,128,2048,0,0,0,0,0,0,0,65280,33471,34968,28672,0,0,16384,0,0,0,3840,33312,34944,12288,0,65280,33471,34968,28672,0,0,256,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,49152,4095,0,3840,33312,34944,12288,0,0,0,0,0,0,0,4096,0,0,0,0,1024,49152,4095,0,0,128,0,0,0,3840,33312,34944,12288,0,0,0,49152,4095,0,0,0,0,0,0,0,1024,49152,4095,0,65280,33471,35064,28672,0,0,0,0,0,0,0,0,4,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,65280,33471,35064,28672,0,0,0,0,0,0,65280,33471,35064,28672,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","ixs","lvaluable","args","records","mRecords","T0","T","loop_a0","loop_a","loop_as","pattern","patterns","a0","as","actions","optionalByRef","e","fun_args","m_fun_args","definition","definitions","charlit","stringlit","boollit","number","atom","bool","int","float","char","string","unit","void","vector","'['","']'","'{'","'}'","'('","')'","','","':'","'=>'","':='","match","with","type","';'","for","while","continue","break","new","by","reference","'|'","identifier","'.'","EOF","'+'","'-'","'*'","'/'","'^'","'%'","'<'","'>'","'!='","'=='","'>='","'<='","'||'","'&&'","'~'","'&'","return","%eof"]
        bit_start = st Prelude.* 80
        bit_end = (st Prelude.+ 1) Prelude.* 80
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..79]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (29) = happyShift action_7
action_0 (30) = happyShift action_8
action_0 (31) = happyShift action_9
action_0 (32) = happyShift action_10
action_0 (33) = happyShift action_11
action_0 (34) = happyShift action_12
action_0 (35) = happyShift action_13
action_0 (36) = happyShift action_14
action_0 (37) = happyShift action_15
action_0 (40) = happyShift action_16
action_0 (50) = happyShift action_17
action_0 (60) = happyShift action_18
action_0 (9) = happyGoto action_3
action_0 (10) = happyGoto action_4
action_0 (23) = happyGoto action_5
action_0 (24) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (38) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (25) = happyShift action_28
action_2 (26) = happyShift action_29
action_2 (27) = happyShift action_30
action_2 (28) = happyShift action_31
action_2 (38) = happyShift action_32
action_2 (42) = happyShift action_33
action_2 (48) = happyShift action_34
action_2 (56) = happyShift action_35
action_2 (60) = happyShift action_36
action_2 (64) = happyShift action_37
action_2 (77) = happyShift action_38
action_2 (78) = happyShift action_39
action_2 (5) = happyGoto action_26
action_2 (20) = happyGoto action_27
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_23

action_4 (59) = happyShift action_24
action_4 (60) = happyShift action_25
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_87

action_6 (29) = happyShift action_7
action_6 (30) = happyShift action_8
action_6 (31) = happyShift action_9
action_6 (32) = happyShift action_10
action_6 (33) = happyShift action_11
action_6 (34) = happyShift action_12
action_6 (35) = happyShift action_13
action_6 (36) = happyShift action_14
action_6 (37) = happyShift action_15
action_6 (40) = happyShift action_16
action_6 (50) = happyShift action_17
action_6 (60) = happyShift action_18
action_6 (80) = happyAccept
action_6 (9) = happyGoto action_3
action_6 (10) = happyGoto action_4
action_6 (23) = happyGoto action_23
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_12

action_8 _ = happyReduce_13

action_9 _ = happyReduce_14

action_10 _ = happyReduce_15

action_11 _ = happyReduce_16

action_12 _ = happyReduce_17

action_13 _ = happyReduce_18

action_14 _ = happyReduce_19

action_15 (69) = happyShift action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (60) = happyShift action_21
action_16 (7) = happyGoto action_20
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (60) = happyShift action_19
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_21

action_19 (47) = happyShift action_71
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (41) = happyShift action_70
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (45) = happyShift action_69
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (29) = happyShift action_7
action_22 (30) = happyShift action_8
action_22 (31) = happyShift action_9
action_22 (32) = happyShift action_10
action_22 (33) = happyShift action_11
action_22 (34) = happyShift action_12
action_22 (35) = happyShift action_13
action_22 (36) = happyShift action_14
action_22 (37) = happyShift action_15
action_22 (40) = happyShift action_16
action_22 (60) = happyShift action_18
action_22 (9) = happyGoto action_3
action_22 (10) = happyGoto action_68
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_86

action_24 (29) = happyShift action_7
action_24 (30) = happyShift action_8
action_24 (31) = happyShift action_9
action_24 (32) = happyShift action_10
action_24 (33) = happyShift action_11
action_24 (34) = happyShift action_12
action_24 (35) = happyShift action_13
action_24 (36) = happyShift action_14
action_24 (37) = happyShift action_15
action_24 (40) = happyShift action_16
action_24 (60) = happyShift action_18
action_24 (9) = happyGoto action_67
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (42) = happyShift action_66
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_53

action_27 (39) = happyShift action_51
action_27 (63) = happyShift action_52
action_27 (64) = happyShift action_53
action_27 (65) = happyShift action_54
action_27 (66) = happyShift action_55
action_27 (67) = happyShift action_56
action_27 (68) = happyShift action_57
action_27 (69) = happyShift action_58
action_27 (70) = happyShift action_59
action_27 (71) = happyShift action_60
action_27 (72) = happyShift action_61
action_27 (73) = happyShift action_62
action_27 (74) = happyShift action_63
action_27 (75) = happyShift action_64
action_27 (76) = happyShift action_65
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_76

action_29 _ = happyReduce_75

action_30 _ = happyReduce_77

action_31 _ = happyReduce_74

action_32 (25) = happyShift action_28
action_32 (26) = happyShift action_29
action_32 (27) = happyShift action_30
action_32 (28) = happyShift action_31
action_32 (38) = happyShift action_32
action_32 (42) = happyShift action_33
action_32 (48) = happyShift action_34
action_32 (56) = happyShift action_35
action_32 (60) = happyShift action_36
action_32 (64) = happyShift action_37
action_32 (77) = happyShift action_38
action_32 (78) = happyShift action_39
action_32 (5) = happyGoto action_26
action_32 (6) = happyGoto action_49
action_32 (20) = happyGoto action_50
action_32 _ = happyReduce_8

action_33 (25) = happyShift action_28
action_33 (26) = happyShift action_29
action_33 (27) = happyShift action_30
action_33 (28) = happyShift action_31
action_33 (38) = happyShift action_32
action_33 (42) = happyShift action_33
action_33 (48) = happyShift action_34
action_33 (56) = happyShift action_35
action_33 (60) = happyShift action_36
action_33 (64) = happyShift action_37
action_33 (77) = happyShift action_38
action_33 (78) = happyShift action_39
action_33 (5) = happyGoto action_26
action_33 (20) = happyGoto action_48
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (25) = happyShift action_28
action_34 (26) = happyShift action_29
action_34 (27) = happyShift action_30
action_34 (28) = happyShift action_31
action_34 (38) = happyShift action_32
action_34 (42) = happyShift action_33
action_34 (48) = happyShift action_34
action_34 (56) = happyShift action_35
action_34 (60) = happyShift action_36
action_34 (64) = happyShift action_37
action_34 (77) = happyShift action_38
action_34 (78) = happyShift action_39
action_34 (5) = happyGoto action_26
action_34 (20) = happyGoto action_47
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (29) = happyShift action_7
action_35 (30) = happyShift action_8
action_35 (31) = happyShift action_9
action_35 (32) = happyShift action_10
action_35 (33) = happyShift action_11
action_35 (34) = happyShift action_12
action_35 (35) = happyShift action_13
action_35 (36) = happyShift action_14
action_35 (37) = happyShift action_15
action_35 (40) = happyShift action_16
action_35 (60) = happyShift action_18
action_35 (9) = happyGoto action_3
action_35 (10) = happyGoto action_46
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (38) = happyShift action_2
action_36 (42) = happyShift action_44
action_36 (61) = happyShift action_45
action_36 (4) = happyGoto action_43
action_36 _ = happyReduce_5

action_37 (25) = happyShift action_28
action_37 (26) = happyShift action_29
action_37 (27) = happyShift action_30
action_37 (28) = happyShift action_31
action_37 (38) = happyShift action_32
action_37 (42) = happyShift action_33
action_37 (48) = happyShift action_34
action_37 (56) = happyShift action_35
action_37 (60) = happyShift action_36
action_37 (64) = happyShift action_37
action_37 (77) = happyShift action_38
action_37 (78) = happyShift action_39
action_37 (5) = happyGoto action_26
action_37 (20) = happyGoto action_42
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (25) = happyShift action_28
action_38 (26) = happyShift action_29
action_38 (27) = happyShift action_30
action_38 (28) = happyShift action_31
action_38 (38) = happyShift action_32
action_38 (42) = happyShift action_33
action_38 (48) = happyShift action_34
action_38 (56) = happyShift action_35
action_38 (60) = happyShift action_36
action_38 (64) = happyShift action_37
action_38 (77) = happyShift action_38
action_38 (78) = happyShift action_39
action_38 (5) = happyGoto action_26
action_38 (20) = happyGoto action_41
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (25) = happyShift action_28
action_39 (26) = happyShift action_29
action_39 (27) = happyShift action_30
action_39 (28) = happyShift action_31
action_39 (38) = happyShift action_32
action_39 (42) = happyShift action_33
action_39 (48) = happyShift action_34
action_39 (56) = happyShift action_35
action_39 (60) = happyShift action_36
action_39 (64) = happyShift action_37
action_39 (77) = happyShift action_38
action_39 (78) = happyShift action_39
action_39 (5) = happyGoto action_26
action_39 (20) = happyGoto action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_55

action_41 _ = happyReduce_71

action_42 _ = happyReduce_56

action_43 (38) = happyShift action_100
action_43 _ = happyReduce_3

action_44 (25) = happyShift action_28
action_44 (26) = happyShift action_29
action_44 (27) = happyShift action_30
action_44 (28) = happyShift action_31
action_44 (38) = happyShift action_32
action_44 (42) = happyShift action_33
action_44 (48) = happyShift action_34
action_44 (56) = happyShift action_35
action_44 (60) = happyShift action_36
action_44 (64) = happyShift action_37
action_44 (77) = happyShift action_38
action_44 (78) = happyShift action_39
action_44 (5) = happyGoto action_26
action_44 (6) = happyGoto action_99
action_44 (20) = happyGoto action_50
action_44 _ = happyReduce_8

action_45 (60) = happyShift action_98
action_45 (5) = happyGoto action_97
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (42) = happyShift action_96
action_46 (59) = happyShift action_24
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (49) = happyShift action_95
action_47 (63) = happyShift action_52
action_47 (64) = happyShift action_53
action_47 (65) = happyShift action_54
action_47 (66) = happyShift action_55
action_47 (67) = happyShift action_56
action_47 (68) = happyShift action_57
action_47 (69) = happyShift action_58
action_47 (70) = happyShift action_59
action_47 (71) = happyShift action_60
action_47 (72) = happyShift action_61
action_47 (73) = happyShift action_62
action_47 (74) = happyShift action_63
action_47 (75) = happyShift action_64
action_47 (76) = happyShift action_65
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (43) = happyShift action_94
action_48 (63) = happyShift action_52
action_48 (64) = happyShift action_53
action_48 (65) = happyShift action_54
action_48 (66) = happyShift action_55
action_48 (67) = happyShift action_56
action_48 (68) = happyShift action_57
action_48 (69) = happyShift action_58
action_48 (70) = happyShift action_59
action_48 (71) = happyShift action_60
action_48 (72) = happyShift action_61
action_48 (73) = happyShift action_62
action_48 (74) = happyShift action_63
action_48 (75) = happyShift action_64
action_48 (76) = happyShift action_65
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (39) = happyShift action_92
action_49 (44) = happyShift action_93
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (63) = happyShift action_52
action_50 (64) = happyShift action_53
action_50 (65) = happyShift action_54
action_50 (66) = happyShift action_55
action_50 (67) = happyShift action_56
action_50 (68) = happyShift action_57
action_50 (69) = happyShift action_58
action_50 (70) = happyShift action_59
action_50 (71) = happyShift action_60
action_50 (72) = happyShift action_61
action_50 (73) = happyShift action_62
action_50 (74) = happyShift action_63
action_50 (75) = happyShift action_64
action_50 (76) = happyShift action_65
action_50 _ = happyReduce_6

action_51 _ = happyReduce_1

action_52 (25) = happyShift action_28
action_52 (26) = happyShift action_29
action_52 (27) = happyShift action_30
action_52 (28) = happyShift action_31
action_52 (38) = happyShift action_32
action_52 (42) = happyShift action_33
action_52 (48) = happyShift action_34
action_52 (56) = happyShift action_35
action_52 (60) = happyShift action_36
action_52 (64) = happyShift action_37
action_52 (77) = happyShift action_38
action_52 (78) = happyShift action_39
action_52 (5) = happyGoto action_26
action_52 (20) = happyGoto action_91
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (25) = happyShift action_28
action_53 (26) = happyShift action_29
action_53 (27) = happyShift action_30
action_53 (28) = happyShift action_31
action_53 (38) = happyShift action_32
action_53 (42) = happyShift action_33
action_53 (48) = happyShift action_34
action_53 (56) = happyShift action_35
action_53 (60) = happyShift action_36
action_53 (64) = happyShift action_37
action_53 (77) = happyShift action_38
action_53 (78) = happyShift action_39
action_53 (5) = happyGoto action_26
action_53 (20) = happyGoto action_90
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (25) = happyShift action_28
action_54 (26) = happyShift action_29
action_54 (27) = happyShift action_30
action_54 (28) = happyShift action_31
action_54 (38) = happyShift action_32
action_54 (42) = happyShift action_33
action_54 (48) = happyShift action_34
action_54 (56) = happyShift action_35
action_54 (60) = happyShift action_36
action_54 (64) = happyShift action_37
action_54 (77) = happyShift action_38
action_54 (78) = happyShift action_39
action_54 (5) = happyGoto action_26
action_54 (20) = happyGoto action_89
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (25) = happyShift action_28
action_55 (26) = happyShift action_29
action_55 (27) = happyShift action_30
action_55 (28) = happyShift action_31
action_55 (38) = happyShift action_32
action_55 (42) = happyShift action_33
action_55 (48) = happyShift action_34
action_55 (56) = happyShift action_35
action_55 (60) = happyShift action_36
action_55 (64) = happyShift action_37
action_55 (77) = happyShift action_38
action_55 (78) = happyShift action_39
action_55 (5) = happyGoto action_26
action_55 (20) = happyGoto action_88
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (25) = happyShift action_28
action_56 (26) = happyShift action_29
action_56 (27) = happyShift action_30
action_56 (28) = happyShift action_31
action_56 (38) = happyShift action_32
action_56 (42) = happyShift action_33
action_56 (48) = happyShift action_34
action_56 (56) = happyShift action_35
action_56 (60) = happyShift action_36
action_56 (64) = happyShift action_37
action_56 (77) = happyShift action_38
action_56 (78) = happyShift action_39
action_56 (5) = happyGoto action_26
action_56 (20) = happyGoto action_87
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (25) = happyShift action_28
action_57 (26) = happyShift action_29
action_57 (27) = happyShift action_30
action_57 (28) = happyShift action_31
action_57 (38) = happyShift action_32
action_57 (42) = happyShift action_33
action_57 (48) = happyShift action_34
action_57 (56) = happyShift action_35
action_57 (60) = happyShift action_36
action_57 (64) = happyShift action_37
action_57 (77) = happyShift action_38
action_57 (78) = happyShift action_39
action_57 (5) = happyGoto action_26
action_57 (20) = happyGoto action_86
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (25) = happyShift action_28
action_58 (26) = happyShift action_29
action_58 (27) = happyShift action_30
action_58 (28) = happyShift action_31
action_58 (38) = happyShift action_32
action_58 (42) = happyShift action_33
action_58 (48) = happyShift action_34
action_58 (56) = happyShift action_35
action_58 (60) = happyShift action_36
action_58 (64) = happyShift action_37
action_58 (77) = happyShift action_38
action_58 (78) = happyShift action_39
action_58 (5) = happyGoto action_26
action_58 (20) = happyGoto action_85
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (25) = happyShift action_28
action_59 (26) = happyShift action_29
action_59 (27) = happyShift action_30
action_59 (28) = happyShift action_31
action_59 (38) = happyShift action_32
action_59 (42) = happyShift action_33
action_59 (48) = happyShift action_34
action_59 (56) = happyShift action_35
action_59 (60) = happyShift action_36
action_59 (64) = happyShift action_37
action_59 (77) = happyShift action_38
action_59 (78) = happyShift action_39
action_59 (5) = happyGoto action_26
action_59 (20) = happyGoto action_84
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (25) = happyShift action_28
action_60 (26) = happyShift action_29
action_60 (27) = happyShift action_30
action_60 (28) = happyShift action_31
action_60 (38) = happyShift action_32
action_60 (42) = happyShift action_33
action_60 (48) = happyShift action_34
action_60 (56) = happyShift action_35
action_60 (60) = happyShift action_36
action_60 (64) = happyShift action_37
action_60 (77) = happyShift action_38
action_60 (78) = happyShift action_39
action_60 (5) = happyGoto action_26
action_60 (20) = happyGoto action_83
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (25) = happyShift action_28
action_61 (26) = happyShift action_29
action_61 (27) = happyShift action_30
action_61 (28) = happyShift action_31
action_61 (38) = happyShift action_32
action_61 (42) = happyShift action_33
action_61 (48) = happyShift action_34
action_61 (56) = happyShift action_35
action_61 (60) = happyShift action_36
action_61 (64) = happyShift action_37
action_61 (77) = happyShift action_38
action_61 (78) = happyShift action_39
action_61 (5) = happyGoto action_26
action_61 (20) = happyGoto action_82
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (25) = happyShift action_28
action_62 (26) = happyShift action_29
action_62 (27) = happyShift action_30
action_62 (28) = happyShift action_31
action_62 (38) = happyShift action_32
action_62 (42) = happyShift action_33
action_62 (48) = happyShift action_34
action_62 (56) = happyShift action_35
action_62 (60) = happyShift action_36
action_62 (64) = happyShift action_37
action_62 (77) = happyShift action_38
action_62 (78) = happyShift action_39
action_62 (5) = happyGoto action_26
action_62 (20) = happyGoto action_81
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (25) = happyShift action_28
action_63 (26) = happyShift action_29
action_63 (27) = happyShift action_30
action_63 (28) = happyShift action_31
action_63 (38) = happyShift action_32
action_63 (42) = happyShift action_33
action_63 (48) = happyShift action_34
action_63 (56) = happyShift action_35
action_63 (60) = happyShift action_36
action_63 (64) = happyShift action_37
action_63 (77) = happyShift action_38
action_63 (78) = happyShift action_39
action_63 (5) = happyGoto action_26
action_63 (20) = happyGoto action_80
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (25) = happyShift action_28
action_64 (26) = happyShift action_29
action_64 (27) = happyShift action_30
action_64 (28) = happyShift action_31
action_64 (38) = happyShift action_32
action_64 (42) = happyShift action_33
action_64 (48) = happyShift action_34
action_64 (56) = happyShift action_35
action_64 (60) = happyShift action_36
action_64 (64) = happyShift action_37
action_64 (77) = happyShift action_38
action_64 (78) = happyShift action_39
action_64 (5) = happyGoto action_26
action_64 (20) = happyGoto action_79
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (25) = happyShift action_28
action_65 (26) = happyShift action_29
action_65 (27) = happyShift action_30
action_65 (28) = happyShift action_31
action_65 (38) = happyShift action_32
action_65 (42) = happyShift action_33
action_65 (48) = happyShift action_34
action_65 (56) = happyShift action_35
action_65 (60) = happyShift action_36
action_65 (64) = happyShift action_37
action_65 (77) = happyShift action_38
action_65 (78) = happyShift action_39
action_65 (5) = happyGoto action_26
action_65 (20) = happyGoto action_78
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (29) = happyShift action_7
action_66 (30) = happyShift action_8
action_66 (31) = happyShift action_9
action_66 (32) = happyShift action_10
action_66 (33) = happyShift action_11
action_66 (34) = happyShift action_12
action_66 (35) = happyShift action_13
action_66 (36) = happyShift action_14
action_66 (37) = happyShift action_15
action_66 (40) = happyShift action_16
action_66 (60) = happyShift action_18
action_66 (9) = happyGoto action_3
action_66 (10) = happyGoto action_75
action_66 (21) = happyGoto action_76
action_66 (22) = happyGoto action_77
action_66 _ = happyReduce_83

action_67 _ = happyReduce_24

action_68 (59) = happyShift action_24
action_68 (70) = happyShift action_74
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (29) = happyShift action_7
action_69 (30) = happyShift action_8
action_69 (31) = happyShift action_9
action_69 (32) = happyShift action_10
action_69 (33) = happyShift action_11
action_69 (34) = happyShift action_12
action_69 (35) = happyShift action_13
action_69 (36) = happyShift action_14
action_69 (37) = happyShift action_15
action_69 (40) = happyShift action_16
action_69 (60) = happyShift action_18
action_69 (9) = happyGoto action_3
action_69 (10) = happyGoto action_73
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_22

action_71 (29) = happyShift action_7
action_71 (30) = happyShift action_8
action_71 (31) = happyShift action_9
action_71 (32) = happyShift action_10
action_71 (33) = happyShift action_11
action_71 (34) = happyShift action_12
action_71 (35) = happyShift action_13
action_71 (36) = happyShift action_14
action_71 (37) = happyShift action_15
action_71 (40) = happyShift action_16
action_71 (60) = happyShift action_18
action_71 (9) = happyGoto action_3
action_71 (10) = happyGoto action_72
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (59) = happyShift action_24
action_72 _ = happyReduce_85

action_73 (44) = happyShift action_117
action_73 (59) = happyShift action_24
action_73 (8) = happyGoto action_116
action_73 _ = happyReduce_10

action_74 _ = happyReduce_20

action_75 (59) = happyShift action_24
action_75 (60) = happyShift action_115
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (44) = happyShift action_114
action_76 _ = happyReduce_82

action_77 (43) = happyShift action_113
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (63) = happyShift action_52
action_78 (64) = happyShift action_53
action_78 (65) = happyShift action_54
action_78 (66) = happyShift action_55
action_78 (67) = happyShift action_56
action_78 (68) = happyShift action_57
action_78 (69) = happyShift action_58
action_78 (70) = happyShift action_59
action_78 (71) = happyShift action_60
action_78 (72) = happyShift action_61
action_78 (73) = happyShift action_62
action_78 (74) = happyShift action_63
action_78 (76) = happyShift action_65
action_78 _ = happyReduce_70

action_79 (63) = happyShift action_52
action_79 (64) = happyShift action_53
action_79 (65) = happyShift action_54
action_79 (66) = happyShift action_55
action_79 (67) = happyShift action_56
action_79 (68) = happyShift action_57
action_79 (69) = happyShift action_58
action_79 (70) = happyShift action_59
action_79 (71) = happyShift action_60
action_79 (72) = happyShift action_61
action_79 (73) = happyShift action_62
action_79 (74) = happyShift action_63
action_79 (75) = happyShift action_64
action_79 (76) = happyShift action_65
action_79 _ = happyReduce_69

action_80 (63) = happyShift action_52
action_80 (64) = happyShift action_53
action_80 (65) = happyShift action_54
action_80 (66) = happyShift action_55
action_80 (67) = happyShift action_56
action_80 (68) = happyShift action_57
action_80 _ = happyReduce_68

action_81 (63) = happyShift action_52
action_81 (64) = happyShift action_53
action_81 (65) = happyShift action_54
action_81 (66) = happyShift action_55
action_81 (67) = happyShift action_56
action_81 (68) = happyShift action_57
action_81 _ = happyReduce_67

action_82 (63) = happyShift action_52
action_82 (64) = happyShift action_53
action_82 (65) = happyShift action_54
action_82 (66) = happyShift action_55
action_82 (67) = happyShift action_56
action_82 (68) = happyShift action_57
action_82 _ = happyReduce_66

action_83 (63) = happyShift action_52
action_83 (64) = happyShift action_53
action_83 (65) = happyShift action_54
action_83 (66) = happyShift action_55
action_83 (67) = happyShift action_56
action_83 (68) = happyShift action_57
action_83 _ = happyReduce_65

action_84 (63) = happyShift action_52
action_84 (64) = happyShift action_53
action_84 (65) = happyShift action_54
action_84 (66) = happyShift action_55
action_84 (67) = happyShift action_56
action_84 (68) = happyShift action_57
action_84 _ = happyReduce_64

action_85 (63) = happyShift action_52
action_85 (64) = happyShift action_53
action_85 (65) = happyShift action_54
action_85 (66) = happyShift action_55
action_85 (67) = happyShift action_56
action_85 (68) = happyShift action_57
action_85 _ = happyReduce_63

action_86 (67) = happyShift action_56
action_86 _ = happyReduce_61

action_87 (67) = happyShift action_56
action_87 _ = happyReduce_60

action_88 (67) = happyShift action_56
action_88 _ = happyReduce_59

action_89 (67) = happyShift action_56
action_89 _ = happyReduce_58

action_90 (65) = happyShift action_54
action_90 (66) = happyShift action_55
action_90 (67) = happyShift action_56
action_90 (68) = happyShift action_57
action_90 _ = happyReduce_62

action_91 (65) = happyShift action_54
action_91 (66) = happyShift action_55
action_91 (67) = happyShift action_56
action_91 (68) = happyShift action_57
action_91 _ = happyReduce_57

action_92 _ = happyReduce_73

action_93 (25) = happyShift action_28
action_93 (26) = happyShift action_29
action_93 (27) = happyShift action_30
action_93 (28) = happyShift action_31
action_93 (38) = happyShift action_32
action_93 (42) = happyShift action_33
action_93 (48) = happyShift action_34
action_93 (56) = happyShift action_35
action_93 (60) = happyShift action_36
action_93 (64) = happyShift action_37
action_93 (77) = happyShift action_38
action_93 (78) = happyShift action_39
action_93 (5) = happyGoto action_26
action_93 (20) = happyGoto action_112
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_72

action_95 (25) = happyShift action_106
action_95 (26) = happyShift action_107
action_95 (27) = happyShift action_108
action_95 (28) = happyShift action_109
action_95 (40) = happyShift action_110
action_95 (60) = happyShift action_111
action_95 (14) = happyGoto action_104
action_95 (15) = happyGoto action_105
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (25) = happyShift action_28
action_96 (26) = happyShift action_29
action_96 (27) = happyShift action_30
action_96 (28) = happyShift action_31
action_96 (38) = happyShift action_32
action_96 (42) = happyShift action_33
action_96 (48) = happyShift action_34
action_96 (56) = happyShift action_35
action_96 (60) = happyShift action_36
action_96 (64) = happyShift action_37
action_96 (77) = happyShift action_38
action_96 (78) = happyShift action_39
action_96 (5) = happyGoto action_26
action_96 (6) = happyGoto action_103
action_96 (20) = happyGoto action_50
action_96 _ = happyReduce_8

action_97 _ = happyReduce_4

action_98 (38) = happyShift action_2
action_98 (61) = happyShift action_45
action_98 (4) = happyGoto action_43
action_98 _ = happyReduce_5

action_99 (43) = happyShift action_102
action_99 (44) = happyShift action_93
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (25) = happyShift action_28
action_100 (26) = happyShift action_29
action_100 (27) = happyShift action_30
action_100 (28) = happyShift action_31
action_100 (38) = happyShift action_32
action_100 (42) = happyShift action_33
action_100 (48) = happyShift action_34
action_100 (56) = happyShift action_35
action_100 (60) = happyShift action_36
action_100 (64) = happyShift action_37
action_100 (77) = happyShift action_38
action_100 (78) = happyShift action_39
action_100 (5) = happyGoto action_26
action_100 (20) = happyGoto action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (39) = happyShift action_128
action_101 (63) = happyShift action_52
action_101 (64) = happyShift action_53
action_101 (65) = happyShift action_54
action_101 (66) = happyShift action_55
action_101 (67) = happyShift action_56
action_101 (68) = happyShift action_57
action_101 (69) = happyShift action_58
action_101 (70) = happyShift action_59
action_101 (71) = happyShift action_60
action_101 (72) = happyShift action_61
action_101 (73) = happyShift action_62
action_101 (74) = happyShift action_63
action_101 (75) = happyShift action_64
action_101 (76) = happyShift action_65
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_79

action_103 (43) = happyShift action_127
action_103 (44) = happyShift action_93
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (46) = happyShift action_126
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (25) = happyShift action_106
action_105 (26) = happyShift action_107
action_105 (27) = happyShift action_108
action_105 (28) = happyShift action_109
action_105 (40) = happyShift action_110
action_105 (60) = happyShift action_111
action_105 (14) = happyGoto action_125
action_105 _ = happyReduce_78

action_106 _ = happyReduce_32

action_107 _ = happyReduce_33

action_108 _ = happyReduce_34

action_109 _ = happyReduce_31

action_110 (60) = happyShift action_21
action_110 (7) = happyGoto action_124
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (57) = happyShift action_120
action_111 (19) = happyGoto action_123
action_111 _ = happyReduce_52

action_112 (63) = happyShift action_52
action_112 (64) = happyShift action_53
action_112 (65) = happyShift action_54
action_112 (66) = happyShift action_55
action_112 (67) = happyShift action_56
action_112 (68) = happyShift action_57
action_112 (69) = happyShift action_58
action_112 (70) = happyShift action_59
action_112 (71) = happyShift action_60
action_112 (72) = happyShift action_61
action_112 (73) = happyShift action_62
action_112 (74) = happyShift action_63
action_112 (75) = happyShift action_64
action_112 (76) = happyShift action_65
action_112 _ = happyReduce_7

action_113 (40) = happyShift action_122
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (29) = happyShift action_7
action_114 (30) = happyShift action_8
action_114 (31) = happyShift action_9
action_114 (32) = happyShift action_10
action_114 (33) = happyShift action_11
action_114 (34) = happyShift action_12
action_114 (35) = happyShift action_13
action_114 (36) = happyShift action_14
action_114 (37) = happyShift action_15
action_114 (40) = happyShift action_16
action_114 (60) = happyShift action_18
action_114 (9) = happyGoto action_3
action_114 (10) = happyGoto action_75
action_114 (21) = happyGoto action_121
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (57) = happyShift action_120
action_115 (19) = happyGoto action_119
action_115 _ = happyReduce_52

action_116 _ = happyReduce_9

action_117 (60) = happyShift action_21
action_117 (7) = happyGoto action_118
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_11

action_119 _ = happyReduce_81

action_120 (58) = happyShift action_142
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_80

action_122 (25) = happyShift action_28
action_122 (26) = happyShift action_29
action_122 (27) = happyShift action_30
action_122 (28) = happyShift action_31
action_122 (29) = happyShift action_7
action_122 (30) = happyShift action_8
action_122 (31) = happyShift action_9
action_122 (32) = happyShift action_10
action_122 (33) = happyShift action_11
action_122 (34) = happyShift action_12
action_122 (35) = happyShift action_13
action_122 (36) = happyShift action_14
action_122 (37) = happyShift action_15
action_122 (38) = happyShift action_32
action_122 (40) = happyShift action_16
action_122 (42) = happyShift action_33
action_122 (48) = happyShift action_34
action_122 (52) = happyShift action_138
action_122 (53) = happyShift action_139
action_122 (56) = happyShift action_35
action_122 (60) = happyShift action_140
action_122 (64) = happyShift action_37
action_122 (77) = happyShift action_38
action_122 (78) = happyShift action_39
action_122 (79) = happyShift action_141
action_122 (5) = happyGoto action_132
action_122 (9) = happyGoto action_3
action_122 (10) = happyGoto action_133
action_122 (16) = happyGoto action_134
action_122 (17) = happyGoto action_135
action_122 (18) = happyGoto action_136
action_122 (20) = happyGoto action_137
action_122 _ = happyReduce_50

action_123 _ = happyReduce_35

action_124 (41) = happyShift action_131
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (46) = happyShift action_130
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (40) = happyShift action_129
action_126 _ = happyFail (happyExpListPerState 126)

action_127 _ = happyReduce_54

action_128 _ = happyReduce_2

action_129 (25) = happyShift action_28
action_129 (26) = happyShift action_29
action_129 (27) = happyShift action_30
action_129 (28) = happyShift action_31
action_129 (29) = happyShift action_7
action_129 (30) = happyShift action_8
action_129 (31) = happyShift action_9
action_129 (32) = happyShift action_10
action_129 (33) = happyShift action_11
action_129 (34) = happyShift action_12
action_129 (35) = happyShift action_13
action_129 (36) = happyShift action_14
action_129 (37) = happyShift action_15
action_129 (38) = happyShift action_32
action_129 (40) = happyShift action_16
action_129 (42) = happyShift action_33
action_129 (48) = happyShift action_34
action_129 (52) = happyShift action_138
action_129 (53) = happyShift action_139
action_129 (56) = happyShift action_35
action_129 (60) = happyShift action_140
action_129 (64) = happyShift action_37
action_129 (77) = happyShift action_38
action_129 (78) = happyShift action_39
action_129 (79) = happyShift action_141
action_129 (5) = happyGoto action_132
action_129 (9) = happyGoto action_3
action_129 (10) = happyGoto action_133
action_129 (16) = happyGoto action_134
action_129 (17) = happyGoto action_135
action_129 (18) = happyGoto action_152
action_129 (20) = happyGoto action_137
action_129 _ = happyReduce_50

action_130 (40) = happyShift action_151
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_36

action_132 (47) = happyShift action_150
action_132 _ = happyReduce_53

action_133 (59) = happyShift action_24
action_133 (60) = happyShift action_149
action_133 _ = happyFail (happyExpListPerState 133)

action_134 _ = happyReduce_48

action_135 (51) = happyShift action_148
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (41) = happyShift action_147
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (63) = happyShift action_52
action_137 (64) = happyShift action_53
action_137 (65) = happyShift action_54
action_137 (66) = happyShift action_55
action_137 (67) = happyShift action_56
action_137 (68) = happyShift action_57
action_137 (69) = happyShift action_58
action_137 (70) = happyShift action_59
action_137 (71) = happyShift action_60
action_137 (72) = happyShift action_61
action_137 (73) = happyShift action_62
action_137 (74) = happyShift action_63
action_137 (75) = happyShift action_64
action_137 (76) = happyShift action_65
action_137 _ = happyReduce_39

action_138 (42) = happyShift action_146
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (42) = happyShift action_145
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (38) = happyShift action_2
action_140 (42) = happyShift action_44
action_140 (59) = happyReduce_21
action_140 (60) = happyReduce_21
action_140 (61) = happyShift action_45
action_140 (4) = happyGoto action_43
action_140 _ = happyReduce_5

action_141 (25) = happyShift action_28
action_141 (26) = happyShift action_29
action_141 (27) = happyShift action_30
action_141 (28) = happyShift action_31
action_141 (35) = happyShift action_144
action_141 (38) = happyShift action_32
action_141 (42) = happyShift action_33
action_141 (48) = happyShift action_34
action_141 (56) = happyShift action_35
action_141 (60) = happyShift action_36
action_141 (64) = happyShift action_37
action_141 (77) = happyShift action_38
action_141 (78) = happyShift action_39
action_141 (5) = happyGoto action_26
action_141 (20) = happyGoto action_143
action_141 _ = happyFail (happyExpListPerState 141)

action_142 _ = happyReduce_51

action_143 (63) = happyShift action_52
action_143 (64) = happyShift action_53
action_143 (65) = happyShift action_54
action_143 (66) = happyShift action_55
action_143 (67) = happyShift action_56
action_143 (68) = happyShift action_57
action_143 (69) = happyShift action_58
action_143 (70) = happyShift action_59
action_143 (71) = happyShift action_60
action_143 (72) = happyShift action_61
action_143 (73) = happyShift action_62
action_143 (74) = happyShift action_63
action_143 (75) = happyShift action_64
action_143 (76) = happyShift action_65
action_143 _ = happyReduce_45

action_144 _ = happyReduce_46

action_145 (25) = happyShift action_28
action_145 (26) = happyShift action_29
action_145 (27) = happyShift action_30
action_145 (28) = happyShift action_31
action_145 (38) = happyShift action_32
action_145 (42) = happyShift action_33
action_145 (48) = happyShift action_34
action_145 (56) = happyShift action_35
action_145 (60) = happyShift action_36
action_145 (64) = happyShift action_37
action_145 (77) = happyShift action_38
action_145 (78) = happyShift action_39
action_145 (5) = happyGoto action_26
action_145 (20) = happyGoto action_159
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (25) = happyShift action_106
action_146 (26) = happyShift action_107
action_146 (27) = happyShift action_108
action_146 (28) = happyShift action_109
action_146 (40) = happyShift action_110
action_146 (60) = happyShift action_111
action_146 (14) = happyGoto action_158
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_84

action_148 (25) = happyShift action_28
action_148 (26) = happyShift action_29
action_148 (27) = happyShift action_30
action_148 (28) = happyShift action_31
action_148 (29) = happyShift action_7
action_148 (30) = happyShift action_8
action_148 (31) = happyShift action_9
action_148 (32) = happyShift action_10
action_148 (33) = happyShift action_11
action_148 (34) = happyShift action_12
action_148 (35) = happyShift action_13
action_148 (36) = happyShift action_14
action_148 (37) = happyShift action_15
action_148 (38) = happyShift action_32
action_148 (40) = happyShift action_16
action_148 (42) = happyShift action_33
action_148 (48) = happyShift action_34
action_148 (52) = happyShift action_138
action_148 (53) = happyShift action_139
action_148 (56) = happyShift action_35
action_148 (60) = happyShift action_140
action_148 (64) = happyShift action_37
action_148 (77) = happyShift action_38
action_148 (78) = happyShift action_39
action_148 (79) = happyShift action_141
action_148 (5) = happyGoto action_132
action_148 (9) = happyGoto action_3
action_148 (10) = happyGoto action_133
action_148 (16) = happyGoto action_157
action_148 (20) = happyGoto action_137
action_148 _ = happyReduce_49

action_149 (47) = happyShift action_156
action_149 _ = happyReduce_44

action_150 (25) = happyShift action_28
action_150 (26) = happyShift action_29
action_150 (27) = happyShift action_30
action_150 (28) = happyShift action_31
action_150 (38) = happyShift action_32
action_150 (42) = happyShift action_33
action_150 (48) = happyShift action_34
action_150 (56) = happyShift action_35
action_150 (60) = happyShift action_36
action_150 (64) = happyShift action_37
action_150 (77) = happyShift action_38
action_150 (78) = happyShift action_39
action_150 (5) = happyGoto action_26
action_150 (20) = happyGoto action_155
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (25) = happyShift action_28
action_151 (26) = happyShift action_29
action_151 (27) = happyShift action_30
action_151 (28) = happyShift action_31
action_151 (29) = happyShift action_7
action_151 (30) = happyShift action_8
action_151 (31) = happyShift action_9
action_151 (32) = happyShift action_10
action_151 (33) = happyShift action_11
action_151 (34) = happyShift action_12
action_151 (35) = happyShift action_13
action_151 (36) = happyShift action_14
action_151 (37) = happyShift action_15
action_151 (38) = happyShift action_32
action_151 (40) = happyShift action_16
action_151 (42) = happyShift action_33
action_151 (48) = happyShift action_34
action_151 (52) = happyShift action_138
action_151 (53) = happyShift action_139
action_151 (56) = happyShift action_35
action_151 (60) = happyShift action_140
action_151 (64) = happyShift action_37
action_151 (77) = happyShift action_38
action_151 (78) = happyShift action_39
action_151 (79) = happyShift action_141
action_151 (5) = happyGoto action_132
action_151 (9) = happyGoto action_3
action_151 (10) = happyGoto action_133
action_151 (16) = happyGoto action_134
action_151 (17) = happyGoto action_135
action_151 (18) = happyGoto action_154
action_151 (20) = happyGoto action_137
action_151 _ = happyReduce_50

action_152 (41) = happyShift action_153
action_152 _ = happyFail (happyExpListPerState 152)

action_153 _ = happyReduce_37

action_154 (41) = happyShift action_163
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (63) = happyShift action_52
action_155 (64) = happyShift action_53
action_155 (65) = happyShift action_54
action_155 (66) = happyShift action_55
action_155 (67) = happyShift action_56
action_155 (68) = happyShift action_57
action_155 (69) = happyShift action_58
action_155 (70) = happyShift action_59
action_155 (71) = happyShift action_60
action_155 (72) = happyShift action_61
action_155 (73) = happyShift action_62
action_155 (74) = happyShift action_63
action_155 (75) = happyShift action_64
action_155 (76) = happyShift action_65
action_155 _ = happyReduce_42

action_156 (25) = happyShift action_28
action_156 (26) = happyShift action_29
action_156 (27) = happyShift action_30
action_156 (28) = happyShift action_31
action_156 (38) = happyShift action_32
action_156 (42) = happyShift action_33
action_156 (48) = happyShift action_34
action_156 (56) = happyShift action_35
action_156 (60) = happyShift action_36
action_156 (64) = happyShift action_37
action_156 (77) = happyShift action_38
action_156 (78) = happyShift action_39
action_156 (5) = happyGoto action_26
action_156 (20) = happyGoto action_162
action_156 _ = happyFail (happyExpListPerState 156)

action_157 _ = happyReduce_47

action_158 (45) = happyShift action_161
action_158 _ = happyFail (happyExpListPerState 158)

action_159 (43) = happyShift action_160
action_159 (63) = happyShift action_52
action_159 (64) = happyShift action_53
action_159 (65) = happyShift action_54
action_159 (66) = happyShift action_55
action_159 (67) = happyShift action_56
action_159 (68) = happyShift action_57
action_159 (69) = happyShift action_58
action_159 (70) = happyShift action_59
action_159 (71) = happyShift action_60
action_159 (72) = happyShift action_61
action_159 (73) = happyShift action_62
action_159 (74) = happyShift action_63
action_159 (75) = happyShift action_64
action_159 (76) = happyShift action_65
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (40) = happyShift action_165
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (25) = happyShift action_28
action_161 (26) = happyShift action_29
action_161 (27) = happyShift action_30
action_161 (28) = happyShift action_31
action_161 (38) = happyShift action_32
action_161 (42) = happyShift action_33
action_161 (48) = happyShift action_34
action_161 (56) = happyShift action_35
action_161 (60) = happyShift action_36
action_161 (64) = happyShift action_37
action_161 (77) = happyShift action_38
action_161 (78) = happyShift action_39
action_161 (5) = happyGoto action_26
action_161 (20) = happyGoto action_164
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (63) = happyShift action_52
action_162 (64) = happyShift action_53
action_162 (65) = happyShift action_54
action_162 (66) = happyShift action_55
action_162 (67) = happyShift action_56
action_162 (68) = happyShift action_57
action_162 (69) = happyShift action_58
action_162 (70) = happyShift action_59
action_162 (71) = happyShift action_60
action_162 (72) = happyShift action_61
action_162 (73) = happyShift action_62
action_162 (74) = happyShift action_63
action_162 (75) = happyShift action_64
action_162 (76) = happyShift action_65
action_162 _ = happyReduce_43

action_163 _ = happyReduce_38

action_164 (43) = happyShift action_172
action_164 (63) = happyShift action_52
action_164 (64) = happyShift action_53
action_164 (65) = happyShift action_54
action_164 (66) = happyShift action_55
action_164 (67) = happyShift action_56
action_164 (68) = happyShift action_57
action_164 (69) = happyShift action_58
action_164 (70) = happyShift action_59
action_164 (71) = happyShift action_60
action_164 (72) = happyShift action_61
action_164 (73) = happyShift action_62
action_164 (74) = happyShift action_63
action_164 (75) = happyShift action_64
action_164 (76) = happyShift action_65
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (25) = happyShift action_28
action_165 (26) = happyShift action_29
action_165 (27) = happyShift action_30
action_165 (28) = happyShift action_31
action_165 (29) = happyShift action_7
action_165 (30) = happyShift action_8
action_165 (31) = happyShift action_9
action_165 (32) = happyShift action_10
action_165 (33) = happyShift action_11
action_165 (34) = happyShift action_12
action_165 (35) = happyShift action_13
action_165 (36) = happyShift action_14
action_165 (37) = happyShift action_15
action_165 (38) = happyShift action_32
action_165 (40) = happyShift action_16
action_165 (42) = happyShift action_33
action_165 (48) = happyShift action_34
action_165 (52) = happyShift action_138
action_165 (53) = happyShift action_139
action_165 (54) = happyShift action_170
action_165 (55) = happyShift action_171
action_165 (56) = happyShift action_35
action_165 (60) = happyShift action_140
action_165 (64) = happyShift action_37
action_165 (77) = happyShift action_38
action_165 (78) = happyShift action_39
action_165 (79) = happyShift action_141
action_165 (5) = happyGoto action_132
action_165 (9) = happyGoto action_3
action_165 (10) = happyGoto action_133
action_165 (11) = happyGoto action_166
action_165 (12) = happyGoto action_167
action_165 (13) = happyGoto action_168
action_165 (16) = happyGoto action_169
action_165 (20) = happyGoto action_137
action_165 _ = happyFail (happyExpListPerState 165)

action_166 _ = happyReduce_29

action_167 (51) = happyShift action_175
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (41) = happyShift action_174
action_168 _ = happyFail (happyExpListPerState 168)

action_169 _ = happyReduce_27

action_170 _ = happyReduce_26

action_171 _ = happyReduce_25

action_172 (40) = happyShift action_173
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (25) = happyShift action_28
action_173 (26) = happyShift action_29
action_173 (27) = happyShift action_30
action_173 (28) = happyShift action_31
action_173 (29) = happyShift action_7
action_173 (30) = happyShift action_8
action_173 (31) = happyShift action_9
action_173 (32) = happyShift action_10
action_173 (33) = happyShift action_11
action_173 (34) = happyShift action_12
action_173 (35) = happyShift action_13
action_173 (36) = happyShift action_14
action_173 (37) = happyShift action_15
action_173 (38) = happyShift action_32
action_173 (40) = happyShift action_16
action_173 (42) = happyShift action_33
action_173 (48) = happyShift action_34
action_173 (52) = happyShift action_138
action_173 (53) = happyShift action_139
action_173 (54) = happyShift action_170
action_173 (55) = happyShift action_171
action_173 (56) = happyShift action_35
action_173 (60) = happyShift action_140
action_173 (64) = happyShift action_37
action_173 (77) = happyShift action_38
action_173 (78) = happyShift action_39
action_173 (79) = happyShift action_141
action_173 (5) = happyGoto action_132
action_173 (9) = happyGoto action_3
action_173 (10) = happyGoto action_133
action_173 (11) = happyGoto action_166
action_173 (12) = happyGoto action_167
action_173 (13) = happyGoto action_177
action_173 (16) = happyGoto action_169
action_173 (20) = happyGoto action_137
action_173 _ = happyFail (happyExpListPerState 173)

action_174 _ = happyReduce_41

action_175 (25) = happyShift action_28
action_175 (26) = happyShift action_29
action_175 (27) = happyShift action_30
action_175 (28) = happyShift action_31
action_175 (29) = happyShift action_7
action_175 (30) = happyShift action_8
action_175 (31) = happyShift action_9
action_175 (32) = happyShift action_10
action_175 (33) = happyShift action_11
action_175 (34) = happyShift action_12
action_175 (35) = happyShift action_13
action_175 (36) = happyShift action_14
action_175 (37) = happyShift action_15
action_175 (38) = happyShift action_32
action_175 (40) = happyShift action_16
action_175 (42) = happyShift action_33
action_175 (48) = happyShift action_34
action_175 (52) = happyShift action_138
action_175 (53) = happyShift action_139
action_175 (54) = happyShift action_170
action_175 (55) = happyShift action_171
action_175 (56) = happyShift action_35
action_175 (60) = happyShift action_140
action_175 (64) = happyShift action_37
action_175 (77) = happyShift action_38
action_175 (78) = happyShift action_39
action_175 (79) = happyShift action_141
action_175 (5) = happyGoto action_132
action_175 (9) = happyGoto action_3
action_175 (10) = happyGoto action_133
action_175 (11) = happyGoto action_176
action_175 (16) = happyGoto action_169
action_175 (20) = happyGoto action_137
action_175 _ = happyReduce_30

action_176 _ = happyReduce_28

action_177 (41) = happyShift action_178
action_177 _ = happyFail (happyExpListPerState 177)

action_178 _ = happyReduce_40

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
		 (happy_var_1 & \(LBool p) -> PBool p
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LInt p) -> PInt p
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LFloat p) -> PFloat p
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LChar p) -> PChar p
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  9 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LString p) -> PString p
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LUnit p) -> PUnit p
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LVoid p) -> PVoid p
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 9 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (happy_var_3 |> happy_var_1 |> \(LVector p) ty -> PVector ty p
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  9 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 & \(LIdentifier t p) -> PId t p
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  9 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (PRecord happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  10 happyReduction_23
happyReduction_23 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (PUnion happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  11 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 |> \(LBreak p) -> Break p
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  11 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 |> \(LContinue p) -> Continue p
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  11 happyReduction_27
happyReduction_27 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn11
		 (LAction happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 <> [happy_var_3]
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  13 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  14 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 |> \(LNumber t p) -> PaNumber t p
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  14 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 |> \(LCharLit t p) -> PaChar t p
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  14 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 |> \(LStringLit t p) -> PaString t p
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  14 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 |> \(LBoolLit t p) -> PaBool t p
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  14 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2 |> happy_var_1 |> \(LIdentifier t p) mref -> PaId t mref p
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  14 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (PaPattern happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 5 15 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ([(happy_var_1,happy_var_4)]
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 6 15 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (happy_var_1 <> [(happy_var_2,happy_var_5)]
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_1  16 happyReduction_39
happyReduction_39 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn16
		 (AExpression happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happyReduce 9 16 happyReduction_40
happyReduction_40 (_ `HappyStk`
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

happyReduce_41 = happyReduce 7 16 happyReduction_41
happyReduction_41 (_ `HappyStk`
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

happyReduce_42 = happySpecReduce_3  16 happyReduction_42
happyReduction_42 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn16
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 16 happyReduction_43
happyReduction_43 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (happy_var_4 |> happy_var_2 |> happy_var_1 |> \ty (LIdentifier t _) e -> Declare ty t (Just e)
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_2  16 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_2 |> happy_var_1 |> \ty (LIdentifier t _) -> Declare ty t Nothing
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  16 happyReduction_45
happyReduction_45 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 |> \(LReturn p) -> Return happy_var_2 p
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  16 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_2 |> happy_var_1 |> \(LReturn p) (LUnit p') -> Return (EUnit p') p
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  17 happyReduction_47
happyReduction_47 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 <> [happy_var_3]
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  17 happyReduction_48
happyReduction_48 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  18 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  18 happyReduction_50
happyReduction_50  =  HappyAbsSyn17
		 ([]
	)

happyReduce_51 = happySpecReduce_2  19 happyReduction_51
happyReduction_51 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 & \(LBy p) -> Just $ ByRef p
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_0  19 happyReduction_52
happyReduction_52  =  HappyAbsSyn19
		 (Nothing
	)

happyReduce_53 = happySpecReduce_1  20 happyReduction_53
happyReduction_53 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn20
		 (ELValuable happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happyReduce 5 20 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_1 |> \(LNew p) -> New happy_var_2 happy_var_4 p
	) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_2  20 happyReduction_55
happyReduction_55 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LRef p) -> Ref happy_var_2 p
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  20 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LMinus p) -> Neg happy_var_2 p
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  20 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Plus happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  20 happyReduction_58
happyReduction_58 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Times happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  20 happyReduction_59
happyReduction_59 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Divide happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  20 happyReduction_60
happyReduction_60 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Power happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  20 happyReduction_61
happyReduction_61 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Mod happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  20 happyReduction_62
happyReduction_62 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Minus happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  20 happyReduction_63
happyReduction_63 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ELT happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  20 happyReduction_64
happyReduction_64 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EGT happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  20 happyReduction_65
happyReduction_65 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (NotEq happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  20 happyReduction_66
happyReduction_66 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EEq happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  20 happyReduction_67
happyReduction_67 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EGTEq happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  20 happyReduction_68
happyReduction_68 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ELTEq happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  20 happyReduction_69
happyReduction_69 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Or happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  20 happyReduction_70
happyReduction_70 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (And happy_var_1 happy_var_3 (getExpressionInfo happy_var_1)
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  20 happyReduction_71
happyReduction_71 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LNot p) -> Not happy_var_2 p
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  20 happyReduction_72
happyReduction_72 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  20 happyReduction_73
happyReduction_73 _
	(HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LOBckt p) -> Arr happy_var_2 p
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  20 happyReduction_74
happyReduction_74 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LNumber t p) -> ENumber t p
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  20 happyReduction_75
happyReduction_75 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LStringLit t p) -> EString t p
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  20 happyReduction_76
happyReduction_76 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LCharLit t p) -> EChar t p
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  20 happyReduction_77
happyReduction_77 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 |> \(LBoolLit t p) -> EBool t p
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happyReduce 4 20 happyReduction_78
happyReduction_78 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_1 |> \(LMatch p) -> Match happy_var_2 happy_var_4 p
	) `HappyStk` happyRest

happyReduce_79 = happyReduce 4 20 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_1 |> \(LIdentifier t p) -> FApp t happy_var_3 p
	) `HappyStk` happyRest

happyReduce_80 = happySpecReduce_3  21 happyReduction_80
happyReduction_80 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <> happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  21 happyReduction_81
happyReduction_81 (HappyAbsSyn19  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_3 |> happy_var_2 |> happy_var_1 |> \t (LIdentifier x _) mref -> [FunArg t x mref $ getPTypesInfo t]
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  22 happyReduction_82
happyReduction_82 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_0  22 happyReduction_83
happyReduction_83  =  HappyAbsSyn22
		 ([]
	)

happyReduce_84 = happyReduce 8 23 happyReduction_84
happyReduction_84 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (happy_var_7 |> happy_var_4 |> happy_var_2 |> happy_var_1 |>
                                                    \t (LIdentifier name _) fs as 
                                                      -> FunctionDef t name fs as (getPTypesInfo t)
	) `HappyStk` happyRest

happyReduce_85 = happyReduce 4 23 happyReduction_85
happyReduction_85 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (happy_var_4 |> happy_var_2 |> happy_var_1 |> \(LType p) (LIdentifier t _) ty -> TypeDef t ty p
	) `HappyStk` happyRest

happyReduce_86 = happySpecReduce_2  24 happyReduction_86
happyReduction_86 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 <> [happy_var_2]
	)
happyReduction_86 _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  24 happyReduction_87
happyReduction_87 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_87 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	LEOF -> action 80 80 tk (HappyState action) sts stk;
	LCharLit _ _ -> cont 25;
	LStringLit _ _ -> cont 26;
	LBoolLit _ _ -> cont 27;
	LNumber _ _ -> cont 28;
	LAtom _ _ -> cont 29;
	LBool _ -> cont 30;
	LInt _ -> cont 31;
	LFloat _ -> cont 32;
	LChar _ -> cont 33;
	LString _ -> cont 34;
	LUnit _ -> cont 35;
	LVoid _ -> cont 36;
	LVector _ -> cont 37;
	LOBckt _ -> cont 38;
	LCBckt _ -> cont 39;
	LOBrc _ -> cont 40;
	LCBrc _ -> cont 41;
	LOParen _ -> cont 42;
	LCParen _ -> cont 43;
	LComma _ -> cont 44;
	LColon _ -> cont 45;
	LFatArrow _ -> cont 46;
	LAssign _ -> cont 47;
	LMatch _ -> cont 48;
	LWith _ -> cont 49;
	LType _ -> cont 50;
	LSemiColon _ -> cont 51;
	LFor _ -> cont 52;
	LWhile _ -> cont 53;
	LContinue _ -> cont 54;
	LBreak _ -> cont 55;
	LNew _ -> cont 56;
	LBy _ -> cont 57;
	LReference _ -> cont 58;
	LVBar _ -> cont 59;
	LIdentifier _ _ -> cont 60;
	LDot _ -> cont 61;
	LEOF -> cont 62;
	LPlus _ -> cont 63;
	LMinus _ -> cont 64;
	LMult _ -> cont 65;
	LDiv _ -> cont 66;
	LPow _ -> cont 67;
	LMod _ -> cont 68;
	LLT _ -> cont 69;
	LGT _ -> cont 70;
	LNEq _ -> cont 71;
	LEq _ -> cont 72;
	LGTE _ -> cont 73;
	LLTE _ -> cont 74;
	LOr _ -> cont 75;
	LAnd _ -> cont 76;
	LNot  _ -> cont 77;
	LRef _ -> cont 78;
	LReturn _ -> cont 79;
	_ -> happyError' (tk, [])
	})

happyError_ explist 80 tk = happyError' (tk, explist)
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
lexer f = alexMonadScan >>= \case 
  LComment {} -> lexer f
  t -> f t
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

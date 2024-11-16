{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer

import Data.List
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,126) ([1536,8704,0,16384,0,65504,7,0,0,0,0,0,0,0,65408,63743,2,0,0,6,34,192,1088,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,64,0,2048,0,3,17,0,32,0,4096,32768,32769,8,48,272,0,2048,0,0,1,24,136,0,512,0,16384,0,0,0,0,0,0,0,0,0,4,0,1024,0,4096,0,3,17,0,64,3072,17408,0,0,1,48,272,1536,8704,0,16384,0,0,16,768,4352,0,0,0,12,68,384,2176,12288,4096,1,0,4,0,512,0,4096,0,0,2,0,0,0,0,8,0,4,0,64,1536,8704,0,0,0,0,0,0,1024,0,0,0,0,0,0,256,0,0,0,0,4,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","SASA","Conds","Param","BDY","SASAs","Op","num","bool","'+'","'-'","'*'","'/'","\"add1\"","\"sub1\"","\"sqrt\"","\"expt\"","'<'","'>'","'='","\"not\"","\"or\"","\"and\"","'('","')'","'['","']'","id","\"let\"","\"let*\"","\"lambda\"","\"if\"","\"else\"","\"cond\"","%eof"]
        bit_start = st Prelude.* 37
        bit_end = (st Prelude.+ 1) Prelude.* 37
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..36]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (10) = happyShift action_4
action_0 (11) = happyShift action_5
action_0 (26) = happyShift action_6
action_0 (30) = happyShift action_7
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (26) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (12) = happyShift action_10
action_2 (13) = happyShift action_11
action_2 (14) = happyShift action_12
action_2 (15) = happyShift action_13
action_2 (16) = happyShift action_14
action_2 (17) = happyShift action_15
action_2 (18) = happyShift action_16
action_2 (19) = happyShift action_17
action_2 (20) = happyShift action_18
action_2 (21) = happyShift action_19
action_2 (22) = happyShift action_20
action_2 (23) = happyShift action_21
action_2 (24) = happyShift action_22
action_2 (25) = happyShift action_23
action_2 (9) = happyGoto action_9
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (37) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_8

action_5 _ = happyReduce_9

action_6 (10) = happyShift action_4
action_6 (11) = happyShift action_5
action_6 (12) = happyShift action_10
action_6 (13) = happyShift action_11
action_6 (14) = happyShift action_12
action_6 (15) = happyShift action_13
action_6 (16) = happyShift action_14
action_6 (17) = happyShift action_15
action_6 (18) = happyShift action_16
action_6 (19) = happyShift action_17
action_6 (20) = happyShift action_18
action_6 (21) = happyShift action_19
action_6 (22) = happyShift action_20
action_6 (23) = happyShift action_21
action_6 (24) = happyShift action_22
action_6 (25) = happyShift action_23
action_6 (26) = happyShift action_6
action_6 (30) = happyShift action_7
action_6 (31) = happyShift action_24
action_6 (32) = happyShift action_25
action_6 (33) = happyShift action_26
action_6 (34) = happyShift action_27
action_6 (36) = happyShift action_28
action_6 (4) = happyGoto action_8
action_6 (9) = happyGoto action_9
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_10

action_8 (10) = happyShift action_4
action_8 (11) = happyShift action_5
action_8 (26) = happyShift action_6
action_8 (30) = happyShift action_7
action_8 (4) = happyGoto action_34
action_8 (8) = happyGoto action_36
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (10) = happyShift action_4
action_9 (11) = happyShift action_5
action_9 (26) = happyShift action_6
action_9 (30) = happyShift action_7
action_9 (4) = happyGoto action_34
action_9 (8) = happyGoto action_35
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_19

action_11 _ = happyReduce_20

action_12 _ = happyReduce_21

action_13 _ = happyReduce_22

action_14 _ = happyReduce_23

action_15 _ = happyReduce_24

action_16 _ = happyReduce_25

action_17 _ = happyReduce_26

action_18 _ = happyReduce_27

action_19 _ = happyReduce_28

action_20 _ = happyReduce_29

action_21 _ = happyReduce_30

action_22 _ = happyReduce_31

action_23 _ = happyReduce_32

action_24 (26) = happyShift action_33
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (26) = happyShift action_32
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (26) = happyShift action_31
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (10) = happyShift action_4
action_27 (11) = happyShift action_5
action_27 (26) = happyShift action_6
action_27 (30) = happyShift action_7
action_27 (4) = happyGoto action_30
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (26) = happyShift action_29
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (28) = happyShift action_47
action_29 (5) = happyGoto action_46
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (10) = happyShift action_4
action_30 (11) = happyShift action_5
action_30 (26) = happyShift action_6
action_30 (30) = happyShift action_7
action_30 (4) = happyGoto action_45
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (10) = happyShift action_4
action_31 (11) = happyShift action_5
action_31 (26) = happyShift action_6
action_31 (30) = happyShift action_7
action_31 (4) = happyGoto action_43
action_31 (6) = happyGoto action_44
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (28) = happyShift action_41
action_32 (7) = happyGoto action_42
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (28) = happyShift action_41
action_33 (7) = happyGoto action_40
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (10) = happyShift action_4
action_34 (11) = happyShift action_5
action_34 (26) = happyShift action_6
action_34 (30) = happyShift action_7
action_34 (4) = happyGoto action_34
action_34 (8) = happyGoto action_39
action_34 _ = happyReduce_17

action_35 (27) = happyShift action_38
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (27) = happyShift action_37
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_7

action_38 _ = happyReduce_1

action_39 _ = happyReduce_18

action_40 (27) = happyShift action_55
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (30) = happyShift action_54
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (27) = happyShift action_53
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (10) = happyShift action_4
action_43 (11) = happyShift action_5
action_43 (26) = happyShift action_6
action_43 (30) = happyShift action_7
action_43 (4) = happyGoto action_43
action_43 (6) = happyGoto action_52
action_43 _ = happyReduce_13

action_44 (27) = happyShift action_51
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (10) = happyShift action_4
action_45 (11) = happyShift action_5
action_45 (26) = happyShift action_6
action_45 (30) = happyShift action_7
action_45 (4) = happyGoto action_50
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (27) = happyShift action_49
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (10) = happyShift action_4
action_47 (11) = happyShift action_5
action_47 (26) = happyShift action_6
action_47 (30) = happyShift action_7
action_47 (4) = happyGoto action_48
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (10) = happyShift action_4
action_48 (11) = happyShift action_5
action_48 (26) = happyShift action_6
action_48 (30) = happyShift action_7
action_48 (4) = happyGoto action_62
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (26) = happyShift action_61
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (27) = happyShift action_60
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (10) = happyShift action_4
action_51 (11) = happyShift action_5
action_51 (26) = happyShift action_6
action_51 (30) = happyShift action_7
action_51 (4) = happyGoto action_59
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_14

action_53 (10) = happyShift action_4
action_53 (11) = happyShift action_5
action_53 (26) = happyShift action_6
action_53 (30) = happyShift action_7
action_53 (4) = happyGoto action_58
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (10) = happyShift action_4
action_54 (11) = happyShift action_5
action_54 (26) = happyShift action_6
action_54 (30) = happyShift action_7
action_54 (4) = happyGoto action_57
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (10) = happyShift action_4
action_55 (11) = happyShift action_5
action_55 (26) = happyShift action_6
action_55 (30) = happyShift action_7
action_55 (4) = happyGoto action_56
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (27) = happyShift action_68
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (29) = happyShift action_67
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (27) = happyShift action_66
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (27) = happyShift action_65
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_4

action_61 (35) = happyShift action_64
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (29) = happyShift action_63
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (28) = happyShift action_47
action_63 (5) = happyGoto action_71
action_63 _ = happyReduce_11

action_64 (10) = happyShift action_4
action_64 (11) = happyShift action_5
action_64 (26) = happyShift action_6
action_64 (30) = happyShift action_7
action_64 (4) = happyGoto action_70
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_6

action_66 _ = happyReduce_3

action_67 (28) = happyShift action_41
action_67 (7) = happyGoto action_69
action_67 _ = happyReduce_15

action_68 _ = happyReduce_2

action_69 _ = happyReduce_16

action_70 (27) = happyShift action_72
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_12

action_72 (27) = happyShift action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_5

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 7 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 7 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Le1 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Ift happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 10 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Con happy_var_4 happy_var_8
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 7 4 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lam happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 4 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (App happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  4 happyReduction_9
happyReduction_9 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn4
		 (Boo happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  4 happyReduction_10
happyReduction_10 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn4
		 (Str happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 5 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ([(happy_var_2, happy_var_3)]
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 5 happyReduction_12
happyReduction_12 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ((happy_var_2, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1:happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 7 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ([((Str happy_var_2),happy_var_3)]
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 5 7 happyReduction_16
happyReduction_16 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (((Str happy_var_2),happy_var_3):happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  8 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  8 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn9
		 (Add
	)

happyReduce_20 = happySpecReduce_1  9 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn9
		 (Sub
	)

happyReduce_21 = happySpecReduce_1  9 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn9
		 (Mul
	)

happyReduce_22 = happySpecReduce_1  9 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn9
		 (Div
	)

happyReduce_23 = happySpecReduce_1  9 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn9
		 (Ad1
	)

happyReduce_24 = happySpecReduce_1  9 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn9
		 (Su1
	)

happyReduce_25 = happySpecReduce_1  9 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn9
		 (Sqt
	)

happyReduce_26 = happySpecReduce_1  9 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn9
		 (Exp
	)

happyReduce_27 = happySpecReduce_1  9 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn9
		 (Meq
	)

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn9
		 (Maq
	)

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn9
		 (Equ
	)

happyReduce_30 = happySpecReduce_1  9 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn9
		 (Not
	)

happyReduce_31 = happySpecReduce_1  9 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn9
		 (Orb
	)

happyReduce_32 = happySpecReduce_1  9 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn9
		 (And
	)

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 10;
	TokenBool happy_dollar_dollar -> cont 11;
	TokenAdd -> cont 12;
	TokenSub -> cont 13;
	TokenMult -> cont 14;
	TokenDiv -> cont 15;
	TokenAdd1 -> cont 16;
	TokenSub1 -> cont 17;
	TokenSqrt -> cont 18;
	TokenExp -> cont 19;
	TokenMq -> cont 20;
	TokenM -> cont 21;
	TokenEq -> cont 22;
	TokenNot -> cont 23;
	TokenOr -> cont 24;
	TokenAnd -> cont 25;
	TokenPA -> cont 26;
	TokenPC -> cont 27;
	TokenPA1 -> cont 28;
	TokenPC1 -> cont 29;
	TokenId happy_dollar_dollar -> cont 30;
	TokenLet -> cont 31;
	TokenLet1 -> cont 32;
	TokenLambda -> cont 33;
	TokenIf -> cont 34;
	TokenElse -> cont 35;
	TokenCond -> cont 36;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 37 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError x = error "Parse error"

data SASA = Num Double
          | Str String
          | Boo Bool
          | Add [SASA]
          | Sub [SASA]
          | Mul [SASA]
          | Div [SASA]
          | Ad1 [SASA]
          | Su1 [SASA]
          | Sqt [SASA]
          | Exp [SASA]
          | Maq [SASA]
          | Meq [SASA]
          | Equ [SASA]
          | Not [SASA]
          | And [SASA]
          | Orb [SASA]
          | Let [(SASA,SASA)] SASA
          | Le1 [(SASA,SASA)] SASA
          | Ift SASA SASA SASA
          | Con [(SASA,SASA)] SASA
          | Lam [SASA] SASA
          | App SASA [SASA]


instance Show SASA where
  show (Str s) = s 
  show (Num d) = show d
  show (Boo b) = if b then "#t" else "#f"
  show (Add xs) = "(" ++ "+ " ++ showListWithoutBrackets xs ++ ")"
  show (Sub xs) = "(" ++ "- " ++ showListWithoutBrackets xs ++ ")"
  show (Mul xs) = "(" ++ "* " ++ showListWithoutBrackets xs ++ ")"
  show (Div xs) = "(" ++ "/ " ++ showListWithoutBrackets xs ++ ")"
  show (Ad1 xs) = "(" ++ "add1 " ++ showListWithoutBrackets xs ++ ")"
  show (Su1 xs) = "(" ++ "sub1 " ++ showListWithoutBrackets xs ++ ")"
  show (Sqt xs) = "(" ++ "sqrt " ++ showListWithoutBrackets xs ++ ")"
  show (Exp xs) = "(" ++ "expt " ++ showListWithoutBrackets xs ++ ")"
  show (Maq xs) = "(" ++ "> " ++ showListWithoutBrackets xs ++ ")"
  show (Meq xs) = "(" ++ "< " ++ showListWithoutBrackets xs ++ ")"
  show (Equ xs) = "(" ++ "= " ++ showListWithoutBrackets xs ++ ")"
  show (Not xs) = "(" ++ "not " ++ showListWithoutBrackets xs ++ ")"
  show (And xs) = "(" ++ "and " ++ showListWithoutBrackets xs ++ ")"
  show (Orb xs) = "(" ++ "or " ++ showListWithoutBrackets xs ++ ")"
  show (Let xs ys) = "(" ++ "let " ++ "(" ++ showListWithoutBrackets2 xs ++ ") " ++ show ys ++ ")"
  show (Le1 xs ys) = "(" ++ "let* " ++ "(" ++ showListWithoutBrackets2 xs ++ ") " ++ show ys ++ ")"
  show (Lam xs ys) = "(" ++ "lambda " ++ "(" ++ showListWithoutBrackets xs ++ ") " ++ show ys ++ ")"
  show (Ift a b c) = "(" ++ "if "++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"
  show (Con xs ys) = "(" ++ "cond " ++ "(" ++ showListWithoutBrackets2 xs ++ ") " ++ "("++ "else " ++ show ys ++")"++ ")"
  show (App xs zs) = "(" ++ show xs ++ " " ++ showListWithoutBrackets zs ++ ")"

-- Función auxiliar para mostrar la lista sin corchetes ni comas
showListWithoutBrackets :: [SASA] -> String
showListWithoutBrackets = unwords . map show

showListWithoutBrackets2 :: [(SASA,SASA)] -> String
showListWithoutBrackets2 [] = ""
showListWithoutBrackets2 ((a,b):xs) = "[" ++ show a ++ " " ++ show b ++ "]" ++ showRest xs
  where
    showRest [] = ""
    showRest ys = " " ++ showListWithoutBrackets2 ys
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

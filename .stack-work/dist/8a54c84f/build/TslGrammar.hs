{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module TslGrammar where
import TslTokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn t4 t5 t6 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4 t5 t6)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4 t5 t6) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: t5 -> (HappyAbsSyn t4 t5 t6)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4 t5 t6) -> t5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: t6 -> (HappyAbsSyn t4 t5 t6)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4 t5 t6) -> t6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyInTok :: (Token) -> (HappyAbsSyn t4 t5 t6)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4 t5 t6) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x40\xe4\xff\x09\x3f\x38\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x07\x04\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x40\x00\x40\xe4\xff\x09\x3f\x38\x20\xf2\xff\x84\x1f\x1c\x10\xf9\x7f\xc2\x0f\x0e\x88\xfc\x3f\xe1\x07\x07\x44\xfe\x9f\xf0\x83\x03\x22\xff\x4f\xf8\xc1\x01\x91\xff\x27\xfc\xe0\x80\xc8\xff\x13\x7e\x70\x40\xe4\xff\x09\x3f\x38\x20\xf2\xff\x84\x1f\x1c\x10\xf9\x7f\xc2\x0f\x0e\x88\xfc\x3f\xe1\x07\x07\x44\xfe\x9f\xf0\x83\x03\x22\xff\x4f\xf8\xc1\x01\x91\xff\x27\xfc\xe0\x80\xc8\xff\x13\x7e\x70\x40\xe4\xff\x09\x3f\x38\x20\xf2\xff\x84\x1f\x1c\x10\xf9\x7f\xc2\x0f\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x60\x0f\x0c\x00\x22\xff\xff\xff\xc5\x01\x91\xff\xff\xff\xe2\x80\xc8\xff\xff\x7f\x71\x40\xe4\xff\xff\xbf\x38\x20\xf2\xff\xff\x5f\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\xc8\xff\xff\x7f\x71\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\xfc\xff\xff\x17\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\xc8\xff\x13\x7e\x70\x00\x00\x00\x00\x00\x20\x20\xf2\xff\x84\x1f\x1c\x10\xf9\x7f\xc2\x0f\x0e\x88\xfc\x3f\xe1\x07\x07\x44\xfe\x9f\xf0\x83\x03\x22\xff\x4f\xf8\xc1\x01\x91\xff\x27\xfc\xe0\x80\xc8\xff\x13\x7e\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\xf6\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\xfc\xff\xff\x17\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\xf2\xff\xff\x5f\x1c\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x10\xf9\x7f\xc2\x0f\x0e\x40\x00\x00\x00\x00\x00\x44\xfe\x9f\xf0\x83\x03\x00\x00\xb0\x07\x06\x00\x40\x00\x00\x00\x00\x00\x02\x00\xec\x01\x01\x40\xe4\xff\x09\x3f\x38\x00\x00\x00\x00\x10\x00\x10\xf9\x7f\xc2\x0f\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x60\x0f\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTsl","Exp","Literal","Type","LET","COLON","IN","ASSIGN","IF","THEN","ELSE","READ","OUTPUT","SIZE","INTERLACE","ROTATE90","ROTATE180","ROTATE270","SCALE","FLIPX","FLIPY","FLIPXY","BLANK","AND","OR","NOT","GREATER","LESS","GREATEREQUAL","LESSEQUAL","SUBTILE","PLACERIGHT","PLACEBELOW","REPEATRIGHT","REPEATDOWN","LPAREN","RPAREN","PLUS","IntType","StringType","TileType","int","string","var","%eof"]
        bit_start = st Prelude.* 47
        bit_end = (st Prelude.+ 1) Prelude.* 47
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..46]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x25\x00\xe1\xff\x00\x00\x00\x00\x00\x00\x41\x00\x02\x00\x1c\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x00\x00\x4d\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x89\x00\x89\x00\x89\x00\x89\x00\x89\x00\x01\x00\x89\x00\x89\x00\x89\x00\x01\x00\x89\x00\x89\x00\x89\x00\x25\x00\xd9\xff\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x89\x00\x89\x00\x89\x00\x89\x00\x89\x00\x89\x00\x89\x00\x4e\x00\x5c\x00\x89\x00\x89\x00\x01\x00\x89\x00\x89\x00\x89\x00\x89\x00\x00\x00\x01\x00\x44\x00\xe0\xff\x36\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x89\x00\x25\x00\x6e\x00\x25\x00\x6b\x00\x98\x00\x3a\x00\x25\x00\x61\x00\x25\x00\x89\x00\x7a\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x24\x00\x8f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x39\x00\x3e\x00\x46\x00\x48\x00\x5d\x00\x5f\x00\x68\x00\x6c\x00\x77\x00\x79\x00\x7b\x00\x86\x00\x88\x00\x8a\x00\x95\x00\x97\x00\x99\x00\xa4\x00\x00\x00\x00\x00\xa6\x00\xa8\x00\xac\x00\xae\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\xb4\x00\x00\x00\x00\x00\x00\x00\xb6\x00\x00\x00\xb8\x00\xba\x00\xbc\x00\xbe\x00\xc0\x00\xc2\x00\xc4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x00\x00\x00\xd1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\x00\x00\x00\xcc\x00\x00\x00\x00\x00\x00\x00\xce\x00\x00\x00\xd0\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\xdf\xff\xe0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xf4\xff\xf5\xff\xf6\xff\xf7\xff\x00\x00\xf9\xff\xfa\xff\xfb\xff\x00\x00\xe5\xff\xe7\xff\xe8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\xff\xea\xff\xeb\xff\xec\xff\xed\xff\xee\xff\xef\xff\x00\x00\x00\x00\xfc\xff\xf8\xff\x00\x00\xf2\xff\xf1\xff\xe3\xff\xe2\xff\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\xdd\xff\xdc\xff\x00\x00\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\x00\x00\xe9\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x28\x00\x01\x00\x23\x00\x24\x00\x25\x00\x05\x00\x26\x00\x27\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x20\x00\x22\x00\x00\x00\x01\x00\x01\x00\x26\x00\x27\x00\x28\x00\x05\x00\x00\x00\x01\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x16\x00\x20\x00\x03\x00\x00\x00\x01\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x00\x00\x01\x00\x06\x00\x26\x00\x27\x00\x28\x00\x14\x00\x15\x00\x02\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x14\x00\x15\x00\x21\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x22\x00\x00\x00\x01\x00\x00\x00\x01\x00\x14\x00\x15\x00\x22\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x29\x00\x20\x00\x00\x00\x01\x00\x21\x00\x22\x00\x14\x00\x15\x00\x04\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x21\x00\x22\x00\x14\x00\x15\x00\x20\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x21\x00\x22\x00\x14\x00\x15\x00\x01\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x21\x00\x22\x00\x14\x00\x15\x00\x07\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\xff\xff\x22\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\xff\xff\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x40\x00\x07\x00\x4e\x00\x4f\x00\x50\x00\x08\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x32\x00\x33\x00\x15\x00\x34\x00\x35\x00\x36\x00\x37\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x31\x00\x38\x00\x05\x00\x02\x00\x07\x00\x04\x00\x05\x00\x1c\x00\x08\x00\x2e\x00\x02\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x2d\x00\x02\x00\x15\x00\x30\x00\x59\x00\x2c\x00\x02\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x2b\x00\x02\x00\x2a\x00\x02\x00\x51\x00\x04\x00\x05\x00\x1c\x00\x32\x00\x33\x00\x4c\x00\x34\x00\x35\x00\x36\x00\x37\x00\x32\x00\x33\x00\x54\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x29\x00\x02\x00\x28\x00\x02\x00\x32\x00\x33\x00\x38\x00\x34\x00\x35\x00\x36\x00\x37\x00\x27\x00\x02\x00\xff\xff\x53\x00\x26\x00\x02\x00\x49\x00\x38\x00\x32\x00\x33\x00\x55\x00\x34\x00\x35\x00\x36\x00\x37\x00\x25\x00\x02\x00\x24\x00\x02\x00\x23\x00\x02\x00\x4b\x00\x38\x00\x32\x00\x33\x00\x5b\x00\x34\x00\x35\x00\x36\x00\x37\x00\x22\x00\x02\x00\x21\x00\x02\x00\x20\x00\x02\x00\x57\x00\x38\x00\x32\x00\x33\x00\x02\x00\x34\x00\x35\x00\x36\x00\x37\x00\x1f\x00\x02\x00\x1e\x00\x02\x00\x1d\x00\x02\x00\x5e\x00\x38\x00\x32\x00\x33\x00\x5a\x00\x34\x00\x35\x00\x36\x00\x37\x00\x1c\x00\x02\x00\x47\x00\x02\x00\x46\x00\x02\x00\x00\x00\x38\x00\x45\x00\x02\x00\x44\x00\x02\x00\x43\x00\x02\x00\x42\x00\x02\x00\x41\x00\x02\x00\x40\x00\x02\x00\x3e\x00\x02\x00\x3d\x00\x02\x00\x3c\x00\x02\x00\x3b\x00\x02\x00\x3a\x00\x02\x00\x39\x00\x02\x00\x38\x00\x02\x00\x49\x00\x02\x00\x51\x00\x02\x00\x55\x00\x02\x00\x57\x00\x02\x00\x5b\x00\x02\x00\x5c\x00\x02\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 35) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35)
	]

happy_n_terms = 42 :: Prelude.Int
happy_n_nonterms = 3 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (Lit happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happyReduce 10# 0# happyReduction_2
happyReduction_2 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (TokenVar _ happy_var_3) -> 
	case happyOut6 happy_x_5 of { happy_var_5 -> 
	case happyOut4 happy_x_8 of { happy_var_8 -> 
	case happyOut4 happy_x_10 of { happy_var_10 -> 
	happyIn4
		 (Let happy_var_3 happy_var_5 happy_var_8 happy_var_10
	) `HappyStk` happyRest}}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_3  0# happyReduction_3
happyReduction_3 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (Interlace happy_var_2 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happySpecReduce_2  0# happyReduction_4
happyReduction_4 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Rotate90 happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_2  0# happyReduction_5
happyReduction_5 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Rotate180 happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happySpecReduce_2  0# happyReduction_6
happyReduction_6 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Rotate270 happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happySpecReduce_3  0# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (Scale happy_var_2 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_2  0# happyReduction_8
happyReduction_8 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (FlipX happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_2  0# happyReduction_9
happyReduction_9 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (FlipY happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_2  0# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (FlipXY happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_2  0# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Blank happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happyReduce 5# 0# happyReduction_12
happyReduction_12 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut4 happy_x_4 of { happy_var_4 -> 
	case happyOut4 happy_x_5 of { happy_var_5 -> 
	happyIn4
		 (Subtile happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_3  0# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (PlaceRight happy_var_2 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_3  0# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (PlaceBelow happy_var_2 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_2  0# happyReduction_15
happyReduction_15 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Not happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_3  0# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (And happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_3  0# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (Or happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_3  0# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (Greater happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_3  0# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (Less happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_3  0# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (GreaterEqual happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_3  0# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (LessEqual happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happyReduce 12# 0# happyReduction_22
happyReduction_22 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut4 happy_x_7 of { happy_var_7 -> 
	case happyOut4 happy_x_11 of { happy_var_11 -> 
	happyIn4
		 (If happy_var_3 happy_var_7 happy_var_11
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happySpecReduce_2  0# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Read happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_2  0# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Output happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happySpecReduce_3  0# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (Plus happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happySpecReduce_2  0# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Size happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happySpecReduce_3  0# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happySpecReduce_3  0# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (RepeatRight happy_var_2 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happySpecReduce_3  0# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (RepeatDown happy_var_2 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happySpecReduce_1  0# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenVar _ happy_var_1) -> 
	happyIn4
		 (Var happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_31 = happySpecReduce_1  1# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenString _ happy_var_1) -> 
	happyIn5
		 (String happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_32 = happySpecReduce_1  1# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInt _ happy_var_1) -> 
	happyIn5
		 (Int happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_33 = happySpecReduce_1  2# happyReduction_33
happyReduction_33 happy_x_1
	 =  happyIn6
		 (IntType
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_34 = happySpecReduce_1  2# happyReduction_34
happyReduction_34 happy_x_1
	 =  happyIn6
		 (StringType
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_35 = happySpecReduce_1  2# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn6
		 (TileType
	)

happyNewToken action sts stk [] =
	happyDoAction 41# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenLet _ -> cont 1#;
	TokenColon _ -> cont 2#;
	TokenIn _ -> cont 3#;
	TokenAssign _ -> cont 4#;
	TokenIf _ -> cont 5#;
	TokenThen _ -> cont 6#;
	TokenElse _ -> cont 7#;
	TokenRead _ -> cont 8#;
	TokenOutput _ -> cont 9#;
	TokenSize _ -> cont 10#;
	TokenInterlace _ -> cont 11#;
	TokenRotate90 _ -> cont 12#;
	TokenRotate180 _ -> cont 13#;
	TokenRotate270 _ -> cont 14#;
	TokenScale _ -> cont 15#;
	TokenFlipX _ -> cont 16#;
	TokenFlipY _ -> cont 17#;
	TokenFlipXY _ -> cont 18#;
	TokenBlank _ -> cont 19#;
	TokenAnd _ -> cont 20#;
	TokenOr _ -> cont 21#;
	TokenNot _ -> cont 22#;
	TokenGreater _ -> cont 23#;
	TokenLess _ -> cont 24#;
	TokenGreaterEqual _ -> cont 25#;
	TokenLessEqual _ -> cont 26#;
	TokenSubtile _ -> cont 27#;
	TokenPlaceRight _ -> cont 28#;
	TokenPlaceBelow _ -> cont 29#;
	TokenRepeatRight _ -> cont 30#;
	TokenRepeatDown _ -> cont 31#;
	TokenLParen _ -> cont 32#;
	TokenRParen _ -> cont 33#;
	TokenPlus _ -> cont 34#;
	TokenIntType _ -> cont 35#;
	TokenStringType _ -> cont 36#;
	TokenTileType _ -> cont 37#;
	TokenInt _ happy_dollar_dollar -> cont 38#;
	TokenString _ happy_dollar_dollar -> cont 39#;
	TokenVar _ happy_dollar_dollar -> cont 40#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 41# tk tks = happyError' (tks, explist)
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
parseTsl tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {x' = happyOut4 x} in x'))

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (t:ts) = error ("Parse error at " ++ (tokenPosn t))

data Exp =
         Let String Type Exp Exp
         | If Exp Exp Exp
         | Read Exp
         | Output Exp

         | Size Exp
         | Plus Exp Exp
         | Interlace Exp Exp
         | Rotate90 Exp
         | Rotate180 Exp
         | Rotate270 Exp
         | Scale Exp Exp
         | FlipX Exp
         | FlipY Exp
         | FlipXY Exp
         | Blank Exp
         | Subtile Exp Exp Exp Exp
         | PlaceRight Exp Exp
         | PlaceBelow Exp Exp
         | Not Exp
         | And Exp Exp
         | Or Exp Exp
         | Greater Exp Exp
         | Less Exp Exp
         | GreaterEqual Exp Exp
         | LessEqual Exp Exp
         | RepeatRight Exp Exp
         | RepeatDown Exp Exp

         | Lit Literal

         | Var String

         | Cl String Exp [(String, Exp)]
         | END
         deriving (Show, Eq)

data Literal =
          Int Int
          | String String
          | Bool Bool
          | Tile [String]
          deriving (Show, Eq)

data Type =
          IntType
          | StringType
          | TileType
          | BoolType
          | FunctionType
          deriving (Show, Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































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
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
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
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


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


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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

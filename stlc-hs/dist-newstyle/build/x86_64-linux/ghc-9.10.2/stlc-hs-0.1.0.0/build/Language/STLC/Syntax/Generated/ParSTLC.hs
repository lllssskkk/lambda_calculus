{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.STLC.Syntax.Generated.ParSTLC
  ( happyError
  , myLexer
  , pProg
  ) where

import Prelude

import qualified Language.STLC.Syntax.Generated.AbsSTLC
import Language.STLC.Syntax.Generated.LexSTLC
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Ident))
happyIn4 :: ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Ident)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Prog))
happyIn5 :: ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Prog)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Atom))
happyIn6 :: ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Atom)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Application))
happyIn7 :: ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Application)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Term))
happyIn8 :: ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Term)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Type))
happyIn9 :: ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Type)) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Type))
happyIn10 :: ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Type)) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Type))
happyIn11 :: ((Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position, Language.STLC.Syntax.Generated.AbsSTLC.Type)) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x08\xe0\xbe\x0b\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x02\x00\x00\x00\x00\x00\x08\xe0\xbe\x0b\x00\x00\x00\x00\x00\x20\x80\xfb\x2e\x00\x04\x70\xdf\x05\x00\x00\x00\x80\x00\x00\x00\x00\x10\x00\x02\xb8\xef\x02\x40\x00\xf7\x5d\x00\x08\xe0\xbe\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x70\xdf\x05\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\xf7\x5d\x00\x08\x0c\x00\x00\x00\x01\xdc\x77\x01\x20\x80\xfb\x2e\x00\x00\x00\x00\x02\x00\x00\x10\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x08\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x80\x00\xee\xbb\x00\x20\x00\x00\x00\x00\x02\xb8\xef\x02\x40\x60\x00\x00\x00\x08\xe0\xbe\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProg_internal","Ident","Prog","Atom","Application","Term","Type1","Type2","Type","'('","')'","','","'->'","'.'","':'","'='","'Bool'","'Nat'","'else'","'false'","'first'","'if'","'in'","'lam'","'let'","'pred'","'second'","'succ'","'then'","'true'","'zero'","'{'","'}'","L_Ident","%eof"]
        bit_start = st               Prelude.* 37
        bit_end   = (st Prelude.+ 1) Prelude.* 37
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..36]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x01\x00\x00\x00\xe8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xff\xff\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\xf6\xff\xff\xff\xf6\xff\xff\xff\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x59\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x20\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\x38\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x01\x00\x00\x00\x40\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x28\x00\x00\x00\x4c\x00\x00\x00\x51\x00\x00\x00\x2d\x00\x00\x00\x32\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x5b\x00\x00\x00\x46\x00\x00\x00\x4b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x55\x00\x00\x00\x60\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\xfc\xff\xff\xff\x00\x00\x00\x00\xfa\xff\xff\xff\xf8\xff\xff\xff\xfd\xff\xff\xff\x00\x00\x00\x00\xf4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xff\xff\xf1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\xff\xff\xed\xff\xff\xff\xf0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xee\xff\xff\xff\x00\x00\x00\x00\xf9\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe7\xff\xff\xff\xeb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xff\xff\xea\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\xff\xff\xf7\xff\xff\xff\xec\xff\xff\xff\xf6\xff\xff\xff\xe8\xff\xff\xff\xf3\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x19\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x19\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x03\x00\x00\x00\x19\x00\x00\x00\x19\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x06\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x06\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x09\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x29\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x23\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x03\x00\x00\x00\x22\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x1c\x00\x00\x00\x03\x00\x00\x00\x20\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x1b\x00\x00\x00\x03\x00\x00\x00\x1f\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x1a\x00\x00\x00\x03\x00\x00\x00\x31\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x17\x00\x00\x00\x03\x00\x00\x00\x32\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x30\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x2f\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x14\x00\x00\x00\x03\x00\x00\x00\x36\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x2b\x00\x00\x00\x03\x00\x00\x00\x2d\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x24\x00\x00\x00\x03\x00\x00\x00\x19\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x23\x00\x00\x00\x03\x00\x00\x00\x18\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x36\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x34\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x32\x00\x00\x00\x21\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x2d\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 24) [
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
        (24 , happyReduce_24)
        ]

happy_n_terms = 27 :: Prelude.Int
happy_n_nonterms = 8 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn4
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.Ident (tokenText happy_var_1))
        )}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
         =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
        happyIn5
                 ((fst happy_var_1, Language.STLC.Syntax.Generated.AbsSTLC.Program (fst happy_var_1) (snd happy_var_1))
        )}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
         =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
        happyIn6
                 ((fst happy_var_1, Language.STLC.Syntax.Generated.AbsSTLC.AVar (fst happy_var_1) (snd happy_var_1))
        )}

happyReduce_4 = happySpecReduce_3  2# happyReduction_4
happyReduction_4 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
        happyIn6
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.ATerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
        )}}

happyReduce_5 = happySpecReduce_1  3# happyReduction_5
happyReduction_5 happy_x_1
         =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
        happyIn7
                 ((fst happy_var_1, Language.STLC.Syntax.Generated.AbsSTLC.ApplicationAtom (fst happy_var_1) (snd happy_var_1))
        )}

happyReduce_6 = happySpecReduce_2  3# happyReduction_6
happyReduction_6 happy_x_2
        happy_x_1
         =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
        case happyOut6 happy_x_2 of { (HappyWrap6 happy_var_2) -> 
        happyIn7
                 ((fst happy_var_1, Language.STLC.Syntax.Generated.AbsSTLC.AApplication (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
        )}}

happyReduce_7 = happySpecReduce_1  4# happyReduction_7
happyReduction_7 happy_x_1
         =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
        happyIn8
                 ((fst happy_var_1, Language.STLC.Syntax.Generated.AbsSTLC.ApplicationTerm (fst happy_var_1) (snd happy_var_1))
        )}

happyReduce_8 = happyReduce 6# 4# happyReduction_8
happyReduction_8 (happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
        case happyOut8 happy_x_4 of { (HappyWrap8 happy_var_4) -> 
        case happyOut8 happy_x_6 of { (HappyWrap8 happy_var_6) -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.LetInTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
        ) `HappyStk` happyRest}}}}

happyReduce_9 = happyReduce 6# 4# happyReduction_9
happyReduction_9 (happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
        case happyOut11 happy_x_4 of { (HappyWrap11 happy_var_4) -> 
        case happyOut8 happy_x_6 of { (HappyWrap8 happy_var_6) -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.LambdaAbsTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
        ) `HappyStk` happyRest}}}}

happyReduce_10 = happySpecReduce_1  4# happyReduction_10
happyReduction_10 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.TrueTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)))
        )}

happyReduce_11 = happySpecReduce_1  4# happyReduction_11
happyReduction_11 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.FalseTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)))
        )}

happyReduce_12 = happyReduce 6# 4# happyReduction_12
happyReduction_12 (happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
        case happyOut8 happy_x_4 of { (HappyWrap8 happy_var_4) -> 
        case happyOut8 happy_x_6 of { (HappyWrap8 happy_var_6) -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.IfTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
        ) `HappyStk` happyRest}}}}

happyReduce_13 = happySpecReduce_2  4# happyReduction_13
happyReduction_13 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.SuccTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
        )}}

happyReduce_14 = happySpecReduce_1  4# happyReduction_14
happyReduction_14 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.ZeroTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)))
        )}

happyReduce_15 = happySpecReduce_2  4# happyReduction_15
happyReduction_15 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.PredTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
        )}}

happyReduce_16 = happyReduce 5# 4# happyReduction_16
happyReduction_16 (happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
        case happyOut8 happy_x_4 of { (HappyWrap8 happy_var_4) -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.PairTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
        ) `HappyStk` happyRest}}}

happyReduce_17 = happySpecReduce_2  4# happyReduction_17
happyReduction_17 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.FstTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
        )}}

happyReduce_18 = happySpecReduce_2  4# happyReduction_18
happyReduction_18 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
        happyIn8
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.SndTerm (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
        )}}

happyReduce_19 = happySpecReduce_3  5# happyReduction_19
happyReduction_19 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
        case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
        happyIn9
                 ((fst happy_var_1, Language.STLC.Syntax.Generated.AbsSTLC.Arrow (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
        )}}

happyReduce_20 = happySpecReduce_1  5# happyReduction_20
happyReduction_20 happy_x_1
         =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
        happyIn9
                 ((fst happy_var_1, (snd happy_var_1))
        )}

happyReduce_21 = happySpecReduce_1  6# happyReduction_21
happyReduction_21 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn10
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.Nat (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)))
        )}

happyReduce_22 = happySpecReduce_1  6# happyReduction_22
happyReduction_22 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn10
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), Language.STLC.Syntax.Generated.AbsSTLC.Bool (uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1)))
        )}

happyReduce_23 = happySpecReduce_3  6# happyReduction_23
happyReduction_23 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
        happyIn10
                 ((uncurry Language.STLC.Syntax.Generated.AbsSTLC.BNFC'Position (tokenLineCol happy_var_1), (snd happy_var_2))
        )}}

happyReduce_24 = happySpecReduce_1  7# happyReduction_24
happyReduction_24 happy_x_1
         =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
        happyIn11
                 ((fst happy_var_1, (snd happy_var_1))
        )}

happyNewToken action sts stk [] =
        happyDoAction 26# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = happyDoAction i tk action sts stk tks in
        case tk of {
        PT _ (TS _ 1) -> cont 1#;
        PT _ (TS _ 2) -> cont 2#;
        PT _ (TS _ 3) -> cont 3#;
        PT _ (TS _ 4) -> cont 4#;
        PT _ (TS _ 5) -> cont 5#;
        PT _ (TS _ 6) -> cont 6#;
        PT _ (TS _ 7) -> cont 7#;
        PT _ (TS _ 8) -> cont 8#;
        PT _ (TS _ 9) -> cont 9#;
        PT _ (TS _ 10) -> cont 10#;
        PT _ (TS _ 11) -> cont 11#;
        PT _ (TS _ 12) -> cont 12#;
        PT _ (TS _ 13) -> cont 13#;
        PT _ (TS _ 14) -> cont 14#;
        PT _ (TS _ 15) -> cont 15#;
        PT _ (TS _ 16) -> cont 16#;
        PT _ (TS _ 17) -> cont 17#;
        PT _ (TS _ 18) -> cont 18#;
        PT _ (TS _ 19) -> cont 19#;
        PT _ (TS _ 20) -> cont 20#;
        PT _ (TS _ 21) -> cont 21#;
        PT _ (TS _ 22) -> cont 22#;
        PT _ (TS _ 23) -> cont 23#;
        PT _ (TS _ 24) -> cont 24#;
        PT _ (TV _) -> cont 25#;
        _ -> happyError' ((tk:tks), [])
        }

happyError_ explist 26# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProg_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap5 x') = happyOut5 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProg :: [Token] -> Err Language.STLC.Syntax.Generated.AbsSTLC.Prog
pProg = fmap snd . pProg_internal
#define HAPPY_COERCE 1
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

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
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail (happyExpListPerState (Happy_GHC_Exts.I# st)) i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
-- trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st sts stk
     = happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyAdjustOffset (happyIndexOffAddr happyGotoOffsets st1)
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            happyThen1 (fn stk tk)
                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st (HappyCons action sts)
                               (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction ERROR_TOK tk action sts (saved_tok`HappyStk`stk)
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk action sts stk =
-- trace "entering error recovery" $
        happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
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

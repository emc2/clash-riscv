-- Copyright (c) 2017 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, TypeOperators #-}

module RISCV.ISA.Formats(
--       module RISCV.ISA.Formats.Instr16,
       module RISCV.ISA.Formats.Instr32,
       module RISCV.ISA.Formats.Types,

       isInstr16,
       isInstr32,
       isInstr48,
       isInstr64,
       isInstr80,
       isInstr96,
       isInstr112,
       isInstr128,
       isInstr144,
       isInstr160,
       isInstr176
       ) where

import CLaSH.Prelude
--import RISCV.ISA.Formats.Instr16
import RISCV.ISA.Formats.Instr32
import RISCV.ISA.Formats.Types

-- | Indicate if an instruction is in the 16-bit format.
isInstr16 :: (BitPack a, BitSize a ~ (2 + i)) =>
             a
          -> Bit
isInstr16 = complement . reduceAnd . slice d1 d0 . pack

-- | Indicate if an instruction is in the 32-bit format.
isInstr32 :: (BitPack a, BitSize a ~ (5 + i)) =>
             a
          -> Bit
isInstr32 d =
  let
    bits = slice d4 d0 (pack d) :: BitVector 5
    (hi, lo) = split bits :: (BitVector 3, BitVector 2)
  in
    reduceAnd lo .&. complement (reduceAnd hi)

-- | Indicate if an instruction is in the 48-bit format.
isInstr48 :: (BitPack a, BitSize a ~ (6 + i)) =>
             a
          -> Bit
isInstr48 d =
  let
    bits = slice d5 d0 (pack d) :: BitVector 6
    (midhi, lo) = split bits :: (BitVector 4, BitVector 2)
    (hi, mid) = split midhi :: (Bit, BitVector 3)
  in
    reduceAnd lo .&. reduceAnd mid .&. complement hi

-- | Indicate if an instruction is in the 64-bit format.
isInstr64 :: (BitPack a, BitSize a ~ (7 + i)) =>
             a
          -> Bit
isInstr64 d =
  let
    bits = slice d6 d0 (pack d) :: BitVector 7
    (midhi, lo) = split bits :: (BitVector 5, BitVector 2)
    (hi, mid) = split midhi :: (Bit, BitVector 4)
  in
    reduceAnd lo .&. reduceAnd mid .&. complement hi

-- | Indicate if an instruction is in the 80-bit format.
isInstr80 :: (BitPack a, BitSize a ~ (15 + i)) =>
             a
          -> Bit
isInstr80 d =
  let
    bits = slice d14 d0 (pack d)
    lo = slice d6 d0 bits
    hi = slice d14 d12 bits
  in
    reduceAnd lo .&. complement (reduceAnd hi)

-- | Indicate if an instruction is in the 96-bit format.
isInstr96 :: (BitPack a, BitSize a ~ (15 + i)) =>
             a
          -> Bit
isInstr96 d =
  let
    bits = slice d14 d0 (pack d)
    lo = slice d6 d0 bits
    hi = slice d14 d12 bits
  in
    reduceAnd lo .&. complement (reduceAnd (hi `xor` 0b001))

-- | Indicate if an instruction is in the 112-bit format.
isInstr112 :: (BitPack a, BitSize a ~ (15 + i)) =>
             a
          -> Bit
isInstr112 d =
  let
    bits = slice d14 d0 (pack d)
    lo = slice d6 d0 bits
    hi = slice d14 d12 bits
  in
    reduceAnd lo .&. complement (reduceAnd (hi `xor` 0b010))

-- | Indicate if an instruction is in the 128-bit format.
isInstr128 :: (BitPack a, BitSize a ~ (15 + i)) =>
             a
          -> Bit
isInstr128 d =
  let
    bits = slice d14 d0 (pack d)
    lo = slice d6 d0 bits
    hi = slice d14 d12 bits
  in
    reduceAnd lo .&. complement (reduceAnd (hi `xor` 0b011))

-- | Indicate if an instruction is in the 144-bit format.
isInstr144 :: (BitPack a, BitSize a ~ (15 + i)) =>
             a
          -> Bit
isInstr144 d =
  let
    bits = slice d14 d0 (pack d)
    lo = slice d6 d0 bits
    hi = slice d14 d12 bits
  in
    reduceAnd lo .&. complement (reduceAnd (hi `xor` 0b100))

-- | Indicate if an instruction is in the 160-bit format.
isInstr160 :: (BitPack a, BitSize a ~ (15 + i)) =>
             a
          -> Bit
isInstr160 d =
  let
    bits = slice d14 d0 (pack d)
    lo = slice d6 d0 bits
    hi = slice d14 d12 bits
  in
    reduceAnd lo .&. complement (reduceAnd (hi `xor` 0b101))

-- | Indicate if an instruction is in the 176-bit format.
isInstr176 :: (BitPack a, BitSize a ~ (15 + i)) =>
             a
          -> Bit
isInstr176 d =
  let
    bits = slice d14 d0 (pack d)
    lo = slice d6 d0 bits
    hi = slice d14 d12 bits
  in
    reduceAnd lo .&. complement (reduceAnd (hi `xor` 0b110))

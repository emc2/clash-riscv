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
{-# LANGUAGE DataKinds, TypeFamilies #-}

module RISCV.ISA.Registers.Regular(
       Reg(..)
       ) where

import Prelude
import CLaSH.Class.BitPack

data Reg =
    ZERO
  | RA
  | SP
  | GP
  | TP
  | T0
  | T1
  | T2
  | FP
  | S1
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | T3
  | T4
  | T5
  | T6
    deriving (Eq, Ord)

instance Enum Reg where
  fromEnum ZERO = 0x00
  fromEnum RA = 0x01
  fromEnum SP = 0x02
  fromEnum GP = 0x03
  fromEnum TP = 0x04
  fromEnum T0 = 0x05
  fromEnum T1 = 0x06
  fromEnum T2 = 0x07
  fromEnum FP = 0x08
  fromEnum S1 = 0x09
  fromEnum A0 = 0x0a
  fromEnum A1 = 0x0b
  fromEnum A2 = 0x0c
  fromEnum A3 = 0x0d
  fromEnum A4 = 0x0e
  fromEnum A5 = 0x0f
  fromEnum A6 = 0x10
  fromEnum A7 = 0x11
  fromEnum S2 = 0x12
  fromEnum S3 = 0x13
  fromEnum S4 = 0x14
  fromEnum S5 = 0x15
  fromEnum S6 = 0x16
  fromEnum S7 = 0x17
  fromEnum S8 = 0x18
  fromEnum S9 = 0x19
  fromEnum S10 = 0x1a
  fromEnum S11 = 0x1b
  fromEnum T3 = 0x1c
  fromEnum T4 = 0x1d
  fromEnum T5 = 0x1e
  fromEnum T6 = 0x1f

  toEnum 0x00 = ZERO
  toEnum 0x01 = RA
  toEnum 0x02 = SP
  toEnum 0x03 = GP
  toEnum 0x04 = TP
  toEnum 0x05 = T0
  toEnum 0x06 = T1
  toEnum 0x07 = T2
  toEnum 0x08 = FP
  toEnum 0x09 = S1
  toEnum 0x0a = A0
  toEnum 0x0b = A1
  toEnum 0x0c = A2
  toEnum 0x0d = A3
  toEnum 0x0e = A4
  toEnum 0x0f = A5
  toEnum 0x10 = A6
  toEnum 0x11 = A7
  toEnum 0x12 = S2
  toEnum 0x13 = S3
  toEnum 0x14 = S4
  toEnum 0x15 = S5
  toEnum 0x16 = S6
  toEnum 0x17 = S7
  toEnum 0x18 = S8
  toEnum 0x19 = S9
  toEnum 0x1a = S10
  toEnum 0x1b = S11
  toEnum 0x1c = T3
  toEnum 0x1d = T4
  toEnum 0x1e = T5
  toEnum 0x1f = T6
  toEnum _ = error "Invalid register ID"

instance BitPack Reg where
  type BitSize Reg = 5

  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

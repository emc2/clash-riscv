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

module RISCV.ISA.Registers.FP(
       FPReg(..)
       ) where

import Prelude
import CLaSH.Class.BitPack

-- | RISC-V floating-point registers.
data FPReg =
    FT0
  | FT1
  | FT2
  | FT3
  | FT4
  | FT5
  | FT6
  | FT7
  | FS0
  | FS1
  | FA0
  | FA1
  | FA2
  | FA3
  | FA4
  | FA5
  | FA6
  | FA7
  | FS2
  | FS3
  | FS4
  | FS5
  | FS6
  | FS7
  | FS8
  | FS9
  | FS10
  | FS11
  | FT8
  | FT9
  | FT10
  | FT11
    deriving (Eq, Ord, Show)

instance Enum FPReg where
  fromEnum FT0 = 0x00
  fromEnum FT1 = 0x01
  fromEnum FT2 = 0x02
  fromEnum FT3 = 0x03
  fromEnum FT4 = 0x04
  fromEnum FT5 = 0x05
  fromEnum FT6 = 0x06
  fromEnum FT7 = 0x07
  fromEnum FS0 = 0x08
  fromEnum FS1 = 0x09
  fromEnum FA0 = 0x0a
  fromEnum FA1 = 0x0b
  fromEnum FA2 = 0x0c
  fromEnum FA3 = 0x0d
  fromEnum FA4 = 0x0e
  fromEnum FA5 = 0x0f
  fromEnum FA6 = 0x10
  fromEnum FA7 = 0x11
  fromEnum FS2 = 0x12
  fromEnum FS3 = 0x13
  fromEnum FS4 = 0x14
  fromEnum FS5 = 0x15
  fromEnum FS6 = 0x16
  fromEnum FS7 = 0x17
  fromEnum FS8 = 0x18
  fromEnum FS9 = 0x19
  fromEnum FS10 = 0x1a
  fromEnum FS11 = 0x1b
  fromEnum FT8 = 0x1c
  fromEnum FT9 = 0x1d
  fromEnum FT10 = 0x1e
  fromEnum FT11 = 0x1f

  toEnum 0x00 = FT0
  toEnum 0x01 = FT1
  toEnum 0x02 = FT2
  toEnum 0x03 = FT3
  toEnum 0x04 = FT4
  toEnum 0x05 = FT5
  toEnum 0x06 = FT6
  toEnum 0x07 = FT7
  toEnum 0x08 = FS0
  toEnum 0x09 = FS1
  toEnum 0x0a = FA0
  toEnum 0x0b = FA1
  toEnum 0x0c = FA2
  toEnum 0x0d = FA3
  toEnum 0x0e = FA4
  toEnum 0x0f = FA5
  toEnum 0x10 = FA6
  toEnum 0x11 = FA7
  toEnum 0x12 = FS2
  toEnum 0x13 = FS3
  toEnum 0x14 = FS4
  toEnum 0x15 = FS5
  toEnum 0x16 = FS6
  toEnum 0x17 = FS7
  toEnum 0x18 = FS8
  toEnum 0x19 = FS9
  toEnum 0x1a = FS10
  toEnum 0x1b = FS11
  toEnum 0x1c = FT8
  toEnum 0x1d = FT9
  toEnum 0x1e = FT10
  toEnum 0x1f = FT11
  toEnum _ = error "Invalid register ID"

instance BitPack FPReg where
  type BitSize FPReg = 5

  pack = toEnum . fromEnum
  unpack = toEnum . fromEnum

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

module RISCV.ISA.Opcodes.Opcode32(
       Opcode32(..)
       ) where

import Prelude
import CLaSH.Class.BitPack

data Opcode32 =
    LOAD
  | LOAD_FP
  | CUSTOM_0
  | MISC_MEM
  | OP_IMM
  | AUIPC
  | OP_IMM_32
  | STORE
  | STORE_FP
  | CUSTOM_1
  | AMO
  | OP
  | LUI
  | OP_32
  | MADD
  | MSUB
  | NMSUB
  | NMADD
  | OP_FP
  | RESERVED_0
  | CUSTOM_2
  | BRANCH
  | JALR
  | RESERVED_1
  | JAL
  | SYSTEM
  | RESERVED_2
  | CUSTOM_3
    deriving (Eq, Ord)

instance BitPack Opcode32 where
  type BitSize Opcode32 = 5

  pack LOAD = 0b00000
  pack LOAD_FP = 0b00001
  pack CUSTOM_0 = 0b00010
  pack MISC_MEM = 0b00011
  pack OP_IMM = 0b00100
  pack AUIPC = 0b00101
  pack OP_IMM_32 = 0b00110
  pack STORE = 0b01000
  pack STORE_FP = 0b01001
  pack CUSTOM_1 = 0b01010
  pack AMO = 0b01011
  pack OP = 0b01100
  pack LUI = 0b01101
  pack OP_32 = 0b01110
  pack MADD = 0b10000
  pack MSUB = 0b10001
  pack NMSUB = 0b10010
  pack NMADD = 0b10011
  pack OP_FP = 0b10100
  pack RESERVED_0 = 0b10101
  pack CUSTOM_2 = 0b10110
  pack BRANCH = 0b11000
  pack JALR = 0b11001
  pack RESERVED_1 = 0b11010
  pack JAL = 0b11011
  pack SYSTEM = 0b11100
  pack RESERVED_2 = 0b11101
  pack CUSTOM_3 = 0b11110

  unpack 0b00000 = LOAD
  unpack 0b00001 = LOAD_FP
  unpack 0b00010 = CUSTOM_0
  unpack 0b00011 = MISC_MEM
  unpack 0b00100 = OP_IMM_32
  unpack 0b00101 = AUIPC
  unpack 0b00110 = OP_IMM_32
  unpack 0b01000 = STORE
  unpack 0b01001 = STORE_FP
  unpack 0b01010 = CUSTOM_1
  unpack 0b01011 = AMO
  unpack 0b01100 = OP
  unpack 0b01101 = LUI
  unpack 0b01110 = OP_32
  unpack 0b10000 = MADD
  unpack 0b10001 = MSUB
  unpack 0b10010 = NMSUB
  unpack 0b10011 = NMADD
  unpack 0b10100 = OP_FP
  unpack 0b10101 = RESERVED_0
  unpack 0b10110 = CUSTOM_2
  unpack 0b11000 = BRANCH
  unpack 0b11001 = JALR
  unpack 0b11010 = RESERVED_1
  unpack 0b11011 = JAL
  unpack 0b11100 = SYSTEM
  unpack 0b11101 = RESERVED_2
  unpack 0b11110 = CUSTOM_3
  unpack code = error ("Invalid opcode " ++ show code)

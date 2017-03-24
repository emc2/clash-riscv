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

module RISCV.ISA.Formats.Types(
       Instr16(..),
       Instr32(..),
       Instr48(..),
       Instr64(..),
       Instr80(..),
       Instr96(..),
       Instr112(..),
       Instr128(..),
       Instr144(..),
       Instr160(..),
       Instr176(..),
       ) where

import CLaSH.Class.BitPack
import CLaSH.Sized.BitVector

newtype Instr16 = Instr16 { instr16 :: BitVector 16 }
newtype Instr32 = Instr32 { instr32 :: BitVector 32 }
newtype Instr48 = Instr48 { instr48 :: BitVector 48 }
newtype Instr64 = Instr64 { instr64 :: BitVector 64 }
newtype Instr80 = Instr80 { instr80 :: BitVector 80 }
newtype Instr96 = Instr96 { instr96 :: BitVector 96 }
newtype Instr112 = Instr112 { instr112 :: BitVector 112 }
newtype Instr128 = Instr128 { instr128 :: BitVector 128 }
newtype Instr144 = Instr144 { instr144 :: BitVector 144 }
newtype Instr160 = Instr160 { instr160 :: BitVector 160 }
newtype Instr176 = Instr176 { instr176 :: BitVector 176 }

instance BitPack Instr16 where
  type BitSize Instr16 = 16
  pack = instr16
  unpack = Instr16

instance BitPack Instr32 where
  type BitSize Instr32 = 32
  pack = instr32
  unpack = Instr32

instance BitPack Instr48 where
  type BitSize Instr48 = 48
  pack = instr48
  unpack = Instr48

instance BitPack Instr64 where
  type BitSize Instr64 = 64
  pack = instr64
  unpack = Instr64

instance BitPack Instr80 where
  type BitSize Instr80 = 80
  pack = instr80
  unpack = Instr80

instance BitPack Instr96 where
  type BitSize Instr96 = 96
  pack = instr96
  unpack = Instr96

instance BitPack Instr112 where
  type BitSize Instr112 = 112
  pack = instr112
  unpack = Instr112

instance BitPack Instr128 where
  type BitSize Instr128 = 128
  pack = instr128
  unpack = Instr128

instance BitPack Instr144 where
  type BitSize Instr144 = 144
  pack = instr144
  unpack = Instr144

instance BitPack Instr160 where
  type BitSize Instr160 = 160
  pack = instr160
  unpack = Instr160

instance BitPack Instr176 where
  type BitSize Instr176 = 176
  pack = instr176
  unpack = Instr176

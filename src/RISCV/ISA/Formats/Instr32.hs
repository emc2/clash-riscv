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

module RISCV.ISA.Formats.Instr32(
       -- * Function Codes
       instr32Funct,
       instr32FunctSub,

       -- * Register Values

       -- ** RD Register
       instr32RegularRD,
       instr32FPRD,

       -- ** RS1 Register
       instr32RegularRS1,
       instr32FPRS1,

       -- ** RS2 Register
       instr32RegularRS2,
       instr32FPRS2,

       -- ** RS2 Register
       instr32FPRS3,
) where

import CLaSH.Prelude
import RISCV.ISA.Registers
import RISCV.ISA.Formats.Types

-- | Get the raw bits for the function sub-code.  This is the @funct3@
-- field in the spec.
instr32Funct :: Instr32
             -- ^ Instruction for which to get the function code.
             -> BitVector 3
instr32Funct = slice d14 d12

-- | Get the raw bits for the function sub-code.  This is the @funct7@
-- field in the spec.
instr32FunctSub :: Instr32
                -- ^ Instruction for which to get the function sub-code.
                -> BitVector 3
instr32FunctSub = slice d14 d12

-- | Get the raw bits for the register RD.
instr32RD :: Instr32
          -- ^ Instruction for which to get the register.
          -> BitVector 5
instr32RD = slice d11 d7

-- | Get the register RD as a regular register.  This does not check
-- that the given instruction actually encodes an RD value.
instr32RegularRD :: Instr32
                 -- ^ Instruction for which to get the register.
                 -> Reg
instr32RegularRD = unpack . instr32RD

-- | Get the register RD as a floating point register.  This does not
-- check that the given instruction actually encodes an RD value.
instr32FPRD :: Instr32
            -- ^ Instruction for which to get the register.
            -> FPReg
instr32FPRD = unpack . instr32RD

-- | Get the raw bits for the register RS1.
instr32RS1 :: Instr32
           -- ^ Instruction for which to get the register.
           -> BitVector 5
instr32RS1 = slice d19 d15

-- | Get the register RS1 as a regular register.  This does not check
-- that the given instruction actually encodes an RS1 value.
instr32RegularRS1 :: Instr32
                  -- ^ Instruction for which to get the register.
                  -> Reg
instr32RegularRS1 = unpack . instr32RS1

-- | Get the register RS1 as a floating point register.  This does not
-- check that the given instruction actually encodes an RS1 value.
instr32FPRS1 :: Instr32
             -- ^ Instruction for which to get the register.
             -> FPReg
instr32FPRS1 = unpack . instr32RS1

-- | Get the raw bits for the register RS2.
instr32RS2 :: Instr32
           -- ^ Instruction for which to get the register.
           -> BitVector 5
instr32RS2 = slice d24 d20

-- | Get the register RS2 as a regular register.  This does not check
-- that the given instruction actually encodes an RS2 value.
instr32RegularRS2 :: Instr32
                  -- ^ Instruction for which to get the register.
                  -> Reg
instr32RegularRS2 = unpack . instr32RS2

-- | Get the register RS2 as a floating point register.  This does not
-- check that the given instruction actually encodes an RS2 value.
instr32FPRS2 :: Instr32
             -- ^ Instruction for which to get the register.
             -> FPReg
instr32FPRS2 = unpack . instr32RS2

-- | Get the raw bits for the register RS3.
instr32RS3 :: Instr32
           -- ^ Instruction for which to get the register.
           -> BitVector 5
instr32RS3 = slice d31 d27

-- | Get the register RS3 as a floating point register.  This does not
-- check that the given instruction actually encodes an RS3 value.
instr32FPRS3 :: Instr32
             -- ^ Instruction for which to get the register.
             -> FPReg
instr32FPRS3 = unpack . instr32RS3

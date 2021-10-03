module VM.Instruction
  ( Instruction (..),
    OpCode (..),
    OpCodeName (..),
    opcode,
    opCodeName,
    argOf,
    loadInt8,
    terminate,
    nop,
  )
where

import Data.Bits (Bits (shiftL, unsafeShiftR, (.&.), (.|.)))
import Data.Int (Int16, Int8)

data OpCodeName
  = LoadInt8
  | Terminate
  | Nop
  | Add
  | Dup
  | Drop
  | Sub
  | Mul
  deriving (Eq, Show, Read, Enum, Bounded)

newtype OpCode = OpCode {getOpCode :: Int}

newtype Instruction = Instruction {getInstruction :: Int16}

mkInstruction1 :: OpCodeName -> Int8 -> Instruction
mkInstruction1 opc arg = Instruction $ shiftL (fromIntegral arg) 8 .|. fromIntegral (fromEnum opc)

mkInstruction :: OpCodeName -> Instruction
mkInstruction opc = mkInstruction1 opc 1

opcode :: Instruction -> OpCode
opcode = OpCode . fromIntegral . (.&. 0xff) . getInstruction

opCodeName :: OpCode -> OpCodeName
opCodeName (OpCode c)
  | c >= fromEnum (minBound @OpCodeName) && c <= fromEnum (maxBound @OpCodeName) = toEnum c
  | otherwise = Nop

argOf :: Instruction -> Int
argOf = fromIntegral . (`unsafeShiftR` 8) . getInstruction

instance Show Instruction where
  show i = unwords $ show opName : argComponents
    where
      opName = opCodeName $ opcode i

      argComponents = case opName of
        LoadInt8 -> [show $ argOf i]
        _ -> []

loadInt8 :: Int8 -> Instruction
loadInt8 = mkInstruction1 LoadInt8

terminate :: Instruction
terminate = mkInstruction Terminate

nop :: Instruction
nop = mkInstruction Nop

module VM.Instruction
  ( Instruction (..),
    OpCode (..),
    OpCodeName (..),
    opcode,
    opCodeName,
    argOf,
    loadConst,
    end,
    nop,
  )
where

import Data.Bits (Bits (shiftL, unsafeShiftR, (.&.), (.|.)))
import Data.Int (Int16, Int8)

data OpCodeName
  = LoadConst
  | End
  | Unknown
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
  | otherwise = Unknown

argOf :: Instruction -> Int
argOf = fromIntegral . (`unsafeShiftR` 8) . getInstruction

instance Show Instruction where
  show i = unwords $ show opName : argComponents
    where
      opName = opCodeName $ opcode i

      argComponents = case opName of
        LoadConst -> [show $ argOf i]
        _ -> []

loadConst :: Int8 -> Instruction
loadConst = mkInstruction1 LoadConst

end :: Instruction
end = mkInstruction End

nop :: Instruction
nop = mkInstruction Unknown

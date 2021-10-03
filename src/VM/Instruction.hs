module VM.Instruction
  ( Instruction (..),
    OpCodeName (..),
    opCodeByteOf,
    opCodeName,
    signedArgOf,
    loadInt8,
    terminate,
    nop,
  )
where

import Data.Bits
import Data.Int
import Data.Word
import Text.Printf

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

newtype Instruction = Instruction {getInstruction :: Int16}

mkInstruction1 :: OpCodeName -> Int8 -> Instruction
mkInstruction1 opc arg = Instruction $ shiftL (fromIntegral arg) 8 .|. fromIntegral (fromEnum opc)

mkInstruction :: OpCodeName -> Instruction
mkInstruction opc = mkInstruction1 opc 1

opCodeByteOf :: Instruction -> Word8
opCodeByteOf = fromIntegral . getInstruction

opCodeName :: Word8 -> OpCodeName
opCodeName byte
  | value >= fromEnum (minBound @OpCodeName) && value <= fromEnum (maxBound @OpCodeName) = toEnum value
  | otherwise = Nop
  where
    value = fromIntegral byte

signedArgOf :: Instruction -> Int
signedArgOf = fromIntegral . (`unsafeShiftR` 8) . getInstruction

instance Show Instruction where
  show i = unwords $ show opName : (args ++ bytes)
    where
      opName = opCodeName $ opCodeByteOf i

      args = case opName of
        LoadInt8 -> [show $ signedArgOf i]
        _ -> []

      bytes = [printf "\t\t\t\t; 0x%04X" $ getInstruction i]

loadInt8 :: Int8 -> Instruction
loadInt8 = mkInstruction1 LoadInt8

terminate :: Instruction
terminate = mkInstruction Terminate

nop :: Instruction
nop = mkInstruction Nop

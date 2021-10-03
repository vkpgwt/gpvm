module VM.Instruction
  ( Instruction (..),
    OpCodeName (..),
    opCodeByteOf,
    opCodeName,
    signedArgOf,
    unsignedArgOf,
    loadInt8,
    terminate,
    nop,
    argByteOf,
    setOpCodeByte,
    setArgByte,
  display)
where

import Data.Bits
import Data.Int
import Data.Word
import Text.Printf

data OpCodeName
  = LoadInt8
  | ExtendWord8
  | Terminate
  | NoOp
  | Add
  | Dup
  | Drop
  | Sub
  | Mul
  | Jmp
  | JmpZ
  | JmpNZ
  deriving (Eq, Show, Read, Enum, Bounded)

newtype Instruction = Instruction {getInstruction :: Int16}

mkInstruction1 :: OpCodeName -> Int8 -> Instruction
mkInstruction1 opc arg = Instruction $ shiftL (fromIntegral arg) 8 .|. fromIntegral (fromEnum opc)

mkInstruction :: OpCodeName -> Instruction
mkInstruction opc = mkInstruction1 opc 1

opCodeByteOf :: Instruction -> Word8
opCodeByteOf = fromIntegral . getInstruction

setOpCodeByte :: Word8 -> Instruction -> Instruction
setOpCodeByte b (Instruction i) =
  Instruction . fromIntegral @Word16 @Int16 $
    fromIntegral i .&. 0xff00 .|. fromIntegral @Word8 @Word16 b

opCodeName :: Word8 -> OpCodeName
opCodeName byte
  | value >= fromEnum (minBound @OpCodeName) && value <= fromEnum (maxBound @OpCodeName) = toEnum value
  | otherwise = NoOp
  where
    value = fromIntegral byte

argByteOf :: Instruction -> Word8
argByteOf = fromIntegral . (`unsafeShiftR` 8) . getInstruction

setArgByte :: Word8 -> Instruction -> Instruction
setArgByte b (Instruction i) = Instruction $ (fromIntegral b `unsafeShiftL` 8) .|. (i .&. 0xff)

signedArgOf :: Instruction -> Int
signedArgOf = fromIntegral . (`unsafeShiftR` 8) . getInstruction

unsignedArgOf :: Instruction -> Int
unsignedArgOf = (.&. 0xff) . signedArgOf

display :: Instruction -> Int -> Int -> String
display i addr codeLen = unwords $ [printf "%04d |" addr, show opName] ++ args ++ bytes
  where
    opName = opCodeName $ opCodeByteOf i

    args = case opName of
      LoadInt8 -> [show $ signedArgOf i]
      ExtendWord8 -> [show $ unsignedArgOf i]
      Jmp -> [show $ signedArgOf i, printf "#%04d" jumpTargetAddr]
      JmpZ -> [show $ signedArgOf i, printf "#%04d" jumpTargetAddr]
      JmpNZ -> [show $ signedArgOf i, printf "#%04d" jumpTargetAddr]
      _ -> []

    jumpTargetAddr = (addr + signedArgOf i) `mod` codeLen
    bytes = [printf "\t\t\t\t; 0x%04X" $ getInstruction i]

loadInt8 :: Int8 -> Instruction
loadInt8 = mkInstruction1 LoadInt8

terminate :: Instruction
terminate = mkInstruction Terminate

nop :: Instruction
nop = mkInstruction NoOp

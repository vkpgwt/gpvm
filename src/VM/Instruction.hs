module VM.Instruction
  ( OpCodeName (..),
    W,
    InstructionMetadata (..),
    opCodeMetadata,
    mkInstruction,
    mkInstruction1,
    mkCode,
    decodeOpCode,
    encodeOpCode,
    decodeSignedArg,
    encodeSignedArg,
    decodeUnsignedArg,
    encodeUnsignedArg,
    display,
  )
where

import Data.Bits
import Data.Int
import qualified Data.Vector.Generic as V
import Text.Printf

data OpCodeName
  = LoadInt8'U
  | ExtendWord8'K
  | Terminate'K
  | NoOp'K
  | Add'P
  | Sub'P
  | Mul'P
  | Div'P
  | Mod'P
  | Inc'K
  | Dec'K
  | AndB'P
  | OrB'P
  | XorB'P
  | NotB'K
  | AndL'P
  | OrL'P
  | NotL'K
  | Dup'U
  | Drop'P
  | Swap'K
  | LoadS'U
  | StoreS'P
  | Jmp'K
  | JmpZ'K
  | JmpNZ'K
  | CJGt'PP
  | CJLt'PP
  | CJGe'PP
  | CJLe'PP
  | CJEq'PP
  | CJNe'PP
  deriving (Eq, Show, Read, Enum, Bounded)

type W = Int8

newtype InstructionMetadata = InstructionMetadata
  { -- | "Safe" argument word values to follow the instruction opcode. They are "safe"
    -- so that the instruction does not change PC.
    nonJumpArguments :: [W]
  }

opCodeMetadata :: OpCodeName -> InstructionMetadata
opCodeMetadata x = case x of
  LoadInt8'U -> InstructionMetadata [0]
  ExtendWord8'K -> InstructionMetadata [0]
  Terminate'K -> InstructionMetadata []
  NoOp'K -> InstructionMetadata []
  Add'P -> InstructionMetadata []
  Sub'P -> InstructionMetadata []
  Mul'P -> InstructionMetadata []
  Div'P -> InstructionMetadata []
  Mod'P -> InstructionMetadata []
  Inc'K -> InstructionMetadata []
  Dec'K -> InstructionMetadata []
  AndB'P -> InstructionMetadata []
  OrB'P -> InstructionMetadata []
  XorB'P -> InstructionMetadata []
  NotB'K -> InstructionMetadata []
  AndL'P -> InstructionMetadata []
  OrL'P -> InstructionMetadata []
  NotL'K -> InstructionMetadata []
  Dup'U -> InstructionMetadata []
  Drop'P -> InstructionMetadata []
  Swap'K -> InstructionMetadata []
  LoadS'U -> InstructionMetadata []
  StoreS'P -> InstructionMetadata []
  Jmp'K -> InstructionMetadata [0]
  JmpZ'K -> InstructionMetadata [0]
  JmpNZ'K -> InstructionMetadata [0]
  CJGt'PP -> InstructionMetadata [0]
  CJLt'PP -> InstructionMetadata [0]
  CJGe'PP -> InstructionMetadata [0]
  CJLe'PP -> InstructionMetadata [0]
  CJEq'PP -> InstructionMetadata [0]
  CJNe'PP -> InstructionMetadata [0]

decodeOpCode :: W -> OpCodeName
decodeOpCode byte
  | value >= fromEnum (minBound @OpCodeName) && value <= fromEnum (maxBound @OpCodeName) = toEnum value
  | otherwise = NoOp'K
  where
    value = fromIntegral byte

mkInstruction :: OpCodeName -> [W]
mkInstruction op = [encodeOpCode op]

mkInstruction1 :: OpCodeName -> Int -> [W]
mkInstruction1 op arg = [encodeOpCode op, encodeSignedArg arg]

mkCode :: V.Vector v W => [[W]] -> v W
mkCode = V.fromList . concat

encodeOpCode :: OpCodeName -> W
encodeOpCode = fromIntegral . fromEnum

decodeSignedArg :: W -> Int
decodeSignedArg = fromIntegral

encodeSignedArg :: Int -> W
encodeSignedArg = fromIntegral

decodeUnsignedArg :: W -> Int
decodeUnsignedArg = (.&. 0xFF) . fromIntegral

encodeUnsignedArg :: Word -> W
encodeUnsignedArg = fromIntegral

display :: W -> W -> Int -> Int -> String
display opCode nextWord addr codeLen = printf "%04d | %-30s ; 0x%02X" addr mnemonics unsignedOpCode
  where
    mnemonics = unwords $ show opName : args
    opName = decodeOpCode opCode
    unsignedOpCode = fromIntegral opCode .&. 0xFF :: Word

    args = case opName of
      LoadInt8'U -> [show signedArg]
      ExtendWord8'K -> [show unsignedArg]
      LoadS'U -> [show unsignedArg]
      StoreS'P -> [show unsignedArg]
      Jmp'K -> [show signedArg, jumpTargetAddr]
      JmpZ'K -> [show signedArg, jumpTargetAddr]
      JmpNZ'K -> [show signedArg, jumpTargetAddr]
      CJEq'PP -> [show signedArg, jumpTargetAddr]
      CJNe'PP -> [show signedArg, jumpTargetAddr]
      CJLt'PP -> [show signedArg, jumpTargetAddr]
      CJGt'PP -> [show signedArg, jumpTargetAddr]
      CJGe'PP -> [show signedArg, jumpTargetAddr]
      CJLe'PP -> [show signedArg, jumpTargetAddr]
      _ -> []

    signedArg = decodeSignedArg nextWord
    unsignedArg = decodeUnsignedArg nextWord
    jumpTargetAddr = printf "(#%04d)" $ (addr + signedArg) `mod` codeLen

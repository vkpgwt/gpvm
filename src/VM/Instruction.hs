module VM.Instruction
  ( Instruction (..),
    OpCode (OpCode),
    OpCodeName (..),
    opcode,
    arg,
    loadConst,
    end,
  )
where

import Data.Bits
import Data.Int (Int16, Int8)
import Data.List (intercalate)
import Data.Word

data OpCodeName
  = LoadConst
  | End
  deriving (Eq, Show, Read, Enum, Bounded)

newtype OpCode = OpCodeRaw {getOpCode :: Int}

pattern OpCode :: OpCodeName -> OpCode
pattern OpCode e <- OpCodeRaw (toEnum @OpCodeName -> e)

newtype Instruction = Instruction {getInstruction :: Int16}

mkInstruction1 :: OpCodeName -> Int8 -> Instruction
mkInstruction1 opc arg = Instruction $ shiftL (fromIntegral arg) 8 .|. fromIntegral (fromEnum opc)

mkInstruction :: OpCodeName -> Instruction
mkInstruction opc = mkInstruction1 opc 1

opcode :: Instruction -> OpCode
opcode = OpCodeRaw . fromIntegral . (.&. 0xff) . getInstruction

arg :: Instruction -> Int
arg = fromIntegral . (`unsafeShiftR` 8) . getInstruction

instance Show Instruction where
  show i = unwords $ mnemonic : argComponents
    where
      argComponents = case opcode i of
        OpCode LoadConst -> [show $ arg i]
        _ -> []

      mnemonic = case opcode i of
        OpCodeRaw c
          | c >= fromEnum @OpCodeName minBound && c <= fromEnum @OpCodeName maxBound ->
            show $ toEnum @OpCodeName c
          | otherwise ->
            "Unknown_" ++ show c

loadConst :: Int8 -> Instruction
loadConst = mkInstruction1 LoadConst

end :: Instruction
end = mkInstruction End

module VM.Instruction
  ( Instruction,
    OpCode (..),
    opcode,
    arg,
    spDelta,
    loadConst,
    end,
  )
where

import Data.Int (Int16, Int8)
import Data.List (intercalate)

data OpCode
  = LoadConst
  | End
  deriving (Enum, Show)

data Instruction = Instruction
  { opcode :: !OpCode,
    inArg :: {-# UNPACK #-} !Int16,
    inSpDelta :: {-# UNPACK #-} !Int8
  }

arg :: Instruction -> Int
arg = fromIntegral . inArg

spDelta :: Instruction -> Int
spDelta = fromIntegral . inSpDelta

instance Show Instruction where
  show (Instruction {opcode = opcode, inArg = arg, inSpDelta = spDelta}) =
    intercalate " " $ [opcodeString] ++ argComponents
    where
      argComponents = case opcode of
        LoadConst -> [show arg]
        End -> []
      opcodeString = show opcode ++ spDeltaSuffix
      spDeltaSuffix
        | spDelta == 0 = "K"
        | spDelta == 1 = "F"
        | spDelta == (-1) = "P"
        | spDelta > 1 = "F" ++ show spDelta
        | otherwise = "P" ++ show (negate spDelta)

loadConst :: Int16 -> Instruction
loadConst arg = Instruction LoadConst arg 1

end :: Instruction
end = Instruction End 0 0

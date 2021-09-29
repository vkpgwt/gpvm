module VM.Instruction
  ( Instruction,
    OpCode (..),
    opcode,
    arg,
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
    inArg :: {-# UNPACK #-} !Int16
  }

arg :: Instruction -> Int
arg = fromIntegral . inArg

instance Show Instruction where
  show Instruction {opcode = opcode, inArg = arg} =
    unwords $ show opcode : argComponents
    where
      argComponents = case opcode of
        LoadConst -> [show arg]
        End -> []

loadConst :: Int16 -> Instruction
loadConst = Instruction LoadConst

end :: Instruction
end = Instruction End 0

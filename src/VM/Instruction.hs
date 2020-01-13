module VM.Instruction where
import Data.Int (Int16, Int8)
import Data.List (intercalate)

data OpCode = LoadConst
            | End
            deriving (Enum, Show)

data Instruction = Instruction { opcode  :: !OpCode
                               , arg     :: !Int16
                               , spDelta :: !Int8
                               }

instance Show Instruction where
    show (Instruction { opcode = opcode, arg = arg, spDelta = spDelta })
        = intercalate " " $ [opcodeString] ++ argComponents
        where
            argComponents = case opcode of
                                LoadConst -> [show arg]
                                End       -> []
            opcodeString = show opcode ++ spDeltaSuffix
            spDeltaSuffix
                | spDelta == 0    = "K"
                | spDelta == 1    = "F"
                | spDelta >  1    = "F" ++ show spDelta
                | spDelta == (-1) = "P"
                | spDelta <  (-1) = "P" ++ show (negate spDelta)

loadConst :: Int16 -> Instruction
loadConst arg = Instruction LoadConst arg 1

end = Instruction End 0 0

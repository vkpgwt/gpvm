module VM where
import qualified VM.Instruction as I

type Word = Int

data VM = VM [I.Instruction]
        deriving (Show)

withCode :: [I.Instruction] -> VM
withCode = VM

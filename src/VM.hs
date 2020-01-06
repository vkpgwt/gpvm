module VM where
import qualified VM.Instruction as I

type Word = Int
type Code = [I.Instruction]

newtype VM = VM Code
             deriving (Show)

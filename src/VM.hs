module VM where
import           Data.Array
import qualified VM.Instruction as I
import Data.List

type Word = Int
type Code = Array Int I.Instruction

newtype VM = VM Code
             deriving (Show)

withCode :: [I.Instruction] -> VM
withCode ops = VM $ listArray (0, length ops - 1) ops
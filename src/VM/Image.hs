module VM.Image (
    Image, RunResult,
    withVM, run
    ) where
import           Control.Monad.Trans.State.Strict
import           Data.Array.Unboxed
import           VM
import qualified VM.Instruction as I

type Code = Array Int I.Instruction
type Stack = UArray Int VM.Word

data Image = Image { code  :: !Code
                   , stack :: !Stack
                   , sp    :: !Int
                   , pc    :: !Int
                   } deriving (Show)

data StepResult = StepOk | StepEndInstruction
                  deriving (Show)

data RunResult = RunEnded | RunMaxInstructionsReached
                 deriving (Show)

stackSize :: Int
stackSize = 16

withVM :: VM -> Maybe Image
withVM (VM code)
    | null code = Nothing
    | otherwise = Just Image { code = listArray (0, length code - 1) code
                             , stack = listArray (0, stackSize-1) (repeat 0)
                             , sp = 0
                             , pc = 0
                             }

run :: Int -> Image -> (RunResult, Image)
run maxSteps image = runState (run' maxSteps) image

run' :: Int -> State Image RunResult
run' maxSteps
    | maxSteps <= 0 = (return RunMaxInstructionsReached)
    | otherwise     = step >>= \result ->
          case result of
              StepOk             -> run' (maxSteps - 1)
              StepEndInstruction -> (return RunEnded)

step :: State Image StepResult
step = do
    instr <- currentInstruction
    let opcode = I.opcode instr
        arg = I.arg instr
        spDelta = I.spDelta instr
    case opcode of
        I.LoadConst -> do
            putToStack 0 arg
            incSP spDelta
            incPC 1
            return StepOk
        I.End -> do
            incSP spDelta
            return StepEndInstruction

currentInstruction :: State Image I.Instruction
currentInstruction = do
    Image { code = code, pc = pc } <- get
    return $ code ! pc

putToStack :: Int -> VM.Word -> State Image ()
putToStack relIdx value = modify' $ \image ->
    image { stack = (stack image) // [(stackAbsIndex relIdx image, value)] }

stackAbsIndex :: Int -> Image -> Int
stackAbsIndex relIdx image = (sp image + relIdx) `mod` stackSize

incSP :: Int -> State Image ()
incSP increment = modify' $ \image ->
    image { sp = stackAbsIndex increment image }

incPC :: Int -> State Image ()
incPC increment = modify' $ \image ->
    image { pc = (pc image + increment) `mod` codeLength image }

codeLength :: Image -> Int
codeLength image = (+1) . snd . bounds $ code image

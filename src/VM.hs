module VM (
    VM, RunResult,
    withCode, run
    ) where
import           Control.Monad.Trans.State.Strict
import           Data.Array.Unboxed
import qualified VM.Instruction as I

type Word = Int
type Code = Array Int I.Instruction
type Stack = UArray Int VM.Word

data VM = VM { code  :: !Code
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

withCode :: [I.Instruction] -> Maybe VM
withCode code
    | null code = Nothing
    | otherwise = Just VM { code = listArray (0, length code - 1) code
                          , stack = listArray (0, stackSize-1) (repeat 0)
                          , sp = 0
                          , pc = 0
                          }

run :: Int -> VM -> (RunResult, VM)
run maxSteps vm = runState (run' maxSteps) vm

run' :: Int -> State VM RunResult
run' maxSteps
    | maxSteps <= 0 = (return RunMaxInstructionsReached)
    | otherwise     = step >>= \result ->
          case result of
              StepOk             -> run' (maxSteps - 1)
              StepEndInstruction -> (return RunEnded)

step :: State VM StepResult
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

currentInstruction :: State VM I.Instruction
currentInstruction = do
    VM { code = code, pc = pc } <- get
    return $ code ! pc

putToStack :: Int -> VM.Word -> State VM ()
putToStack relIdx value = modify' $ \vm ->
    vm { stack = (stack vm) // [(stackAbsIndex relIdx vm, value)] }

stackAbsIndex :: Int -> VM -> Int
stackAbsIndex relIdx vm = (sp vm + relIdx) `mod` stackSize

incSP :: Int -> State VM ()
incSP increment = modify' $ \vm ->
    vm { sp = stackAbsIndex increment vm }

incPC :: Int -> State VM ()
incPC increment = modify' $ \vm ->
    vm { pc = (pc vm + increment) `mod` codeLength vm }

codeLength :: VM -> Int
codeLength vm = (+1) . snd . bounds $ code vm

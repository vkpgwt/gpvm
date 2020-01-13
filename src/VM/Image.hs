module VM.Image (Image, withVM, step, run) where
import           Control.Monad.Trans.State.Strict
import           Data.Array.Unboxed
import           Data.Int (Int8)
import           VM
import qualified VM.Instruction as I

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

stackSize = 16

withVM :: VM -> Maybe Image
withVM (VM code)
    | null (assocs code) = Nothing
    | otherwise = Just Image { code = code
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
        arg = fromIntegral (I.arg instr) :: Int
        spDelta = fromIntegral (I.spDelta instr) :: Int
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

codeLength image = (+1) . snd . bounds $ code image

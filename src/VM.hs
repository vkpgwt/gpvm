module VM (
    VM, RunResult,
    withCode, run
    ) where
import           Control.Monad.ST
import           Data.STRef
import           Data.Array
import           Data.Array.ST.Safe
import qualified VM.Instruction as I

type Word = Int

data VM = VM {
    code  :: !(Array Int I.Instruction),
    stack :: !(Array Int VM.Word),
    sp    :: !Int,
    pc    :: !Int
    } deriving (Show)

data MutVM s = MutVM {
    mutCode    :: !(Array Int I.Instruction),
    mutCodeLen :: !Int,
    mutStack   :: !(STUArray s Int VM.Word),
    mutSP      :: !(STRef s Int),
    mutPC      :: !(STRef s Int)
    }

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
run maxSteps vm = runST $ do
    mutVM <- thawVM vm
    result <- runMutVM maxSteps mutVM
    newVM <- freezeVM mutVM
    return (result, newVM)

thawVM :: VM -> ST s (MutVM s)
thawVM vm = do
    mutStack <- thaw $ stack vm
    spRef    <- newSTRef $ sp vm
    pcRef    <- newSTRef $ pc vm
    return MutVM {
        mutCode    = code vm,
        mutCodeLen = (+1) . snd . bounds . code $ vm,
        mutStack   = mutStack,
        mutSP      = spRef,
        mutPC      = pcRef
        }

freezeVM :: MutVM s -> ST s VM
freezeVM mutVM = do
    stack <- freeze $ mutStack mutVM
    sp    <- readSTRef $ mutSP mutVM
    pc    <- readSTRef $ mutPC mutVM
    return VM {
        code = mutCode mutVM,
        stack = stack,
        sp = sp,
        pc = pc
        }

runMutVM :: Int -> MutVM s -> ST s RunResult
runMutVM maxSteps mutVM
    | maxSteps <= 0 = return RunMaxInstructionsReached
    | otherwise = do
          result <- step mutVM
          case result of
              StepOk             -> runMutVM (maxSteps - 1) mutVM
              StepEndInstruction -> return RunEnded

step :: MutVM s -> ST s StepResult
step vm = do
    instr <- currentInstruction vm

    let opcode = I.opcode instr
        arg = I.arg instr
        spDelta = I.spDelta instr

    case opcode of
        I.LoadConst -> do
            putToStack 0 arg vm
            incSP spDelta vm
            incPC 1 vm
            return StepOk
        I.End -> do
            incSP spDelta vm
            return StepEndInstruction

currentInstruction :: MutVM s -> ST s I.Instruction
currentInstruction vm = do
    pc <- readSTRef $ mutPC vm
    return $ mutCode vm ! pc

putToStack :: Int -> VM.Word -> MutVM s -> ST s ()
putToStack relIdx value vm = do
    sp <- readSTRef $ mutSP vm
    writeArray (mutStack vm) (stackAbsIndex relIdx sp) value

stackAbsIndex :: Int -> Int -> Int
stackAbsIndex relIdx sp = (sp + relIdx) `mod` stackSize

incSP :: Int -> MutVM s -> ST s ()
incSP increment vm = modifySTRef' (mutSP vm) (stackAbsIndex increment)

incPC :: Int -> MutVM s -> ST s ()
incPC increment vm = modifySTRef' (mutPC vm) transform
    where transform pc
              | increment >= 0 && increment <= codeLen =
                if increment + pc >= codeLen
                then increment + pc - codeLen
                else increment + pc
              | otherwise = (increment + pc) `mod` codeLen
          codeLen = mutCodeLen vm

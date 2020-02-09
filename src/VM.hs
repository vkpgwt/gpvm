module VM (
    VM, RunResult,
    withCode, run
    ) where
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Unboxed.Mutable (MVector)
import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified VM.Instruction as I

type Word = Int

data VM = VM {
    code  :: ![I.Instruction],
    stack :: ![VM.Word],
    sp    :: !Int,
    pc    :: !Int
    } deriving (Show)

data MutVM s = MutVM {
    mutCode    :: {-# UNPACK #-} !(Vector I.Instruction),
    mutCodeLen :: {-# UNPACK #-} !Int,
    mutStack   :: {-# UNPACK #-} !(MVector s VM.Word),
    mutSP      :: {-# UNPACK #-} !(STRef s Int),
    mutPC      :: {-# UNPACK #-} !(STRef s Int)
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
    | otherwise = Just VM { code = code
                          , stack = replicate stackSize 0
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
    mutStack <- V.thaw . V.fromList $ stack vm
    spRef    <- newSTRef $ sp vm
    pcRef    <- newSTRef $ pc vm
    return MutVM {
        mutCode    = V.fromList $ code vm,
        mutCodeLen = length $ code vm,
        mutStack   = mutStack,
        mutSP      = spRef,
        mutPC      = pcRef
        }

freezeVM :: MutVM s -> ST s VM
freezeVM mutVM = do
    stack <- V.freeze $ mutStack mutVM
    sp    <- readSTRef $ mutSP mutVM
    pc    <- readSTRef $ mutPC mutVM
    return VM {
        code = V.toList $ mutCode mutVM,
        stack = V.toList stack,
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
    return $ mutCode vm `V.unsafeIndex` pc

putToStack :: Int -> VM.Word -> MutVM s -> ST s ()
putToStack relIdx value vm = do
    sp <- readSTRef $ mutSP vm
    MV.unsafeWrite (mutStack vm) (stackAbsIndex relIdx sp) value

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

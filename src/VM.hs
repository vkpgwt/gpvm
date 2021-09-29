{-# LANGUAGE RankNTypes #-}

module VM
  ( VM,
    RunResult,
    withCode,
    run,
  )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector.Unboxed.Mutable (MVector)
import qualified VM.Instruction as I

type Word = Int

data VM = VM
  { code :: ![I.Instruction],
    stack :: ![VM.Word],
    sp :: !Int,
    pc :: !Int
  }
  deriving (Show)

type Run s a = (ReaderT (ROData s) (StateT MutData (ST s)) a)

data ROData s = ROData
  { roCode :: {-# UNPACK #-} !(Vector I.Instruction),
    roCodeLen :: {-# UNPACK #-} !Int,
    roStack :: {-# UNPACK #-} !(MVector s VM.Word)
  }

data MutData = MutData
  { mutSP :: {-# UNPACK #-} !Int,
    mutPC :: {-# UNPACK #-} !Int
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
  | otherwise =
    Just
      VM
        { code = code,
          stack = replicate stackSize 0,
          sp = 0,
          pc = 0
        }

run :: Int -> VM -> (RunResult, VM)
run maxSteps vm = withVM vm $ do
  result <- runMutVM maxSteps
  vm' <- freezeVM
  return (result, vm')

withVM :: VM -> (forall s. Run s a) -> a
withVM vm action = runST $ do
  stack <- V.thaw . V.fromList $ stack vm
  let roData =
        ROData
          { roCode = V.fromList $ code vm,
            roCodeLen = length $ code vm,
            roStack = stack
          }
      mutData =
        MutData
          { mutSP = sp vm,
            mutPC = pc vm
          }
  evalStateT (runReaderT action roData) mutData

freezeVM :: Run s VM
freezeVM = do
  roData <- ask
  mutData <- get
  stack <- V.freeze $ roStack roData
  return
    VM
      { code = V.toList $ roCode roData,
        stack = V.toList stack,
        sp = mutSP mutData,
        pc = mutPC mutData
      }

runMutVM :: Int -> Run s RunResult
runMutVM maxSteps
  | maxSteps <= 0 = return RunMaxInstructionsReached
  | otherwise = do
    result <- step
    case result of
      StepOk -> runMutVM (maxSteps - 1)
      StepEndInstruction -> return RunEnded

step :: Run s StepResult
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

currentInstruction :: Run s I.Instruction
currentInstruction = liftA2 V.unsafeIndex (asks roCode) (gets mutPC)

putToStack :: Int -> VM.Word -> Run s ()
putToStack relIdx value = do
  sp <- gets mutSP
  stack <- asks roStack
  MV.unsafeWrite stack (stackAbsIndex relIdx sp) value

stackAbsIndex :: Int -> Int -> Int
stackAbsIndex relIdx sp = (sp + relIdx) `mod` stackSize

incSP :: Int -> Run s ()
incSP increment = modify' $ \s -> s {mutSP = stackAbsIndex increment (mutSP s)}

incPC :: Int -> Run s ()
incPC increment = do
  codeLen <- asks roCodeLen
  modify' $ \s -> s {mutPC = pureIncPC increment codeLen $ mutPC s}

pureIncPC :: Int -> Int -> Int -> Int
pureIncPC increment codeLen pc
  | increment >= 0 && increment <= codeLen =
    if increment + pc >= codeLen
      then increment + pc - codeLen
      else increment + pc
  | otherwise = (increment + pc) `mod` codeLen

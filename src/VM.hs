{-# LANGUAGE RankNTypes #-}

module VM
  ( VM (..),
    Snapshot,
    RunResult,
    mkSnapshot,
    run,
  )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.Int
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Mutable (MVector)
import Records
import qualified VM.Instruction as I

-- | The machine word
type W = Int

data VM = VM
  { code :: ![I.Instruction],
    stackSize :: !Int
  }

data Snapshot = Snapshot
  { code :: ![I.Instruction],
    stack :: ![W],
    sp :: !Int,
    pc :: !Int
  }
  deriving (Show)

type Run s a = ReaderT (ROData s) (StateT MutData (ST s)) a

data ROData s = ROData
  { roCode :: {-# UNPACK #-} !(UV.Vector Int16),
    roCodeLen :: {-# UNPACK #-} !Int,
    roStack :: {-# UNPACK #-} !(MVector s W)
  }

data MutData = MutData
  { mutSP :: {-# UNPACK #-} !Int,
    mutPC :: {-# UNPACK #-} !Int
  }

data StepResult = StepOk | StepEndInstruction
  deriving (Show)

data RunResult = RunEnded | RunMaxInstructionsReached
  deriving (Show)

mkSnapshot :: VM -> Maybe Snapshot
mkSnapshot vm
  | null $ vm ^. #code = Nothing
  | otherwise =
    Just
      Snapshot
        { code = vm ^. #code,
          stack = replicate (vm ^. #stackSize) 0,
          sp = 0,
          pc = 0
        }

run :: Int -> Snapshot -> (RunResult, Snapshot)
run maxSteps vm = withMutVM vm $ do
  result <- runMut maxSteps
  vm' <- freezeVM
  return (result, vm')

withMutVM :: Snapshot -> (forall s. Run s a) -> a
withMutVM vm action = runST $ do
  stack <- V.thaw . V.fromList $ stack vm
  let roData =
        ROData
          { roCode = V.fromList . map I.getInstruction $ vm ^. #code,
            roCodeLen = length $ vm ^. #code,
            roStack = stack
          }
      mutData =
        MutData
          { mutSP = sp vm,
            mutPC = pc vm
          }
  evalStateT (runReaderT action roData) mutData

freezeVM :: Run s Snapshot
freezeVM = do
  roData <- ask
  mutData <- get
  stack <- V.freeze $ roStack roData
  return
    Snapshot
      { code = map I.Instruction . V.toList $ roCode roData,
        stack = V.toList stack,
        sp = mutSP mutData,
        pc = mutPC mutData
      }

runMut :: Int -> Run s RunResult
runMut maxSteps
  | maxSteps <= 0 = return RunMaxInstructionsReached
  | otherwise = do
    result <- step
    case result of
      StepOk -> runMut (maxSteps - 1)
      StepEndInstruction -> return RunEnded

step :: Run s StepResult
step = do
  instr <- currentInstruction

  let opcode = I.opcode instr
      arg = I.arg instr

  case opcode of
    I.OpCode I.LoadConst -> do
      putToStack 0 arg
      incSP 1
      incPC 1
      return StepOk
    I.OpCode I.End ->
      return StepEndInstruction
    _ ->
      pure StepOk

currentInstruction :: Run s I.Instruction
currentInstruction = asks ((I.Instruction .) . V.unsafeIndex . roCode) <*> gets mutPC

putToStack :: Int -> W -> Run s ()
putToStack relIdx value = do
  sp <- gets mutSP
  stack <- asks roStack
  addr <- getStackAddr relIdx
  MV.unsafeWrite stack addr value

-- NB: may be slow
getStackAddr :: Int -> Run s Int
getStackAddr relIdx = do
  len <- asks (MV.length . roStack)
  sp <- gets mutSP
  pure $ (sp + relIdx) `mod` len

incSP :: Int -> Run s ()
incSP increment = do
  newValue <- getStackAddr increment
  modify' $ \s -> s {mutSP = newValue}

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

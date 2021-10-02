{-# LANGUAGE RankNTypes #-}

module VM
  ( VM (..),
    Snapshot (..),
    RunResult (..),
    W,
    mkSnapshot,
    snapshotRelativeStack,
    run,
  )
where

import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.Int
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Mutable (MVector)
import Records
import qualified VM.Instruction as I

-- | The machine data word
type W = Int

data VM = VM
  { code :: !(BV.Vector I.Instruction),
    stackSize :: !Int
  }

instance Show VM where
  show VM {..} =
    unlines $
      [ "{ VM",
        "    stackSize = " ++ show stackSize,
        "    codeSize = " ++ show (V.length code),
        "    code = ["
      ]
        ++ map (("        " ++) . show) (V.toList code)
        ++ ["    ]", "}"]

data Snapshot = Snapshot
  { code :: !(BV.Vector I.Instruction),
    stack :: !(UV.Vector W),
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
          stack = V.replicate (vm ^. #stackSize) 0,
          sp = 0,
          pc = 0
        }

-- | Returns the stack of a snapshot as a list where i-th element corresponds to the stack element at address SP-i
snapshotRelativeStack :: Snapshot -> [W]
snapshotRelativeStack Snapshot {stack, sp} =
  let list = V.toList stack
   in reverse (take (sp + 1) list) ++ reverse (drop (sp + 1) list)

run :: Int -> Snapshot -> (RunResult, Snapshot)
run maxSteps vm = withMutVM vm $ do
  result <- runMut maxSteps
  vm' <- freezeVM
  return (result, vm')

withMutVM :: Snapshot -> (forall s. Run s a) -> a
withMutVM vm action = runST $ do
  stack <- V.thaw $ stack vm
  let roData =
        ROData
          { roCode = V.convert . fmap I.getInstruction $ vm ^. #code,
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
      { code = fmap I.Instruction . V.convert $ roCode roData,
        stack = stack,
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
  incPC 1

  let opcode = I.opCodeName $ I.opcode instr
      arg = I.argOf instr

  case opcode of
    I.LoadConst -> do
      incSP 1
      setStackW 0 arg
      pure StepOk
    I.End ->
      pure StepEndInstruction
    I.Unknown ->
      pure StepOk
    I.Add -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (a +)
      pure StepOk
    I.Dup -> do
      incSP 1
      x <- getStackW (-1)
      setStackW 0 x
      pure StepOk
    I.Drop -> do
      incSP (-1)
      pure StepOk
    I.Sub -> do
      incSP (-1)
      s <- getStackW 1
      updateStackW 0 (subtract s)
      pure StepOk
    I.Mul -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (a *)
      pure StepOk

currentInstruction :: Run s I.Instruction
currentInstruction = asks ((I.Instruction .) . V.unsafeIndex . roCode) <*> gets mutPC

getStackW :: Int -> Run s W
getStackW relIdx = do
  stack <- asks roStack
  addr <- getStackAddr relIdx
  MV.unsafeRead stack addr

setStackW :: Int -> W -> Run s ()
setStackW relIdx value = do
  stack <- asks roStack
  addr <- getStackAddr relIdx
  MV.unsafeWrite stack addr value

updateStackW :: Int -> (W -> W) -> Run s ()
updateStackW relIdx update = do
  stack <- asks roStack
  addr <- getStackAddr relIdx
  MV.unsafeModify stack update addr

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

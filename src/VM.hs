{-# LANGUAGE RankNTypes #-}

module VM
  ( Snapshot (..),
    ResultingSnapshot (..),
    RunResult (..),
    SnapshotError (..),
    W,
    run,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import GHC.Types (SPEC (..))
import Records
import qualified VM.Instruction as I

-- | The machine data word
type W = Int

data Snapshot = Snapshot
  { code :: !(SV.Vector I.Instruction),
    stack :: !(UV.Vector W),
    sp :: !Int,
    pc :: !Int
  }

newtype ResultingSnapshot = ResultingSnapshot {stackTop :: W}
  deriving (Show)

type Run s a = ReaderT (ROData s) (StateT MutData (ST s)) a

data ROData s = ROData
  { roCode :: {-# UNPACK #-} !(SV.Vector I.Instruction),
    roStack :: {-# UNPACK #-} !(UMV.MVector s W)
  }

data MutData = MutData
  { mutSP :: {-# UNPACK #-} !Int,
    mutPC :: {-# UNPACK #-} !Int
  }

data StepResult = StepOk | StepEndInstruction
  deriving (Show)

data RunResult
  = RunTerminated
  | RunMaxInstructionsReached
  deriving (Eq, Show)

data SnapshotError
  = InvalidCodeSize
  | InvalidStackSize
  deriving (Eq, Show)

-- {-# SCC run #-}

run :: Int -> Snapshot -> Either SnapshotError (RunResult, ResultingSnapshot)
run maxSteps vm = withMutVM vm $ do
  result <- runMut SPEC maxSteps
  topWord <- getStackTop
  return (result, ResultingSnapshot topWord)

checkCodeLen :: Snapshot -> Either SnapshotError ()
checkCodeLen vm
  | isPowerOf2 $ V.length $ vm ^. #code = Right ()
  | otherwise = Left InvalidCodeSize

checkStackLen :: Snapshot -> Either SnapshotError ()
checkStackLen vm
  | isPowerOf2 $ V.length $ vm ^. #stack = Right ()
  | otherwise = Left InvalidStackSize

withMutVM :: Snapshot -> (forall s. Run s a) -> Either SnapshotError a
withMutVM vm action = runExcept $ do
  ExceptT $ pure $ checkCodeLen vm
  ExceptT $ pure $ checkStackLen vm
  pure $
    runST $ do
      stack <- V.thaw $ stack vm
      let roData =
            ROData
              { roCode = vm ^. #code,
                roStack = stack
              }
          mutData =
            MutData
              { mutSP = sp vm,
                mutPC = pc vm
              }
      evalStateT (runReaderT action roData) mutData

runMut :: SPEC -> Int -> Run s RunResult
runMut !_ maxSteps
  | maxSteps <= 0 = return RunMaxInstructionsReached
  | otherwise = do
    result <- step
    case result of
      StepOk -> runMut SPEC (maxSteps - 1)
      StepEndInstruction -> return RunTerminated

step :: Run s StepResult
step = do
  instr <- currentInstruction
  incPC 1

  let opcode = I.opCodeName $ I.opCodeByteOf instr
      signedArg = I.signedArgOf instr
      unsignedArg = I.unsignedArgOf instr

  case opcode of
    I.LoadInt8'U -> do
      incSP 1
      setStackTop signedArg
      pure StepOk
    I.ExtendWord8'K -> unaryOp'K $ (unsignedArg +) . (`unsafeShiftL` 8)
    I.Terminate'K -> pure StepEndInstruction
    I.NoOp'K -> pure StepOk
    I.Add'P -> binaryOp'P (+)
    I.Inc'K -> unaryOp'K (1 +)
    I.Dec'K -> unaryOp'K (subtract 1)
    I.NotB'K -> unaryOp'K complement
    I.NotL'K -> unaryOp'K $ boolToW . not . wToBool
    I.AndB'P -> binaryOp'P (.&.)
    I.OrB'P -> binaryOp'P (.|.)
    I.XorB'P -> binaryOp'P xor
    I.AndL'P -> binaryOp'P $ \x y -> boolToW $ wToBool x && wToBool y
    I.OrL'P -> binaryOp'P $ \x y -> boolToW $ wToBool x || wToBool y
    I.Dup'U -> do
      x <- getStackTop
      incSP 1
      setStackTop x
      pure StepOk
    I.Drop'P -> do
      incSP (-1)
      pure StepOk
    I.Swap'K -> do
      a <- getStackTop
      b <- getStackW (-1)
      setStackW (-1) a
      setStackTop b
      pure StepOk
    I.LoadS'U -> do
      w <- getStackW (negate signedArg)
      setStackW 1 w
      incSP 1
      pure StepOk
    I.StoreS'P -> do
      w <- getStackTop
      setStackW (negate signedArg) w
      incSP (-1)
      pure StepOk
    I.Sub'P -> binaryOp'P (-)
    I.Mul'P -> binaryOp'P (*)
    I.Div'P -> binaryOp'P $ \x y -> if y == 0 then 0 else x `div` y
    I.Mod'P -> binaryOp'P $ \x y -> if y == 0 then 0 else x `mod` y
    I.Jmp'K -> do
      incPC $ signedArg - 1
      pure StepOk
    I.JmpZ'K -> do
      top <- getStackTop
      when (top == 0) $ incPC $ signedArg - 1
      pure StepOk
    I.JmpNZ'K -> do
      top <- getStackTop
      when (top /= 0) $ incPC $ signedArg - 1
      pure StepOk
    I.CJEq'PP -> compareAndJumpIf'PP (==) signedArg
    I.CJNe'PP -> compareAndJumpIf'PP (/=) signedArg
    I.CJGt'PP -> compareAndJumpIf'PP (>) signedArg
    I.CJLt'PP -> compareAndJumpIf'PP (<) signedArg
    I.CJGe'PP -> compareAndJumpIf'PP (>=) signedArg
    I.CJLe'PP -> compareAndJumpIf'PP (<=) signedArg
  where
    unaryOp'K op = do
      updateStackTop op
      pure StepOk

    binaryOp'P op = do
      y <- getStackTop
      updateStackW (-1) (`op` y)
      incSP (-1)
      pure StepOk

    compareAndJumpIf'PP cond pcRelativeOffset = do
      a <- getStackTop
      b <- getStackW (-1)
      incSP (-2)
      when (a `cond` b) $ incPC $ pcRelativeOffset - 1
      pure StepOk

    boolToW True = 1
    boolToW False = 0
    wToBool x = x /= 0

currentInstruction :: Run s I.Instruction
currentInstruction = asks (V.unsafeIndex . roCode) <*> gets mutPC

getStackW :: Int -> Run s W
getStackW relIdx = do
  stack <- asks roStack
  addr <- getStackAddr relIdx
  MV.unsafeRead stack addr

getStackTop :: Run s W
getStackTop = do
  stack <- asks roStack
  addr <- gets mutSP
  MV.unsafeRead stack addr

setStackW :: Int -> W -> Run s ()
setStackW relIdx value = do
  stack <- asks roStack
  addr <- getStackAddr relIdx
  MV.unsafeWrite stack addr value

setStackTop :: W -> Run s ()
setStackTop value = do
  stack <- asks roStack
  addr <- gets mutSP
  MV.unsafeWrite stack addr value

updateStackW :: Int -> (W -> W) -> Run s ()
updateStackW relIdx update = do
  stack <- asks roStack
  addr <- getStackAddr relIdx
  MV.unsafeModify stack update addr

updateStackTop :: (W -> W) -> Run s ()
updateStackTop update = do
  stack <- asks roStack
  addr <- gets mutSP
  MV.unsafeModify stack update addr

-- NB: may be slow
getStackAddr :: Int -> Run s Int
getStackAddr relIdx = do
  len <- asks (MV.length . roStack)
  sp <- gets mutSP
  pure $ (sp + relIdx) `modPowerOf2` len

incSP :: Int -> Run s ()
incSP increment = do
  newValue <- getStackAddr increment
  modify' $ \s -> s {mutSP = newValue}

incPC :: Int -> Run s ()
incPC increment = do
  codeLen <- asks (V.length . roCode)
  modify' $ \s -> s {mutPC = (increment + mutPC s) `modPowerOf2` codeLen}

isPowerOf2 :: (Ord a, Num a, Bits a) => a -> Bool
isPowerOf2 x = x > 0 && x .&. (x - 1) == 0

modPowerOf2 :: (Bits a, Num a) => a -> a -> a
modPowerOf2 a b = a .&. (b - 1)

{-# LANGUAGE RankNTypes #-}

module VM
  ( Snapshot (..),
    RunResult (..),
    SnapshotError (..),
    W,
    snapshotRelativeStack,
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
import Data.Vector.Unboxed.Mutable (MVector)
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

type Run s a = ReaderT (ROData s) (StateT MutData (ST s)) a

data ROData s = ROData
  { roCode :: {-# UNPACK #-} !(SV.Vector I.Instruction),
    roStack :: {-# UNPACK #-} !(MVector s W),
    roCodeLen :: {-# UNPACK #-} !(PowerOf2 Int),
    roStackLen :: {-# UNPACK #-} !(PowerOf2 Int)
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

-- | Returns the stack of a snapshot as a list where i-th element corresponds to the stack element at address SP-i
snapshotRelativeStack :: Snapshot -> [W]
snapshotRelativeStack Snapshot {stack, sp} =
  let list = V.toList stack
   in reverse (take (sp + 1) list) ++ reverse (drop (sp + 1) list)

run :: Int -> Snapshot -> Either SnapshotError (RunResult, Snapshot)
run maxSteps vm = withMutVM vm $ do
  result <- runMut maxSteps
  vm' <- freezeVM
  return (result, vm')

checkCodeLen :: Snapshot -> Either SnapshotError (PowerOf2 Int)
checkCodeLen vm = maybe (Left InvalidCodeSize) Right $ toPowerOf2 $ V.length $ vm ^. #code

checkStackLen :: Snapshot -> Either SnapshotError (PowerOf2 Int)
checkStackLen vm = maybe (Left InvalidStackSize) Right $ toPowerOf2 $ V.length $ vm ^. #stack

withMutVM :: Snapshot -> (forall s. Run s a) -> Either SnapshotError a
withMutVM vm action = runExcept $ do
  codeLen <- ExceptT $ pure $ checkCodeLen vm
  stackLen <- ExceptT $ pure $ checkStackLen vm
  pure $
    runST $ do
      stack <- V.thaw $ stack vm
      let roData =
            ROData
              { roCode = vm ^. #code,
                roStack = stack,
                roCodeLen = codeLen,
                roStackLen = stackLen
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
      { code = roCode roData,
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
      setStackW 0 signedArg
      pure StepOk
    I.ExtendWord8'K -> do
      updateStackW 0 ((unsignedArg +) . (`unsafeShiftL` 8))
      pure StepOk
    I.Terminate'K ->
      pure StepEndInstruction
    I.NoOp'K ->
      pure StepOk
    I.Add'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (a +)
      pure StepOk
    I.Inc'K -> do
      updateStackW 0 (1 +)
      pure StepOk
    I.Dec'K -> do
      updateStackW 0 (subtract 1)
      pure StepOk
    I.NotB'K -> do
      updateStackW 0 complement
      pure StepOk
    I.NotL'K -> do
      updateStackW 0 (boolToW . not . wToBool)
      pure StepOk
    I.AndB'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (a .&.)
      pure StepOk
    I.OrB'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (a .|.)
      pure StepOk
    I.XorB'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (a `xor`)
      pure StepOk
    I.AndL'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (\x -> boolToW $ wToBool a && wToBool x)
      pure StepOk
    I.OrL'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (\x -> boolToW $ wToBool a || wToBool x)
      pure StepOk
    I.Dup'U -> do
      incSP 1
      x <- getStackW (-1)
      setStackW 0 x
      pure StepOk
    I.Drop'P -> do
      incSP (-1)
      pure StepOk
    I.Swap'K -> do
      a <- getStackW 0
      b <- getStackW (-1)
      setStackW (-1) a
      setStackW 0 b
      pure StepOk
    I.LoadS'U -> do
      w <- getStackW (negate signedArg)
      setStackW 1 w
      incSP 1
      pure StepOk
    I.StoreS'P -> do
      w <- getStackW 0
      setStackW (negate signedArg) w
      incSP (-1)
      pure StepOk
    I.Sub'P -> do
      incSP (-1)
      s <- getStackW 1
      updateStackW 0 (subtract s)
      pure StepOk
    I.Mul'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (a *)
      pure StepOk
    I.Div'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (\x -> if a == 0 then 0 else x `div` a)
      pure StepOk
    I.Mod'P -> do
      incSP (-1)
      a <- getStackW 1
      updateStackW 0 (\x -> if a == 0 then 0 else x `mod` a)
      pure StepOk
    I.Jmp'K -> do
      incPC $ signedArg - 1
      pure StepOk
    I.JmpZ'K -> do
      top <- getStackW 0
      when (top == 0) $ incPC $ signedArg - 1
      pure StepOk
    I.JmpNZ'K -> do
      top <- getStackW 0
      when (top /= 0) $ incPC $ signedArg - 1
      pure StepOk
    I.CJEq'PP -> do
      incSP (-2)
      a <- getStackW 1
      b <- getStackW 2
      when (a == b) $ incPC $ signedArg - 1
      pure StepOk
    I.CJNe'PP -> do
      incSP (-2)
      a <- getStackW 1
      b <- getStackW 2
      when (a /= b) $ incPC $ signedArg - 1
      pure StepOk
    I.CJGt'PP -> do
      incSP (-2)
      a <- getStackW 1
      b <- getStackW 2
      when (a > b) $ incPC $ signedArg - 1
      pure StepOk
    I.CJLt'PP -> do
      incSP (-2)
      a <- getStackW 1
      b <- getStackW 2
      when (a < b) $ incPC $ signedArg - 1
      pure StepOk
    I.CJGe'PP -> do
      incSP (-2)
      a <- getStackW 1
      b <- getStackW 2
      when (a >= b) $ incPC $ signedArg - 1
      pure StepOk
    I.CJLe'PP -> do
      incSP (-2)
      a <- getStackW 1
      b <- getStackW 2
      when (a <= b) $ incPC $ signedArg - 1
      pure StepOk
  where
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
  len <- asks roStackLen
  sp <- gets mutSP
  pure $ (sp + relIdx) `modPowerOf2` len

incSP :: Int -> Run s ()
incSP increment = do
  newValue <- getStackAddr increment
  modify' $ \s -> s {mutSP = newValue}

incPC :: Int -> Run s ()
incPC increment = do
  codeLen <- asks roCodeLen
  modify' $ \s -> s {mutPC = (increment + mutPC s) `modPowerOf2` codeLen}

newtype PowerOf2 a = PowerOf2 a

toPowerOf2 :: (Ord a, Num a, Bits a) => a -> Maybe (PowerOf2 a)
toPowerOf2 x
  | x > 0 && x .&. (x - 1) == 0 = Just $ PowerOf2 x
  | otherwise = Nothing

modPowerOf2 :: (Bits a, Num a) => a -> PowerOf2 a -> a
modPowerOf2 a (PowerOf2 b) = a .&. (b - 1)

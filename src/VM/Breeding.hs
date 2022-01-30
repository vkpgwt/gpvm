module VM.Breeding
  ( selectionEngineHandle,
    VM (..),
  )
where

import Control.Monad
import Data.Bits
import Data.Foldable
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as V
import Data.Word
import GHC.Base (NonEmpty ((:|)))
import Records
import SelectionEngine (Fitness, Handle (..))
import System.Random.Stateful
import qualified VM
import qualified VM.Instruction as I

selectionEngineHandle :: Handle VM
selectionEngineHandle =
  Handle
    { reproduce = mutate,
      display = show,
      fitnessOf = \vm ->
        let details = detailedFitnessOf vm
         in (toNumericFitness details, show $ toList details)
    }

data VM = VM
  { code :: !(BV.Vector I.Instruction),
    stackSize :: !Int
  }

instance Show VM where
  show VM {..} =
    unlines $
      [ "{ VM",
        "\tstackSize = " ++ show stackSize,
        "\tcodeSize = " ++ show (V.length code),
        "\tcode = ["
      ]
        ++ (map (\(idx, inst) -> "\t\t" ++ I.display inst idx (V.length code)) . V.toList . V.indexed $ code)
        ++ ["\t]", "}"]

mkSnapshot :: VM -> VM.Snapshot
mkSnapshot vm =
  VM.Snapshot
    { code = vm ^. #code,
      stack = V.replicate (vm ^. #stackSize) 0,
      sp = 0,
      pc = 0
    }

mutate :: RandomGenM g r m => VM -> g -> m VM
mutate vm@VM {..} gen = do
  code1 <- duplicateCodeSlicesRandomly code gen
  code2 <- mutateInstructions code1 gen
  pure $ (vm :: VM) {code = code2}

data SingleIntructionMutation
  = MutateIBit !Int -- bitNo
  | MutateIByte !Bool !Word8 -- byteNo, newByte

mutateInstructions :: RandomGenM g r m => BV.Vector I.Instruction -> g -> m (BV.Vector I.Instruction)
mutateInstructions code gen = do
  let maxNumErrors = max 1 $ length code `div` oneErrorPerThisManyInstructions
  numErrors <- randomRM (0, maxNumErrors) gen
  mutations <- replicateM numErrors $ do
    iAddr <- randomRM (0, length code - 1) gen
    bitErrorNotRandomByte <- randomM gen
    mutation <-
      if bitErrorNotRandomByte
        then MutateIBit <$> randomRM (0, finiteBitSize (I.getInstruction undefined) - 1) gen
        else do
          MutateIByte <$> randomM gen <*> randomM gen
    pure (iAddr, mutation)
  pure $ V.accum mutateInstruction code mutations
  where
    mutateInstruction (I.Instruction i) (MutateIBit bitNo) = I.Instruction $ complementBit i bitNo
    mutateInstruction inst (MutateIByte opCodeNotArg newByte)
      | opCodeNotArg = I.setOpCodeByte newByte inst
      | otherwise = I.setArgByte newByte inst

duplicateCodeSlicesRandomly :: RandomGenM g r m => BV.Vector a -> g -> m (BV.Vector a)
duplicateCodeSlicesRandomly code gen
  | V.length code >= maxCodeSize = pure code
  | otherwise = do
    p <- randomRM (0, 99) gen
    if p < duplicationPercent
      then pure $ V.take maxCodeSize $ code <> code
      else pure code

toNumericFitness :: NonEmpty Fitness -> Fitness
toNumericFitness = average

detailedFitnessOf :: VM -> NonEmpty Fitness
detailedFitnessOf vm = fmap fit testData
  where
    testData = (0, 0) :| [(2, 1), (13, 3), (89, 9), (610, 24), (4181, 64), (28657, 169)]

    fit (x, expected) = case vmUnaryFunction (mkSnapshot vm) x of
      Left _ -> (-1e10)
      Right (runResult, vmR) -> fitnessForResult expected vmR runResult

-- todo: сделать стек растущим вниз, чтобы адресация относительно верхушки шла вверх

vmUnaryFunction :: VM.Snapshot -> VM.W -> Either VM.SnapshotError (VM.RunResult, VM.W)
vmUnaryFunction snapshot x = case VM.run maxExecutionSteps startState of
  Left err -> Left err
  Right (runResult, endState) ->
    let (y : _) = VM.snapshotRelativeStack endState
     in Right (runResult, y)
  where
    startState = snapshot {VM.stack = x `V.cons` V.drop 1 (snapshot ^. #stack)}

average :: (Fractional a) => NonEmpty a -> a
average (x0 :| xs) = total / count
  where
    (total, count) = foldl' f (x0, 1) xs
    f (!s, !c) x = (s + x, c + 1)

-- averageSqS :: (Fractional a) => NonEmpty a -> a
-- averageSqS = average . fmap (\a -> a * a * signum a)

fitnessForResult :: VM.W -> VM.W -> VM.RunResult -> Fitness
fitnessForResult expected actual runResult = negate (errorPenalty + nonTerminationPenalty)
  where
    errorPenalty = abs $ realToFrac expected - realToFrac actual
    nonTerminationPenalty = case runResult of
      VM.RunTerminated -> 0
      VM.RunMaxInstructionsReached -> 1

oneErrorPerThisManyInstructions :: Int
oneErrorPerThisManyInstructions = 3

duplicationPercent :: Int
duplicationPercent = 2 :: Int

maxCodeSize :: Int
maxCodeSize = 512

maxExecutionSteps :: Int
maxExecutionSteps = 512

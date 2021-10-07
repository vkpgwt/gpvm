module VM.Breeding
  ( mkSelectable,
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
import Selectable
import System.Random.Stateful
import VM (VM (..))
import qualified VM
import qualified VM.Instruction as I

mkSelectable :: VM -> Selectable
mkSelectable vm =
  let detailedFitness = detailedFitnessOf vm
   in Selectable
        { breed = fmap mkSelectable . mutate vm,
          fitness = toNumericFitness detailedFitness,
          display = show vm,
          fitnessDetails = show $ toList detailedFitness
        }

mutate :: RandomGenM g r m => VM -> g -> m VM
mutate vm@VM {..} gen = do
  code1 <- duplicateCodeSlicesRandomly code gen
  code2 <- mutateInstructions code1 gen
  pure $ vm {code = code2}

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
detailedFitnessOf vm = withSnapshot $ \defaultSnapshot ->
  fmap (fit defaultSnapshot) testData
  where
    testData = (0, 0) :| [(2, 1), (3, 1), (10, 3), (16, 4), (39, 6), (68, 8), (111, 10)]

    fit defaultSnapshot (x, expected) =
      let (runResult, vmR) = vmUnaryFunction defaultSnapshot x
       in fitnessForResult expected vmR runResult

    withSnapshot cont =
      maybe ((- 1e10) :| []) cont (VM.mkSnapshot vm)

-- сделать стек растущим вниз, чтобы адресация относительно верхушки шла вверх

vmUnaryFunction :: VM.Snapshot -> VM.W -> (VM.RunResult, VM.W)
vmUnaryFunction defaultSnapshot x =
  let startState =
        defaultSnapshot {VM.stack = x `V.cons` V.drop 1 (defaultSnapshot ^. #stack)}
      (result, endState) = VM.run maxExecutionSteps startState
      (y : _) = VM.snapshotRelativeStack endState
   in (result, y)

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
      VM.RunEnded -> 0
      VM.RunMaxInstructionsReached -> 0.1

oneErrorPerThisManyInstructions :: Int
oneErrorPerThisManyInstructions = 3

duplicationPercent :: Int
duplicationPercent = 2 :: Int

maxCodeSize :: Int
maxCodeSize = 300

maxExecutionSteps :: Int
maxExecutionSteps = 300

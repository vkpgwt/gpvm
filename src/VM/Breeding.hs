module VM.Breeding
  ( mkSelectable,
  )
where

import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Maybe
import qualified Data.Vector as V
import Records
import Selectable
import System.Random.Stateful
import VM
import qualified VM.Instruction as I

mkSelectable :: VM -> Selectable
mkSelectable vm =
  Selectable
    { breed = fmap mkSelectable . mutate vm,
      fitness = getFitness vm,
      display = show vm
    }

mutate :: RandomGenM g r m => VM -> g -> m VM
mutate vm@VM {..} gen = do
  code1 <- duplicateCodeSlicesRandomly code gen
  code2 <- mutateCodeWithBitErrors code1 gen
  pure $ vm {code = code2}
  where
    mutateInstruction (I.Instruction i) bitNo = I.Instruction $ complementBit i bitNo

mutateCodeWithBitErrors :: RandomGenM g r m => V.Vector I.Instruction -> g -> m (V.Vector I.Instruction)
mutateCodeWithBitErrors code gen = do
  let oneErrorPerNInstructions = 10
  let maxNumErrors = max 1 $ length code `div` oneErrorPerNInstructions
  let instSize = finiteBitSize (I.getInstruction undefined)
  numErrors <- randomRM (0, maxNumErrors) gen
  bitAddrs <- replicateM numErrors $ do
    bitAddr <- randomRM (0, instSize * length code - 1) gen
    pure $ bitAddr `divMod` instSize
  pure $ V.accum mutateInstruction code bitAddrs
  where
    mutateInstruction (I.Instruction i) bitNo = I.Instruction $ complementBit i bitNo

duplicateCodeSlicesRandomly :: RandomGenM g r m => V.Vector a -> g -> m (V.Vector a)
duplicateCodeSlicesRandomly code gen = do
  let maxCodeSize = 127
  let duplicationPercent = 2 :: Int
  p <- randomRM (0, 99) gen
  if p < duplicationPercent && V.length code <= maxCodeSize
    then pure $ V.take maxCodeSize $ code <> code
    else pure code

getFitness :: VM -> Fitness
getFitness vm =
  case VM.mkSnapshot vm of
    Nothing -> (-1000000)
    Just s0 ->
      let (result, s1) = VM.run maxExecutionSteps s0
          (value : _) = VM.snapshotRelativeStack s1
          target = 100
          basicFitness = negate . abs $ realToFrac target - realToFrac value
          penalty = case result of
            VM.RunMaxInstructionsReached -> 10
            VM.RunEnded -> 0
       in basicFitness - penalty

maxExecutionSteps :: Int
maxExecutionSteps = 300

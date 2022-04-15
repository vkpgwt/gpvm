module VM.Breeding
  ( selectionEngineHandle,
    VM (..),
  )
where

import Control.Monad
import Data.Bits
import Data.Foldable
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
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

-- {-# SCC mutate #-}
-- {-# SCC toNumericFitness #-}

data VM = VM
  { code :: !(SV.Vector I.W),
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
        ++ zipWith3 showInstruction (V.toList code) oneWordShiftedCode [0 ..]
        ++ ["\t]", "}"]
    where
      showInstruction op arg idx = "\t\t" ++ I.display op arg idx (V.length code)
      oneWordShiftedCode = case V.toList code of
        [] -> []
        x : xs -> xs ++ [x]

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
  | MutateIByte !I.W -- newByte

mutateInstructions :: (V.Vector v I.W, RandomGenM g r m) => v I.W -> g -> m (v I.W)
mutateInstructions code gen = do
  let maxNumErrors = max 1 $ V.length code `div` oneErrorPerThisManyInstructions
  numErrors <- randomRM (0, maxNumErrors) gen
  mutations <- replicateM numErrors $ do
    iAddr <- randomRM (0, V.length code - 1) gen
    bitErrorNotRandomByte <- randomM gen
    mutation <-
      if bitErrorNotRandomByte
        then MutateIBit <$> randomRM (0, finiteBitSize (undefined :: I.W) - 1) gen
        else MutateIByte <$> randomM gen
    pure (iAddr, mutation)
  pure $ V.accum mutateInstruction code mutations
  where
    mutateInstruction w (MutateIBit bitNo) = complementBit w bitNo
    mutateInstruction _ (MutateIByte newW) = newW

duplicateCodeSlicesRandomly :: (V.Vector v a, RandomGenM g r m) => v a -> g -> m (v a)
duplicateCodeSlicesRandomly code gen
  | V.length code >= maxCodeSize = pure code
  | otherwise = do
    p <- randomRM (0, 99) gen
    if p < duplicationPercent
      then pure $ V.take maxCodeSize $ code V.++ code
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
  Right (runResult, VM.ResultingSnapshot y) -> Right (runResult, y)
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

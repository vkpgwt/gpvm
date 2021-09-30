module VM.Breeding
  ( mkSelectable
  )
where

import Control.Monad
import Data.Bits
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
mutate vm0 gen = do
  let errorPerNInstructions = 10
  let numErrors = max 1 $ length (vm0 ^. #code) `div` errorPerNInstructions
  let instSize = finiteBitSize (I.getInstruction undefined)
  bitAddrs <- replicateM numErrors $ do
    bitAddr <- randomRM (0, instSize * length (vm0 ^. #code) - 1) gen
    pure $ bitAddr `divMod` instSize
  pure vm0 {code = V.accum mutateInstruction (vm0 ^. #code) bitAddrs}
  where
    mutateInstruction (I.Instruction i) bitNo = I.Instruction $ complementBit i bitNo

getFitness :: VM -> Fitness
getFitness vm =
  case VM.mkSnapshot vm of
    Nothing -> (-1000000)
    Just s0 ->
      let (_result, s1) = VM.run 20 s0
          value = fromMaybe (error "Unknown stack index 0") $ VM.snapshotStackIndex 0 s1
          target = 100
       in negate . abs $ realToFrac target - realToFrac value
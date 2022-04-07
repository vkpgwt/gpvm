module Main (main) where

import qualified Data.Vector.Storable as SV
import Records
import qualified SelectionEngine
import Text.Printf
import qualified VM.Breeding
import qualified VM.Instruction as I

main :: IO ()
main = do
  let handle = VM.Breeding.selectionEngineHandle
  let config =
        SelectionEngine.Config
          { maxPopulation = 1000,
            fertility = 1,
            fitnessNoiseAmp = 0.1,
            maxAge = 10,
            handle
          }
  let state0 = SelectionEngine.mkInitialState config 1 [VM.Breeding.VM {code, stackSize = 16}]
  print state0

  let (passedNumSteps, state1) = SelectionEngine.run 2000 (-0.01) config state0
  putStrLn ""
  printf "Generations performed: %d" passedNumSteps
  putStrLn ""
  print $ take 1 $ state1 ^. #items

code :: SV.Vector I.Instruction
code = [I.drop'P, I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.terminate]

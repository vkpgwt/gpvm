module Main (main) where

import qualified Data.Vector as V
import Records
import qualified SelectionEngine
import Text.Printf
import qualified VM
import qualified VM.Breeding
import qualified VM.Instruction as I

main :: IO ()
main = do
  let handle = VM.Breeding.selectionEngineHandle
  let config =
        SelectionEngine.Config
          { maxPopulation = 1000,
            fertility = 1,
            fitnessNoiseAmp = 0.01,
            handle
          }
  let state0 = SelectionEngine.mkState config 0 [VM.VM {code, stackSize = 16}]
  print state0

  let (passedNumSteps, state1) = SelectionEngine.run 1000 (-0.1) config state0
  putStrLn ""
  printf "Generations performed: %d" passedNumSteps
  putStrLn ""
  print $ take 1 $ state1 ^. #items

code :: V.Vector I.Instruction
code = [I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.terminate]

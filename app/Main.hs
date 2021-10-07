module Main (main) where

import qualified Data.Vector as V
import qualified SelectionEngine
import Text.Printf
import qualified VM
import qualified VM.Breeding
import qualified VM.Instruction as I

main :: IO ()
main = do
  let e0 = SelectionEngine.new [VM.Breeding.mkSelectable $ VM.VM {code, stackSize = 16}] 0
  print e0
  let (steps, e1) = SelectionEngine.run 1000 (-0.1) e0
  putStrLn ""
  printf "Generations performed: %d" steps
  putStrLn ""
  print $ take 1 $ SelectionEngine.selectables e1

code :: V.Vector I.Instruction
code = [I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.nop, I.terminate]

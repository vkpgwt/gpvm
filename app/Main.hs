module Main (main) where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified SelectionEngine
import qualified VM
import qualified VM.Breeding
import qualified VM.Instruction as I

main :: IO ()
main = do
  let e0 = SelectionEngine.new [VM.Breeding.mkSelectable $ VM.VM {code, stackSize = 16}] 0
  let e1 = SelectionEngine.run 100 50.0 e0
  print e1

code :: V.Vector I.Instruction
code = [I.loadConst 10]

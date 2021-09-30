module Main (main) where

import Data.Maybe (fromJust)
import qualified VM
import qualified VM.Instruction as I

main :: IO ()
main = do
  print runResult

code :: [I.Instruction]
code =
  [ I.loadConst 10,
    I.loadConst 20,
    I.loadConst 50
    --, I.end
  ]

runResult :: (VM.RunResult, VM.Snapshot)
runResult = VM.run 500000000 $ fromJust $ VM.mkSnapshot $ VM.VM { code, stackSize = 16 }

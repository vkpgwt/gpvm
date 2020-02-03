module Main (main) where
import           Data.Maybe (fromJust)
import qualified VM
import qualified VM.Instruction as I

main :: IO ()
main = do
    print runResult

code :: [I.Instruction]
code = [ I.loadConst 10
       , I.loadConst 20
       , I.loadConst 50
       --, I.end
       ]

runResult :: (VM.RunResult, VM.VM)
runResult = VM.run 100000000 $ fromJust $ VM.withCode code

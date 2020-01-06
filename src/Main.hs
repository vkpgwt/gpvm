module Main (main) where
import           Data.Maybe (fromJust)
import qualified VM
import qualified VM.Instruction as I
import qualified VM.State

main :: IO ()
main = do
    print runResult

vm = VM.VM [ I.loadConst 10
           , I.loadConst 20
           , I.loadConst 50
           , I.end
           ]

runResult = VM.State.run 100 $ fromJust $ VM.State.withVM vm
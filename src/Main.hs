module Main (main) where
import           Data.Maybe (fromJust)
import qualified VM
import qualified VM.Instruction as I
import qualified VM.Image

main :: IO ()
main = do
    print runResult

vm = VM.withCode [ I.loadConst 10
                 , I.loadConst 20
                 , I.loadConst 50
                 , I.end
                 ]

runResult = VM.Image.run 100 $ fromJust $ VM.Image.withVM vm

module Main
  ( main,
  )
where

import Criterion.Main
import Data.Either
import Data.List (find)
import Data.Maybe
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified VM
import qualified VM.Instruction as I

main :: IO ()
main =
  defaultMain
    [ bgroup
        "VM.run"
        [ bench ("All instructions (" ++ show (V.length allOpsCode) ++ ")") $
            whnf run $ snapshotWithCode allOpsCode,
          benchInstruction I.LoadInt8'U 1,
          benchInstruction I.Dup'U 1,
          benchInstruction I.Add'P 1,
          benchInstruction I.NotL'K 1
        ]
    ]
  where
    run = fromRight (error "Unexpected Left in VM.run result") . VM.run 1000000

    benchInstruction opc arg =
      let i = I.mkInstruction1 opc arg
       in bench (show opc) $
            whnf run $ snapshotWithCode [i, i, i, i]

snapshotWithCode :: SV.Vector I.Instruction -> VM.Snapshot
snapshotWithCode code =
  VM.Snapshot
    { code = code,
      stack = V.replicate 16 0,
      sp = 0,
      pc = 0
    }

allOpsCode :: SV.Vector I.Instruction
allOpsCode = V.fromList . map (`I.mkInstruction1` 1) $ replicate (neededLen - length opcodes) I.NoOp'K ++ opcodes
  where
    opcodes = filter (/= I.Terminate'K) [minBound .. maxBound]
    nearestPowerOf2 x = fromJust . find (>= x) $ iterate (* 2) 1
    neededLen = nearestPowerOf2 $ length opcodes

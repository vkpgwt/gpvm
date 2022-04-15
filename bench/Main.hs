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
          benchInstruction1 I.LoadInt8'U 0,
          benchInstruction I.Dup'U,
          benchInstruction I.Add'P,
          benchInstruction I.NotL'K
        ]
    ]
  where
    run = fromRight (error "Unexpected Left in VM.run result") . VM.run 1000000

    benchInstruction1 opc arg =
      let i = I.mkInstruction1 opc arg
       in bench (show opc) $
            whnf run $ snapshotWithCode $ I.mkCode [i, i, i, i]

    benchInstruction opc =
      let i = I.mkInstruction opc
       in bench (show opc) $
            whnf run $ snapshotWithCode $ I.mkCode [i, i, i, i]

snapshotWithCode :: SV.Vector I.W -> VM.Snapshot
snapshotWithCode code =
  VM.Snapshot
    { code = code,
      stack = V.replicate 16 0,
      sp = 0,
      pc = 0
    }

allOpsCode :: SV.Vector I.W
allOpsCode = V.fromList $ paddingWords ++ opWords
  where
    opWords = concatMap mkDefaultInstruction . filter (/= I.Terminate'K) $ [minBound .. maxBound]
    mkDefaultInstruction opc = I.encodeOpCode opc : I.nonJumpArguments (I.opCodeMetadata opc)
    neededLen = nearestPowerOf2 $ length opWords
    nearestPowerOf2 x = fromJust . find (>= x) $ iterate (* 2) 1
    paddingWords = replicate (neededLen - length opWords) (I.encodeOpCode I.NoOp'K)

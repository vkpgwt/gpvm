module Main
  ( main,
  )
where

import Criterion.Main
import Data.Either
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified VM
import qualified VM.Instruction as I

main :: IO ()
main =
  defaultMain
    [ bgroup
        "VM.run"
        [ bench "LoadInt8" $
            whnf (fromRight (error "Unexpected Left in VM.run result") . VM.run 1000000) $
              VM.Snapshot
                { code = code,
                  stack = V.replicate 16 0,
                  sp = 0,
                  pc = 0
                }
        ]
    ]

code :: SV.Vector I.Instruction
code =
  [ I.loadInt8 10,
    I.loadInt8 20,
    I.loadInt8 50,
    I.loadInt8 50
  ]

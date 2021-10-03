module Main
  ( main,
  )
where

import Criterion.Main
import Data.Maybe
import qualified Data.Vector as V
import qualified VM
import qualified VM.Instruction as I

main :: IO ()
main =
  defaultMain
    [ bgroup
        "VM.run"
        [ bench "LoadInt8" $
            whnf (VM.run 1000000) $
              fromJust . VM.mkSnapshot $
                VM.VM
                  { code = code,
                    stackSize = 16
                  }
        ]
    ]

code :: V.Vector I.Instruction
code =
  [ I.loadInt8 10,
    I.loadInt8 20,
    I.loadInt8 50
  ]

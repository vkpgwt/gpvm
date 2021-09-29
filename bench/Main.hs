module Main
  ( main,
  )
where

import Control.Applicative
import Criterion.Main
import Data.List
import Data.Maybe
import qualified VM
import qualified VM.Instruction as I

main :: IO ()
main =
  defaultMain
    [ bgroup
        "VM.run"
        [ bench "LoadConst" $ whnf (VM.run 1000000) $ fromJust $ VM.withCode code
        ]
    ]

code :: [I.Instruction]
code =
  [ I.loadConst 10,
    I.loadConst 20,
    I.loadConst 50
  ]

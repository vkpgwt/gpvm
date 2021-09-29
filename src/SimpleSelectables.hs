module SimpleSelectables
  ( NumSeekingTo100 (..),
  )
where

import Selectable
import System.Random (randomR)

newtype NumSeekingTo100 = NumSeekingTo100 Double
  deriving (Show)

instance Selectable NumSeekingTo100 where
  fitness (NumSeekingTo100 x) =
    negate . abs $ x - 100.0

  breed (NumSeekingTo100 x) g =
    let (bias, g') = randomR (-100.0, 100.0) g
     in (NumSeekingTo100 $ bias + x, g')

module Selectable
  ( Selectable (..),
    Fitness,
  )
where

import Records
import System.Random.Stateful

data Selectable = Selectable
  { breed :: forall g r m. RandomGenM g r m => g -> m Selectable,
    fitness :: Fitness,
    display :: String,
    fitnessDetails :: String
  }

instance Show Selectable where
  show s = "Selectable { f=" ++ show (s ^. #fitness) ++ " " ++ s ^. #fitnessDetails ++ ", " ++ (s ^. #display) ++ " }"

-- | Значение функции приспособленности: больше - приспособленнее
type Fitness = Double

module SelectionEngine
  ( run,
    new,
    SelectionEngine (..),
  )
where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as S
import Data.List (sortOn)
import Data.Ord (Down (..))
import Records
import Selectable
import System.Random.Stateful

data SelectionEngine = SelectionEngine
  { selectables :: [Selectable],
    gen :: !StdGen,
    config :: Config
  }
  deriving (Show)

data Config = Config
  { fertility :: !Int,
    maxPopulation :: !Int
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { fertility = 1,
      maxPopulation = 1000
    }

type State a = S.State SelectionEngine a

type NumSteps = Int

-- | Выполнение заданного числа итераций отбора
run :: NumSteps -> Fitness -> SelectionEngine -> (NumSteps, SelectionEngine)
run steps maxFitness = S.runState (go 0)
  where
    go n
      | n >= steps = pure n
      | otherwise = do
        sels <- S.gets selectables
        case sels of
          (Selectable {fitness} : _)
            | fitness >= maxFitness -> pure n
          _ ->
            doSelectionStep >> go (n + 1)

-- | Инициализация заданными данными
new :: [Selectable] -> Int -> SelectionEngine
new sels randomSeed =
  SelectionEngine
    { selectables = sels,
      gen = mkStdGen randomSeed,
      config = defaultConfig
    }

-- | Одна итерация отбора
doSelectionStep :: State ()
doSelectionStep = do
  conf <- S.gets config
  parents <- S.gets selectables
  children <- concat <$> mapM (spawnMany $ conf ^. #fertility) parents
  let candidates = parents ++ children
  noises <- withRandomGen $ replicateM (length candidates) . randomRM (0, 0.01 :: Double)
  let bestSelectables =
        map fst
          . take (conf ^. #maxPopulation)
          . sortOn (Down . (\(sel, noise) -> sel ^. #fitness + noise))
          $ zip candidates noises
  S.modify' $ \e -> e {selectables = bestSelectables}

spawnMany :: Int -> Selectable -> State [Selectable]
spawnMany num = replicateM num . spawnOne

spawnOne :: Selectable -> State Selectable
spawnOne s = withRandomGen $ Selectable.breed s

withRandomGen :: (StateGenM StdGen -> S.State StdGen a) -> State a
withRandomGen f = do
  oldGen <- S.gets gen
  let (a, newGen) = runStateGen oldGen f
  S.modify' $ \e -> e {gen = newGen}
  pure a

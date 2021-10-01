module SelectionEngine
  ( run,
    new,
  )
where

import Control.Monad
import qualified Control.Monad.Trans.State.Strict as S
import Data.List (sortBy, sortOn)
import Data.Maybe
import Data.Ord (Down (..), comparing)
import Data.Tuple
import Records
import Selectable
import System.Random (StdGen, mkStdGen)
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
    { fertility = 10,
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
  allChildren <- concat <$> mapM (spawnMany $ conf ^. #fertility) parents
  let bestSelectables =
        take (conf ^. #maxPopulation)
          . sortOn (Down . (^. #fitness))
          $ allChildren ++ parents
  S.modify' $ \e -> e {selectables = bestSelectables}

spawnMany :: Int -> Selectable -> State [Selectable]
spawnMany num = replicateM num . spawnOne

spawnOne :: Selectable -> State Selectable
spawnOne s = do
  g <- S.gets gen
  let (child, g') = runStateGen g $ Selectable.breed s
  S.modify' $ \e -> e {gen = g'}
  pure child

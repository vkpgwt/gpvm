module SelectionEngine
  ( select,
  )
where

import qualified Control.Monad.Trans.State.Strict as S
import Data.List (sortBy, sortOn)
import Data.Ord (Down (..), comparing)
import Selectable hiding (spawnOne)
import System.Random (StdGen, mkStdGen)
import Control.Monad
import Records

data SelectionEngine = SelectionEngine
  { selectables :: [Selectable],
    gen :: !StdGen,
    config :: Config
  }

data Config = Config
  { fertility :: !Int,
    maxPopulation :: !Int
  }

defaultConfig :: Config
defaultConfig =
  Config
    { fertility = 1,
      maxPopulation = 10
    }

type State a = S.State SelectionEngine a

-- | Выполнение заданного числа итераций отбора
select :: Int -> SelectionEngine -> SelectionEngine
select steps = S.execState (replicateM_ steps doSelectionStep)

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
          $ parents ++ allChildren
  S.modify' $ \e -> e {selectables = bestSelectables}

spawnMany :: Int -> Selectable -> State [Selectable]
spawnMany num = replicateM num . spawnOne

spawnOne :: Selectable -> State Selectable
spawnOne s = do
  g <- S.gets gen
  let (child, g') = (s ^. #breed) g
  S.modify' $ \e -> e {gen = g'}
  pure child

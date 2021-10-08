module SelectionEngine
  ( run,
    mkState,
    Config (..),
    State (..),
    Handle (..),
    Fitness,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S
import Data.List (sortOn)
import Data.Ord (Down (..))
import Records
import System.Random.Stateful

-- | Значение функции приспособленности: больше - приспособленнее
type Fitness = Double

data State s = State
  { items :: ![Item s],
    gen :: !StdGen
  }
  deriving (Show)

data Config s = Config
  { fertility :: !Int,
    maxPopulation :: !Int,
    fitnessNoiseAmp :: !Double,
    handle :: !(Handle s)
  }

data Handle s = Handle
  { reproduce :: !(forall g r m. RandomGenM g r m => s -> g -> m s),
    display :: !(s -> String),
    fitnessOf :: !(s -> (Fitness, String)) -- fitness, fitness details (reports only)
  }

data Item s = Item
  { fitness :: Fitness,
    fitnessDetails :: String,
    selectable :: !s
  }
  deriving (Show)

type RunM s a = R.ReaderT (Config s) (S.State (State s)) a

type NumSteps = Int

-- | Выполнение заданного числа итераций отбора
run :: NumSteps -> Fitness -> Config s -> State s -> (NumSteps, State s)
run steps maxFitness = S.runState . R.runReaderT (go 0)
  where
    go n
      | n >= steps = pure n
      | otherwise = do
        sels <- gets items
        case sels of
          (Item {fitness} : _)
            | fitness >= maxFitness -> pure n
          _ ->
            doSelectionStep >> go (n + 1)

-- | Инициализация заданными данными
mkState :: Config s -> Int -> [s] -> State s
mkState config randomSeed sels =
  State
    { items = map (mkItem config) sels,
      gen = mkStdGen randomSeed
    }

mkItem :: Config s -> s -> Item s
mkItem Config {handle = Handle {..}} sel =
  let (fitness, fitnessDetails) = fitnessOf sel
   in Item
        { selectable = sel,
          fitness,
          fitnessDetails
        }

-- | Одна итерация отбора
doSelectionStep :: RunM s ()
doSelectionStep = do
  conf <- ask
  parents <- gets items
  children <- concat <$> mapM (spawnMany (conf ^. #fertility) . (^. #selectable)) parents
  let candidates = parents ++ children
  noises <- replicateM (length candidates) generateFitnessNoise
  let bestItems =
        map fst
          . take (conf ^. #maxPopulation)
          . sortOn (Down . (\(item, noise) -> item ^. #fitness + noise))
          $ zip candidates noises
  modify' $ \s -> s {items = bestItems}

generateFitnessNoise :: RunM s Fitness
generateFitnessNoise = do
  conf <- ask
  withRandomGen $ randomRM (conf ^. #fitnessNoiseAmp * (-0.5), conf ^. #fitnessNoiseAmp * 0.5)

spawnMany :: Int -> s -> RunM s [Item s]
spawnMany num = replicateM num . spawnOne

spawnOne :: s -> RunM s (Item s)
spawnOne s = do
  config <- ask
  sel <- withRandomGen $ reproduce (config ^. #handle) s
  pure $ mkItem config sel

withRandomGen :: (StateGenM StdGen -> S.State StdGen a) -> RunM s a
withRandomGen f = do
  oldGen <- gets gen
  let (a, newGen) = runStateGen oldGen f
  modify' $ \e -> e {gen = newGen}
  pure a

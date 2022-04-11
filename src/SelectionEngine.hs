module SelectionEngine
  ( run,
    mkInitialState,
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
    gen :: !StdGen,
    stepNo :: !Int
  }
  deriving (Show)

data Config s = Config
  { fertility :: !Int,
    maxPopulation :: !Int,
    fitnessNoiseAmp :: !Double,
    maxAge :: !Int,
    handle :: !(Handle s)
  }

data Handle s = Handle
  { reproduce :: !(forall r. RandomGen r => s -> StateGenM r -> S.State r s),
    display :: !(s -> String),
    fitnessOf :: !(s -> (Fitness, String)) -- fitness, fitness details (reports only)
  }

data Item s = Item
  { fitness :: Fitness,
    fitnessDetails :: String,
    bornAt :: !Int,
    pedigree :: !Int,
    beast :: !s
  }
  deriving (Show)

type RunM s a = R.ReaderT (Config s) (S.State (State s)) a

type NumSteps = Int

-- {-# SCC run #-}

-- | Выполнение заданного числа итераций отбора
run :: NumSteps -> Fitness -> Config s -> State s -> (NumSteps, State s)
run steps maxFitness = S.runState . R.runReaderT (go 0)
  where
    go n
      | n >= steps = pure n
      | otherwise = do
        beasts <- gets items
        case beasts of
          (Item {fitness} : _)
            | fitness >= maxFitness -> pure n
          _ ->
            doSelectionStep >> go (n + 1)

-- | Инициализация заданными данными
mkInitialState :: Config s -> Int -> [s] -> State s
mkInitialState config randomSeed beasts =
  State
    { items = map (mkItem config 0 Nothing) beasts,
      gen = mkStdGen randomSeed,
      stepNo = 1
    }

mkItem :: Config s -> Int -> Maybe (Item s) -> s -> Item s
mkItem Config {handle = Handle {..}} stepNo mbParent beast =
  let (fitness, fitnessDetails) = fitnessOf beast
   in Item
        { beast,
          fitness,
          fitnessDetails,
          bornAt = stepNo,
          pedigree = maybe 0 (succ . (^. #pedigree)) mbParent
        }

-- | Одна итерация отбора
doSelectionStep :: RunM s ()
doSelectionStep = do
  conf <- ask
  parents <- gets items
  stepNo' <- gets stepNo
  children <- concat <$> mapM (spawnMany (conf ^. #fertility)) parents
  let aliveParents = filter (\i -> stepNo' - (i ^. #bornAt) < conf ^. #maxAge) parents
  let candidates = aliveParents ++ children
  noises <- replicateM (length candidates) generateFitnessNoise
  let bestItems =
        map fst
          . take (conf ^. #maxPopulation)
          . sortOn (Down . (\(item, noise) -> item ^. #fitness + noise))
          $ zip candidates noises
  modify' $ \s -> s {items = bestItems, stepNo = stepNo' + 1}

generateFitnessNoise :: RunM s Fitness
generateFitnessNoise = do
  conf <- ask
  withRandomGen $ randomRM (conf ^. #fitnessNoiseAmp * (-0.5), conf ^. #fitnessNoiseAmp * 0.5)

spawnMany :: Int -> Item s -> RunM s [Item s]
spawnMany num = replicateM num . spawnOne

spawnOne :: Item s -> RunM s (Item s)
spawnOne s = do
  config <- ask
  stepNo' <- gets stepNo
  beast <- withRandomGen $ reproduce (config ^. #handle) (s ^. #beast)
  pure $ mkItem config stepNo' (Just s) beast

withRandomGen :: (StateGenM StdGen -> S.State StdGen a) -> RunM s a
withRandomGen f = do
  oldGen <- gets gen
  let (a, newGen) = runStateGen oldGen f
  modify' $ \e -> e {gen = newGen}
  pure a

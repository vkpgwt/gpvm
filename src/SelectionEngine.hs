{-# LANGUAGE StandaloneDeriving #-}

module SelectionEngine
  ( select,
  )
where

import qualified Control.Monad.Trans.State.Strict as S
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Selectable (Selectable)
import qualified Selectable as Sel
import System.Random (StdGen, mkStdGen)

data Engine s = Engine
  { engSelectedItems :: [(s, Sel.Fitness)],
    engRandomGen :: !StdGen
  }

deriving instance (Show s) => Show (Engine s)

type MyState s a = S.State (Engine s) a

maxItems, randomSeed, fertility :: Int
maxItems = 10
randomSeed = 0
fertility = 1

-- | Выполнение заданного числа итераций отбора
select :: (Selectable s) => Int -> [s] -> [(s, Sel.Fitness)]
select num = engSelectedItems . S.execState monad . new
  where
    monad = sequence_ (replicate num generation)

-- | Инициализация заданными данными
new :: (Selectable s) => [s] -> Engine s
new sels =
  Engine
    { engSelectedItems = map makeItem sels,
      engRandomGen = mkStdGen randomSeed
    }

makeItem :: (Selectable s) => s -> (s, Sel.Fitness)
makeItem s = s `seq` f `seq` (s, f)
  where
    f = Sel.fitness s

-- | Одна итерация отбора
generation :: Selectable s => MyState s ()
generation = do
  items <- S.gets engSelectedItems
  allChildren <- concat <$> mapM (makeChildrenOf fertility . fst) items
  let bestItems = take maxItems . sortBy (comparing (Down . snd)) $ items ++ allChildren
  S.modify' $ \e -> e {engSelectedItems = bestItems}

makeChildrenOf :: Selectable s => Int -> s -> MyState s [(s, Sel.Fitness)]
makeChildrenOf num = sequence . replicate num . breed

breed :: (Selectable s) => s -> MyState s (s, Sel.Fitness)
breed s = do
  g <- S.gets engRandomGen
  let (child, g') = Sel.breed s g
  S.modify' $ \e -> e {engRandomGen = g'}
  return $ makeItem child

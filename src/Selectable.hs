module Selectable
  ( Selectable (..),
    Fitness,
    breedEnumWithRange,
    breedIntWithRange,
  )
where

import System.Random (RandomGen, randomR)

-- | Сущность, подлежащая отбору. Для нее определена функция порождения мутантной
-- | копии и функция вычисления приспособленности.
class Selectable s where
  breed :: RandomGen g => s -> g -> (s, g)
  fitness :: s -> Fitness

-- | Значение функции приспособленности: больше - приспособленнее
type Fitness = Double

breedIntWithRange :: RandomGen g => Int -> Int -> g -> (Int, g)
breedIntWithRange r _ _ | r < 1 = error "range must be positive"
breedIntWithRange 1 a g = (a, g)
breedIntWithRange r a g = (a + delta, g')
  where
    (delta, g') = randomR (r `div` (-2), r `div` 2) g

breedEnumWithRange :: (Enum a, Bounded a, RandomGen g) => Int -> a -> g -> (a, g)
breedEnumWithRange _ a g | lo == hi = (a, g)
  where
    (lo, hi) = boundedIntRange a
breedEnumWithRange r a g = (a', g')
  where
    (int, g') = breedIntWithRange r (fromEnum a) g
    a' = toEnum . wrap $ int
    wrap x
      | x >= lo && x <= hi = x
      | otherwise = (x - lo) `mod` (hi - lo + 1)
    (lo, hi) = boundedIntRange a

boundedIntRange :: (Bounded a, Enum a) => a -> (Int, Int)
boundedIntRange a = (fromEnum (minBound `asTypeOf` a), fromEnum (maxBound `asTypeOf` a))

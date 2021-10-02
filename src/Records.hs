module Records ((^.), (?.)) where

import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits (Symbol)

data Field (name :: Symbol) = Field

instance (name1 ~ name2) => IsLabel name1 (Field name2) where
  fromLabel = Field

infixl 8 ^., ?.

-- | Reading a field using syntax: record ^. #field . It does not
-- require 'Generic' or any other type class instance! 'HasField'
-- instance is derived by ghc automatically.
--
-- >>> data Point = Point {x :: Int, y :: Int};
-- >>> Point 111 222 ^. #x
-- 111
(^.) :: forall name r a. HasField name r a => r -> Field name -> a
r ^. _ = getField @name r

-- | Like (^.), but within a functor:
--
-- >>> data Point = Point {x :: Int, y :: Int};
-- >>> Just (Point 111 222) ?. #x
-- Just 111
--
-- >>> data Point = Point {x :: Int, y :: Int};
-- >>> (Nothing :: Maybe Point) ?. #x
-- Nothing
--
-- >>> data Point = Point {x :: Int, y :: Int};
-- >>> [Point 1 2, Point 3 4] ?. #x
-- [1,3]
(?.) :: forall name r a f. (HasField name r a, Functor f) => f r -> Field name -> f a
r ?. _ = fmap (getField @name) r

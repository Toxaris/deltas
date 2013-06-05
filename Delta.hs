-- to compute Delta types:
{-# LANGUAGE TypeFamilies #-}

-- for the test cases:
{-# LANGUAGE ScopedTypeVariables #-}

module Delta where

import Prelude hiding ((+), (-), id)
import Properties

-- deltas and derivatives
--
-- based on the theory by Cai Yufai
-- Haskell implementation by Tillmann Rendel

data Base
  = X | Y | Z
  deriving (Eq, Show)

data Replace a = Replace a a
  deriving (Eq, Show)

class Changing a where
  -- Using type families here is a bit convenient, but means risking trouble later. Also, I'd rather use a type class in ChangeCategory.
  type Delta a
  id :: a -> Delta a
  -- Can we use operators not used in Prelude? I need integers elsewhere...
  (+) :: a -> Delta a -> a
  (-) :: a -> a -> Delta a

instance Changing Base where
  type Delta Base = Replace Base
  id x = Replace x x
  x + Replace y z | x == y = z
  x - y = Replace y x

instance (Changing a, Changing b) => Changing (a -> b) where
  type Delta (a -> b) = a -> Delta a -> Delta b
  id f = \x dx -> id (f (x + dx))
  f + df = \x -> f x + df x (id x)
  f - g = \x dx -> f (x + dx) - g x

-- support instances for automatic test case generation

instance Generate Base where
  generate = [X, Y, Z]

instance Consume Base where
  consume results = do
    rsX <- results
    rsY <- results
    rsZ <- results
    return (\x -> case x of
      X -> rsX
      Y -> rsY
      Z -> rsZ)

instance Generate a => Generate (Replace a) where
  generate = do
    x <- generate
    y <- generate
    return (Replace x y)

instance Consume a => Consume (Replace a) where
  consume results = do
    f <- consume (consume results)
    return (\x -> case x of
      Replace x y -> f x y)

-- lemma 1 from Cai's text on the theory of deltas

lemma1a = forall (\(v :: Base) -> v + id v == v)
lemma1b = forall (\(v :: Base -> Base) -> v + id v == v)
lemma1c = forall (\(v :: Base -> (Base -> Base)) -> v + id v == v)

{-
lemma1d = forall (\(v :: (Base -> Base) -> Base) -> v + id v == v)
lemma1e = forall (\(v :: (Base -> Base) -> (Base -> Base)) -> v + id v == v)
-}

lemma1 = and [lemma1a, lemma1b, lemma1c]

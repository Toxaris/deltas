-- for the computing Delta types:
{-# LANGUAGE TypeFamilies #-}

-- for the test cases:
{-# LANGUAGE ScopedTypeVariables #-}

module Delta where

import Prelude hiding ((+), (-), id)


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
  type Delta a
  id :: a -> Delta a
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

-- property-based testing with exhaustive test case generation
--
-- based on QuickCheck and SmallCheck

class Generate a where
  generate :: [a]

class Consume a where
  consume :: [b] -> [a -> b]

instance (Generate a, Eq b) => Eq (a -> b) where
  f == g = and [f x == g x | x <- generate]

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

instance (Consume a, Generate b) => Generate (a -> b) where
  generate = do
    consume generate


{-
instance Consume a => Consume [a] where
  consume results = do
    let f results [] = results
        f results (x : xs) = [y | f <- consume results, y <- f x]
    return (f results)

instance (Generate a, Consume b) => Consume (a -> b) where
  consume results = do
    let args = generate 

    return (\f -> map f args)

    where
      nest results [] = results
      next results (arg : args) = do
-}

class Property p where
  forall :: p -> Bool
  exists :: p -> Bool

instance Property Bool where
  forall x = x
  exists x = x

instance (Generate a, Property p) => Property (a -> p) where
  forall f = and [forall (f x) | x <- generate]
  exists f = or [exists (f x) | x <- generate]

-- lemma 1 from Cai's text on the theory of deltas

lemma1a = forall (\(v :: Base) -> v + id v == v)
lemma1b = forall (\(v :: Base -> Base) -> v + id v == v)
lemma1c = forall (\(v :: Base -> (Base -> Base)) -> v + id v == v)

{-
lemma1d = forall (\(v :: (Base -> Base) -> Base) -> v + id v == v)
lemma1e = forall (\(v :: (Base -> Base) -> (Base -> Base)) -> v + id v == v)
-}

lemma1 = and [lemma1a, lemma1b, lemma1c]

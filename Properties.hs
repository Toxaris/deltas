module Properties where

-- property-based testing with exhaustive test case generation
--
-- based on QuickCheck and SmallCheck

class Generate a where
  generate :: [a]

class Consume a where
  consume :: [b] -> [a -> b]

instance (Generate a, Eq b) => Eq (a -> b) where
  f == g = and [f x == g x | x <- generate]

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

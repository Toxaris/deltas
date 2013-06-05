-- to compute Delta types:
{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}

module DeltaInt where

import Prelude hiding ((+), (-), id)
import qualified Prelude as P

import Delta

instance Changing Integer where
  type Delta Integer = Integer
  id = const 0
  (+) = (P.+)
  (-) = (P.-)

-- (-) is too ambiguous on integers, because of injectivity
diffInt :: Integer -> Integer -> Integer
diffInt = (-)

{-
-- Doesn't typecheck
v :: Delta Integer
v = 2 - 1
-}

-- Typechecks
v :: Delta Integer
v = diffInt 2 1

-- XXX: the below is an alpha-renaming of the code, written during bug
-- reporting. It might be useful for later, to refactor the code with
-- more standard names.
{-

class Changing a where
  -- Using type families here is a bit convenient, but means risking trouble later.
  type Delta a
  nil :: a -> Delta a
  apply :: a -> Delta a -> a
  diff :: a -> a -> Delta a

instance Changing Integer where
  type Delta Integer = Integer
  nil = const 0
  apply = (+)
  diff = (-)

v :: Delta Integer
v = diff (2 :: Integer) 1
-}

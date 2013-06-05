{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction #-}
module DeltaBag where

import Prelude hiding ((+), (-), id)
import qualified Prelude as P

import qualified Data.Map as M
import Data.Map(Map, (!))
import Data.Natural

import Control.Arrow
import qualified Control.Category as C

import ArrowUtils

import Delta
import DeltaInt

class Changing a => ChangeCategory a where
  -- We can't use type here...
  data AddressedDelta a
  -- ... because otherwise this type would be ambiguous
  -- (http://www.haskell.org/haskellwiki/GHC/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity)
  baseDelta :: AddressedDelta a -> Delta a
  src :: AddressedDelta a -> a

instance ChangeCategory Base where
  newtype AddressedDelta Base = N { unN :: Delta Base } deriving (Show, Eq, Ord)
  src (N (Replace a _)) = a
  baseDelta = unN

instance ChangeCategory Integer where
  data AddressedDelta Integer = I { base :: Integer, unI :: Integer } deriving (Show, Eq, Ord)
  src = base
  baseDelta = unI

type BagℕMap t = Map t Natural
type BagℤMap t = Map t Integer

newtype Bag a = Bag { unBag :: BagℕMap a }
  deriving (Show, Eq)

-- normalizes a bag: if an element has multiplicity count 0, it should not be explicitly mentioned.
mapNormalize = M.filter (/= 0)
bagNormalize = Bag . mapNormalize . unBag

newtype BagSubChanges a deltaA = BSC (Map a (BagℕMap deltaA))
  deriving (Eq, Show)
emptySubChanges = BSC M.empty

subChangesFromList :: (Ord a, Ord deltaA) => a -> [deltaA] -> BagSubChanges a deltaA
subChangesFromList base changes = BSC $ M.fromList [(base, unBag . fromList $ changes)]

-- We need the correct argument for delta
data BagℕChange a deltaA = BagℕChange (BagSubChanges a deltaA) (BagℤMap a)
                         deriving (Show, Eq)

toBagℤ :: BagℕMap k -> BagℤMap k
toBagℤ = M.map toInteger

-- Can cause errors if the maps contains negative numbers
toBagℕ :: BagℤMap k -> BagℕMap k
toBagℕ = M.map fromInteger

instance (Ord a, ChangeCategory a) => Changing (Bag a) where
  type Delta (Bag a) = BagℕChange a (AddressedDelta a)

  id x = BagℕChange emptySubChanges M.empty

  (Bag b1) - (Bag b2) =
    BagℕChange emptySubChanges $ correctDiff $ M.unionWith (P.-) convB1 convB2
      where
        convB1 = toBagℤ b1
        convB2 = toBagℤ b2
        -- Elements only in convB2 will have the wrong sign in the result
        correctDiff bag = mapNormalize $ M.unionWith (\ new old -> old P.- 2 * new) bag diff
        diff = M.difference convB2 convB1

  (Bag bagMap) + (BagℕChange deltas c) =
    bagNormalize . Bag . toBagℕ . M.unionWith (P.+) c . toBagℤ . applyChanges deltas $ bagMap
      where
        applyChanges :: BagSubChanges a (AddressedDelta a) -> BagℕMap a -> BagℕMap a
        applyChanges (BSC deltas) = M.foldl replace2 ?? deltas --foldl replace ?? deltas
        replace2 :: BagℕMap a -> BagℕMap (AddressedDelta a) -> BagℕMap a
        replace2 bagMap deltas = M.foldlWithKey replaceN bagMap deltas

        replaceN :: BagℕMap a -> AddressedDelta a -> Natural -> BagℕMap a
        replaceN bagMap delta n =
          M.insertWith' (P.+) dst n .
          M.insert toReplace (currMult P.- n) .
          M.delete toReplace $ bagMap
         where
            toReplace = src delta
            currMult = bagMap ! toReplace
            dst = toReplace + baseDelta delta

fromList :: (Ord t) => [t] -> Bag t
fromList l = Bag $ foldl (\ bag el -> M.insertWith (\ new old -> new P.+ old) el 1 bag) M.empty l

v1 = fromList [X, X, X]
v2 = fromList [X, Y, Z]
v3 = fromList [X]

il1, il2, il3 :: Bag Integer
il1 = fromList [1, 1, 1]
il2 = fromList [1, 2, 3]
il3 = fromList [1]

test1C :: BagℕChange Base (AddressedDelta Base)
test1C = v2 - v1

test1CI = il2 - il1

-- should be asserted
test1 = v1 + test1C == v2

test1I = il1 + test1CI == il2

test2C :: BagℕChange Base (AddressedDelta Base)
test2C = BagℕChange (subChangesFromList X [N (Y - X), N (Z - X)]) M.empty

test2CI :: BagℕChange Integer (AddressedDelta Integer)
test2CI = BagℕChange (subChangesFromList 1 [I 1 (2 `diffInt` 1), I 1 (3 `diffInt` 1)]) M.empty

test2 = v1 + test2C == v2
test2I = il1 + test2CI == il2

test3C = v3 - v1
test3 = v1 + test3C == v3

test4C = v3 - v2
test4 = v2 + test4C == v3

test3CI = il3 - il1
test3I = il1 + test3CI == il3

test4CI = il3 - il2
test4I = il2 + test4CI == il3

-- Reused:
check :: (Eq a, Changing a) => (s -> a) -> (s -> a) -> (s -> Delta a) -> s -> Bool
check ilA ilB aToB n = ilA n + aToB n == ilB n
diffN ilA ilB n = ilB n - ilA n

-- Tests using check
--il1A :: (Num t, Ord t) => Integer -> Bag t

-- This type signature is required to avoid ambiguities
il1A, il1B :: Integer -> Bag Integer
il1A n = fromList $ replicate (fromInteger n) 1
il1B n = fromList $ replicate (fromInteger n) 2
aToB1 n = BagℕChange (subChangesFromList 1 (replicate (fromInteger n) (I 1 (2 `diffInt` 1)))) M.empty

check1 = check il1A il1B aToB1
aToB1' = diffN il1A il1B
check1' = check il1A il1B aToB1'

il2A, il2B :: Integer -> Bag Integer
il2A n = fromList $ [1, 2 .. n]
il2B n = fromList $ [2, 3 .. n]
aToB2 = diffN il2A il2B
check2 = check il2A il2B aToB2

il3A = il2A
il3B n = fromList $ [2, 3 .. n + 1]
aToB3 = diffN il3A il3B
check3 = check il3A il3B aToB3

il4A, il4B :: Integer -> Bag Integer
il4A = il2A
il4B n = fromList $ [2, 4 .. n]
aToB4 = diffN il4A il4B
check4 = check il4A il4B aToB4

il5A, il5B :: Integer -> Bag Integer
il5A = il2A
il5B n = fromList $ [2, 4 .. 2 * n]
aToB5 = diffN il5A il5B
check5 = check il5A il5B aToB5

check1, check1', check2, check3, check4, check5 :: Integer -> Bool

checks n = and $ map ($ n) [check1, check1', check2, check3, check4, check5]
testsFor20 = checks 20

-- Print it to check it's true. TODO: a proper testing framework.
allTests = and [test1, test1I, test2, test2I, test3, test3I, test4, test4I, testsFor20]

changes n = map ($ n) [aToB1, aToB1', aToB2, aToB3, aToB4, aToB5]
-- Call this to look at the changes.
changes10 = mapM_ print (changes 10)

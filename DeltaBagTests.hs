module DeltaBagTests where

import Prelude hiding ((+), (-), id)
import qualified Prelude as P
import qualified Data.Map as M

import Delta
import DeltaInt
import DeltaBag

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

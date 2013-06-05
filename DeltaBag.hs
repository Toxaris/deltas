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

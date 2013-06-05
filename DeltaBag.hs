{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction #-}
module DeltaBag where

import Prelude hiding ((+), (-), id)
import qualified Prelude as P
import qualified Data.Map as M
import Data.Map(Map, (!))
import Control.Arrow
import qualified Control.Category as C
import Delta

import ArrowUtils

-- Needs package natural-numbers
import Data.Natural

class Changing a => ChangeCategory a where
  -- We can't use type here...
  data AddressedDelta a
  -- ... because otherwise this type would be ambiguous
  -- (http://www.haskell.org/haskellwiki/GHC/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity)
  baseDelta :: AddressedDelta a -> Delta a
  src :: AddressedDelta a -> a

instance ChangeCategory Base where
  newtype AddressedDelta Base = N { unN :: Delta Base } deriving (Show, Eq)
  src (N (Replace a _)) = a
  baseDelta = unN

type BagℕMap t = Map t Natural
type BagℤMap t = Map t Integer

newtype Bag a = Bag (BagℕMap a)
  deriving (Show, Eq)

-- We need the correct argument for delta
data BagℕChange a deltaA = BagℕChange [deltaA] (BagℤMap a)
                         deriving (Show, Eq)

toBagℤ :: BagℕMap k -> BagℤMap k
toBagℤ = M.map toInteger

-- Can cause errors if the maps contains negative numbers
toBagℕ :: BagℤMap k -> BagℕMap k
toBagℕ = M.map fromInteger

instance (Ord a, ChangeCategory a) => Changing (Bag a) where
  type Delta (Bag a) = BagℕChange a (AddressedDelta a)

  id x = BagℕChange [] M.empty

  (Bag b1) - (Bag b2) = BagℕChange [] $ M.unionWith (P.-) (toBagℤ b1) (toBagℤ b2)

  (Bag bagMap) + (BagℕChange deltas c) =
    Bag . toBagℕ . M.unionWith (P.+) c . toBagℤ . (foldl replace ?? deltas) $ bagMap
      where
        -- Apply the delta only to one copy of its source.
        replace :: BagℕMap a -> AddressedDelta a -> BagℕMap a
        replace bagMap delta =
          M.insertWith' (P.+) dst 1 .
          M.insert toReplace (currMult P.- 1) .
          M.delete toReplace $ bagMap
         where
            toReplace = src delta
            currMult = bagMap ! toReplace
            dst = toReplace + baseDelta delta

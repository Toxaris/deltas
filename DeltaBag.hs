{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module DeltaBag where

import Prelude hiding ((+), (-), id)
import qualified Prelude as P
import qualified Data.Map as M
import Data.Map(Map, (!))
import Delta

class Changing a => ChangeCategory a where
  -- We can't use type here...
  data AddressedDelta a
  -- ... because otherwise this type would be ambiguous
  -- (http://www.haskell.org/haskellwiki/GHC/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity)
  baseDelta :: AddressedDelta a -> Delta a
  src :: AddressedDelta a -> a

instance ChangeCategory Base where
  newtype AddressedDelta Base = N { unN :: Delta Base }
  src (N (Replace a b)) = a
  baseDelta = unN

type BagMap a = Map a Int
newtype Bag a = Bag (BagMap a)
data BagChange a deltas = BagChange [deltas] (BagMap a)

(??) = flip

instance (Ord a, ChangeCategory a) => Changing (Bag a) where
  type Delta (Bag a) = BagChange a (AddressedDelta a)

  id x = BagChange [] M.empty

  (Bag b1) - (Bag b2) = BagChange [] $ M.unionWith (P.-) b1 b2

  (Bag bagMap) + (BagChange deltas c) =
    Bag . M.unionWith (P.+) c . (foldl replace ?? deltas) $ bagMap
      where
        -- Apply the delta only to one copy of its source.
        replace :: BagMap a -> AddressedDelta a -> BagMap a
        replace bagMap delta =
          M.insertWith' (P.+) dst 1 .
          M.insert toReplace (currMult P.- 1) .
          M.delete toReplace $ bagMap
         where
            toReplace = src delta
            currMult = bagMap ! toReplace
            dst = toReplace + baseDelta delta

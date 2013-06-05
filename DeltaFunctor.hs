{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction #-}
module DeltaFunctor where

import Prelude hiding ((+), (-), id, (.))
import qualified Prelude as P
import Control.Category hiding (id)
import Control.Arrow
{-
import qualified Data.Map as M
import Data.Map(Map, (!))
-}

import Delta

-- star2' :: Arrow a => (b -> a c d) -> (b' -> a c' d') -> (b, b') -> a (c, c') (d, d')
-- star2' op1 op2 = uncurry (***) . (op1 *** op2)
-- plus2 :: (Changing c, Changing c') => (c, c') -> (Delta c, Delta c') -> (c, c')
-- plus2 = (+) `star2` (+)

-- Yet better. This is (***) for binary functions.
star2 :: (Arrow a1, Arrow a2) =>
 (a1 b (a2 c d)) -> (a1 b' (a2 c' d')) -> a1 (b, b') (a2 (c, c') (d, d'))
star2 op1 op2 = arr (uncurry (***)) . (op1 *** op2)

instance (Changing a, Changing b) => Changing (a, b) where
  type Delta (a, b) = (Delta a, Delta b)
  id = id *** id
  (+) = (+) `star2` (+)
  (-) = (-) `star2` (-)

data DeltaEither a b da db
  = DLeft da
  | DRight db
  | DToLeft b a
  | DToRight a b

instance (Changing a, Changing b) => Changing (Either a b) where
  type Delta (Either a b) = DeltaEither a b (Delta a) (Delta b)

  id = either (DLeft . id) (DRight . id)

  (Left a) + (DLeft da) = Left $ a + da
  (Right b) + (DRight db) = Right $ b + db
  _ + (DToLeft _ a) = Left a
  _ + (DToRight _ b) = Right b

  (Left a1) - (Left a2) = DLeft $ a1 - a2
  (Right b1) - (Right b2) = DRight $ b1 - b2
  (Left a) - (Right b) = DToLeft b a
  (Right b) - (Left a) = DToRight a b
  

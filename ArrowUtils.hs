{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction #-}
module ArrowUtils where

import Prelude hiding ((.))
import Control.Category
import Control.Arrow

-- star2' :: Arrow a => (b -> a c d) -> (b' -> a c' d') -> (b, b') -> a (c, c') (d, d')
-- star2' op1 op2 = uncurry (***) . (op1 *** op2)
-- plus2 :: (Changing c, Changing c') => (c, c') -> (Delta c, Delta c') -> (c, c')
-- plus2 = (+) `star2` (+)

-- Yet better. This is (***) for binary functions.
star2 :: (Arrow a1, Arrow a2) =>
 (a1 b (a2 c d)) -> (a1 b' (a2 c' d')) -> a1 (b, b') (a2 (c, c') (d, d'))
star2 op1 op2 = arr (uncurry (***)) . (op1 *** op2)

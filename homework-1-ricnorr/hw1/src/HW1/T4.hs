module HW1.T4
  ( tfoldr
  ) where

import HW1.T3 (Tree(..))

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Leaf                         = acc
tfoldr f acc (Branch _ left val right) = tfoldr f (f val (tfoldr f acc right)) left

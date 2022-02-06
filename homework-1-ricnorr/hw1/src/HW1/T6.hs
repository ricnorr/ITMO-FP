module HW1.T6
  ( epart
  , mcat
  ) where

import Data.Foldable (Foldable(fold))

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap eitherToPair where
                eitherToPair :: (Monoid a, Monoid b) => Either a b -> (a, b)
                eitherToPair (Left a)  = (a, mempty)
                eitherToPair (Right b) = (mempty, b)

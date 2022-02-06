module HW1.T5
  ( joinWith
  , splitOn
  ) where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..), toList, (<|))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn del = foldr f ([] :| [])
  where
    f el (x :| xs)
      | el == del = [] <| (x :| xs)
      | otherwise = (el : x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith del x = intercalate [del] (toList x)

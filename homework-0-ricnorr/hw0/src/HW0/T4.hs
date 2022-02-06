module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec lst -> case lst of
   []   -> []
   h:hs -> f h : rec hs)

fib :: Natural -> Natural
fib i  = fix (scanl (+) 0 . (1:))!!fromIntegral i

fac :: Natural -> Natural
fac = fix (\rec x -> if x == 0 then 1 else x * rec(x - 1))

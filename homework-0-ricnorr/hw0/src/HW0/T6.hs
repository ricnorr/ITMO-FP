module HW0.T6
  where

import Data.Char (isSpace)
import HW0.T1 (distrib)

a :: (Either [Char] b, Either [Char] c)
a = HW0.T1.distrib (Left ("AB" ++ "CD" ++ "EF")) :: (Either [Char] b, Either [Char] c)

b :: [Bool]
b = map isSpace "Hello, World"

c :: [Char]
c = if 1 > 0 || error "X" then "Y" else "Z"

a_whnf = (Left "ABCDEF", Left "ABCDEF")
b_whnf = isSpace 'H' : map isSpace "ello, World"
c_whnf = "Y"

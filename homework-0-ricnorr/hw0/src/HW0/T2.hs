module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a)
doubleNeg a x = x a

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg a = a . doubleNeg

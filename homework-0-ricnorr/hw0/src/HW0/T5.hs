module HW0.T5
  ( Nat
  , nFromNatural
  , nToNum
  , nmult
  , nplus
  , ns
  , nz
  ) where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a
nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns n f x = f (n f x)

nplus :: (t1 -> t2 -> t3) -> (t1 -> t4 -> t2) -> t1 -> t4 -> t3
nplus a b f x = a f (b f x)

nmult :: (b -> c) -> (a -> b) -> a -> c
nmult a b = a . b

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural x = ns (nFromNatural (x - 1))

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0

module HW1.T2
  ( N (..)
  , nEven
  , nFromNatural
  , nOdd
  , nToNum
  , ncmp
  , ndiv
  , nmod
  , nmult
  , nplus
  , nsub
  ) where

import Data.Maybe (fromJust, isNothing)
import Numeric.Natural (Natural)

data N = Z | S N deriving (Eq, Show)

nplus :: N -> N -> N
nplus Z b     = b
nplus (S a) b = S (nplus a b)

nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult a (S b) = nplus a (nmult a b)

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub (S a) (S b) = nsub a b
nsub _ _         = Nothing

ncmp :: N -> N -> Ordering
ncmp a b
  | sub == Just Z = EQ
  | isNothing sub = LT
  | otherwise = GT
  where
    sub = nsub a b

one :: N
one = S Z

two :: N
two = S (S Z)

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = nplus one (nFromNatural (pred x))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = 1 + nToNum a

nEven :: N -> Bool
nEven a
  | modRes == Z = True
  | otherwise = False
  where
    modRes = nmod a two

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z = error "zero division"
ndiv a b
  | ncmp a b == LT = Z
  | otherwise = nplus one (ndiv (fromJust (nsub a b)) b)

nmod :: N -> N -> N
nmod _ Z = error "zero division"
nmod a b = fromJust (nsub a (nmult b (ndiv a b)))

module HW1.T7
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

import Data.Semigroup ()

data ListPlus a = a :+ ListPlus a | Last a

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (x :+ xs) b = x :+ (xs <> b)
  (<>) (Last x) b  = x :+ b

data Inclusive a b = This a | That b | Both a b deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
    (<>) (This x) (That y)     = Both x y
    (<>) (This x) (This y)     = This (x <> y)
    (<>) (This x) (Both y z)   = Both (x <> y) z
    (<>) (Both x y) (That z)   = Both x (y <> z)
    (<>) (Both x y) (This z)   = Both (x <> z) y
    (<>) (Both x y) (Both z v) = Both (x <> z) (y <> v)
    (<>) (That x) (That y)     = That (x <> y)
    (<>) (That x) (This y)     = Both y x
    (<>) (That x) (Both y z)   = Both y (x <> z)

newtype DotString = DS String

instance Semigroup DotString where
  (<>) (DS "") b     = b
  (<>) a (DS "")     = a
  (<>) (DS a) (DS b) = DS (a ++ "." ++ b)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F a) (F b) = F (a . b)

instance Monoid (Fun a) where
  mempty = F id

module HW2.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  )
where

import HW2.T1 (Annotated(..), Except(..), Fun(..), List(..), Option(..))

import HW2.T2 (concatLists)

-- Unwrap option from option
joinOption    :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some a) = a

-- Unwrap except from except
joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Success a) = a
joinExcept (Error e) = Error e

-- Unwrap annotated from annotated (combining e)
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a1 :# e1) :# e2) = a1 :# (e2 <> e1)

-- Unwrap list from list
joinList      :: List (List a) -> List a
joinList (h :. t) = concatLists h (joinList t)
joinList Nil = Nil

-- Unwrap Fun from Fun
joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F fun) = F (\i -> let (F fun2)  = fun i in fun2 i)


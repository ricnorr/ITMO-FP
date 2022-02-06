module HW2.T2
  ( distOption
  , distExcept
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  , concatLists
  )
  where

import HW2.T1
  (Annotated(..), Except(..), Fun(..), List(..), Option(..), Pair(..), Prioritised(..), Quad(..),
  Stream(..))

-- collapse tuple of options to one option
distOption      :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

-- collapse tuple of pairs to one pair
distPair        :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 x2, P y1 y2) = P (x1, y1) (x2, y2)

-- collapse tuple of quads to one quad
distQuad        :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 x2 x3 x4, Q y1 y2 y3 y4) = Q (x1, y1) (x2, y2) (x3, y3) (x4, y4)

-- collapse tuple of annotated to one annotated
distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a1 :# e1, a2 :# e2) = (a1, a2) :# (e1 <> e2)

-- collapse tuple of prioritised to one prioritised, selects highest priority
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a, Low b)      =  High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, High b)     = High (a, b)

-- collapse tuple of streams to one stream
distStream      :: (Stream a, Stream b) -> Stream (a, b)
distStream (h1 :> t1, h2 :> t2) = (h1, h2) :> distStream (t1, t2)

-- concat lists
concatLists :: List a -> List a -> List a
concatLists (h1 :. Nil) list = h1 :. list
concatLists Nil Nil          = Nil
concatLists Nil list         = list
concatLists (h1 :. t1) list  = h1 :. concatLists t1 list

-- collapse tuple of lists to one list. Like cartesian product (total size is n * m)
distList        :: (List a, List b) -> List (a, b)
distList (Nil, _)         = Nil
distList (_, Nil)         = Nil
distList (h1 :. t1, list) = elToList h1 list `concatLists` distList (t1, list) where
                            elToList :: a -> List b -> List (a, b)
                            elToList _ Nil        = error "second list is empty"
                            elToList a (h :. Nil) = (a, h) :. Nil
                            elToList a (h :. t)   = (a, h) :. elToList a t

-- collapse tuple of Funs to one Fun
distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f1, F f2) = F (\x -> (f1 x, f2 x))

-- collapse tuple of Excepts to one Except
distExcept      :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e

-- wrap value in Option
wrapOption      :: a -> Option a
wrapOption = Some

-- wrap value in Pair
wrapPair        :: a -> Pair a
wrapPair a = P a a

-- wrap value in Quad
wrapQuad        :: a -> Quad a
wrapQuad a = Q a a a a

-- wrap value in Annotated
wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

-- wrap value in Except
wrapExcept      :: a -> Except e a
wrapExcept = Success

-- wrap value in Prioritised (Low)
wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- wrap value in Stream
wrapStream      :: a -> Stream a
wrapStream a = a :> wrapStream a

-- wrap value in List
wrapList        :: a -> List a
wrapList a = a :. Nil

-- wrap value in Fun
wrapFun         :: a -> Fun i a
wrapFun a = F (const a)

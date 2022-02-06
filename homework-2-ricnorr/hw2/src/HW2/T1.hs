module HW2.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  )
  where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e

infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a

infixr 5 :>

data List a = Nil | a :. List a

infixr 5 :.

data Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- map in option context
mapOption      :: (a -> b) -> (Option a -> Option b)
mapOption f option = case option of
                     None     -> None
                     (Some a) -> Some (f a)

-- map in pair context
mapPair        :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P x1 x2) = P (f x1) (f x2)

-- map in quad context
mapQuad        :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q x1 x2 x3 x4) = Q (f x1) (f x2) (f x3) (f x4)

-- map in annotated context
mapAnnotated   :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

-- map in except context
mapExcept      :: (a -> b) -> (Except e a -> Except e b)
mapExcept f exc = case exc of
                  Success a -> Success (f a)
                  Error e   -> Error e

-- map in prioritised context
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f prior = case prior of
                         (Low a)    -> Low (f a)
                         (Medium a) -> Medium (f a)
                         (High a)   -> High (f a)

-- map in stream context
mapStream      :: (a -> b) -> (Stream a -> Stream b)
mapStream f (x :> stream) = f x :> mapStream f stream

-- map in list context
mapList        :: (a -> b) -> (List a -> List b)
mapList f (x :. list) = f x :. mapList f list
mapList _ Nil         = Nil

-- map in Fun context
mapFun         :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F fun) = F (\x -> f (fun x ))

-- map in tree context
mapTree        :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf                     = Leaf
mapTree f (Branch  left val right) = Branch (mapTree f left) (f val) (mapTree f right)

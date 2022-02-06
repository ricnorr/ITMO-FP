module HW2.T4
  ( State (..)
  , eval
  , Expr (..)
  , Prim (..)
  , wrapState
  , secondOfAnnotated
  , firstOfAnnotated
  , mapState
  , joinState
  , modifyState
  )
  where

import Control.Monad (ap)
import HW2.T1 (Annotated(..), mapAnnotated)

-- context for evaluation, can be used for logging with result
data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState fun state = S (mapAnnotated fun . runS state)

-- wrap value in State context
wrapState :: a -> State s a
wrapState a = S (a :#)

-- return a from Annotated
secondOfAnnotated :: Annotated s a -> a
secondOfAnnotated (a :# _) = a

-- return s from Annotated
firstOfAnnotated :: Annotated s a -> s
firstOfAnnotated (_ :# s) = s

joinState :: State s (State s a) -> State s a
joinState state = S (\s -> let
                  annotated = runS state s
                  in runS (secondOfAnnotated annotated) (firstOfAnnotated annotated))


modifyState :: (s -> s) -> State s ()
modifyState func = S (\s ->
                        () :# func s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
     m >>= f = joinState (fmap f m)

-- math expression without constants
data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

-- math expression
data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)


instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

doActionBinary :: Expr -> Expr -> (Double -> Double -> b) -> (Double -> Double -> Prim Double) -> State [Prim Double] b
doActionBinary x1 x2 action constructor = do
                         res1 <- eval x1
                         res2 <- eval x2
                         modifyState ([constructor res1 res2] ++)
                         return $ res1 `action` res2

doActionUnary :: Expr -> (Double -> b) -> (Double -> Prim Double) -> State [Prim Double] b
doActionUnary x action constructor = do
                     res <- eval x
                     modifyState ([constructor res] ++)
                     return $ action res

-- evaluate expression, write logs and result in context
eval :: Expr -> State [Prim Double] Double
eval (Val a)          = pure a
eval (Op (Add x1 x2)) = doActionBinary x1 x2 (+) Add
eval (Op (Mul x1 x2)) = doActionBinary x1 x2 (*) Mul
eval (Op (Sub x1 x2)) = doActionBinary x1 x2 (-) Sub
eval (Op (Div x1 x2)) = doActionBinary x1 x2 (/) Div
eval (Op (Abs x1))    = doActionUnary x1 abs Abs
eval (Op (Sgn x1))    = doActionUnary x1 signum Sgn


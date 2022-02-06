module HW2.T5
  ( ExceptState (..)
  , EvaluationError (..)
  , eval
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  )
  where

import HW2.T1 (Annotated(..), Except(Error, Success), mapAnnotated, mapExcept)

import HW2.T2 (wrapExcept)

import HW2.T4 (Expr(..), Prim(..), firstOfAnnotated, secondOfAnnotated)

import Control.Monad (ap)

-- context for evaluation with error, result, logging
data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

convertExcept :: (a -> b) -> Except e1 (Annotated e2 a) -> Except e1 (Annotated e2 b)
convertExcept f = mapExcept (mapAnnotated f)

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f exceptState = ES (\s -> let
                               except = runES exceptState s
                               in convertExcept f except
                               )

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> wrapExcept (a :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState state = ES (\s ->
                            let except = runES state s
                            in case except of
                            Success annotated -> runES (secondOfAnnotated annotated) (firstOfAnnotated annotated)
                            Error e   -> Error e
                            )

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (():# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
    fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

-- stores error, which could appear during evaluation
data EvaluationError = DivideByZero

doActionBinary :: Expr -> Expr -> (Double -> Double -> Double) -> (Double -> Double -> Prim Double) -> ExceptState EvaluationError [Prim Double] Double
doActionBinary x1 x2 action constructor = do
                         res1 <- eval x1
                         res2 <- eval x2
                         modifyExceptState ([constructor res1 res2] ++)
                         return $ res1 `action` res2

doActionUnary :: Expr -> (Double -> Double) -> (Double -> Prim Double) -> ExceptState EvaluationError [Prim Double] Double
doActionUnary x action constructor = do
                     res <- eval x
                     modifyExceptState ([constructor res] ++)
                     return $ action res

-- put expression in evaluation context, with logging, result, error
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val a)               = pure a
eval (Op (Add x1 x2)) = doActionBinary x1 x2 (+) Add
eval (Op (Mul x1 x2)) = doActionBinary x1 x2 (*) Mul
eval (Op (Sub x1 x2)) = doActionBinary x1 x2 (-) Sub
eval (Op (Div x1 x2)) = do
  res1 <- eval x1
  res2 <- eval x2
  modifyExceptState ([Div res1 res2] ++)
  if res2 == 0
    then throwExceptState DivideByZero
  else
    return $ res1 / res2
eval (Op (Abs x1))         = doActionUnary x1 abs Abs
eval (Op (Sgn x1))         = doActionUnary x1 signum Sgn

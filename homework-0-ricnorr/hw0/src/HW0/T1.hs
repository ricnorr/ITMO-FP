{-# LANGUAGE TypeOperators #-}
module HW0.T1
  ( assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  , type (<->) (Iso)
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)      = (Left a, Left a)
distrib (Right (b,c)) = (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso isoEitherFunc isoEitherReverseFunc
  where
    isoEitherFunc :: Either a (Either b b1) -> Either (Either a b) b1
    isoEitherFunc (Left x)          = Left (Left x)
    isoEitherFunc (Right (Right x)) = Right x
    isoEitherFunc (Right (Left x))  = Left (Right x)
    isoEitherReverseFunc (Left (Left x))  = Left x
    isoEitherReverseFunc (Left (Right x)) = Right (Left x)
    isoEitherReverseFunc (Right x)        = Right (Right x)

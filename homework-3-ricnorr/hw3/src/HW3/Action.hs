module HW3.Action
  ( HIO(..)
  , HiPermission(..)
  , PermissionException(..)
  ) where
import Control.Exception.Base (Exception)
import Control.Monad (ap, liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Set (Set)
data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime deriving (Show, Eq, Ord, Bounded, Enum)

data PermissionException = PermissionRequired HiPermission deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap = liftM

instance Applicative HIO where
  pure = return
  (<*>) = ap

instance Monad HIO where
  (>>=) = \monad fun -> HIO (\set -> do
      val <- runHIO monad set
      let valAfterFun = fun val
      runHIO valAfterFun set)
  return x = HIO (\_ -> return x)

instance MonadIO HIO where
  liftIO a = HIO (const a)

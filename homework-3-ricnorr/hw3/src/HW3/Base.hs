{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HW3.Base
  ( HiFun (..)
  , HiValue (..)
  , HiExpr (..)
  , HiError (..)
  , HiAction (..)
  , HiMonad (..)
  ) where
import Codec.Serialise (Serialise)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString, readFile, writeFile)
import Data.Map (Map)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set, member)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import HW3.Action
  (HIO(HIO), HiPermission(AllowRead, AllowTime, AllowWrite),
  PermissionException(PermissionRequired))
import System.Directory
  (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Generic, Ord)

instance Serialise HiFun

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text deriving (Show, Eq, Generic, Ord)

instance Serialise HiAction

data HiValue =
    HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Generic, Ord)

instance Serialise HiValue

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq, Generic)

instance Serialise HiExpr

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance HiMonad HIO where
  runAction (HiActionMkDir dirName) = do
    handleWrite
    liftIO $ createDirectory dirName
    return HiValueNull
  runAction (HiActionChDir dirname) = do
    handleRead
    liftIO $ setCurrentDirectory dirname
    return HiValueNull
  runAction (HiActionRead name) = do
    handleRead
    flag <- liftIO $ doesDirectoryExist name
    if flag then
      fmap (HiValueList . Seq.fromList . map (HiValueString . Data.Text.pack)) (liftIO $ listDirectory name)
    else
      liftIO $ readDecodeOrNull name
  runAction (HiActionWrite path str) = do
    handleWrite
    HiValueNull <$ liftIO (Data.ByteString.writeFile path str)
  runAction HiActionCwd = do
    handleRead
    HiValueString . Data.Text.pack <$> liftIO getCurrentDirectory
  runAction HiActionNow = do
    handleTime
    time <- liftIO getCurrentTime
    return $ HiValueTime time
  runAction (HiActionRand l r) = do
    rand <- liftIO $ getStdRandom $ uniformR (l, r)
    return $ HiValueNumber (fromIntegral rand)
  runAction (HiActionEcho text) = do
    handleWrite
    liftIO $ putStrLn (unpack text)
    return HiValueNull

-- check permission for read op
handleRead :: HIO ()
handleRead = do
  flag <- contains (member AllowRead)
  if flag then return () else liftIO throwReadRequired
  where
  throwReadRequired :: IO a
  throwReadRequired = throwIO $ PermissionRequired AllowRead

-- check permission for write op
handleWrite :: HIO ()
handleWrite = do
  flag <- contains (member AllowWrite)
  if flag then return () else liftIO throwWriteRequired
  where
  throwWriteRequired :: IO a
  throwWriteRequired = throwIO $ PermissionRequired AllowWrite

-- check permission for time op
handleTime :: HIO ()
handleTime = do
  flag <- contains (member AllowTime)
  if flag then return () else liftIO throwTimeRequired
  where
  throwTimeRequired :: IO a
  throwTimeRequired = throwIO $ PermissionRequired AllowTime

contains :: (Set HiPermission -> a) -> HIO a
contains func  = HIO (return . func)

-- decode utf8, if success, return decoded, if failed, return bytestring
readDecodeOrNull :: FilePath -> IO HiValue
readDecodeOrNull fileName = do
  text <- Data.ByteString.readFile fileName
  let decodedText = decodeUtf8' text
  case decodedText of
    Right result -> return $ HiValueString result
    _ -> return $ HiValueBytes text

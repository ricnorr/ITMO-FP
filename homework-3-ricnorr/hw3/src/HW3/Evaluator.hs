{-# LANGUAGE OverloadedStrings #-}
module HW3.Evaluator where
import Codec.Compression.Zlib
  (CompressParams(compressLevel), bestCompression, compressWith, decompress, defaultCompressParams)
import Codec.Serialise (deserialise, serialise)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.ByteString as BS (ByteString, append, drop, index, length, pack, reverse, take, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import qualified Data.Functor
import Data.Map (Map, adjust, elems, empty, foldrWithKey, insert, keys, member)
import qualified Data.Map
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup(stimes))
import Data.Sequence as Seq (Seq, fromList, index, length, reverse, (><), (|>))
import Data.String (IsString(fromString))
import Data.Text as T
  (Text, concat, drop, index, length, pack, reverse, singleton, strip, take, toLower, toUpper,
  unpack)
import Data.Text.Encoding as E (decodeUtf8', encodeUtf8)
import Data.Time as Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Word as W (Word8)
import HW3.Base (HiAction(..), HiError(..), HiExpr(..), HiFun(..), HiMonad(runAction), HiValue(..))
import Text.Read (readMaybe)

-- evaluate HiExpr
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (eval' expr)

-- evaluate HiExpr in ExceptT
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprRun x) = applyAction x
eval' (HiExprValue (HiValueAction x)) = return (HiValueAction x)
eval' (HiExprDict dict) = unwrapList dict Data.Functor.<&> (HiValueDict . Data.Map.fromList)
  where
    unwrapList :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m [(HiValue, HiValue)]
    unwrapList [] = return []
    unwrapList ((key,val) : tailList) = do
      keyEvaluated <- eval' key
      valEvaluated <- eval' val
      tailEvaluated <- unwrapList tailList
      return ((keyEvaluated, valEvaluated) : tailEvaluated)
eval' (HiExprValue (HiValueNumber val)) = return (HiValueNumber val)
eval' (HiExprValue HiValueNull) = return HiValueNull
eval' (HiExprValue (HiValueFunction fun)) = return (HiValueFunction fun)
eval' (HiExprValue (HiValueBytes bytes)) = return (HiValueBytes bytes)
eval' (HiExprValue (HiValueBool bool)) = return (HiValueBool bool)
eval' (HiExprValue (HiValueString string)) = return (HiValueString string)
eval' (HiExprApply (HiExprValue (HiValueFunction HiFunIf)) args) = applyFunLazy HiFunIf args
eval' (HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) args) = applyFunLazy HiFunAnd args
eval' (HiExprApply (HiExprValue (HiValueFunction HiFunOr)) args) = applyFunLazy HiFunOr args
eval' (HiExprApply (HiExprValue (HiValueFunction fun)) args) = mapM eval' args >>= applyFun fun
eval' (HiExprApply (HiExprDict dict) args) = do
    dictValue <- eval' (HiExprDict dict)
    mapM eval' args >>= applyValue dictValue
eval' (HiExprApply (HiExprValue x) lst) = mapM eval' lst >>= applyValue x
eval' (HiExprApply (HiExprApply inFun inArg) outArg) = do
    res <- eval' (HiExprApply inFun inArg)
    eval' (HiExprApply (HiExprValue res) outArg)
eval' (HiExprApply (HiExprRun action) args) = do
  action' <- eval' action
  eval' (HiExprApply (HiExprValue action') args)
eval' _ = throwE HiErrorInvalidFunction

-- evaluate HiValueAction
applyAction :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
applyAction (HiExprValue (HiValueAction action)) =
  lift $ runAction action
applyAction expr = do
  evaluated <- eval' expr
  case evaluated of
    HiValueAction action -> applyAction (HiExprValue (HiValueAction action))
    _ -> throwE HiErrorInvalidArgument

-- evaluate 'lazy' funs (if, or, and)
applyFunLazy :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
applyFunLazy fun args =
  if Prelude.length args `notElem` getArgLenForFunction fun
  then throwE HiErrorArityMismatch
  else evalFunLazy fun args
  where
    evalFunLazy :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
    evalFunLazy HiFunIf [x, y, z] = do
      x' <- eval' x
      case x' of
        HiValueBool bool -> if bool then eval' y else eval' z
        _ -> throwE HiErrorInvalidArgument
    evalFunLazy HiFunAnd [x, y] = do
      x' <- eval' x
      case x' of
        HiValueBool False -> return x'
        HiValueNull -> return x'
        _ -> eval' y
    evalFunLazy HiFunOr [x, y] = do
      x' <- eval' x
      case x' of
        HiValueBool False -> eval' y
        HiValueNull -> eval' y
        _ -> return x'
    evalFunLazy _ _ = throwE HiErrorInvalidArgument

-- check arity, evaluate not lazy fun
applyFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
applyFun fun args =
    if Prelude.length args `notElem` getArgLenForFunction fun
    then throwE HiErrorArityMismatch
    else evalFun fun args

-- check arity, evaluate value as function
applyValue :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
applyValue value args = do
    possibleArgLens <- getArgLenForValue value
    if Prelude.length args `notElem` possibleArgLens
    then throwE HiErrorArityMismatch
    else evalValue value args

-- evaluate function
evalFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalFun HiFunAdd [HiValueNumber x, HiValueNumber y] = return $ HiValueNumber $ x + y
evalFun HiFunAdd [HiValueString x, HiValueString y] = return $ HiValueString $ T.concat [x, y]
evalFun HiFunAdd [HiValueList x, HiValueList y] = return $ HiValueList $ x >< y
evalFun HiFunAdd [HiValueBytes x, HiValueBytes y] = return $ HiValueBytes $ x `append` y
evalFun HiFunAdd [HiValueTime x, HiValueNumber y] = evalOperatorHelper' y (fromInteger . fromIntegral) x HiValueTime addUTCTime
evalFun HiFunAdd [HiValueNumber y, HiValueTime x] = evalFun HiFunAdd [HiValueTime x, HiValueNumber y]
evalFun HiFunSub [HiValueNumber x, HiValueNumber y] = return $ HiValueNumber $ x - y
evalFun HiFunSub [HiValueTime x, HiValueTime y] = return $ HiValueNumber (realToFrac $ diffUTCTime x y)
evalFun HiFunDiv [HiValueString x, HiValueString y] = return $ HiValueString $ T.concat [x, "/", y]
evalFun HiFunMul [HiValueNumber x, HiValueNumber y] = return $ HiValueNumber $ x * y
evalFun HiFunMul [HiValueString x, HiValueNumber y] = evalOperatorHelper y x HiValueString stimes
evalFun HiFunMul [HiValueNumber y, HiValueString x] = evalFun HiFunMul [HiValueString x, HiValueNumber y]
evalFun HiFunMul [HiValueList y, HiValueNumber x] = evalOperatorHelper x y HiValueList stimes
evalFun HiFunMul [HiValueNumber y, HiValueList x] = evalFun HiFunMul [HiValueList x, HiValueNumber y]
evalFun HiFunMul [HiValueBytes y, HiValueNumber x] = evalOperatorHelper x y HiValueBytes stimes
evalFun HiFunMul [HiValueNumber y, HiValueBytes x] = evalFun HiFunMul [HiValueBytes x, HiValueNumber y]
evalFun HiFunNot [HiValueBool x] = return $ HiValueBool $  not x
evalFun HiFunAnd [x, y] = case x of
        HiValueBool False -> return x
        HiValueNull -> return x
        _ -> return y
evalFun HiFunOr [x, y] = case x of
        HiValueBool False -> return y
        HiValueNull -> return y
        _ -> return x
evalFun HiFunEquals [x, y] = return $ HiValueBool $ x == y
evalFun HiFunNotEquals [x, y] = return $ HiValueBool $ x /= y
evalFun HiFunLessThan [HiValueNumber x, HiValueNumber y] = return $ HiValueBool $ x < y
evalFun HiFunLessThan [HiValueFunction x, HiValueFunction y] = return $ HiValueBool $ x < y
evalFun HiFunLessThan [HiValueBool x, HiValueBool y] = return $ HiValueBool $ x < y
evalFun HiFunLessThan [HiValueBool _, HiValueNumber _] = return $ HiValueBool True
evalFun HiFunLessThan [HiValueNumber _, HiValueBool _] = return $ HiValueBool False
evalFun HiFunNotLessThan lst =  applyManyEvaluators [HiFunLessThan, HiFunNot] lst
evalFun HiFunNotGreaterThan lst = applyManyEvaluators [HiFunGreaterThan, HiFunNot] lst
evalFun HiFunLength [HiValueString str] = return $ HiValueNumber $ fromIntegral $ T.length str
evalFun HiFunLength [HiValueList list] = return $ HiValueNumber $ fromIntegral $ Seq.length list
evalFun HiFunLength [HiValueBytes list] = return $ HiValueNumber $ fromIntegral $ BS.length list
evalFun HiFunToUpper [HiValueString str] = return $ HiValueString $ toUpper str
evalFun HiFunToLower [HiValueString str] = return $ HiValueString $ toLower str
evalFun HiFunReverse [HiValueString str] = return $ HiValueString $ T.reverse str
evalFun HiFunReverse [HiValueList list] = return $ HiValueList $ Seq.reverse list
evalFun HiFunReverse [HiValueBytes bytes] = return $ HiValueBytes $ BS.reverse bytes
evalFun HiFunTrim [HiValueString str] = return $ HiValueString $ strip str
evalFun HiFunList lst = return $ HiValueList $ fromList lst
evalFun HiFunRange [HiValueNumber l, HiValueNumber r] = return $ HiValueList $ Seq.fromList (map HiValueNumber [l..r])
evalFun HiFunDiv [HiValueNumber x, HiValueNumber y] =
  if y == 0
  then throwE HiErrorDivideByZero
  else return $ HiValueNumber $ x / y
evalFun HiFunFold (HiValueFunction fun : [HiValueList list]) = if null list then return HiValueNull else foldl1 (\a b ->
  do
    a' <- a
    b' <- b
    evalFun fun [a', b']) (fmap return list)
evalFun HiFunGreaterThan [x, y] = do
  notLess <- evalFun HiFunNotLessThan [x,y]
  notEq <- evalFun HiFunNotEquals [x, y]
  applyFunLazy HiFunAnd [HiExprValue notLess, HiExprValue notEq]
evalFun HiFunPackBytes [HiValueList list] = packBytesHelper list
  where
    packBytesHelper :: HiMonad m => Seq HiValue -> ExceptT HiError m HiValue
    packBytesHelper seqf = do
      words8 <- mapM validatePackHelper (toList seqf)
      return $ HiValueBytes $ BS.pack words8
    validatePackHelper :: HiMonad m => HiValue -> ExceptT HiError m Word8
    validatePackHelper (HiValueNumber x) = do
      x' <- getOptionalIntFromRatio x
      return $ fromIntegral x'
    validatePackHelper _ = throwE HiErrorInvalidArgument
evalFun HiFunUnpackBytes [HiValueBytes bytes] = return $ HiValueList (fromList $ map (HiValueNumber . fromIntegral) (BS.unpack bytes))
evalFun HiFunEncodeUtf8 [HiValueString string] = return $ HiValueBytes $ encodeUtf8 string
evalFun HiFunDecodeUtf8 [HiValueBytes bytes] = do
  let decoded = decodeUtf8' bytes
  case decoded of
      Right y -> return $ HiValueString y
      _  -> return HiValueNull
evalFun HiFunZip [HiValueBytes bytes] = return $ HiValueBytes $
  toStrict (compressWith defaultCompressParams { compressLevel = bestCompression} (fromStrict bytes))
evalFun HiFunUnzip [HiValueBytes bytes] = return $ HiValueBytes $ toStrict (decompress (fromStrict bytes))
evalFun HiFunSerialise [x] = return $ HiValueBytes $ toStrict $ serialise x
evalFun HiFunDeserialise [HiValueBytes bytes] = return $ deserialise (fromStrict bytes)
evalFun HiFunRead [HiValueString name] = return $   HiValueAction $ HiActionRead (T.unpack name)
evalFun HiFunWrite [HiValueString name, HiValueString text] = return $ HiValueAction $ HiActionWrite
  (T.unpack name) (fromString (T.unpack text))
evalFun HiFunWrite [HiValueString name, HiValueBytes bytes] = return $ HiValueAction $ HiActionWrite (T.unpack name) bytes
evalFun HiFunMkDir [HiValueString name] = return $ HiValueAction $ HiActionMkDir (T.unpack name)
evalFun HiFunChDir [HiValueString name] = return $ HiValueAction $ HiActionChDir (T.unpack name)
evalFun HiFunParseTime [HiValueString time] = do
    let maybeTime = readMaybe (T.unpack time) :: Maybe UTCTime
    case maybeTime of
        Nothing -> return HiValueNull
        Just parsedTime -> return $ HiValueTime parsedTime
evalFun HiFunRand  [HiValueNumber l, HiValueNumber r] = do
    lInt <- getOptionalIntFromRatio l
    rInt <- getOptionalIntFromRatio r
    return $ HiValueAction $ HiActionRand lInt rInt
evalFun HiFunEcho [HiValueString string] = return $ HiValueAction $ HiActionEcho string
evalFun HiFunKeys [HiValueDict dict] = return $ HiValueList (fromList $ keys dict)
evalFun HiFunValues [HiValueDict dict] = return $ HiValueList (fromList $ elems dict)
evalFun HiFunCount [HiValueString str] = return $ HiValueDict $ countHelper (\it -> HiValueString (T.pack [it])) (T.unpack str)
evalFun HiFunCount [HiValueBytes bytes] = return $ HiValueDict $ countHelper (HiValueNumber . fromIntegral) (BS.unpack bytes)
evalFun HiFunCount [HiValueList list] = return $ HiValueDict $ countHelper id list
evalFun HiFunInvert [HiValueDict dict] = return $ HiValueDict $ foldrWithKey invertHelper empty dict
  where
    invertHelper :: HiValue -> HiValue -> Map HiValue HiValue -> Map HiValue HiValue
    invertHelper a b c =  if member b c then adjust (addToList a) b c else insert b (HiValueList $ fromList [a]) c
    addToList :: HiValue -> HiValue -> HiValue
    addToList el (HiValueList lst) = HiValueList $ lst |> el
    addToList _ _ = error "not a list"
evalFun _ _ = throwE HiErrorInvalidArgument

-- evaluate value as function
evalValue :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
evalValue (HiValueString str) [HiValueNumber ind] = indexHelper ind T.length str HiValueString (\a b -> T.singleton $ T.index a b)
evalValue (HiValueList list) [HiValueNumber ind] = indexHelper ind Seq.length list id Seq.index
evalValue (HiValueBytes bytes) [HiValueNumber ind] = indexHelper ind BS.length bytes (HiValueNumber . fromIntegral) BS.index
evalValue (HiValueString str) arg = sliceHelper arg HiValueString substring str (fromIntegral . T.length)
  where
    substring :: Text -> Int -> Int -> Text
    substring text l r = subseq text l r T.length T.take T.drop
evalValue (HiValueList list) arg = sliceHelper arg (HiValueList . Seq.fromList) sublist' (toList list) (fromIntegral . Prelude.length)
  where
    sublist' :: [a] -> Int -> Int -> [a]
    sublist' lst l r = subseq lst l r Prelude.length Prelude.take Prelude.drop
evalValue (HiValueBytes list) lst = sliceHelper lst HiValueBytes subBytesString list (fromIntegral . BS.length)
  where
    subBytesString :: BS.ByteString -> Int -> Int -> BS.ByteString
    subBytesString bytes l r = subseq bytes l r BS.length BS.take BS.drop
evalValue (HiValueDict dict) [val] = do
  let maybeValue = Data.Map.lookup val dict
  case maybeValue of
    Just found -> return found
    Nothing -> return HiValueNull
evalValue _ _  = throwE HiErrorInvalidArgument

-- helper function for indexing in list, bytestring, string
indexHelper :: HiMonad m => Rational -> (a -> Int) -> a -> (b -> HiValue) -> (a -> Int -> b) -> ExceptT HiError m HiValue
indexHelper ind len struct constructor fun = do
    int <- getOptionalIntFromRatio ind
    if int >= len struct || int < 0
    then return HiValueNull
    else return $ constructor $ fun struct int

-- helper function for slicing in list, bytestring, string
sliceHelper :: HiMonad m => [HiValue] -> (a -> HiValue) -> (a -> Int -> Int -> a) -> a -> (a -> Rational) -> ExceptT HiError m HiValue
sliceHelper [HiValueNull, x] create slice struct len = sliceHelper [HiValueNumber 0,x] create slice struct len
sliceHelper [x, HiValueNull] create slice struct len = sliceHelper [x, HiValueNumber $ len struct] create slice struct len
sliceHelper [HiValueNumber lInd, HiValueNumber rInd] create slice struct _ = do
  lIntInd <- getOptionalIntFromRatio lInd
  rIntInd <- getOptionalIntFromRatio rInd
  return $ create $ slice struct lIntInd rIntInd
sliceHelper _ _ _ _ _ = throwE HiErrorInvalidArgument

-- helper function for count operation
countHelper :: (Foldable t1, Ord k) => (t2 -> k) -> t1 t2 -> Map k HiValue
countHelper mapEl = foldl (\container element ->
                                   if member (mapEl element) container
                                   then adjust incNumber (mapEl element) container
                                   else insert (mapEl element) (HiValueNumber 1) container) empty
                           where
                            incNumber :: HiValue -> HiValue
                            incNumber (HiValueNumber x) = HiValueNumber $ x + 1
                            incNumber _ = error "not number incremented"

-- helper function for evaluating operation with hi-structure and number (with mapping number)
evalOperatorHelper :: HiMonad m => Rational -> t1 -> (t2 -> b) -> (Int -> t1 -> t2) -> ExceptT HiError m b
evalOperatorHelper x = evalOperatorHelper' x id

-- helper function for evaluating operation with hi-structure and number (without mapping number)
evalOperatorHelper' :: HiMonad m => Rational -> (Int -> t3) -> t1 -> (t2 -> b) -> (t3 -> t1 -> t2) -> ExceptT HiError m b
evalOperatorHelper' x mapInt y constructor action = do
                        intX <- getOptionalIntFromRatio x
                        intXValidated <- validateIntIsPositive intX
                        let intX' = mapInt intXValidated
                        return $ constructor $ action intX' y
  where
    validateIntIsPositive :: HiMonad m => Int -> ExceptT HiError m Int
    validateIntIsPositive num = if x > 0 then return num else throwE HiErrorInvalidArgument

-- helper function for slicing
subseq :: a -> Int -> Int -> (a -> Int) -> (Int -> a -> a) -> (Int -> a -> a) ->  a
subseq text l r lenFun takeFun dropFun
  | l < 0 && abs l >= len = subseq text 0 r lenFun takeFun dropFun
  | r < 0 && abs r >= len = subseq text l 0 lenFun takeFun dropFun
  | l < 0 = subseq text (len + l) r lenFun takeFun dropFun
  | r < 0 = subseq text l (lenFun text + r) lenFun takeFun dropFun
  | otherwise = dropFun l (takeFun r text)
  where
    len = lenFun text

-- helper function for casting rational to int, if fail - throws HiErrorInvalidArgument
getOptionalIntFromRatio :: HiMonad m => Rational -> ExceptT HiError m Int
getOptionalIntFromRatio x =
    if mod num den == 0 && (division >= toInteger (minBound :: Int)) && (division <= toInteger (maxBound :: Int))
    then return (fromInteger (div num den))
    else throwE HiErrorInvalidArgument
    where num = numerator x
          den = denominator x
          division = div num den

-- apply many functions to list of values
applyManyEvaluators :: HiMonad m => [HiFun] -> [HiValue] -> ExceptT HiError m HiValue
applyManyEvaluators [fun] args = evalFun fun args
applyManyEvaluators (fun : funTail) vals = evalFun fun vals >>= (\x -> applyManyEvaluators funTail [x])
applyManyEvaluators _ _ = error "can't be applied to no funs"

-- arity for value if used as function
getArgLenForValue :: HiMonad m => HiValue -> ExceptT HiError m [Int]
getArgLenForValue (HiValueString _) = return [1,2]
getArgLenForValue (HiValueBytes _) = return [1, 2]
getArgLenForValue (HiValueList _) = return [1,2]
getArgLenForValue (HiValueDict _) = return [1]
getArgLenForValue _ = throwE HiErrorInvalidFunction

-- arity for function
getArgLenForFunction :: HiFun -> [Int]
getArgLenForFunction HiFunAdd = [2]
getArgLenForFunction HiFunDiv = [2]
getArgLenForFunction HiFunSub = [2]
getArgLenForFunction HiFunMul = [2]
getArgLenForFunction HiFunNot = [1]
getArgLenForFunction HiFunOr = [2]
getArgLenForFunction HiFunAnd = [2]
getArgLenForFunction HiFunGreaterThan = [2]
getArgLenForFunction HiFunNotGreaterThan = [2]
getArgLenForFunction HiFunLessThan  = [2]
getArgLenForFunction HiFunNotLessThan = [2]
getArgLenForFunction HiFunEquals = [2]
getArgLenForFunction HiFunNotEquals = [2]
getArgLenForFunction HiFunIf = [3]
getArgLenForFunction HiFunLength = [1]
getArgLenForFunction HiFunToUpper = [1]
getArgLenForFunction HiFunToLower = [1]
getArgLenForFunction HiFunReverse = [1]
getArgLenForFunction HiFunTrim = [1]
getArgLenForFunction HiFunList = [0..]
getArgLenForFunction HiFunFold = [2]
getArgLenForFunction HiFunRange = [2]
getArgLenForFunction HiFunPackBytes = [1]
getArgLenForFunction HiFunUnpackBytes = [1]
getArgLenForFunction HiFunEncodeUtf8 = [1]
getArgLenForFunction HiFunDecodeUtf8 = [1]
getArgLenForFunction HiFunZip = [1]
getArgLenForFunction HiFunUnzip = [1]
getArgLenForFunction HiFunSerialise = [1]
getArgLenForFunction HiFunDeserialise = [1]
getArgLenForFunction HiFunMkDir = [1]
getArgLenForFunction HiFunRead = [1]
getArgLenForFunction HiFunWrite = [2]
getArgLenForFunction HiFunChDir = [1]
getArgLenForFunction HiFunParseTime = [1]
getArgLenForFunction HiFunRand = [2]
getArgLenForFunction HiFunEcho = [1]
getArgLenForFunction HiFunKeys = [1]
getArgLenForFunction HiFunValues = [1]
getArgLenForFunction HiFunCount = [1]
getArgLenForFunction HiFunInvert = [1]

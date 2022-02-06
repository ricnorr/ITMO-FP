{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( runP
  , pAbbr
  , ParseError (..)
  , parseExpr
  )
  where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus, mfilter)
import Data.Char (digitToInt)
import qualified Data.Char
import Data.Scientific (scientific, toRealFloat)
import GHC.Natural (Natural)

import HW2.T1 (Annotated(..), Except(..))
import HW2.T4 (Expr(..), Prim(..))
import HW2.T5 (ExceptState(ES, runES))

-- Stores position, where parser failed
data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- parse string, returns result or error
runP :: Parser a -> String -> Except ParseError a
runP (P exceptState) string = let except = runES exceptState (0, string)
                              in case except of
                                 Success (a :# _)     -> Success a
                                 Error x -> Error x

-- read symbol from string, put in state (position+1, tail of string)
-- if string is empty - put in state error
pChar :: Parser Char
pChar = P $ ES \(pos, s) -> case s of
                    []     -> Error (ErrorAtPos pos)
                    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P a) <|> (P b) =  P $ ES \(pos, s) -> case runES a (pos, s) of
                                         Error _ -> runES b (pos, s)
                                         x -> x

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES \(pos, s) -> case s of
                []     -> Success (() :# (pos, s))
                _ -> Error (ErrorAtPos pos)

-- parse UpperCase chars from the begging, fails if not all chars are UpperCase
pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  pure abbr

pCharEq :: Char -> Parser Char
pCharEq char = mfilter (== char) pChar

pStart :: Parser Expr
pStart = do
  expr <- pExpr
  pEof
  pure expr

pExpr :: Parser Expr
pExpr = do
  pSpaces
  expr <- pTerm
  pExpr' expr

pExpr' :: Expr -> Parser Expr
pExpr' expr = pSpaces *> (pExpr'helper '+' Add <|> pExpr'helper '-' Sub <|> (do pure expr))
  where
  pExpr'helper :: Char -> (Expr -> Expr -> Prim Expr) -> Parser Expr
  pExpr'helper char constructor = do
    _ <- pCharEq char
    term <- pTerm
    pExpr' (Op (constructor expr term))

pTerm :: Parser Expr
pTerm = do
  pSpaces
  expr <- pFactor
  pTerm' expr

pTerm' :: Expr -> Parser Expr
pTerm' expr = pSpaces *> (pTerm'helper '*' Mul <|> pTerm'helper '/' Div <|> (do pure expr))
  where
  pTerm'helper :: Char -> (Expr -> Expr -> Prim Expr) -> Parser Expr
  pTerm'helper char constructor = do
    _ <- pCharEq char
    factor <- pFactor
    pTerm' (Op (constructor expr factor))

pFactor :: Parser Expr
pFactor = pSpaces *> (pBrackets <|> pVal)

pBrackets :: Parser Expr
pBrackets = do
    pSpaces
    _ <- pCharEq '('
    val <- pExpr
    pSpaces
    _ <- pCharEq ')'
    pure val

pSpaces :: Parser ()
pSpaces = do
  _ <- many (mfilter Data.Char.isSpace pChar)
  pure ()

strToNum :: [Char] -> Integer
strToNum s = toInteger (foldl (\acc el -> acc * 10 + digitToInt el) 0 s)

strToDouble :: Integer -> Int -> Double
strToDouble number shift = toRealFloat $ scientific number shift

pVal :: Parser Expr
pVal = parseWithDot <|> parseWithoutDot where
    parseWithDot :: Parser Expr
    parseWithDot =  do
      pSpaces
      beforePoint <- some (mfilter Data.Char.isDigit pChar)
      _ <- pCharEq '.'
      afterPoint <- some (mfilter Data.Char.isDigit pChar)
      pure $ Val (strToDouble (strToNum $ beforePoint ++ afterPoint) (-1 * length afterPoint))
    parseWithoutDot :: Parser Expr
    parseWithoutDot = do
      pSpaces
      number <- some (mfilter Data.Char.isDigit pChar)
      pure $ Val (fromIntegral $ strToNum number)

-- parse String to math expression
parseExpr :: String -> Except ParseError Expr
parseExpr = runP pStart

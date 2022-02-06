{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module HW3.Parser (parse) where
import Control.Monad.Combinators (sepBy)
import Control.Monad.Combinators.Expr (Operator(InfixL, InfixN, InfixR, Postfix), makeExprParser)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Char as Char
import Data.List (intercalate)
import Data.Map ((!))
import Data.Scientific (Scientific)
import Data.Text (pack)
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction(..), HiExpr(..), HiFun(..), HiValue(..))
import HW3.Util (funToStringBijection)
import Text.Megaparsec
  (MonadParsec(eof, notFollowedBy), ParseErrorBundle, Parsec, between, choice, empty, many,
  manyTill, runParser, satisfy, sepBy1, some, try, (<|>))
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser =  Parsec Void String

-- consume spaces
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- use parser, and consume space after it
lexema :: Parser a -> Parser a
lexema = L.lexeme spaceConsumer

-- komma parser
kommaP :: Parser Char
kommaP = lexema $ char ','

-- base parser for function name
funBaseP :: HiFun -> Parser HiExpr
funBaseP fun = HiExprValue . HiValueFunction <$> (fun  <$ lexema (string (funToStringBijection ! fun)))

-- work with lists parser
listTerms :: Parser HiExpr
listTerms = choice [funBaseP HiFunList, funBaseP HiFunRange, funBaseP HiFunFold, listElementsInlineP]
  where
  listElementsInlineP = HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> brackets (sepBy hiP kommaP)
  brackets = between (lexema $ char '[') (lexema $ char ']')

-- work with strings parser
stringFunTerms :: Parser HiExpr
stringFunTerms = choice [funBaseP HiFunLength, funBaseP HiFunToUpper,
  funBaseP HiFunToLower, funBaseP HiFunReverse, funBaseP HiFunTrim, stringP]
  where
  stringP :: Parser HiExpr
  stringP = (HiExprValue <$> HiValueString) . pack <$> lexema (char '\"' *> manyTill L.charLiteral (char '\"'))

-- null parser
nullTerm :: Parser HiExpr
nullTerm = HiExprValue HiValueNull <$ lexema (string "null")

-- work with dicts parser
dictTerms :: Parser HiExpr
dictTerms = choice [funBaseP HiFunCount, funBaseP HiFunKeys, funBaseP HiFunValues,
  funBaseP HiFunInvert,
  dictInlineP]
  where
  dictInlineP :: Parser HiExpr
  dictInlineP = HiExprDict <$> between leftCurlyBracket rightCurlyBracket (sepBy dictPairP kommaP)
  dictPairP :: Parser (HiExpr, HiExpr)
  dictPairP = do
    left <- hiP
    lexema $ char ':'
    right <- hiP
    return (left, right)
  leftCurlyBracket :: Parser Char
  leftCurlyBracket = lexema $ char '{'
  rightCurlyBracket :: Parser Char
  rightCurlyBracket = lexema $ char '}'

-- work with numbers and arithmetics parser
numTerms :: Parser HiExpr
numTerms = choice [funBaseP HiFunAdd, funBaseP HiFunSub, funBaseP HiFunMul,
             funBaseP HiFunDiv, HiExprValue . HiValueNumber <$> (toRational <$> (scientificP <|> signedScientificP))]
  where
  signedScientificP :: Parser Scientific
  signedScientificP = L.signed spaceConsumer scientificP
  scientificP :: Parser Scientific
  scientificP = lexema L.scientific

-- work with boolean algebra parser
boolTerms :: Parser HiExpr
boolTerms = choice [funBaseP HiFunNot, funBaseP HiFunAnd, funBaseP HiFunOr,
                    funBaseP HiFunIf, falseOrTrueP]
  where
    falseOrTrueP :: Parser HiExpr
    falseOrTrueP = HiExprValue . HiValueBool <$> (True <$ lexema (string "true") <|> False <$ lexema (string "false"))

-- work with comparison parser
comparisonTerms :: Parser HiExpr
comparisonTerms = choice [funBaseP HiFunEquals, funBaseP HiFunNotEquals,
                          funBaseP HiFunLessThan, funBaseP HiFunNotLessThan,
                          funBaseP HiFunGreaterThan, funBaseP HiFunNotGreaterThan]

-- work with bytes parser
byteTerms :: Parser HiExpr
byteTerms = choice [funBaseP HiFunPackBytes, funBaseP HiFunUnzip, funBaseP HiFunZip,
            funBaseP HiFunEncodeUtf8, funBaseP HiFunDecodeUtf8,
            funBaseP HiFunDeserialise, funBaseP HiFunSerialise,
            funBaseP HiFunUnpackBytes,
            HiExprValue . HiValueBytes <$> (B.pack <$> between (lexema (string "[#")) (lexema (string "#]")) pBytes)]
            where
            pBytes :: Parser [Word8]
            pBytes = lexema $ do
              high <- hexDigitChar
              low <- hexDigitChar
              tailBytes <- (space1 >> pBytes) <|> return []
              return $ fromIntegral (Char.digitToInt high * 16 + Char.digitToInt low) : tailBytes
              <|> return []

-- work with io parser
ioTerms :: Parser HiExpr
ioTerms = choice [funBaseP HiFunRead, funBaseP HiFunWrite, funBaseP HiFunMkDir,
                  funBaseP HiFunEcho, funBaseP HiFunChDir, actionBaseP HiActionCwd "cwd",
                  funBaseP HiFunRand, funBaseP HiFunParseTime, actionBaseP HiActionNow "now"
                 ]

-- term parser
termP :: Parser HiExpr
termP = choice [numTerms, comparisonTerms, nullTerm, stringFunTerms, boolTerms,
                    byteTerms, listTerms, ioTerms, dictTerms, try $ parens termP]

-- apply arguments to term parser
postfixExprTailP :: Parser (HiExpr -> HiExpr)
postfixExprTailP = flip HiExprApply <$> lexema argumentsP where
    argumentsP :: Parser [HiExpr]
    argumentsP = parens (sepBy hiP kommaP)

-- apply dot to term parser
postfixExprDotP :: Parser (HiExpr -> HiExpr)
postfixExprDotP = flip HiExprApply <$> lexema dotP
  where
    dotP :: Parser [HiExpr]
    dotP = do
      term <- string "." >> (HiExprValue . HiValueString . pack) . intercalate "-" <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-')
      return [term]

-- wrap parser in "()"
parens :: Parser a -> Parser a
parens = between (lexema $ char '(') (lexema $ char ')')

-- parser for hi language (including operator notation)
hiP :: Parser HiExpr
hiP = makeExprParser (choice [termP, parens hiP]) operatorTable

-- base parser for actions
actionBaseP ::  HiAction -> String -> Parser HiExpr
actionBaseP action name = HiExprValue . HiValueAction <$> (action <$ lexema (string name))

-- infixL operator parser constructor
infixExprL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixExprL name f = InfixL (f <$ lexema (string name))

-- infixL operator parser constructor (with not followed by)
infixExprLNotFollowedBy :: String -> String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixExprLNotFollowedBy name except f = InfixL (f <$ lexema (try $ string name <* notFollowedBy (string except)))

-- infixR operator parser constructor
infixExprR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixExprR name f = InfixR (f <$ lexema (string name))

-- infixN operator parser constructor
infixExpr :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixExpr name f = InfixN (f <$ lexema (string name))

-- mapper for operator table
infixExprBinMapper :: HiFun -> HiExpr -> HiExpr -> HiExpr
infixExprBinMapper fun x y = HiExprApply (HiExprValue $ HiValueFunction fun) [x, y]

-- parser for (!), (.), function apply
postfixOps :: Parser (HiExpr -> HiExpr)
postfixOps = choice [postfixExprDotP, postfixExprTailP, HiExprRun <$ lexema (string "!")]

-- operator table for makeExprParser
operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ Postfix (foldr1 (flip (.)) <$> some postfixOps) ]
    , [ infixExprL "*" (infixExprBinMapper HiFunMul)
      , infixExprLNotFollowedBy "/" "=" (infixExprBinMapper HiFunDiv)
      ]
    , [ infixExprL "+" (infixExprBinMapper HiFunAdd)
      , infixExprL "-" (infixExprBinMapper HiFunSub)
      ]
    , [ infixExpr "<" (infixExprBinMapper HiFunLessThan)
      , infixExpr ">" (infixExprBinMapper HiFunGreaterThan)
      , infixExpr "<=" (infixExprBinMapper HiFunNotGreaterThan)
      , infixExpr ">=" (infixExprBinMapper HiFunNotLessThan)
      , infixExpr "==" (infixExprBinMapper HiFunEquals)
      , infixExpr "/=" (infixExprBinMapper HiFunNotEquals)
      ]
    , [ infixExprR "&&" (infixExprBinMapper HiFunAnd)]
    , [ infixExprR "||" (infixExprBinMapper HiFunOr)]
  ]

-- consume spaces, parse hi, check eof
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (spaceConsumer >> hiP <* eof) "log"

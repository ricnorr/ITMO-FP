module HW3.Pretty (prettyValue, prettyError, prettyParseError) where
import qualified Data.ByteString as B
import Data.Foldable (toList)
import Data.Map (toList, (!))
import Data.Ratio (denominator, numerator, (%))
import Data.Scientific
  (FPFormat(Fixed), formatScientific, fromRationalRepetendUnlimited, unsafeFromRational)
import Data.Void (Void)
import HW3.Base (HiAction(..), HiError(..), HiValue(..))
import HW3.Util (funToStringBijection)
import Prettyprinter
  (Doc, Pretty(pretty), braces, comma, concatWith, dquotes, rparen, space, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Text.Printf (printf)

-- print pretty HiValue
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber ratio) | mod num den == 0 = pretty (fromEnum ratio)
                                  | isFiniteRational ratio = pretty $ formatScientific Fixed Nothing (unsafeFromRational ratio)
                                  | abs num >= den = prettyMixedFraction ratio
                                  | otherwise = prettyFraction ratio
                                  where
                                    den = denominator ratio
                                    num = numerator ratio
prettyValue (HiValueAction (HiActionRand x y)) = pretty "rand(" <> pretty x <> comma <+> pretty y <> rparen
prettyValue (HiValueAction (HiActionRead x)) = pretty "read(" <> dquotes (pretty x) <> rparen
prettyValue (HiValueAction (HiActionChDir x)) = pretty "cd(" <> dquotes (pretty x) <> rparen
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction (HiActionEcho x)) = pretty "echo(" <> dquotes (pretty x) <> rparen
prettyValue (HiValueAction (HiActionMkDir x)) = pretty "mkdir(" <> dquotes (pretty x) <> rparen
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionWrite x y)) = pretty "write(" <> dquotes (pretty x) <> comma <+> prettyValue (HiValueBytes y) <> rparen
prettyValue (HiValueBool bool) = if bool then pretty "true" else pretty "false"
prettyValue (HiValueList lst) = pretty "[" <+> concatWith (\a b -> a <> pretty "," <+> b) (map prettyValue (Data.Foldable.toList lst)) <+> pretty "]"
prettyValue (HiValueBytes bytes) = pretty "[# " <> pretty (concatMap (printf "%02x ") (B.unpack bytes) :: String) <> pretty "#]"
prettyValue (HiValueString string) = viaShow string
prettyValue (HiValueTime time) = pretty "parse-time(\"" <> pretty (show time) <> pretty "\")"
prettyValue (HiValueFunction x) = pretty (funToStringBijection ! x)
prettyValue (HiValueDict dict) = braces (space <> concatWith (\a b -> a <> pretty "," <+> b)
  (map (\(a, b) -> prettyValue a <> pretty ":" <+> prettyValue b) (Data.Map.toList dict)) <> space)
prettyValue HiValueNull = pretty "null"

-- for printing pretty fraction
prettyFraction :: Rational -> Doc AnsiStyle
prettyFraction x = pretty (numerator x) <> pretty "/" <> pretty (denominator x)

-- for printing pretty mixed fraction
prettyMixedFraction :: Rational -> Doc AnsiStyle
prettyMixedFraction x = do
    let (division, remainder) = quotRem (numerator x) (denominator x)
    pretty division <+> prettyFractionWithLeadingPlus (remainder % denominator x)
  where
  prettyFractionWithLeadingPlus :: Rational -> Doc AnsiStyle
  prettyFractionWithLeadingPlus ratio
                                | numerator ratio < 0 = pretty '-' <+> prettyFraction (abs ratio)
                                | otherwise = pretty '+' <+> prettyFraction ratio

-- check that rational is finite
isFiniteRational :: Rational -> Bool
isFiniteRational ratio = do
  let (_, period) = fromRationalRepetendUnlimited ratio
  case period of
    Just _ -> False
    _ -> True

-- for printing pretty error
prettyError :: HiError  -> Doc AnsiStyle
prettyError x = pretty (show x)

-- for printing pretty parse error
prettyParseError :: ParseErrorBundle String Void -> Doc AnsiStyle
prettyParseError x = pretty (errorBundlePretty x)

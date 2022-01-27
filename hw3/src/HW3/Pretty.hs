module HW3.Pretty (
    prettyValue
    ) where

import Data.ByteString (ByteString, unpack)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Map (toList)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import GHC.Real (Ratio ((:%)))
import HW3.Base (HiAction (HiActionChDir, HiActionCwd, HiActionEcho, HiActionMkDir, HiActionNow, HiActionRand, HiActionRead, HiActionWrite),
                 HiFun (HiFunChDir, HiFunMkDir),
                 HiValue (HiValueAction, HiValueBool, HiValueBytes, HiValueDict, HiValueFunction, HiValueList, HiValueNull, HiValueNumber, HiValueString, HiValueTime))
import Prettyprinter (Doc, braces, comma, concatWith, enclose, pretty, punctuate, space, surround)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Symbols.Ascii (brackets)
import Text.Casing (fromHumps, toKebab)
import Text.Printf (printf)

-- numbers pretty
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber (nom :% denom)) =
    case quotRem nom denom of
        (x, 0) -> pretty $ show x
        (x, y) -> case fromRationalRepetendUnlimited (nom :% denom) of
                (sc, Nothing) -> pretty $ formatScientific Fixed Nothing sc
                (sc, _) -> pretty $
                    quotePart ++ remPart ++ "/" ++ show denom where
                        quotePart = (if x /= 0 then show x ++ " " ++ sign ++ " " else "")
                        remPart = (if x == 0 && y < 0 then "-" else "") ++ show (abs y)
                        sign = if y > 0 then "+" else "-"

prettyValue (HiValueBool val) = pretty $ (map toLower . show) val
prettyValue (HiValueFunction HiFunMkDir) = pretty "mkdir"
prettyValue (HiValueFunction HiFunChDir) = pretty "cd"
prettyValue (HiValueFunction val) = pretty $ (toKebab . fromHumps . drop 5 . show) val
prettyValue (HiValueString val) = pretty (show val)
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList seq) = brackets $ concatWith
         (surround $ comma <> space) (map prettyValue $ Data.Foldable.toList seq)
prettyValue (HiValueTime time) = pretty "parse-time" <> enclose (pretty "(\"") (pretty "\")") (pretty $ show time)

prettyValue (HiValueBytes bytes) = prettyByteString bytes
-- actions pretty
prettyValue (HiValueAction action) =
    case action of
        (HiActionRead path) -> pretty "read" <> enclose (pretty "(\"") (pretty "\")") (pretty path)
        (HiActionWrite path bytes) -> pretty "write" <> enclose (pretty "(") (pretty ")")
             (concatWith (surround space) (punctuate comma [enclose (pretty "\"") (pretty "\"") $ pretty path, prettyByteString bytes]))
        (HiActionMkDir path) -> pretty "mkdir" <> enclose (pretty "(\"") (pretty "\")") (pretty path)
        (HiActionChDir path) -> pretty "cd" <> enclose (pretty "(\"") (pretty "\")") (pretty path)
        HiActionCwd -> pretty "cwd"
        HiActionNow -> pretty "now"
        HiActionRand x y -> pretty "rand" <> enclose (pretty "(") (pretty ")")
                 (concatWith (surround space) (punctuate comma $ map pretty [x, y]))
        HiActionEcho x -> pretty "echo" <> enclose (pretty "(\"") (pretty "\")") (pretty x)

prettyValue (HiValueDict dict) = braces $ concatWith (surround $ comma <> space)
        (map (\(x, y) -> prettyValue x <> pretty " : " <> prettyValue y) (Data.Map.toList dict))

-- bytestring pretty
prettyByteString :: ByteString -> Doc AnsiStyle
prettyByteString bytes = brackets $ pretty (("# " ++ unwords (map (printf "%02x") (unpack bytes))) ++ " #")

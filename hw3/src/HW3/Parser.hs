{-# LANGUAGE ConstraintKinds #-}
module HW3.Parser (
    parse, unsafeParse
    ) where

import Control.Applicative (Alternative ((<|>)), (<$))
import Control.Applicative.Combinators (choice)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR), makeExprParser)
import Data.ByteString (ByteString, pack)
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.Maybe (fromMaybe)
import qualified Data.Maybe
import Data.Sequence (fromList)
import Data.Text (Text, intercalate, pack)
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (MonadParsec (eof, notFollowedBy), Parsec, Stream, between, many, manyTill,
                        optional, runParser, satisfy, sepBy1, some, try)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (char, digitChar, hexDigitChar, space, space1)
import Text.Megaparsec.Char.Lexer (scientific)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type HiParser = Parsec Void String

-- parser core

-- base function for spaces after given parser
lexeme :: HiParser a -> HiParser a
lexeme = L.lexeme space

myChar :: Char -> HiParser Char
myChar x = lexeme $ char x

myString :: String -> HiParser String
myString x = lexeme $ string x

parens :: HiParser a -> HiParser a
parens = between (myChar '(') (myChar ')')

brackets :: HiParser a -> HiParser a
brackets = between (myChar '[') (myChar ']')

braces :: HiParser a -> HiParser a
braces = between (myChar '{') (myChar '}')

byteArrayBrackets :: HiParser a -> HiParser a
byteArrayBrackets x = brackets $ between (myChar '#') (myChar '#') x

-- parser for word8
word8 :: HiParser Word8
word8 = lexeme $ do
    char1 <- hexDigitChar
    char2 <- hexDigitChar
    _ <- notFollowedBy digitChar
    return $ fromIntegral (digitToInt char1 * 16 + digitToInt char2)

bytes :: HiParser ByteString
bytes = byteArrayBrackets $ Data.ByteString.pack <$> many word8

stringLiteral :: HiParser Text
stringLiteral = Data.Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

-- parsers for given grammar

-- HiFun parsing cases
pHiFun :: HiParser HiFun
pHiFun = lexeme $ choice
    [
        HiFunDiv <$ string "div",
        HiFunMul <$ string "mul",
        HiFunAdd <$ string "add",
        HiFunSub <$ string "sub",
        HiFunAnd <$ string "and",
        HiFunOr <$ string "or",
        HiFunLessThan <$ string "less-than",
        HiFunGreaterThan <$ string "greater-than",
        HiFunEquals <$ string "equals",
        HiFunNotLessThan <$ string "not-less-than",
        HiFunNotGreaterThan <$ string "not-greater-than",
        HiFunNotEquals <$ string "not-equals",
        HiFunNot <$ string "not",
        HiFunIf <$ string "if",
        HiFunLength <$ string "length",
        HiFunToUpper <$ string "to-upper",
        HiFunToLower <$ string "to-lower",
        HiFunReverse <$ string "reverse",
        HiFunTrim <$ string "trim",
        HiFunList <$ string "list",
        HiFunRange <$ string "range",
        HiFunFold <$ string "fold",
        HiFunPackBytes <$ string "pack-bytes",
        HiFunUnpackBytes <$ string "unpack-bytes",
        HiFunEncodeUtf8 <$ string "encode-utf8",
        HiFunDecodeUtf8 <$ string "decode-utf8",
        HiFunZip <$ string "zip",
        HiFunUnzip <$ string "unzip",
        HiFunSerialise <$ string "serialise",
        HiFunDeserialise <$ string "deserialise",
        HiFunRead <$ string "read",
        HiFunWrite <$ string "write",
        HiFunMkDir <$ string "mkdir",
        HiFunChDir <$ string "cd",
        HiFunParseTime <$ string "parse-time",
        HiFunRand <$ string "rand",
        HiFunEcho <$ string "echo",
        HiFunCount <$ string "count",
        HiFunKeys <$ string "keys",
        HiFunValues <$ string "values",
        HiFunInvert <$ string "invert"
    ]

-- parser for HiValue
pHiValue :: HiParser HiExpr
pHiValue = HiExprValue <$> lexeme (choice
    [
        HiValueNumber . toRational <$> L.signed space scientific,
        HiValueBool True <$ string "true",
        HiValueBool False <$ string "false",
        HiValueFunction <$> pHiFun,
        HiValueNull <$ string "null",
        HiValueString <$> stringLiteral,
        HiValueAction HiActionCwd <$ string "cwd",
        HiValueAction HiActionNow <$ string "now"
    ])

-- bytes parser
pHiValueBytes :: HiParser HiExpr
pHiValueBytes = HiExprValue <$> lexeme (HiValueBytes <$> bytes)

-- parser for expression apply arguments
pArgs :: HiParser [HiExpr]
pArgs = lexeme $
    do
        arg <- pOperatorsExpr <* space
        args <- many (char ',' *> space *> pOperatorsExpr <* space)
        return $ arg:args
    <|> return []

-- key : value parsing
pKeyValue :: HiParser (HiExpr, HiExpr)
pKeyValue = lexeme $
    do
        key <- lexeme pOperatorsExpr
        _ <- myChar ':'
        value <- lexeme pOperatorsExpr
        return (key, value)

-- dict parsing
pDict :: HiParser [(HiExpr, HiExpr)]
pDict = lexeme $ (
    do
        arg <- pKeyValue
        args <- many (char ',' *> space *> pKeyValue <* space)
        return $ arg:args
    <|>
        return [])


-- parser for left recursion elimination in pHiExpr parser
pHiExpr' :: HiExpr -> HiParser HiExpr
pHiExpr' expr = lexeme $ (
    do
        exprs <- parens pArgs
        op <- optional (pHiExpr' (HiExprApply expr exprs))
        return $ fromMaybe (HiExprApply expr exprs) op)
    <|> (
    do
        sign <- myChar '!'
        op <- optional (pHiExpr' (HiExprRun expr))
        return $ fromMaybe (HiExprRun expr) op
    )
    <|>
    (
    do
        sign <- char '.'
        afterDot <- pAfterDot -- concat with minuses
        let concated = Data.Text.intercalate (Data.Text.pack "-") (map Data.Text.pack afterDot)
        op <- optional (pHiExpr' (HiExprApply expr [HiExprValue $ HiValueString concated]))
        return $ fromMaybe (HiExprApply expr [HiExprValue $ HiValueString concated]) op
    )

-- parser for expressions in dot access (exprs after dot)
pAfterDot :: HiParser [[Char]]
pAfterDot = lexeme $ ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

pHiExpr :: HiParser HiExpr
pHiExpr = lexeme (
    do
        expr <- pHiValue
            <|> parens pOperatorsExpr
            <|> try pHiValueBytes
            <|> HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> brackets pArgs -- parse list [expr_1, expr_2, ...] construction
            <|> HiExprDict <$> braces pDict
        op <- optional (pHiExpr' expr)
        return $ fromMaybe expr op
    )

-- parsers for operators support

binaryL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator HiParser HiExpr
binaryL name f = InfixL (f <$ myString name)

binaryR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator HiParser HiExpr
binaryR name f = InfixR (f <$ myString name)

binaryN :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator HiParser HiExpr
binaryN name f = InfixN (f <$ myString name)

createExpr :: HiFun -> HiExpr -> HiExpr -> HiExpr
createExpr fun left right = (HiExprApply $ HiExprValue $ HiValueFunction fun) [left, right]

operatorTable :: [[Operator HiParser HiExpr]]
operatorTable =
  [
    [ binaryL "*" $ createExpr HiFunMul
    , InfixL (createExpr HiFunDiv <$ (lexeme . try) (myChar '/' <* notFollowedBy (char '=')))
    ],
    [ binaryL "+" $ createExpr HiFunAdd
    , binaryL "-" $ createExpr HiFunSub
    ],
    [ binaryN "<=" $ createExpr HiFunNotGreaterThan
    , binaryN ">=" $ createExpr HiFunNotLessThan
    , binaryN "<" $ createExpr HiFunLessThan
    , binaryN ">" $ createExpr HiFunGreaterThan
    , binaryN "==" $ createExpr HiFunEquals
    , binaryN "/=" $ createExpr HiFunNotEquals
    ],
    [ binaryR "&&" $ createExpr HiFunAnd
    ],
    [ binaryR "||" $ createExpr HiFunOr
    ]
  ]

pOperatorsExpr :: HiParser HiExpr
pOperatorsExpr = makeExprParser pHiExpr operatorTable

totalParser :: HiParser HiExpr
totalParser = space *> pOperatorsExpr <* eof

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser totalParser ""

-- debugging function
unsafeParse :: String -> HiExpr
unsafeParse str = case parse str of
                    (Right x) -> x
                    _         -> undefined

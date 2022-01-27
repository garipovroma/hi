{-# LANGUAGE DeriveGeneric #-}
module HW3.Base (
    HiFun(..),
    HiValue(..),
    HiExpr(..),
    HiError(..),
    HiMonad(..),
    HiAction(..)) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data HiFun = -- function names (e.g. div, sort, length, ...)
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
    | HiFunInvert deriving (Eq, Ord, Show, GHC.Generics.Generic)

data HiValue = -- values (numbers, booleans, strings, ...)
    HiValueBool Bool
    | HiValueNumber Rational
    | HiValueFunction HiFun
    | HiValueNull
    | HiValueString Text
    | HiValueList (Seq HiValue)
    | HiValueBytes ByteString
    | HiValueAction HiAction
    | HiValueTime UTCTime
    | HiValueDict (Map HiValue HiValue)  deriving (Eq, Ord, Show, GHC.Generics.Generic)

data HiExpr =  -- expressions (literals, function calls, ...)
    HiExprValue HiValue
    | HiExprApply HiExpr [HiExpr]
    | HiExprRun HiExpr
    | HiExprDict [(HiExpr, HiExpr)] deriving (Eq, Ord, Show)

data HiError = -- evaluation errors (invalid arguments, ...)
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero deriving (Eq, Ord, Show, GHC.Generics.Generic)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text deriving (Eq, Ord, Show, GHC.Generics.Generic)

instance Serialise HiAction
instance Serialise HiFun
instance Serialise HiValue

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

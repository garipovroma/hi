{-# LANGUAGE ScopedTypeVariables #-}
module HW3.Evaluator (
    eval
    ) where

import Codec.Compression.Zlib (CompressParams (compressLevel), CompressionLevel (BestCompression),
                               compressWith, decompress, decompressWith, defaultCompressParams)
import Codec.Serialise (deserialise, serialise)
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExcept, runExceptT, throwE)
import Data.ByteString (ByteString, append, drop, index, length, pack, reverse, singleton, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Map (Map, assocs, elems, empty, fromList, fromListWith, insertWith, keys, lookup, map,
                 mapKeys)
import Data.Sequence (Seq ((:|>)), cycleTaking, drop, fromList, index, length, replicate, reverse,
                      singleton, take, (><), (|>))
import Data.Text (Text, drop, dropEnd, empty, index, intercalate, length, pack, reverse, singleton,
                  strip, toLower, toUpper, unpack)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import qualified Data.Text.Encoding as Data.ByteString
import Data.Time (diffUTCTime)
import Data.Time.Clock (addUTCTime)
import Data.Tuple (swap)
import Data.Word (Word8)
import GHC.Base (Semigroup (stimes))
import GHC.Real (Ratio ((:%)))
import GHC.ResponseFile (escapeArgs)
import HW3.Action ()
import HW3.Base (HiAction (HiActionChDir, HiActionEcho, HiActionMkDir, HiActionRand, HiActionRead, HiActionWrite),
                 HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction), HiValue (..))
import Text.Read (readMaybe)

type ExceptM m = ExceptT HiError m HiValue

-- evaluation rules

evalFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptM m
evalFun fun arr = case fun of
        HiFunIf -> do
            let [x, y, z] = arr
            evaluatedX <- evalHiExpr x
            case evaluatedX of
                        (HiValueBool True)  -> evalHiExpr y
                        (HiValueBool False) -> evalHiExpr z
                        _                   -> throwE HiErrorInvalidArgument
        HiFunAnd -> do
            let [x, y] = arr
            evaluatedX <- evalHiExpr x
            case evaluatedX of
                        (HiValueBool False) -> return $ evaluatedX
                        HiValueNull         -> return $ evaluatedX
                        _                   -> evalHiExpr y
        HiFunOr -> do
            let [x, y] = arr
            evaluatedX <- evalHiExpr x
            case evaluatedX of
                        (HiValueBool False) -> evalHiExpr y
                        HiValueNull         -> evalHiExpr y
                        _                   -> return evaluatedX
        _ -> do -- strict evaluation, firstly - arguments evaluated
            evaluatedArgs <- mapM evalHiExpr arr
            calc fun evaluatedArgs where
            calc func args = case (func, args) of
                -- arithmetics
                (HiFunAdd, [HiValueNumber x, HiValueNumber y]) -> return $ HiValueNumber $ x + y
                (HiFunSub, [HiValueNumber x, HiValueNumber y]) -> return $ HiValueNumber $ x - y
                (HiFunMul, [HiValueNumber x, HiValueNumber y]) -> return $ HiValueNumber $ x * y
                (HiFunDiv, [HiValueNumber x, HiValueNumber y]) ->
                    if y == 0 then
                        throwE HiErrorDivideByZero
                    else
                        return $ HiValueNumber $ x / y
                -- boolean arithmetics
                (HiFunAnd, [HiValueBool l, HiValueBool r]) -> return $ HiValueBool $ l && r
                (HiFunOr, [HiValueBool l, HiValueBool r]) -> return $ HiValueBool $ l || r
                (HiFunLessThan, [x, y]) -> return $ HiValueBool $ x < y
                (HiFunGreaterThan, [x, y]) -> return $ HiValueBool $ x > y
                (HiFunEquals, [x, y]) -> return $ HiValueBool $ x == y
                (HiFunNotLessThan, [x, y]) -> return $ HiValueBool $ x >= y
                (HiFunNotGreaterThan, [x, y]) -> return $ HiValueBool $ x <= y
                (HiFunNotEquals, [x, y]) -> return $  HiValueBool $ x /= y
                -- (HiFunIf, [HiValueBool x, y, z]) -> return $ if x then y else z -- strict version of if
                (HiFunNot, [HiValueBool x]) -> return $ HiValueBool $ not x
                -- strings 
                (HiFunLength , [HiValueString x]) -> return $ HiValueNumber $ toRational (Data.Text.length x)
                (HiFunToUpper, [HiValueString x]) -> return $ HiValueString $ Data.Text.toUpper x
                (HiFunToLower, [HiValueString x]) -> return $ HiValueString $ Data.Text.toLower x
                (HiFunReverse, [HiValueString x]) -> return $ HiValueString $ Data.Text.reverse x
                (HiFunTrim, [HiValueString x]) -> return $ HiValueString $ Data.Text.strip x
                (HiFunAdd, [HiValueString x, HiValueString y]) -> return $ HiValueString $ Data.Text.intercalate Data.Text.empty [x, y]
                (HiFunMul, [HiValueString x, HiValueNumber y]) ->
                    case y of
                        n :% 1 -> if n > 0 then return $ HiValueString $ stimes n x else throwE HiErrorInvalidArgument
                        _ -> throwE HiErrorInvalidArgument
                (HiFunDiv, [HiValueString x, HiValueString y]) -> return $ HiValueString $ Data.Text.intercalate (Data.Text.singleton '/') [x, y]
                -- lists
                (HiFunList, args) -> return $ HiValueList $ Data.Sequence.fromList args
                (HiFunLength, [HiValueList seq]) -> return $ HiValueNumber $ toRational $ Data.Sequence.length seq
                (HiFunReverse, [HiValueList seq]) -> return $ HiValueList $ Data.Sequence.reverse seq
                (HiFunAdd, [HiValueList x, HiValueList y]) -> return $ HiValueList $ x >< y
                (HiFunMul, [HiValueList x, HiValueNumber y]) ->
                    case y of
                        n :% 1 ->
                                if n > 0 then
                                     return $ HiValueList $ Data.Sequence.cycleTaking (fromInteger n * Data.Sequence.length x) x
                                else
                                    throwE HiErrorInvalidArgument
                        _ -> throwE HiErrorInvalidArgument
                (HiFunRange, [HiValueNumber x, HiValueNumber y]) -> return $ HiValueList $ Data.Sequence.fromList (fmap HiValueNumber [x..y])
                (HiFunFold, [HiValueFunction op, HiValueList seq]) ->
                    if Data.Sequence.length seq == 0 then
                        return HiValueNull
                    else
                        foldM (\ x y -> calc op [x, y]) (Data.Sequence.index seq 0) (Data.Sequence.drop 1 seq)
                (HiFunPackBytes, [HiValueList seq]) ->
                    if check seq then
                        return $ HiValueBytes $ Data.ByteString.pack (toList $ fmap packHelper seq)
                    else throwE HiErrorInvalidArgument where
                    packHelper :: HiValue -> Word8
                    packHelper (HiValueNumber (x :% 1)) = fromIntegral x
                    packHelper _ = error "packHelper supports only 0-255 int value converting"
                    check :: Seq HiValue -> Bool
                    check checkSeq = and $ fmap (\elem -> case elem of
                        HiValueNumber (x :% y) -> y == 1 && x >= 0 && x <= 255
                        _                      -> False
                        ) checkSeq
                -- bytes
                (HiFunUnpackBytes, [HiValueBytes bytes]) ->
                    return $ HiValueList $ Data.Sequence.fromList $ (Prelude.map (HiValueNumber <$> toRational) . Data.ByteString.unpack) bytes
                (HiFunEncodeUtf8, [HiValueString s]) -> return $ HiValueBytes $ encodeUtf8 s
                (HiFunDecodeUtf8, [HiValueBytes bytes]) -> return $
                            case decodeUtf8' bytes of
                                Left exception -> HiValueNull
                                Right value    -> HiValueString value
                (HiFunZip, [HiValueBytes bytes]) -> return $ HiValueBytes $ toStrict $ cmp $ fromStrict bytes where
                    cmp = compressWith defaultCompressParams { compressLevel = BestCompression }
                (HiFunUnzip, [HiValueBytes bytes]) -> return $ HiValueBytes $ toStrict $ cmp $ fromStrict bytes where
                    cmp = decompress
                (HiFunSerialise, [x]) -> return $ HiValueBytes $ toStrict $ serialise x
                (HiFunDeserialise, [HiValueBytes bytes]) -> return $ deserialise $ fromStrict bytes

                (HiFunLength, [HiValueBytes bytes]) -> return $ HiValueNumber $ toRational $ Data.ByteString.length bytes
                (HiFunReverse, [HiValueBytes bytes]) -> return $ HiValueBytes $ Data.ByteString.reverse bytes
                (HiFunAdd, [HiValueBytes x, HiValueBytes y]) -> return $ HiValueBytes $ x `append` y
                (HiFunMul, [HiValueBytes x, HiValueNumber y]) -> case y of
                        n :% 1 -> if (n > 0) then return $ HiValueBytes $ stimes (fromInteger n) x else throwE HiErrorInvalidArgument
                        _ -> throwE HiErrorInvalidArgument
                -- actions
                (HiFunRead, [HiValueString x]) -> return $ HiValueAction $ HiActionRead $ Data.Text.unpack x
                (HiFunWrite, [HiValueString x, HiValueString y]) -> return $ HiValueAction $ HiActionWrite (Data.Text.unpack x) (encodeUtf8 y)
                (HiFunMkDir, [HiValueString x]) -> return $ HiValueAction $ HiActionMkDir $ Data.Text.unpack x
                (HiFunChDir, [HiValueString x]) -> return $ HiValueAction $ HiActionChDir $ Data.Text.unpack x
                -- time
                (HiFunParseTime, [HiValueString x]) -> case readMaybe (Data.Text.unpack x) of
                                                        Just value -> return $ HiValueTime value
                                                        Nothing    -> return HiValueNull
                (HiFunAdd, [HiValueTime x, HiValueNumber y]) -> return $ HiValueTime $ addUTCTime (realToFrac y) x
                (HiFunSub, [HiValueTime x, HiValueTime y]) -> return $ HiValueNumber $ toRational $ diffUTCTime x y
                -- rand
                (HiFunRand, [HiValueNumber x, HiValueNumber y]) -> case (x, y) of
                                                (a :% 1, c :% 1) -> if checkBounds a && checkBounds c then
                                                                        return $ HiValueAction $ HiActionRand (fromIntegral a) (fromIntegral c)
                                                                    else
                                                                        throwE HiErrorInvalidArgument
                                                    where
                                                        checkBounds x = x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int)
                                                _ -> throwE HiErrorInvalidArgument
                -- echo
                (HiFunEcho, [HiValueString t]) -> return $ HiValueAction $ HiActionEcho t
                -- dict
                (HiFunKeys, [HiValueDict dict]) -> return $ HiValueList $ Data.Sequence.fromList $ Data.Map.keys dict
                (HiFunValues, [HiValueDict dict]) -> return $ HiValueList $ Data.Sequence.fromList $ Data.Map.elems dict
                (HiFunCount, [HiValueList list]) ->
                    return $ HiValueDict $ Data.Map.map HiValueNumber (countHelper $ Data.Foldable.toList list)
                (HiFunCount, [HiValueString str]) ->
                    return $ HiValueDict $ Data.Map.map HiValueNumber $ Data.Map.mapKeys (\ch -> HiValueString $
                    Data.Text.pack [ch]) (countHelper $ Data.Foldable.toList $ Data.Text.unpack str)
                (HiFunCount, [HiValueBytes bytes]) ->
                    return $ HiValueDict $ Data.Map.map HiValueNumber $
                    Data.Map.mapKeys (\(x :: Word8) -> HiValueNumber  . toRational $ x) (countHelper $ Data.ByteString.unpack bytes)
                (HiFunInvert, [HiValueDict dict]) ->
                    return $ HiValueDict $ Data.Map.map (HiValueList . Data.Sequence.fromList) $ Data.Map.fromListWith helper $
                         Prelude.map ((\((x,y) :: (HiValue, HiValue)) -> (x, [y])) . swap) $ Data.Map.assocs dict
                                 where
                                        helper :: [HiValue] -> [HiValue] -> [HiValue]
                                        helper x y = x ++ y
                _  -> throwE HiErrorInvalidArgument

-- helper for count evaluation
countHelper :: Ord x => [x] -> Map x Rational
countHelper = foldl (\x y -> Data.Map.insertWith innerHelper y 1 x) Data.Map.empty where
    innerHelper :: Rational -> Rational -> Rational
    innerHelper x y = x + y

-- str indexing & slices
evalStr :: HiMonad m => Text -> [HiExpr] -> ExceptM m
evalStr str args = do
    exprs <- mapM evalHiExpr args
    case exprs of
        [HiValueNumber x] ->
            case x of
                n :% 1 ->
                    if 0 <= fromInteger n && fromInteger n < strLen then
                        return $ HiValueString $ Data.Text.singleton $ Data.Text.index str ((fromInteger n + strLen)  `mod` strLen)
                    else
                        return HiValueNull
                _ -> throwE HiErrorInvalidArgument
        [a, b] ->
            case [a,b] of
                [HiValueNumber x, HiValueNumber y] -> calc x y
                [HiValueNull, HiValueNumber y]     -> calc (toRational 0) y
                [HiValueNumber x, HiValueNull]     -> calc x (toRational strLen)
                [HiValueNull, HiValueNull]         -> calc (toRational 0) (toRational strLen)
                [_, _]                             -> throwE HiErrorInvalidArgument
                where
                    calc x y = case (x, y) of
                        (n :% 1, m :% 1) ->
                            return $ HiValueString $ (Data.Text.drop pn . Data.Text.dropEnd (strLen - pm)) str where
                                processIndex :: Integer -> Int
                                processIndex ind = if ind_ < 0 then ind_ + strLen else ind_ where
                                    ind_ = fromInteger ind
                                pn = processIndex n
                                pm = processIndex m
                        _ -> throwE HiErrorInvalidArgument
        _ -> throwE HiErrorInvalidArgument
    where
        strLen = Data.Text.length str
        checkIndex x = True

-- list indexing & slices
evalList :: HiMonad m => Seq HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalList seq args = do
    exprs <- mapM evalHiExpr args
    case exprs of
        [HiValueNumber x] ->
            case x of
                n :% 1 ->
                    if 0 <= fromInteger n && fromInteger n < seqLen then
                        return $ Data.Sequence.index seq ((fromInteger n + seqLen) `mod` seqLen)
                    else
                        return HiValueNull
                _ -> throwE HiErrorInvalidArgument
        [a, b] ->
            case [a,b] of
                [HiValueNumber x, HiValueNumber y] -> calc x y
                [HiValueNull, HiValueNumber y]     -> calc (toRational 0) y
                [HiValueNumber x, HiValueNull]     -> calc x $ toRational $ seqLen
                [HiValueNull, HiValueNull]         -> calc (toRational 0) (toRational $ seqLen)
                [_, _]                             -> throwE HiErrorInvalidArgument
                where
                    calc x y = case (x, y) of
                        (n :% 1, m :% 1) ->
                            return $ HiValueList $
                             (Data.Sequence.drop pn . (Data.Sequence.reverse . Data.Sequence.drop (seqLen - pm) . Data.Sequence.reverse)) seq where
                                processIndex :: Integer -> Int
                                processIndex ind = if ind_ < 0 then ind_ + seqLen else ind_ where
                                    ind_ = fromInteger ind
                                pn = processIndex n
                                pm = processIndex m
                        _ -> throwE HiErrorInvalidArgument

        _ -> throwE HiErrorInvalidArgument

    where
        seqLen = Data.Sequence.length seq
        checkIndex x = True

-- bytes indexing & slices
evalBytes :: HiMonad m => ByteString -> [HiExpr] -> ExceptT HiError m HiValue
evalBytes bytes args = do
    exprs <- mapM evalHiExpr args
    case exprs of
        [HiValueNumber x] ->
            case x of
                n :% 1 ->
                    if 0 <= fromInteger n && fromInteger n < bytesLen then
                        return $ HiValueNumber $ toRational $ Data.ByteString.index bytes ((fromInteger n + bytesLen) `mod` bytesLen)
                    else
                        return HiValueNull
                _ -> throwE HiErrorInvalidArgument
        [a, b] ->
            case [a,b] of
                [HiValueNumber x, HiValueNumber y] -> calc x y
                [HiValueNull, HiValueNumber y]     -> calc (toRational 0) y
                [HiValueNumber x, HiValueNull]     -> calc x $ toRational $ bytesLen
                [HiValueNull, HiValueNull]         -> calc (toRational 0) (toRational $ bytesLen)
                [_, _]                             -> throwE HiErrorInvalidArgument
                where
                    calc x y = case (x, y) of
                        (n :% 1, m :% 1) ->
                            return $ HiValueBytes $
                            (Data.ByteString.drop pn . (Data.ByteString.reverse . Data.ByteString.drop (bytesLen - pm) . Data.ByteString.reverse)) bytes where
                                processIndex :: Integer -> Int
                                processIndex ind = if ind_ < 0 then ind_ + bytesLen else ind_ where
                                    ind_ = fromInteger ind
                                pn = processIndex n
                                pm = processIndex m
                        _ -> throwE HiErrorInvalidArgument

        _ -> throwE HiErrorInvalidArgument

    where
        bytesLen = Data.ByteString.length bytes
        checkIndex x = True

-- dict indexing
evalDict :: HiMonad m => Map HiValue HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalDict dict args = do
    exprs <- mapM evalHiExpr args
    case exprs of
        [x] -> case Data.Map.lookup x dict of
                (Just value) -> return value
                Nothing      -> return HiValueNull

        _ -> throwE HiErrorInvalidArgument



-- operation arity functions

unaryOperation :: HiMonad m => HiFun -> [HiExpr] -> ExceptM m
unaryOperation fun [x] = evalFun fun [x]
unaryOperation fun _   = throwE HiErrorArityMismatch

binaryOperation :: HiMonad m => HiFun -> [HiExpr] -> ExceptM m
binaryOperation fun [x, y] = evalFun fun [x, y]
binaryOperation fun _      = throwE HiErrorArityMismatch

ternaryOperation :: HiMonad m => HiFun -> [HiExpr] -> ExceptM m
ternaryOperation fun [x, y, z] = evalFun fun [x, y, z]
ternaryOperation fun _         = throwE HiErrorArityMismatch

unbounedArityOperation :: HiMonad m => HiFun -> [HiExpr] -> ExceptM m
unbounedArityOperation = evalFun

-- expr evaluation rule

evalHiExprApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptM m
evalHiExprApply expr args = do
    evaluatedExpr <- evalHiExpr expr
    case evaluatedExpr of
        HiValueFunction fun -> evalFunArityCheck fun args
        HiValueString str   -> evalStrArityCheck str args
        HiValueList list    -> evalListArityCheck list args
        HiValueBytes bytes  -> evalByteStringArityCheck bytes args
        HiValueDict dict    -> evalDictArityCheck dict args
        _                   -> throwE HiErrorInvalidFunction

-- HiValue arity rules

evalListArityCheck :: HiMonad m => Seq HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalListArityCheck seq args =
    case args of
        [x]    -> evalList seq args
        [x, y] -> evalList seq args
        _      -> throwE HiErrorArityMismatch

evalStrArityCheck :: HiMonad m => Text -> [HiExpr] -> ExceptM m
evalStrArityCheck str args =
    case args of
        [x]    -> evalStr str args
        [x, y] -> evalStr str args
        _      -> throwE HiErrorArityMismatch

evalByteStringArityCheck :: HiMonad m => ByteString -> [HiExpr] -> ExceptM m
evalByteStringArityCheck bytes args =
    case args of
        [x]    -> evalBytes bytes args
        [x, y] -> evalBytes bytes args
        _      -> throwE HiErrorArityMismatch

evalDictArityCheck :: HiMonad m => Map HiValue HiValue -> [HiExpr] -> ExceptM m
evalDictArityCheck dict args =
    case args of
        [x] -> evalDict dict args
        _   -> throwE HiErrorArityMismatch

-- function arity checking
evalFunArityCheck :: HiMonad m => HiFun -> [HiExpr] -> ExceptM m
evalFunArityCheck fun =
    case fun of
        HiFunNot            -> unaryOperation HiFunNot
        HiFunLength         -> unaryOperation HiFunLength
        HiFunToUpper        -> unaryOperation HiFunToUpper
        HiFunToLower        -> unaryOperation HiFunToLower
        HiFunReverse        -> unaryOperation HiFunReverse
        HiFunTrim           -> unaryOperation HiFunTrim

        HiFunAdd            -> binaryOperation HiFunAdd
        HiFunSub            -> binaryOperation HiFunSub
        HiFunMul            -> binaryOperation HiFunMul
        HiFunDiv            -> binaryOperation HiFunDiv

        HiFunAnd            -> binaryOperation HiFunAnd
        HiFunOr             -> binaryOperation HiFunOr
        HiFunLessThan       -> binaryOperation HiFunLessThan
        HiFunGreaterThan    -> binaryOperation HiFunGreaterThan
        HiFunEquals         -> binaryOperation HiFunEquals
        HiFunNotLessThan    -> binaryOperation HiFunNotLessThan
        HiFunNotGreaterThan -> binaryOperation HiFunNotGreaterThan
        HiFunNotEquals      -> binaryOperation HiFunNotEquals

        HiFunIf             -> ternaryOperation HiFunIf

        HiFunList           -> unbounedArityOperation HiFunList
        HiFunRange          -> binaryOperation HiFunRange
        HiFunFold           -> binaryOperation HiFunFold

        HiFunPackBytes      -> unaryOperation HiFunPackBytes
        HiFunUnpackBytes    -> unaryOperation HiFunUnpackBytes
        HiFunEncodeUtf8     -> unaryOperation HiFunEncodeUtf8
        HiFunDecodeUtf8     -> unaryOperation HiFunDecodeUtf8
        HiFunZip            -> unaryOperation HiFunZip
        HiFunUnzip          -> unaryOperation HiFunUnzip
        HiFunSerialise      -> unaryOperation HiFunSerialise
        HiFunDeserialise    -> unaryOperation HiFunDeserialise
        HiFunRead           -> unaryOperation HiFunRead
        HiFunWrite          -> binaryOperation HiFunWrite
        HiFunMkDir          -> unaryOperation HiFunMkDir
        HiFunChDir          -> unaryOperation HiFunChDir
        HiFunParseTime      -> unaryOperation HiFunParseTime
        HiFunRand           -> binaryOperation HiFunRand
        HiFunEcho           -> unaryOperation HiFunEcho
        HiFunKeys           -> unaryOperation HiFunKeys
        HiFunValues         -> unaryOperation HiFunValues
        HiFunCount          -> unaryOperation HiFunCount
        HiFunInvert         -> unaryOperation HiFunInvert

evalHiExprRun :: HiMonad m => HiExpr -> ExceptM m
evalHiExprRun expr = do
    evaluatedExpr <- evalHiExpr expr
    case evaluatedExpr of
        (HiValueAction action) -> lift $ runAction action
        _                      -> throwE HiErrorInvalidArgument

-- hi expr eval
evalHiExpr :: HiMonad m => HiExpr -> ExceptM m
evalHiExpr (HiExprValue value)      = evalHiExprValue value
evalHiExpr (HiExprApply expr exprs) = evalHiExprApply expr exprs
evalHiExpr (HiExprRun expr)         = evalHiExprRun expr
evalHiExpr (HiExprDict dict)        = evalHiExprDict dict

-- dict evaluation
evalHiExprDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptM m
evalHiExprDict dict = do
    let keys = Prelude.map fst dict
    let values = Prelude.map snd dict
    evaluatedKeys <- mapM evalHiExpr keys
    evaluatedValues <- mapM evalHiExpr values
    return $ HiValueDict $ Data.Map.fromList $ zip evaluatedKeys evaluatedValues

-- HiValue evaluation
evalHiExprValue :: HiMonad m => HiValue -> ExceptM m
evalHiExprValue (HiValueNumber x)      = return (HiValueNumber x)
evalHiExprValue (HiValueBool x)        = return (HiValueBool x)
evalHiExprValue (HiValueFunction f)    = return (HiValueFunction f)
evalHiExprValue (HiValueString x)      = return (HiValueString x)
evalHiExprValue HiValueNull            = return HiValueNull
evalHiExprValue (HiValueList seq)      = return (HiValueList seq)
evalHiExprValue (HiValueBytes bytes)   = return (HiValueBytes bytes)
evalHiExprValue (HiValueAction action) = return (HiValueAction action)
evalHiExprValue (HiValueTime time)     = return (HiValueTime time)
evalHiExprValue (HiValueDict dict)     = return (HiValueDict dict)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evalHiExpr expr

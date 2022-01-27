{-# LANGUAGE DeriveFunctor #-}
module HW3.Action
    (HiPermission(..), PermissionException(..), HIO(..))
    where

import Control.Exception (Exception, throwIO)
import Control.Monad (ap)
import Data.ByteString (readFile, writeFile)
import Data.Sequence (fromList)
import Data.Set (Set, member)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import HW3.Base (HiAction (..), HiMonad (runAction), HiValue (..))
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime deriving (Eq, Ord, Show)

data PermissionException =
  PermissionRequired HiPermission deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving Functor

instance Applicative HIO where
  pure = return
  x <*> y = do { a <- x; a <$> y;}

instance Monad HIO where
  return x = HIO (\_ -> return x)
  h >>= x = HIO (\perms -> do
        y <- runHIO h perms
        runHIO (x y) perms)

instance HiMonad HIO where
  runAction (HiActionRead path) =
     HIO $ \perms ->
        if not (member AllowRead perms) then
           throwIO $ PermissionRequired AllowRead
        else
          do
            flag <- doesFileExist path
            (if flag then do
                  content <- Data.ByteString.readFile path
                  case decodeUtf8' content of
                      Left exception -> return $ HiValueBytes content
                      Right value    -> return $ HiValueString value
            else
              do
              kek <- listDirectory path
              return $ HiValueList $ (fromList . map (HiValueString . pack)) kek)

  runAction (HiActionWrite path bytes) =
    HIO $ \perms ->
        if not (member AllowWrite perms) then
          throwIO $ PermissionRequired AllowWrite
        else
          do
          Data.ByteString.writeFile path bytes
          return HiValueNull

  runAction HiActionCwd =
    HIO $ \perms ->
      if not (member AllowRead perms)
      then throwIO $ PermissionRequired AllowRead
      else
          HiValueString . pack <$> getCurrentDirectory

  runAction (HiActionChDir path) =
     HIO $ \perms ->
        if not (member AllowRead perms)
        then throwIO $ PermissionRequired AllowRead
        else
          do
          setCurrentDirectory path
          return HiValueNull

  runAction (HiActionMkDir path) =
     HIO $ \perms ->
        if not (member AllowWrite perms)
        then throwIO $ PermissionRequired AllowWrite
        else
          do
          createDirectory path
          return HiValueNull

  runAction HiActionNow =
    HIO $ \perms ->
      if not (member AllowTime perms)
        then throwIO $ PermissionRequired AllowTime
      else
          HiValueTime <$> getCurrentTime

  runAction (HiActionRand x y) =
    HIO $ \perms ->
      do
        value <- getStdRandom (uniformR (x, y))
        return $ HiValueNumber $ toRational value

  runAction (HiActionEcho x) =
    HIO $ \perms ->
      if not (member AllowWrite perms)
        then throwIO $ PermissionRequired AllowWrite
      else
      do
        putStrLn $ unpack x
        return HiValueNull

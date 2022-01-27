module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (Identity))
import Data.Set (fromList)
import Debug.Trace
import HW3.Action (HIO (runHIO), HiPermission (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Lib
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
   where
       hiPermissionsSet = fromList [AllowWrite, AllowRead, AllowTime]
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do case parse input of
                                    (Left parseError) -> outputStrLn $ errorBundlePretty parseError
                                    (Right expr) -> do
                                                value <- liftIO $ runHIO (eval expr) hiPermissionsSet
                                                case value of
                                                    (Left error) -> outputStrLn $ show error
                                                    (Right value) -> outputStrLn $ show $ prettyValue value


                                loop

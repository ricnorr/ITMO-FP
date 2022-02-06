module Main where
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Set (fromList)
import HW3.Action (HIO(runHIO), HiPermission(AllowRead, AllowTime, AllowWrite))
import HW3.Base (HiError, HiValue)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyError, prettyParseError, prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)

-- print pretty value or error
handleEval :: Either HiError HiValue -> HIO ()
handleEval (Left x) = liftIO $ print (prettyError x)
handleEval (Right x) = liftIO $ print (prettyValue x)

-- parse, eval, print
parseAndEval :: [Char] -> HIO ()
parseAndEval s = let parsed = parse s in case parsed of
          Left x -> liftIO $ print $ prettyParseError x
          Right x -> do
            afterEval <- eval x
            handleEval afterEval

-- run Hi
main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                                liftIO $ runHIO (parseAndEval input) (fromList [AllowWrite, AllowRead, AllowTime])
                                loop

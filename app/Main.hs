module Main where

import System.Environment (getArgs, getProgName)
import System.IO
import System.Exit (exitFailure, exitSuccess)
import qualified Lib
import Data.Foldable (for_)

data Opts =
    Help
  | Check String
  | Run String

help :: String -> String
help exec = "usage:\n" ++
  "  " ++ exec ++ " help - displays this help message\n" ++
  "  " ++ exec ++ " check [file] - check a file\n" ++
  "  " ++ exec ++ " run [file] - run a file\n"

parseArgs :: String -> [String] -> Either String Opts
parseArgs _ [] = Right Help
parseArgs _ ["help"] = Right Help
parseArgs _ ["check"] = Left "`check` expects a filename"
parseArgs _ ["check", f] = Right $ Check f
parseArgs _ ("check" : _) = Left "`check` expects a filename"
parseArgs _ ["run"] = Left "`run` expects a filename"
parseArgs _ ["run", f] = Right $ Run f
parseArgs _ ("run" : _) = Left "`run` expects a filename"
parseArgs exec _ = Left $ help exec

main :: IO ()
main =
  do
    exec <- getProgName
    args <- getArgs
    (_should_run, file) <- case parseArgs exec args of
      Left e -> hPutStrLn stderr e >> exitFailure
      Right Help -> putStrLn (help exec) >> exitSuccess
      Right (Check file) -> return (False, file)
      Right (Run file) -> return (True, file)
    contents <- withFile file ReadMode hGetContents'
    tops <- case Lib.parse file contents of
      Left e -> hPutStrLn stderr e >> exitFailure
      Right v -> return v
    for_ tops print
    putStrLn "\n-------------------------------------------------------------\n"
    case Lib.typecheck tops of
      Left e -> hPutStrLn stderr e >> exitFailure
      Right v ->
        for_ (map (\(x, t) -> x ++ ": " ++ show t) v)
          putStrLn

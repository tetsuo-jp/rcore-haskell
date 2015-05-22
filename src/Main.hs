module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Timeout
import Text.PrettyPrint (render)
-- import Debug.Trace

import RCore.Parser
import RCore.Eval
import RCore.Ast (pretty)
import RCore.ToData (todata)

data Flag = Time Int | Value deriving Show

options :: [OptDescr Flag]
options = [ Option ['t'] ["time"] (ReqArg (Time . read) "N") "timeout after N seconds"
          , Option ['v'] ["value"] (NoArg Value) "tranlation from R-CORE program to R-CORE value"
          ]

processArg :: [Flag] -> (Int,Bool)
processArg [] = (1,False)
processArg (Time t:as) = let (_,v) = processArg as in (t,v)
processArg (Value:as) = let (t,_) = processArg as in (t,True)

main :: IO ()
main = do args <- getArgs
          case getOpt Permute options args of
            (t,[prog],[]) ->
                let (time,value) = processArg t
                in do prog_str <- loadFile prog
                      case parseProgram prog_str of
                          Left err   -> print err >> exitWith (ExitFailure 1)
                          Right prog -> 
                                  putStrLn $ render $ if value
                                                      then pretty (RCore.ToData.todata prog) 
                                                      else pretty prog
            (t,[prog,val],[]) ->
                do res <- let (time,_) = processArg t
                          in timeout (time * 1000000) $ parseAndRun prog val
                   case res of
                     Nothing -> exitWith $ ExitFailure 124
                     _       -> return ()
            (_,_,errs)      -> ioError (userError (concat errs ++ "\n" ++ usageInfo header options))
              where header = "Usage: rw [options] <file>"

loadFile :: String -> IO String
loadFile "-"      = getContents
loadFile filename = readFile filename

parseAndRun :: String -> String -> IO ()
parseAndRun prog_file val_file =
  do prog_str <- loadFile prog_file
     val_str  <- loadFile val_file
     case parseProgram prog_str of
           Left err   -> print err >> exitWith (ExitFailure 1)
           Right prog -> case parseVal val_str of
                           Left err  -> print err >> exitWith (ExitFailure 1)
                           Right val -> print $ pretty $ execProgram prog val

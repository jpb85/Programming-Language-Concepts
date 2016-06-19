module Main where

import System.Console.GetOpt
import System.Environment
import System.IO

import While

import qualified Interpreter as I
import qualified MonadicInterpreter as M
import qualified OutputInterpreter as O

main :: IO ()
main = do
    (flags, files) <- getArgs >>= parseOpts
    let interp = chooseInterpreter flags
    mapM_ (runInterp interp) files
  where
    chooseInterpreter :: [Flag] -> Stm -> IO ()
    chooseInterpreter flags stm
        | MonadicInterpreter `elem` flags = print $ M.runI (M.sStep stm) M.emptyState
        | OutputInterpreter `elem` flags  = print $ O.runI (O.sStep stm) O.emptyState
        | otherwise                       = print $ I.sStep stm I.emptyState

    runInterp :: (Stm -> IO ()) -> FilePath -> IO ()
    runInterp interp path = do
        s <- readFile path
        let maybe_stm = parseWhile s
        case maybe_stm of
          Nothing  -> hPutStrLn stderr $ "Could not parse '" ++ path ++ "'"
          Just stm -> interp stm

options :: [OptDescr Flag]
options =
 [ Option [] ["monadic"]     (NoArg MonadicInterpreter)    "run the monadic interpreter"
 , Option [] ["output"]      (NoArg OutputInterpreter)     "run the interpreter with output"
 ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
    case getOpt Permute options argv of
      (o,args,[]) -> return (o,args)
      (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: while [OPTION...] FILES"

data Flag = MonadicInterpreter
          | OutputInterpreter
  deriving (Eq, Show)

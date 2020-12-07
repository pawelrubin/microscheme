module Main where

import qualified Data.Text.IO as T
import Options.Applicative

data Action
  = Ast
  | SAst
  | LLVM
  | Compile FilePath
  | Run
  | Repl

data Options = Options
  { action :: Action,
    inputFile :: Maybe FilePath
  }

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    infoString = "Simple Scheme compiler"

actionP :: Parser Action
actionP =
  flag' Ast (long "ast" <> short 'a' <> help "Pretty print the ast")
    <|> flag' SAst (long "sast" <> short 's' <> help "Pretty print the sast")
    <|> flag'
      LLVM
      (long "llvm" <> short 'l' <> help "Pretty print the generated llvm")
    <|> flag'
      Compile
      (long "compile" <> short 'c' <> help "Compile to an executable")
    <*> strOption (short 'o' <> value "a.out" <> metavar "FILE")
    -- Run the code
    <|> pure Run

optionsP :: Parser Options
optionsP =
  Options
    <$> actionP
    <*> optional (strArgument (help "Source file" <> metavar "FILE"))

runOpts :: Options -> IO ()
runOpts (Options action inputFile) = do
  case inputFile of
    Nothing -> do
      putStrLn "Running REPL"
    Just path -> do
      code <- T.readFile path
      putStrLn "Work in Progress"
      case action of
        Ast -> putStrLn "AST"
        SAst -> putStrLn "SAst"
        LLVM -> putStrLn "LLVM"
        Compile path -> putStrLn $ "Compile. output path = " <> show path
        _ -> error "Invalid usage"

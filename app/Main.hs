module Main where

import Data.String.Conversions
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import HaScheme
import LLVM.Pretty
import Options.Applicative
import System.Directory
import Text.Pretty.Simple

data Action
  = Code
  | Ast
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
  flag' Code (long "code" <> short 'c' <> help "Pretty print the parsed code")
    <|> flag' Ast (long "ast" <> short 'a' <> help "Pretty print the ast")
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
      exists <- doesFileExist path
      if exists
        then do
          code <- T.readFile path
          case readExprFile path code of
            Right ast -> case action of
              Code -> pPrint ast
              -- _ -> case createProgram ast of
              _ -> case evalProgram ast of
                Right program ->
                  let generatedIR = codegenList program
                   in
                  case action of
                    Ast -> pPrint program
                    -- Code -> putDoc $ pretty ast <> "\n"
                    LLVM -> T.putStrLn . cs . ppllvm $ generatedIR
                    -- Compile path -> compile generatedIR path
                    -- Run -> run generatedIR >>= T.putStr
                    _ -> error "Invalid usage"
                Left err -> print err
            Left err -> print err
        else do
          errorWithoutStackTrace "File does not exist"

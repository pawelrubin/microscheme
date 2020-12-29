module Main where

import Data.String.Conversions
import qualified Data.Text.IO as T
import LLVM.Pretty
import MicroScheme
import Options.Applicative
import System.Directory
import Text.Pretty.Simple

data Action
  = Parse
  | Ast
  | LLVM FilePath
  | Asm FilePath
  | Compile FilePath
  | Run

data Options = Options
  { action :: Action,
    inputFile :: FilePath
  }

actionP :: Parser Action
actionP =
  flag' Parse (long "parse" <> short 'p' <> help "Print the parse tree")
    <|> flag' Ast (long "ast" <> short 'a' <> help "Print the ast")
    <|> flag' Asm (long "asm" <> short 's' <> help "Print asm code to a file")
    <*> strOption (short 'o' <> value "obj.s" <> metavar "FILE")
    <|> flag'
      LLVM
      (long "llvm" <> short 'l' <> help "Print the LLVM IR to a file")
    <*> strOption (short 'o' <> value "llvm.ll" <> metavar "FILE")
    <|> flag' Run (long "run" <> short 'r' <> help "Compile and run the code")
    <|> pure (Compile "a.out")

optionsP :: Parser Options
optionsP =
  Options
    <$> actionP
    <*> strArgument (help "Source file" <> metavar "SRC_FILE")

runOpts :: Options -> IO ()
runOpts (Options action path) = do
  exists <- doesFileExist path
  if exists
    then do
      code <- T.readFile path
      case readExprFile path code of
        Right ast -> case action of
          Parse -> pPrint ast
          _ -> case evalProgram ast of
            Right program ->
              let generatedIR = codegenProgram program
               in case action of
                    Ast -> pPrint program
                    LLVM path -> writeFile path $ cs . ppllvm $ generatedIR
                    Asm path -> asm generatedIR path
                    Compile path -> compile generatedIR path
                    Run -> run generatedIR >>= T.putStr
                    -- Parse action is already matched before
                    _ -> undefined
            Left err -> print err
        Left err -> print err
    else do
      errorWithoutStackTrace "File does not exist"

main :: IO ()
main = runOpts =<< execParser (withInfo optionsP "Micro Scheme programming language compiler.")
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

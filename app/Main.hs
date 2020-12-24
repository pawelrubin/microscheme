module Main where

import Data.String.Conversions
import qualified Data.Text.IO as T
import HaScheme
import LLVM.Pretty
import Options.Applicative
import System.Directory
import Text.Pretty.Simple

data Action
  = Parse
  | Ast
  | LLVM
  | Asm FilePath
  | Compile FilePath
  | Run

data Options = Options
  { action :: Action,
    inputFile :: FilePath
  }

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    infoString = "Micro Scheme compiler"

actionP :: Parser Action
actionP =
  flag' Parse (long "parse" <> short 'p' <> help "Pretty print the parse tree")
    <|> flag' Ast (long "ast" <> short 'a' <> help "Pretty print the ast")
    <|> flag' Asm (long "asm" <> short 's' <> help "Print asm code")
    <*> strOption (short 'o' <> value "obj.s" <> metavar "FILE")
    <|> flag'
      LLVM
      (long "llvm" <> short 'l' <> help "Pretty print the generated llvm")
    <|> flag' Run (long "run" <> short 'r' <> help "Compile and run the code [experimental]")
    <|> pure (Compile "a.out")

optionsP :: Parser Options
optionsP =
  Options
    <$> actionP
    <*> strArgument (help "Source file" <> metavar "FILE")

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
              let generatedIR = codegenList program
               in case action of
                    Ast -> pPrint program
                    LLVM -> T.putStrLn . cs . ppllvm $ generatedIR
                    Asm path -> asm generatedIR path
                    Compile path -> compile generatedIR path
                    Run -> run generatedIR >>= T.putStr
                    -- Parse action is already matched before
                    _ -> undefined
            Left err -> print err
        Left err -> print err
    else do
      errorWithoutStackTrace "File does not exist"

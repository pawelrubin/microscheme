module HaScheme.TopLevel where

import Control.Exception (bracket)
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text.IO as T
import LLVM.AST
import LLVM.Pretty
import System.Directory
import System.IO
import System.Posix.Temp
import System.Process

compile :: Module -> FilePath -> IO ()
compile llvmModule outFile =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      -- create temporary file for "output.ll"
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      -- write the llvmModule to a file
      T.hPutStrLn llvmHandle (cs $ ppllvm llvmModule)
      hClose llvmHandle
      -- link the runtime with the assembly
      callProcess
        "clang"
        ["-Wno-override-module", "-lm", llvm, "-o", "../" <> outFile]

run :: Module -> IO Text
run llvmModule = do
  compile llvmModule "./a.out"
  result <- cs <$> readProcess "./a.out" [] []
  removePathForcibly "./a.out"
  return result

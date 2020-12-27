-- |
-- Module      : MicroScheme.TopLevel
-- Copyright   : Paweł Rubin
--
-- This module introduces an API for LLVM IR generation and compilation using clang.
module MicroScheme.TopLevel where

import Control.Exception (bracket)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text.IO as T
import LLVM.AST (Module)
import LLVM.Pretty (ppllvm)
import System.Directory
  ( removePathForcibly,
    withCurrentDirectory,
  )
import System.IO (hClose)
import System.Posix.Temp (mkdtemp, mkstemps)
import System.Process (callProcess, readProcess)

asm :: Module -> FilePath -> IO ()
asm llvmModule outFile =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      T.hPutStrLn llvmHandle (cs $ ppllvm llvmModule)
      hClose llvmHandle
      callProcess "llc-9" [llvm, "-o", "../" <> outFile]

compile :: Module -> FilePath -> IO ()
compile llvmModule outFile =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      -- create temporary file for "output.ll"
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      -- write the llvmModule to a file
      T.hPutStrLn llvmHandle (cs $ ppllvm llvmModule)
      let runtime = "../src/runtime.c"
      hClose llvmHandle
      -- link the runtime with the assembly
      callProcess
        "clang"
        ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outFile]

run :: Module -> IO Text
run llvmModule = do
  compile llvmModule "./a.out"
  result <- cs <$> readProcess "./a.out" [] []
  removePathForcibly "./a.out"
  return result

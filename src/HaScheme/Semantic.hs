module HaScheme where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Text as T
import HaScheme.Ast
import HaScheme.SAst
import qualified Data.Map as M

type Definitions = M.Map T.Text Definition
type Expressions = [Expr]

data TEnv = TEnv {
  definitions :: Definitions,
  expressions :: Expressions
}

type Semantic = ExceptT SemanticError (State TEnv)

type Name = T.Text

data SemanticError
  = UndefinedSymbol Name SchemeVal

transformAst :: SchemeVal -> Semantic Program
transformAst ast = case ast of
  


checkDefinition :: SchemeVal -> Semantic Definition
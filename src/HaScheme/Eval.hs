module HaScheme where

import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), liftIO, runExceptT)
import Data.IORef
import qualified Data.Text as T
import HaScheme.Ast

eval :: Env -> SchemeVal -> ThrowsError SchemeVal
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List atoms) =
  case atoms of
    [Atom "define", Atom var, form] -> eval env form >>= setVar env var
    
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

getVar :: Env -> T.Text -> ThrowsError SchemeVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVariable "Getting an unbound variable " (T.pack var))
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> T.Text -> SchemeVal -> ThrowsError SchemeVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVariable "Setting an unbound variable " var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value
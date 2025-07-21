module Language.UntypedLC.BigStepCBV (Value, callByValueBigStep, debugShowValue) where

import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState (get, put), State, evalState, runState, withState)
import Data.List
import Data.List (intercalate)
import Data.Map qualified as Map
import Debug.Trace
import Language.UntypedLC.Debruijn (DebruijnExpr (..))
import Language.UntypedLC.Syntax qualified as Syn

type Env = [Value]

data Value = VClosure Syn.Ident DebruijnExpr Env

instance Show Value where
  show :: Value -> String
  show test@(VClosure ident body _env) = debugShowValue test

-- "<closure λ{" ++ show ident ++ "}. " ++ show body ++ ">"

debugShowValue :: Value -> String
debugShowValue (VClosure ident body env) =
  "<closure λ{"
    ++ show ident
    ++ "} . "
    ++ show body
    ++ " with env: ["
    ++ intercalate ", " (map debugShowValue env)
    ++ "]>"

type CallByValueBigStep = State Env

callByValueBigStep :: DebruijnExpr -> Value
callByValueBigStep expr = evalState (go expr) []
  where
    go :: DebruijnExpr -> CallByValueBigStep Value
    go (DeBruijnIndex _pos index) = do
      gamma <- get
      let retrieved = gamma !? index
      case retrieved of
        Just x -> pure x
        Nothing -> error $ "The index is " ++ show index ++ " , The environment list is " ++ show gamma
    go (DeBruijnAbs _pos binder body) = do
      gamma <- get
      pure $ VClosure binder body gamma
    go (DeBruijnApp _pos f arg) = do
      arg' <- go arg
      context <- get
      (VClosure _binder f' f'_env) <- go f
      put (arg' : f'_env)
      evalApp <- go f'
      put context
      pure evalApp
    go test@(DeBruijnLet _pos (_binder, binding) body) = do
      context <- get
      binding' <- go binding
      withState (\s -> binding' : s) $ go body

-- callByValueBigStep :: DebruijnExpr -> DebruijnExpr
-- callByValueBigStep expr = case eval expr of
--   (VClosure _ expr' _) -> expr'
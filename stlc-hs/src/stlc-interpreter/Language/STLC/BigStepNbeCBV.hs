module Language.STLC.BigStepNbeCBV (callByValueBigStepNbe) where

import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.State (MonadState (get), State, evalState, withState)
import Data.List
import Data.List (intercalate)
import Data.Map qualified as Map
import Debug.Trace
import Language.STLC.Debruijn (DebruijnExpr (..))
import Language.STLC.Syntax qualified as Syn

type NbeEnv = [Normal]

data Normal = Closure Syn.BNFC'Position Syn.Ident (Normal -> Normal) | Neutral Syn.BNFC'Position Neutral

data Neutral = NVar Syn.BNFC'Position Int | NApp Syn.BNFC'Position Neutral Normal

instance Show Normal where
  show (Closure _pos ident _) = "<Closure λ{" ++ show ident ++ "}>"
  show (Neutral _pos neutral) = show neutral

instance Show Neutral where
  show (NVar _pos index) = "x" ++ show index
  show (NApp _pos f x) = "(" ++ show f ++ " " ++ show x ++ ")"

reflect :: NbeEnv -> DebruijnExpr -> Normal
reflect env = \case
  (DeBruijnIndex _pos index) -> env !! index
  (DeBruijnAbs pos binder body) -> Closure pos binder (\expr -> reflect (expr : env) body) -- project the expression into the semantic domain
  test@(DeBruijnApp pos f arg) ->
    case reflect env f of
      (Closure pos ident f) -> f (reflect env arg)
      (Neutral pos n) -> Neutral pos (NApp pos n (reflect env arg))
  (DeBruijnLet _pos (_binder, binding) body) ->
    let binding' = reflect env binding
     in reflect (binding' : env) body

reify :: Int -> Normal -> DebruijnExpr
reify k (Closure pos ident f) = DeBruijnAbs pos ident (reify (k + 1) (f (Neutral pos (NVar pos k))))
reify k (Neutral pos n) = reifyNeutral k n

reifyNeutral :: Int -> Neutral -> DebruijnExpr
reifyNeutral k (NVar pos n) = DeBruijnIndex pos (k - n - 1)
reifyNeutral k (NApp pos n v) = DeBruijnApp pos (reifyNeutral k n) (reify k v)

callByValueBigStepNbe :: DebruijnExpr -> DebruijnExpr
callByValueBigStepNbe t = reify 0 (reflect [] t)

module Language.UntypedLC.SmallStepCBV where

import Control.Monad.State (MonadState (get), State, evalState, withState)
import Data.List
import Debug.Trace (traceM, traceShowId)
import Language.UntypedLC.Debruijn (DebruijnExpr (..))
import Language.UntypedLC.Syntax qualified as Syn

data Binding = NameBind

type Context = [(Syn.Ident, Binding)]

isValue :: DebruijnExpr -> Bool
isValue (DeBruijnAbs _ _ _) = True
isValue _ = False

isLetExpression :: DebruijnExpr -> Bool
isLetExpression (DeBruijnLet _ _ _) = True
isLetExpression _ = False

type CutOff = Int

-- shifting renumbers the indices of the free variables in a term
shifting :: Int -> DebruijnExpr -> DebruijnExpr
shifting d expr = go 0 expr
  where
    go :: Int -> DebruijnExpr -> DebruijnExpr
    go cutoff = \case
      (DeBruijnIndex pos index) ->
        if index >= cutoff
          then (DeBruijnIndex pos $ index + d)
          else (DeBruijnIndex pos index)
      (DeBruijnAbs pos binder body) -> DeBruijnAbs pos binder $ go (cutoff + 1) body
      (DeBruijnApp pos f arg) -> DeBruijnApp pos (go cutoff f) (go cutoff arg)
      (DeBruijnLet pos (binder, binding) body) -> DeBruijnLet pos (binder, go cutoff binding) $ go cutoff body

substitution :: Int -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
substitution j s = \case
  (DeBruijnIndex pos k) ->
    if k == j
      then s
      else (DeBruijnIndex pos k)
  (DeBruijnAbs pos binder body) ->
    let s' = shifting 1 s
        j' = j + 1
     in DeBruijnAbs pos binder $ substitution j' s' body
  (DeBruijnApp pos f arg) -> DeBruijnApp pos (substitution j s f) (substitution j s arg)
  (DeBruijnLet pos (binder, binding) body) -> DeBruijnLet pos (binder, substitution j s binding) $ substitution j s body

step :: DebruijnExpr -> Maybe DebruijnExpr
step (DeBruijnApp _posApp (DeBruijnAbs _posAbs _ident t12) v2)
  | isValue v2 =
      let v2' = shifting 1 v2
          subs = substitution 0 v2' t12
       in pure $ shifting (-1) subs
  | otherwise = Nothing
step (DeBruijnApp _posApp v1 expr)
  | isValue v1 = case step expr of
      Just expr' -> pure (DeBruijnApp _posApp v1 expr')
      Nothing -> Nothing
step (DeBruijnApp _posApp f arg) = case step f of
  Just f' -> pure $ DeBruijnApp _posApp f' arg
  Nothing -> Nothing
step (DeBruijnLet _pos (_letIdent, binding) bodyExpr) =
  -- try to normalize the binding first, do one step evaluation
  case step binding of
    Just binding' -> pure $ DeBruijnLet _pos (_letIdent, binding') bodyExpr
    -- the binding is already in the normalized form.
    Nothing -> case isLetExpression bodyExpr of -- check if the body is also a let expression
      True -> case step bodyExpr of -- if so, step the body
        Just bodyExpr' -> pure (DeBruijnLet _pos (_letIdent, binding) bodyExpr') -- wrap the evaluated body in the let binding
        Nothing -> Nothing
      False ->
        -- The body is not a let binding, it means that we are reaching the last let statement.
        -- We safely substitute the binding in the body
        let binding' = shifting 1 binding
            subs = substitution 0 binding' bodyExpr
         in pure $ shifting (-1) subs
step (DeBruijnIndex _ _) = Nothing
step (DeBruijnAbs _ _ _) = Nothing

smallStepCBV :: DebruijnExpr -> DebruijnExpr
smallStepCBV expr = case traceShowId (step expr) of
  Just expr' -> do
    smallStepCBV expr'
  Nothing -> expr

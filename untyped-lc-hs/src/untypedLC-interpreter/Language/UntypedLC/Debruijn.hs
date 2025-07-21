module Language.UntypedLC.Debruijn (DebruijnExpr (..), debruijnization) where

import Control.Monad.State (MonadState (get), State, runState)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isNothing)
import Language.UntypedLC.Syntax qualified as Syn

data DebruijnExpr where
  DeBruijnIndex :: Syn.BNFC'Position -> Int -> DebruijnExpr
  DeBruijnAbs :: Syn.BNFC'Position -> Syn.Ident -> DebruijnExpr -> DebruijnExpr
  DeBruijnApp :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
  DeBruijnLet :: Syn.BNFC'Position -> (Syn.Ident, DebruijnExpr) -> DebruijnExpr -> DebruijnExpr

instance Eq DebruijnExpr where
  (==) :: DebruijnExpr -> DebruijnExpr -> Bool
  (==) (DeBruijnIndex _ index1) (DeBruijnIndex _ index2) = index1 == index2
  (==) (DeBruijnAbs _ _ expr1) (DeBruijnAbs _ _ expr2) = expr1 == expr2
  (==) (DeBruijnApp _ f1 arg1) (DeBruijnApp _ f2 arg2) = f1 == f2 && arg1 == arg2
  (==) (DeBruijnLet _ (_, binding1) body1) (DeBruijnLet _ (_, binding2) body2) = binding1 == binding2 && body1 == body2
  (==) _ _ = False

instance Show DebruijnExpr where
  show :: DebruijnExpr -> String
  show (DeBruijnIndex _ index) =
    show index
  show (DeBruijnAbs _ binder body) =
    "λ {" ++ show binder ++ "}. " ++ show body
  show (DeBruijnApp _ f arg) =
    "(" ++ show f ++ " " ++ show arg ++ ")"
  show (DeBruijnLet _ (ident, expr) body) =
    "let " ++ show ident ++ " = " ++ show expr ++ " in " ++ show body

type NamingContext = [Syn.Ident]

type App = State NamingContext

debruijnization :: Syn.Prog -> DebruijnExpr
debruijnization (Syn.Program _pos term) = fst $ runState (debruijnizeTerm term) []

debruijnizeTerm :: Syn.Term -> App DebruijnExpr
debruijnizeTerm (Syn.ApplicationTerm _pos s) = debruijnApplication s
debruijnizeTerm (Syn.LetInTerm pos binder expr body) = do
  gamma <- get
  let (expr', _exprGamma) = runState (debruijnizeTerm expr) gamma
      (body', _bodyGamma) = runState (debruijnizeTerm body) $ binder : gamma
  pure $ DeBruijnLet pos (binder, expr') body'
debruijnizeTerm (Syn.LambdaAbsTerm pos binder body) = do
  gamma <- get
  let (body', _bodyGamma) = runState (debruijnizeTerm body) $ binder : gamma
  pure $ DeBruijnAbs pos binder body'

debruijnApplication :: Syn.Application -> App DebruijnExpr
debruijnApplication (Syn.ApplicationAtom _pos atom) = debruijnAtom atom
debruijnApplication (Syn.AApplication pos f arg) = do
  gamma <- get
  let (f', _fGamma) = runState (debruijnApplication f) gamma
      (arg', _argGamma) = runState (debruijnAtom arg) gamma
  pure $ DeBruijnApp pos f' arg'

debruijnAtom :: Syn.Atom -> App DebruijnExpr
debruijnAtom (Syn.AVar pos ident) = do
  gamma <- get
  let index = elemIndex ident gamma
  if isNothing index
    then error $ "Unbounded Variable :" ++ show ident
    else pure $ DeBruijnIndex pos $ fromJust index
debruijnAtom (Syn.ATerm _pos term) = debruijnizeTerm term

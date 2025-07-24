module Language.STLC.Debruijn (DebruijnExpr (..), Typ (..), DeBruijnNum (..), debruijnization) where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.State (MonadState (get), State, runState)
import Data.Either (fromLeft, fromRight, isRight)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isNothing)
import GHC.Num (integerToWord)
import Language.STLC.Common
import Language.STLC.Syntax qualified as Syn

data Typ where
  TNat :: Typ
  TBool :: Typ
  TArrow :: Typ -> Typ -> Typ
  TPair :: Typ -> Typ -> Typ

deriving stock instance Show Typ

deriving stock instance Eq Typ

data DeBruijnNum where
  -- Natural Number
  DeBruijnSucc :: Syn.BNFC'Position -> DebruijnExpr -> DeBruijnNum
  DeBruijnZero :: Syn.BNFC'Position -> DeBruijnNum
  DeBruijnPred :: Syn.BNFC'Position -> DebruijnExpr -> DeBruijnNum

instance Show DeBruijnNum where
  show :: DeBruijnNum -> String
  show ((DeBruijnSucc _pos i)) = "(succ " ++ show i ++ ")"
  show ((DeBruijnPred _pos i)) = "(pred " ++ show i ++ ")"
  show ((DeBruijnZero pos)) = "zero"

data DebruijnExpr where
  DeBruijnIndex :: Syn.BNFC'Position -> Int -> DebruijnExpr
  DeBruijnAbs :: Syn.BNFC'Position -> Syn.Ident -> Typ -> DebruijnExpr -> DebruijnExpr
  DeBruijnApp :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
  DeBruijnLet :: Syn.BNFC'Position -> (Syn.Ident, DebruijnExpr) -> DebruijnExpr -> DebruijnExpr
  DeBruijnIfThenElse :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
  -- Tuple
  DeBruijnPair :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
  DeBruijnFirst :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr
  DeBruijnSecond :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr
  -- Boolean
  DeBruijnTrue :: Syn.BNFC'Position -> DebruijnExpr
  DeBruijnFalse :: Syn.BNFC'Position -> DebruijnExpr
  -- Num
  DeBruijnNum :: DeBruijnNum -> DebruijnExpr

instance Eq DebruijnExpr where
  (==) :: DebruijnExpr -> DebruijnExpr -> Bool
  (==) (DeBruijnIndex _ index1) (DeBruijnIndex _ index2) = index1 == index2
  (==) (DeBruijnAbs _ _ _ expr1) (DeBruijnAbs _ _ _ expr2) = expr1 == expr2
  (==) (DeBruijnApp _ f1 arg1) (DeBruijnApp _ f2 arg2) = f1 == f2 && arg1 == arg2
  (==) (DeBruijnLet _ (_, binding1) body1) (DeBruijnLet _ (_, binding2) body2) = binding1 == binding2 && body1 == body2
  (==) _ _ = False

instance Show DebruijnExpr where
  show :: DebruijnExpr -> String
  show (DeBruijnIndex _ index) =
    show index
  show (DeBruijnAbs _ binder typ body) =
    "λ {" ++ show binder ++ " : " ++ show typ ++ "}. " ++ show body
  show (DeBruijnApp _ f arg) =
    "(" ++ show f ++ " " ++ show arg ++ ")"
  show (DeBruijnLet _ (ident, expr) body) =
    "let " ++ show ident ++ " = " ++ show expr ++ " in " ++ show body
  show (DeBruijnTrue _) =
    "true"
  show (DeBruijnFalse _) =
    "false"
  show (DeBruijnIfThenElse _ cond thenBranch elseBranch) =
    "if " ++ show cond ++ " then " ++ show thenBranch ++ " else " ++ show elseBranch
  show (DeBruijnPair _ fst snd) = "{" ++ show fst ++ " , " ++ show snd ++ "}"
  show (DeBruijnFirst _ elem) = "{first(" ++ show elem ++ ")}"
  show (DeBruijnSecond _ elem) = "{second(" ++ show elem ++ ")}"
  show (DeBruijnNum num) = show num

type NamingContext = [Syn.Ident]

data DebruijnError = NegativeNaturalNumber String | UnboundedVariable String deriving stock (Show)

type App a = ExceptT DebruijnError (State NamingContext) a

convertTyp :: Syn.Type -> Typ
convertTyp (Syn.Nat _pos) = TNat
convertTyp (Syn.Bool _pos) = TBool
convertTyp (Syn.Arrow _pos f arg) = TArrow (convertTyp f) (convertTyp arg)

debruijnization :: Syn.Prog -> Either DebruijnError DebruijnExpr
debruijnization (Syn.Program _pos term) = do
  fst $ runState (runExceptT (debruijnizeTerm term)) []

debruijnizeTerm :: Syn.Term -> App DebruijnExpr
debruijnizeTerm (Syn.ApplicationTerm _pos s) = debruijnApplication s
debruijnizeTerm (Syn.LetInTerm pos binder expr body) = do
  gamma <- get
  let (eitherExpr, _exprGamma) = runState (runExceptT (debruijnizeTerm expr)) gamma
  guardM (isRight eitherExpr) $ fromLeft undefined eitherExpr
  let (eitherBody, _bodyGamma) = runState (runExceptT (debruijnizeTerm body)) $ binder : gamma
  guardM (isRight eitherBody) $ fromLeft undefined eitherBody
  let expr' = fromRight undefined eitherExpr
      body' = fromRight undefined eitherBody
  pure $ DeBruijnLet pos (binder, expr') body'
debruijnizeTerm (Syn.LambdaAbsTerm pos binder typ body) = do
  gamma <- get
  let typ' = convertTyp typ
  let (eitherBody, _bodyGamma) = runState (runExceptT (debruijnizeTerm body)) $ binder : gamma
  guardM (isRight eitherBody) $ fromLeft undefined eitherBody
  let body' = fromRight undefined eitherBody
  pure $ DeBruijnAbs pos binder typ' body'
debruijnizeTerm (Syn.IfTerm pos condition thenBranch elseBranch) = do
  condition' <- debruijnizeTerm condition
  thenBranch' <- debruijnizeTerm thenBranch
  elseBranch' <- debruijnizeTerm elseBranch
  pure $ DeBruijnIfThenElse pos condition' thenBranch' elseBranch'
debruijnizeTerm (Syn.TrueTerm pos) = pure $ DeBruijnTrue pos
debruijnizeTerm (Syn.FalseTerm pos) = pure $ DeBruijnFalse pos
debruijnizeTerm (Syn.SuccTerm pos num) = do
  num' <- debruijnizeTerm num
  pure . DeBruijnNum $ DeBruijnSucc pos num'
debruijnizeTerm (Syn.PredTerm pos num) = do
  num' <- debruijnizeTerm num
  pure . DeBruijnNum $ DeBruijnPred pos num'
debruijnizeTerm (Syn.ZeroTerm pos) = do
  pure . DeBruijnNum $ DeBruijnZero pos
debruijnizeTerm (Syn.PairTerm pos fst snd) = do
  fst' <- debruijnizeTerm fst
  snd' <- debruijnizeTerm snd
  pure $ DeBruijnPair pos fst' snd'
debruijnizeTerm (Syn.FstTerm pos element) = do
  element' <- debruijnizeTerm element
  pure $ DeBruijnFirst pos element'
debruijnizeTerm (Syn.SndTerm pos element) = do
  element' <- debruijnizeTerm element
  pure $ DeBruijnSecond pos element'

debruijnApplication :: Syn.Application -> App DebruijnExpr
debruijnApplication (Syn.ApplicationAtom _pos atom) = debruijnAtom atom
debruijnApplication (Syn.AApplication pos f arg) = do
  gamma <- get
  let (eitherF, _fGamma) = runState (runExceptT (debruijnApplication f)) gamma
  guardM (isRight eitherF) $ fromLeft undefined eitherF
  let (eitherArg, _argGamma) = runState (runExceptT (debruijnAtom arg)) gamma
  guardM (isRight eitherArg) $ fromLeft undefined eitherArg
  let f' = fromRight undefined eitherF
      arg' = fromRight undefined eitherArg
  pure $ DeBruijnApp pos f' arg'

debruijnAtom :: Syn.Atom -> App DebruijnExpr
debruijnAtom (Syn.AVar pos ident) = do
  gamma <- get
  let index = elemIndex ident gamma
  if isNothing index
    then
      throwError $
        UnboundedVariable $
          mconcat
            [ "At position ",
              show pos,
              ", Unbounded Variable : ",
              show ident
            ]
    else pure $ DeBruijnIndex pos $ fromJust index
debruijnAtom (Syn.ATerm _pos term) = debruijnizeTerm term

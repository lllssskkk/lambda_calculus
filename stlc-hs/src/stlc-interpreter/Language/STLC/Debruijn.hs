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
import Language.STLC.Common (guardM)
import Language.STLC.Syntax qualified as Syn

data Typ where
  TNat :: Typ
  TBool :: Typ
  TArrow :: Typ -> Typ -> Typ
  TPair :: Typ -> Typ -> Typ
  TList :: Typ -> Typ

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
  show ((DeBruijnZero _pos)) = "zero"

data DebruijnExpr where
  DeBruijnIndex :: Syn.BNFC'Position -> Int -> DebruijnExpr
  DeBruijnAbs :: Syn.BNFC'Position -> Syn.Ident -> Typ -> DebruijnExpr -> DebruijnExpr
  DeBruijnApp :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
  DeBruijnLet :: Syn.BNFC'Position -> (Syn.Ident, DebruijnExpr) -> DebruijnExpr -> DebruijnExpr
  DeBruijnIfThenElse :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
  DeBruijnFix :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr
  -- Tuple
  DeBruijnPair :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
  DeBruijnFirst :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr
  DeBruijnSecond :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr
  -- Boolean
  DeBruijnTrue :: Syn.BNFC'Position -> DebruijnExpr
  DeBruijnFalse :: Syn.BNFC'Position -> DebruijnExpr
  -- Num
  DeBruijnNum :: DeBruijnNum -> DebruijnExpr
  DeBruijnIsZero :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr
  DeBruijnIsSucc :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr
  DeBruijnIsPred :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr
  -- List
  DeBruijnCons :: Syn.BNFC'Position -> Typ -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
  DeBruijnNil :: Syn.BNFC'Position -> Typ -> DebruijnExpr
  DeBruijnIsNil :: Syn.BNFC'Position -> Typ -> DebruijnExpr -> DebruijnExpr
  DeBruijnHead :: Syn.BNFC'Position -> Typ -> DebruijnExpr -> DebruijnExpr
  DeBruijnTail :: Syn.BNFC'Position -> Typ -> DebruijnExpr -> DebruijnExpr

instance Eq DebruijnExpr where
  (==) :: DebruijnExpr -> DebruijnExpr -> Bool
  (==) (DeBruijnIndex _ index1) (DeBruijnIndex _ index2) = index1 == index2
  (==) (DeBruijnAbs _ _ _ expr1) (DeBruijnAbs _ _ _ expr2) = expr1 == expr2
  (==) (DeBruijnApp _ f1 arg1) (DeBruijnApp _ f2 arg2) = f1 == f2 && arg1 == arg2
  (==) (DeBruijnLet _ (_, binding1) body1) (DeBruijnLet _ (_, binding2) body2) = binding1 == binding2 && body1 == body2
  (==) _ _ = undefined

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
  show (DeBruijnIfThenElse _ cond thenBranch elseBranch) =
    "if " ++ show cond ++ " then " ++ show thenBranch ++ " else " ++ show elseBranch
  show (DeBruijnFix _ f) = "fix " ++ show f
  show (DeBruijnTrue _) =
    "true"
  show (DeBruijnFalse _) =
    "false"
  show (DeBruijnPair _ first second) = "{" ++ show first ++ " , " ++ show second ++ "}"
  show (DeBruijnFirst _ pair) = "{first(" ++ show pair ++ ")}"
  show (DeBruijnSecond _ pair) = "{second(" ++ show pair ++ ")}"
  show (DeBruijnNum num) = show num
  show (DeBruijnIsZero _pos num) = "isZero(" ++ show num ++ ")"
  show (DeBruijnIsSucc _pos num) = "isSucc(" ++ show num ++ ")"
  show (DeBruijnIsPred _pos num) = "isPred(" ++ show num ++ ")"
  -- List
  show (DeBruijnCons _pos typ headElem tailElem) = "cons[" ++ show typ ++ "] " ++ (show headElem) ++ " " ++ show tailElem
  show (DeBruijnNil _pos typ) = "nil[" ++ show typ ++ "]"
  show (DeBruijnIsNil _pos typ expr) = "isNil[" ++ show typ ++ "]" ++ show expr
  show (DeBruijnHead _pos typ expr) = "head[" ++ show typ ++ "]" ++ show expr
  show (DeBruijnTail _pos typ expr) = "tail[" ++ show typ ++ "]" ++ show expr

type NamingContext = [Syn.Ident]

data DebruijnError = NegativeNaturalNumber String | UnboundedVariable String deriving stock (Show)

type App a = ExceptT DebruijnError (State NamingContext) a

convertTyp :: Syn.Type -> Typ
-- convertTyp (Syn.Nat _pos) = TNat
-- convertTyp (Syn.Bool _pos) = TBool
convertTyp (Syn.Arrow _pos f arg) = TArrow (convertBaseTyp f) (convertTyp arg)
convertTyp (Syn.Base _pos base) = convertBaseTyp base

convertBaseTyp :: Syn.BaseType -> Typ
convertBaseTyp (Syn.Nat _pos) = TNat
convertBaseTyp (Syn.Bool _pos) = TBool
convertBaseTyp (Syn.Pair _pos firstTyp secondTyp) = TPair (convertTyp firstTyp) (convertTyp secondTyp)
convertBaseTyp (Syn.ListT _pos base) = TList $ convertTyp base

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
debruijnizeTerm (Syn.FixTerm pos f) = do
  f' <- debruijnizeTerm f
  pure $ DeBruijnFix pos f'
debruijnizeTerm (Syn.TrueTerm pos) = pure $ DeBruijnTrue pos
debruijnizeTerm (Syn.FalseTerm pos) = pure $ DeBruijnFalse pos
-- Num
debruijnizeTerm (Syn.SuccTerm pos num) = do
  num' <- debruijnizeTerm num
  pure . DeBruijnNum $ DeBruijnSucc pos num'
debruijnizeTerm (Syn.PredTerm pos num) = do
  num' <- debruijnizeTerm num
  pure . DeBruijnNum $ DeBruijnPred pos num'
debruijnizeTerm (Syn.ZeroTerm pos) = do
  pure . DeBruijnNum $ DeBruijnZero pos
debruijnizeTerm (Syn.IsZeroTerm pos term) = do
  term' <- debruijnizeTerm term
  pure $ DeBruijnIsZero pos term'
debruijnizeTerm (Syn.IsSuccTerm pos term) = do
  term' <- debruijnizeTerm term
  pure $ DeBruijnIsSucc pos term'
debruijnizeTerm (Syn.IsPredTerm pos term) = do
  term' <- debruijnizeTerm term
  pure $ DeBruijnIsPred pos term'
-- Pair
debruijnizeTerm (Syn.PairTerm pos first second) = do
  first' <- debruijnizeTerm first
  second' <- debruijnizeTerm second
  pure $ DeBruijnPair pos first' second'
debruijnizeTerm (Syn.FstTerm pos element) = do
  element' <- debruijnizeTerm element
  pure $ DeBruijnFirst pos element'
debruijnizeTerm (Syn.SndTerm pos element) = do
  element' <- debruijnizeTerm element
  pure $ DeBruijnSecond pos element'
-- List
debruijnizeTerm (Syn.NilTerm pos typ) = pure $ DeBruijnNil pos (convertTyp typ)
debruijnizeTerm (Syn.IsNilTerm pos typ xs) = do
  xs' <- debruijnizeTerm xs
  pure $ DeBruijnIsNil pos (convertTyp typ) xs'
debruijnizeTerm (Syn.HeadTerm pos typ xs) = do
  xs' <- debruijnizeTerm xs
  pure $ DeBruijnHead pos (convertTyp typ) xs'
debruijnizeTerm (Syn.TailTerm pos typ xs) = do
  xs' <- debruijnizeTerm xs
  pure $ DeBruijnTail pos (convertTyp typ) xs'

debruijnApplication :: Syn.Application -> App DebruijnExpr
debruijnApplication (Syn.ApplicationAtom _pos atom) = debruijnAtom atom
debruijnApplication (Syn.AApplication pos (Syn.AApplication _pos' (Syn.ApplicationAtom _pos (Syn.ACons _pos'' typ)) headElem) tailElem) = do
  gamma <- get
  let (eitherHead, _HeadGamma) = runState (runExceptT (debruijnAtom headElem)) gamma
  guardM (isRight eitherHead) $ fromLeft undefined eitherHead
  let (eitherTail, _argGamma) = runState (runExceptT (debruijnAtom tailElem)) gamma
  guardM (isRight eitherTail) $ fromLeft undefined eitherTail
  let headElem' = fromRight undefined eitherHead
      tailElem' = fromRight undefined eitherTail
  pure $ DeBruijnCons pos (convertTyp typ) headElem' tailElem'
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
debruijnAtom (Syn.ACons _pos _typ) = error "We shouldn't enter this branch, it is probably the case that your list definition is incorrect."

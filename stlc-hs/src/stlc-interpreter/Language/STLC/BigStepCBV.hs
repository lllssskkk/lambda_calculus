{-# LANGUAGE RecursiveDo #-}

module Language.STLC.BigStepCBV (Value, callByValueBigStep, debugShowValue, getType) where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Fix (mfix)
import Control.Monad.State (MonadState (get, put), State, evalState, modify, runState, withState)
import Data.List (intercalate, (!?))
import Language.STLC.Common
import Language.STLC.Debruijn (DeBruijnNum (..), DebruijnExpr (..), Typ (..))
import Language.STLC.Syntax.Generated.AbsSTLC qualified as Syn

type Env = [Value]

data NumValue where
  VSucc :: Value -> NumValue
  VZero :: NumValue
  VPred :: Value -> NumValue

instance Show NumValue where
  show n =
    let (countSucc, countPred) = go n 0 0
     in show $ countSucc - countPred
    where
      go :: NumValue -> Int -> Int -> (Int, Int)
      go VZero countSucc countPred = (countSucc, countPred)
      go (VSucc (VNum v)) countSucc countPred = go v (countSucc + 1) countPred
      go (VPred (VNum v)) countSucc countPred = go v countSucc (countPred + 1)
      go _ _ _ = error "Impossible : Beacuase of the strong normalization property of STLC, no other value could be supplied to NumValue"

data Value where
  VClosure :: Syn.Ident -> Typ -> DebruijnExpr -> Env -> Value
  VTrue :: Value
  VFalse :: Value
  VNum :: NumValue -> Value
  VPair :: Value -> Value -> Value

-- VFix :: Value -> Value

instance Show Value where
  show :: Value -> String
  show test = debugShowValue test

-- "<closure λ{" ++ show ident ++ "}. " ++ show body ++ ">"

debugShowValue :: Value -> String
debugShowValue (VClosure ident typ body env) =
  "<closure λ{"
    ++ show ident
    ++ " : "
    ++ show typ
    ++ "} . "
    ++ show body
    ++ " with env: ["
    ++ intercalate ", " (map debugShowValue env)
    ++ "]>"
debugShowValue (VTrue) = "True"
debugShowValue (VFalse) = "False"
debugShowValue (VNum num) = show num
debugShowValue (VPair first second) = "Pair(" ++ show first ++ "," ++ show second ++ ")"

type CallByValueBigStep = State Env

type TypingCtx = [Typ]

data TypeCheckingError = TypeCheckingError String deriving stock (Show)

type TypeChecking a = ExceptT TypeCheckingError (State TypingCtx) a

getType :: DebruijnExpr -> Either TypeCheckingError Typ
getType expr = do
  fst $ runState (runExceptT (go expr)) []
  where
    go :: DebruijnExpr -> TypeChecking Typ
    go (DeBruijnIndex pos i) = do
      typCtx <- get
      guardM (i <= length typCtx) $
        TypeCheckingError $
          mconcat
            [ "At position ",
              show pos,
              "A DeBruijnIndex ",
              show i,
              " tries to access the typing context ",
              show typCtx,
              ", but out of bound"
            ]
      pure $ typCtx !! i
    go (DeBruijnAbs _ _var typ body) = do
      modify $ (typ :)
      bodyTyp <- go body
      pure $ TArrow typ bodyTyp
    go app@(DeBruijnApp pos f arg) = do
      fTyp <- go f
      guardM (isFunctionTyp fTyp) $
        TypeCheckingError $
          mconcat
            [ "At position ",
              show pos,
              "function application expression ",
              show app,
              " should have a Arrow type, but instead receives a value of type ",
              show fTyp
            ]
      argTyp <- go arg
      case fTyp of
        (TArrow input output) ->
          if input == argTyp
            then pure output
            else
              throwError $
                TypeCheckingError $
                  mconcat
                    [ "At position ",
                      show pos,
                      " function application expression ",
                      show app,
                      " should have a ",
                      show output,
                      "type as input, but instead receives a value of type ",
                      show argTyp
                    ]
        _ -> error "Impossible "
    go (DeBruijnLet _ (_binder, binding) body) = do
      bindingTyp <- go binding
      modify (bindingTyp :)
      go body
    go (DeBruijnTrue _pos) = pure TBool
    go (DeBruijnFalse _pos) = pure TBool
    go ifExpr@(DeBruijnIfThenElse pos condition thenBranch elseBranch) = do
      conditionTyp <- go condition
      guardM (conditionTyp == TBool) $
        TypeCheckingError $
          mconcat
            [ "At position, ",
              show pos,
              "If expression ",
              show ifExpr,
              " should receive a TBool condition, but instead receives a value of type ",
              show conditionTyp
            ]
      thenBranchTyp <- go thenBranch
      elseBranchTyp <- go elseBranch
      guardM (thenBranchTyp == elseBranchTyp) $
        TypeCheckingError $
          mconcat
            [ "At position ",
              show pos,
              " If expression ",
              show ifExpr,
              " should have the same type on both branches, but instead thenBranch has type of ",
              show thenBranchTyp,
              " and elseBranch has type of ",
              show elseBranchTyp
            ]
      pure thenBranchTyp
    go (DeBruijnFix pos f) = do
      fTyp <- go f
      case fTyp of
        (TArrow inputTyp outputTyp) ->
          if inputTyp == outputTyp
            then pure outputTyp
            else
              throwError $
                TypeCheckingError $
                  mconcat
                    [ "At Position ",
                      show pos,
                      " Fix expression expects the input function has the same argument type of return type, but the input function has a typ of ",
                      show fTyp
                    ]
        _ ->
          throwError $
            TypeCheckingError $
              mconcat
                [ "At Position ",
                  show pos,
                  " Fix expression expects an argument of Function Type, but receives a value of type ",
                  show fTyp
                ]
    go (DeBruijnNum num) = do
      case num of
        (DeBruijnSucc pos i) -> do
          iTyp <- go i
          guardM (iTyp == TNat) $
            TypeCheckingError $
              mconcat
                [ "At Position ",
                  show pos,
                  " succ expression expects an argument of type Nat, but receives a value of type ",
                  show iTyp
                ]
          pure TNat
        (DeBruijnPred pos i) -> do
          iTyp <- go i
          guardM (iTyp == TNat) $
            TypeCheckingError $
              mconcat
                [ "At Position ",
                  show pos,
                  " Pred expression expects an argument of type Nat, but receives a value of type ",
                  show iTyp
                ]
          pure TNat
        (DeBruijnZero _pos) -> pure TNat
    go (DeBruijnIsZero pos term) = do
      termTyp <- go term
      guardM (termTyp == TNat) $
        TypeCheckingError $
          mconcat
            [ "At Position ",
              show pos,
              " isZero expression expects an argument of type Nat, but receives a value of type ",
              show termTyp
            ]
      pure TBool
    go (DeBruijnIsSucc pos term) = do
      termTyp <- go term
      guardM (termTyp == TNat) $
        TypeCheckingError $
          mconcat
            [ "At Position ",
              show pos,
              " isSucc expression expects an argument of type Nat, but receives a value of type ",
              show termTyp
            ]
      pure TBool
    go (DeBruijnIsPred pos term) = do
      termTyp <- go term
      guardM (termTyp == TNat) $
        TypeCheckingError $
          mconcat
            [ "At Position ",
              show pos,
              " isPred expression expects an argument of type Nat, but receives a value of type ",
              show termTyp
            ]
      pure TBool
    go (DeBruijnPair _pos first second) = do
      context <- get
      firstTyp <- go first
      put context
      secondTyp <- go second
      pure $ TPair firstTyp secondTyp
    go (DeBruijnFirst pos pair) = do
      pairTyp <- go pair
      guardM (isPairTyp pairTyp) $
        TypeCheckingError $
          mconcat
            [ "At Position ",
              show pos,
              " First expression expects an argument of type Pair, but receives a value of type ",
              show pairTyp
            ]
      case pairTyp of
        (TPair _ secondTyp) -> pure secondTyp
        _ ->
          error $
            mconcat
              [ "Impossible : At Position ",
                show pos,
                " First expression expects an argument of type Pair, but receives a value of type ",
                show pairTyp
              ]
    go (DeBruijnSecond pos pair) = do
      pairTyp <- go pair
      guardM (isPairTyp pairTyp) $
        TypeCheckingError $
          mconcat
            [ "At Position ",
              show pos,
              " Second expression expects an argument of type Pair, but receives a value of type ",
              show pairTyp
            ]
      case pairTyp of
        (TPair _ secondTyp) -> pure secondTyp
        _ ->
          error $
            mconcat
              [ "Impossible : At Position ",
                show pos,
                " Pair expression expects an argument of type Pair, but receives a value of type ",
                show pairTyp
              ]

isFunctionTyp :: Typ -> Bool
isFunctionTyp (TArrow _ _) = True
isFunctionTyp _ = False

isPairTyp :: Typ -> Bool
isPairTyp (TPair _ _) = True
isPairTyp _ = False

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
    go (DeBruijnAbs _pos binder typ body) = do
      gamma <- get
      pure $ VClosure binder typ body gamma
    go (DeBruijnApp _pos f arg) = do
      arg' <- go arg
      context <- get
      f' <- go f
      case f' of
        (VClosure _binder _typ closureF closureEnv) -> do
          put (arg' : closureEnv)
          evalApp <- go closureF
          put context
          pure evalApp
        _ -> error "Impossible One : Beacuase of the strong normalization property of STLC, only Function value could be supplied to DeBruijnApp"
    go (DeBruijnLet _pos (_binder, binding) body) = do
      binding' <- go binding
      withState (\s -> binding' : s) $ go body
    go (DeBruijnFix _pos f) = do
      f' <- go f
      _context <- get
      case f' of
        (VClosure _binder _typ body bodyEnv) -> do
          -- ! Need more clearance on this
          -- mfix ties the knot:  v is the result of the *whole* fix,
          -- and we evaluate the body in an env where x ↦ v
          mfix $ \v -> withState (const (v : bodyEnv)) (go body)
        _ -> error "Impossible Two : Beacuase of the strong normalization property of STLC, only Function value could be supplied to DeBruijnFix"
    go (DeBruijnTrue _pos) = pure VTrue
    go (DeBruijnFalse _pos) = pure VFalse
    go (DeBruijnNum num) = do
      result <- case num of
        (DeBruijnSucc _pos i) -> do
          i' <- go i
          pure $ VSucc i'
        (DeBruijnPred _pos i) -> do
          i' <- go i
          pure $ VPred i'
        (DeBruijnZero __pos) -> pure VZero
      pure . VNum $ normalizeNumValue result
    go (DeBruijnIsZero _pos term) = do
      term' <- go term
      case term' of
        (VNum VZero) -> pure VTrue
        (VNum _) -> pure VFalse
        _ -> error "Impossible Three : Beacuase of the strong normalization property of STLC, only Num value could be supplied to DeBruijnIsZero"
    go (DeBruijnIsSucc _pos term) = do
      term' <- go term
      case term' of
        (VNum (VSucc _)) -> pure VTrue
        (VNum _) -> pure VFalse
        _ -> error "Impossible Three : Beacuase of the strong normalization property of STLC, only Num value could be supplied to DeBruijnIsSucc"
    go (DeBruijnIsPred _pos term) = do
      term' <- go term
      case term' of
        (VNum (VPred _)) -> pure VTrue
        (VNum _) -> pure VFalse
        _ -> error "Impossible Three : Beacuase of the strong normalization property of STLC, only Num value could be supplied to DeBruijnIsSucc"
    go (DeBruijnPair _pos first second) = do
      context <- get
      first' <- go first
      put context
      second' <- go second
      pure $ VPair first' second'
    go (DeBruijnFirst _pos pair) = do
      pair' <- go pair
      case pair' of
        VPair first _second -> pure first
        _ -> error "Impossible Four: Beacuase of the strong normalization property of STLC, only Pair value could be supplied to DeBruijnFirst"
    go (DeBruijnSecond _pos pair) = do
      pair' <- go pair
      case pair' of
        VPair _first second -> pure second
        _ -> error "Impossible Five: Beacuase of the strong normalization property of STLC, only Pair value could be supplied to DeBruijnFirst"
    go (DeBruijnIfThenElse _pos condition thenBranch elseBranch) = do
      condition' <- go condition
      case condition' of
        VTrue -> go thenBranch
        VFalse -> go elseBranch
        _ -> error "Impossible Six: Beacuase of the strong normalization property of STLC, only Bool value could be supplied to DeBruijnIfThenElse as condition"

normalizeNumValue :: NumValue -> NumValue
normalizeNumValue numV =
  let (countSucc, countPred) = go numV 0 0
   in buildNum (countSucc - countPred)
  where
    go :: NumValue -> Int -> Int -> (Int, Int)
    go VZero countSucc countPred = (countSucc, countPred)
    go (VSucc (VNum v)) countSucc countPred = go v (countSucc + 1) countPred
    go (VPred (VNum v)) countSucc countPred = go v countSucc (countPred + 1)
    go _ _ _ = error "Impossible Seven: Beacuase of the strong normalization property of STLC, no other value could be supplied to NumValue"
    buildNum :: Int -> NumValue
    buildNum n
      | n < 0 = VPred . VNum . buildNum $ (n + 1)
      | n > 0 = VSucc . VNum . buildNum $ (n - 1)
      | otherwise = VZero

-- DeBruijnTrue :: Syn.BNFC'Position -> DebruijnExpr
-- DeBruijnFalse :: Syn.BNFC'Position -> DebruijnExpr
-- DeBruijnIfThenElse :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr

-- callByValueBigStep :: DebruijnExpr -> DebruijnExpr
-- callByValueBigStep expr = case eval expr of
--   (VClosure _ expr' _) -> expr'
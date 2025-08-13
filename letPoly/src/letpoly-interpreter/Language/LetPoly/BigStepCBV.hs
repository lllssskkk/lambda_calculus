{-# LANGUAGE RecursiveDo #-}

module Language.LetPoly.BigStepCBV (Value, callByValueBigStep, debugShowValue) where

import Control.Monad.Fix (mfix)
import Control.Monad.Reader (MonadReader (ask), Reader, local, runReader)
import Data.List (intercalate, (!?))
import Language.LetPoly.Syntax.Generated.AbsLetPoly qualified as Syn
import Language.LetPoly.UntypedDeBruijn (DeBruijnNum (..), UntypedDeBruijnExpr (..))

type Env = [Value]

data ListValue where
  VNil :: ListValue
  VCons :: Value -> Value -> ListValue

instance Show ListValue where
  show = go
    where
      -- Collect the textual form of each element, then join with “, ”
      go :: ListValue -> String
      go VNil = "[]"
      go xs = "[" ++ intercalate ", " (elems xs) ++ "]"

      -- Walk down the list, making sure we stop when the tail is
      -- anything other than another list (so we do not crash on
      -- ill-formed values).
      elems :: ListValue -> [String]
      elems VNil = []
      elems (VCons hd tl) =
        show hd
          : case tl of
            VList rest -> elems rest -- proper list tail
            _ -> error "Impossible : Beacuase of the strong normalization property of LetPoly, no other value could be supplied to VCons"

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
      go _ _ _ = error "Impossible : Beacuase of the strong normalization property of LetPoly, no other value could be supplied to NumValue"

data Value where
  VClosure :: Syn.Ident -> UntypedDeBruijnExpr -> Env -> Value
  VTrue :: Value
  VFalse :: Value
  VList :: ListValue -> Value
  VNum :: NumValue -> Value
  VPair :: Value -> Value -> Value

instance Show Value where
  show :: Value -> String
  show test = debugShowValue test


debugShowValue :: Value -> String
debugShowValue (VClosure ident body env) =
  "<closure λ{"
    ++ show ident
    ++ "} . "
    ++ show body
    ++ " with env: ["
    ++ intercalate ", " (map debugShowValue env)
    ++ "]>"
debugShowValue (VTrue) = "True"
debugShowValue (VFalse) = "False"
debugShowValue (VNum num) = show num
debugShowValue (VPair first second) = "Pair(" ++ show first ++ "," ++ show second ++ ")"
debugShowValue (VList xs) = show xs

type CallByValueBigStep = Reader Env

callByValueBigStep :: UntypedDeBruijnExpr -> Value
callByValueBigStep expr = runReader (go expr) []
  where
    go :: UntypedDeBruijnExpr -> CallByValueBigStep Value
    go (DeBruijnIndex _pos index) = do
      gamma <- ask
      let retrieved = gamma !? index
      case retrieved of
        Just x -> pure x
        Nothing -> error $ "The index is " ++ show index ++ " , The environment list is " ++ show gamma
    go (DeBruijnAbs _pos binder body) = do
      gamma <- ask
      pure $ VClosure binder body gamma
    go (DeBruijnApp _pos f arg) = do
      arg' <- go arg
      f' <- go f
      case f' of
        (VClosure _binder closureF closureEnv) -> do
          evalApp <- local (const (arg' : closureEnv)) (go closureF)
          pure evalApp
        _ -> error "Impossible One : Beacuase of the strong normalization property of LetPoly, only Function value could be supplied to DeBruijnApp"
    go (DeBruijnLet _pos (_binder, binding) body) = do
      binding' <- go binding
      local (\s -> binding' : s) $ go body
    go (DeBruijnFix _pos f) = do
      f' <- go f
      case f' of
        (VClosure _binder body bodyEnv) -> do
          -- ! Need more clearance on this
          -- mfix ties the knot:  v is the result of the *whole* fix,
          -- and we evaluate the body in an env where x ↦ v
          mfix $ \v -> local (const (v : bodyEnv)) (go body)
        _ -> error "Impossible Two : Beacuase of the strong normalization property of LetPoly, only Function value could be supplied to DeBruijnFix"
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
        _ -> error "Impossible Three : Beacuase of the strong normalization property of LetPoly, only Num value could be supplied to DeBruijnIsZero"
    go (DeBruijnIsSucc _pos term) = do
      term' <- go term
      case term' of
        (VNum (VSucc _)) -> pure VTrue
        (VNum _) -> pure VFalse
        _ -> error "Impossible Three : Beacuase of the strong normalization property of LetPoly, only Num value could be supplied to DeBruijnIsSucc"
    go (DeBruijnIsPred _pos term) = do
      term' <- go term
      case term' of
        (VNum (VPred _)) -> pure VTrue
        (VNum _) -> pure VFalse
        _ -> error "Impossible Three : Beacuase of the strong normalization property of LetPoly, only Num value could be supplied to DeBruijnIsSucc"
    go (DeBruijnPair _pos first second) = do
      first' <- go first
      second' <- go second
      pure $ VPair first' second'
    go (DeBruijnFirst _pos pair) = do
      pair' <- go pair
      case pair' of
        VPair first _second -> pure first
        _ -> error "Impossible Four: Beacuase of the strong normalization property of LetPoly, only Pair value could be supplied to DeBruijnFirst"
    go (DeBruijnSecond _pos pair) = do
      pair' <- go pair
      case pair' of
        VPair _first second -> pure second
        _ -> error "Impossible Five: Beacuase of the strong normalization property of LetPoly, only Pair value could be supplied to DeBruijnFirst"
    go (DeBruijnIfThenElse _pos condition thenBranch elseBranch) = do
      condition' <- go condition
      case condition' of
        VTrue -> go thenBranch
        VFalse -> go elseBranch
        _ -> error "Impossible Six: Beacuase of the strong normalization property of LetPoly, only Bool value could be supplied to DeBruijnIfThenElse as condition"
    go (DeBruijnCons _pos x xs) = do
      x' <- go x
      xs' <- go xs
      pure $ VList $ VCons x' xs'
    go (DeBruijnNil _pos) = pure $ VList VNil
    go (DeBruijnIsNil _pos xs) = do
      xs' <- go xs
      case xs' of
        (VList VNil) -> pure VTrue
        (VList _) -> pure VFalse
        _ -> error "Impossible Seven: Beacuase of the strong normalization property of LetPoly, only VList value could be supplied to DeBruijnIsNil"
    go (DeBruijnHead _pos xs) = do
      xs' <- go xs
      case xs' of
        (VList VNil) -> error "Runtime Error : A Head operation is applied to an empty list"
        (VList (VCons headElem _tailElem)) -> pure headElem
        _ -> error "Impossible Eight: Beacuase of the strong normalization property of LetPoly, only VList value could be supplied to DeBruijnHead"
    go (DeBruijnTail _pos xs) = do
      xs' <- go xs
      case xs' of
        (VList VNil) -> pure (VList VNil)
        (VList (VCons _headElem tailElem)) -> pure tailElem
        _ -> error "Impossible Nine: Beacuase of the strong normalization property of LetPoly, only VList value could be supplied to DeBruijnHead"

normalizeNumValue :: NumValue -> NumValue
normalizeNumValue numV =
  let (countSucc, countPred) = go numV 0 0
   in buildNum (countSucc - countPred)
  where
    go :: NumValue -> Int -> Int -> (Int, Int)
    go VZero countSucc countPred = (countSucc, countPred)
    go (VSucc (VNum v)) countSucc countPred = go v (countSucc + 1) countPred
    go (VPred (VNum v)) countSucc countPred = go v countSucc (countPred + 1)
    go _ _ _ = error "Impossible Ten: Beacuase of the strong normalization property of LetPoly, no other value could be supplied to NumValue"
    buildNum :: Int -> NumValue
    buildNum n
      | n < 0 = VPred . VNum . buildNum $ (n + 1)
      | n > 0 = VSucc . VNum . buildNum $ (n - 1)
      | otherwise = VZero

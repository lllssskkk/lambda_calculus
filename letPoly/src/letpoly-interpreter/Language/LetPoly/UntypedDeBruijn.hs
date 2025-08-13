module Language.LetPoly.UntypedDeBruijn where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Reader (MonadReader (ask, local), Reader, runReader)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import Data.List (elemIndex, uncons)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text qualified as T
import GHC.Types.Unique.Supply qualified as GHC
import Language.LetPoly.Common
import Language.LetPoly.Syntax (BNFC'Position)
import Language.LetPoly.Syntax.Generated.AbsLetPoly qualified as Syn

data Typ where
  TNat :: Typ
  TBool :: Typ
  TArrow :: Typ -> Typ -> Typ
  TPair :: Typ -> Typ -> Typ
  TList :: Typ -> Typ
  TVariable :: T.Text -> Typ

deriving stock instance Show Typ

deriving stock instance Eq Typ

data DeBruijnNum where
  -- Natural Number
  DeBruijnSucc :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> DeBruijnNum
  DeBruijnZero :: Syn.BNFC'Position -> DeBruijnNum
  DeBruijnPred :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> DeBruijnNum

instance Show DeBruijnNum where
  show :: DeBruijnNum -> String
  show ((DeBruijnSucc _pos i)) = "(succ " ++ show i ++ ")"
  show ((DeBruijnPred _pos i)) = "(pred " ++ show i ++ ")"
  show ((DeBruijnZero _pos)) = "zero"

data UntypedDeBruijnExpr where
  DeBruijnIndex :: Syn.BNFC'Position -> Int -> UntypedDeBruijnExpr
  DeBruijnAbs :: Syn.BNFC'Position -> Syn.Ident -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnApp :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnLet :: Syn.BNFC'Position -> (Syn.Ident, UntypedDeBruijnExpr) -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnIfThenElse :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnFix :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  -- Tuple
  DeBruijnPair :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnFirst :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnSecond :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  -- Boolean
  DeBruijnTrue :: Syn.BNFC'Position -> UntypedDeBruijnExpr
  DeBruijnFalse :: Syn.BNFC'Position -> UntypedDeBruijnExpr
  -- Num
  DeBruijnNum :: DeBruijnNum -> UntypedDeBruijnExpr
  DeBruijnIsZero :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnIsSucc :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnIsPred :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  -- List
  DeBruijnCons :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnNil :: Syn.BNFC'Position -> UntypedDeBruijnExpr
  DeBruijnIsNil :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnHead :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
  DeBruijnTail :: Syn.BNFC'Position -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr

instance Show UntypedDeBruijnExpr where
  show :: UntypedDeBruijnExpr -> String
  show (DeBruijnIndex _ index) =
    show index
  show (DeBruijnAbs _ binder body) =
    "λ {" ++ show binder ++ "}. " ++ show body
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
  show (DeBruijnCons _pos headElem tailElem) = "cons" ++ (show headElem) ++ " " ++ show tailElem
  show (DeBruijnNil _pos) = "nil"
  show (DeBruijnIsNil _pos expr) = "isNil(" ++ show expr ++ ")"
  show (DeBruijnHead _pos expr) = "head(" ++ show expr ++ ")"
  show (DeBruijnTail _pos expr) = "tail(" ++ show expr ++ ")"

isNumericValue :: DeBruijnNum -> Bool
isNumericValue (DeBruijnSucc _pos expr) = isValue expr
isNumericValue (DeBruijnZero _) = True
isNumericValue (DeBruijnPred _pos expr) = isValue expr

isValue :: UntypedDeBruijnExpr -> Bool
isValue (DeBruijnTrue _) = True
isValue (DeBruijnFalse _) = True
isValue (DeBruijnNum num) = isNumericValue num
isValue (DeBruijnAbs _ _ _) = True
isValue (DeBruijnCons _ x xs) = isValue x && isValue xs
isValue (DeBruijnNil _) = True
isValue _ = False

type NamingContext = [Syn.Ident]

data DebruijnError = NegativeNaturalNumber String | UnboundedVariable String deriving stock (Show)

type DeBruijnPhase a = ExceptT DebruijnError (Reader NamingContext) a

debruijnization :: Syn.Prog -> Either DebruijnError UntypedDeBruijnExpr
debruijnization (Syn.Program _pos term) = do
  runReader (runExceptT (debruijnizeTerm term)) []

debruijnizeTerm :: Syn.Term -> DeBruijnPhase UntypedDeBruijnExpr
debruijnizeTerm (Syn.ApplicationTerm _pos s) = debruijnApplication s
debruijnizeTerm (Syn.LetInTerm pos binder expr body) = do
  expr' <- debruijnizeTerm expr
  body' <- local (binder :) (debruijnizeTerm body)
  pure $ DeBruijnLet pos (binder, expr') body'
debruijnizeTerm (Syn.LambdaAbsTerm pos binder body) = do
  body' <- local (binder :) (debruijnizeTerm body)
  pure $ DeBruijnAbs pos binder body'
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
debruijnizeTerm (Syn.NilTerm pos) = pure $ DeBruijnNil pos
debruijnizeTerm (Syn.IsNilTerm pos xs) = do
  xs' <- debruijnizeTerm xs
  pure $ DeBruijnIsNil pos xs'
debruijnizeTerm (Syn.HeadTerm pos xs) = do
  xs' <- debruijnizeTerm xs
  pure $ DeBruijnHead pos xs'
debruijnizeTerm (Syn.TailTerm pos xs) = do
  xs' <- debruijnizeTerm xs
  pure $ DeBruijnTail pos xs'

debruijnApplication :: Syn.Application -> DeBruijnPhase UntypedDeBruijnExpr
debruijnApplication (Syn.ApplicationAtom _pos atom) = debruijnAtom atom
debruijnApplication (Syn.AApplication pos (Syn.AApplication _pos' (Syn.ApplicationAtom _pos (Syn.ACons _pos'')) headElem) tailElem) = do
  headElem' <- debruijnAtom headElem
  tailElem' <- debruijnAtom tailElem
  pure $ DeBruijnCons pos headElem' tailElem'
debruijnApplication (Syn.AApplication pos f arg) = do
  f' <- debruijnApplication f
  arg' <- debruijnAtom arg
  pure $ DeBruijnApp pos f' arg'

debruijnAtom :: Syn.Atom -> DeBruijnPhase UntypedDeBruijnExpr
debruijnAtom (Syn.AVar pos ident) = do
  gamma <- ask
  let index = elemIndex ident gamma
  if isNothing index
    then
      throwError $
        UnboundedVariable $
          mconcat
            [ "In debruijnization Phase, ",
              "at position ",
              show pos,
              ", Unbounded Variable : ",
              show ident
            ]
    else pure $ DeBruijnIndex pos $ fromJust index
debruijnAtom (Syn.ATerm _pos term) = debruijnizeTerm term
debruijnAtom (Syn.ACons _pos) = error "We shouldn't enter this branch, it is probably the case that your list definition is incorrect."

type TypingCtx = [Typ]

type TypeReconstruction a = ExceptT TypeInferenceError (StateT [GHC.UniqSupply] (Reader TypingCtx)) a

data TypeInferenceError = TypeInferenceError String | RuntimeError String | UnificationError String deriving stock (Show)

type Constraint = (Typ, Typ)

isFunctionTyp :: Typ -> Bool
isFunctionTyp (TArrow _ _) = True
isFunctionTyp _ = False

isPairTyp :: Typ -> Bool
isPairTyp (TPair _ _) = True
isPairTyp _ = False

isListTyp :: Typ -> Bool
isListTyp (TList _) = True
isListTyp _ = False

isTypVariable :: Typ -> Bool
isTypVariable (TVariable _) = True
isTypVariable _ = False

generateNewTypVariableIdent :: TypeReconstruction T.Text
generateNewTypVariableIdent = do
  uniqs <- get
  let unconsUniqSupply = uncons uniqs
  guardM (isJust unconsUniqSupply) $ RuntimeError (mconcat ["Impossible : We are extracting a new type variable from [GHC.UniqSupply], but it is an empty list now"])
  let unconsResult = (fromJust unconsUniqSupply)
  put (snd unconsResult)
  pure $ T.show . GHC.uniqFromSupply $ fst unconsResult

genConstraint :: UntypedDeBruijnExpr -> [GHC.UniqSupply] -> Either TypeInferenceError (Typ, [Constraint])
genConstraint expr uniqSupplies = runReader (evalStateT (runExceptT (generateConstraints expr)) uniqSupplies) []

generateConstraints :: UntypedDeBruijnExpr -> TypeReconstruction (Typ, [Constraint])
generateConstraints (DeBruijnIndex pos i) = do
  typCtx <- ask
  guardM (i <= length typCtx) $
    TypeInferenceError $
      mconcat
        [ "At position ",
          show pos,
          "A DeBruijnIndex ",
          show i,
          " tries to access the typing context ",
          show typCtx,
          ", but out of bound"
        ]
  let emptyConstraints = []
  pure $ (typCtx !! i, emptyConstraints)
generateConstraints (DeBruijnAbs _pos _ident body) = do
  newTypeVariable <- TVariable <$> generateNewTypVariableIdent
  (bodyTyp, bodyConstraints) <- local (newTypeVariable :) (generateConstraints body)
  pure (TArrow newTypeVariable bodyTyp, bodyConstraints)
generateConstraints (DeBruijnApp _pos f arg) = do
  (fTyp, fConstraint) <- generateConstraints f
  (argTyp, argConstraint) <- generateConstraints arg
  newTypeVariable <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(fTyp, TArrow argTyp newTypeVariable)]
  let c' = mconcat [fConstraint, argConstraint, newConstraint]
  pure (newTypeVariable, c')
generateConstraints (DeBruijnLet _pos (_ident, binding) body) = do
  -- ! Not Let Polymorphism yet
  -- (bindingType, bindingConstraint) <- generateConstraints binding
  -- (bodyType, bodyConstraint) <- local (bindingType :) (generateConstraints body)
  -- bindingTypVariable <- TVariable <$> generateNewTypVariableIdent
  -- bodyTypeVariable <- TVariable <$> generateNewTypVariableIdent
  -- let newConstraints = [(bindingTypVariable, bindingType), (bodyTypeVariable, bodyType)]
  -- let c' = newConstraints ++ bindingConstraint ++ bodyConstraint
  -- pure (bodyTypeVariable, c')
  -- ! Let Polymorphism version
  if not (isValue binding)
    then do
      (bindingTyp, bindingConstraint) <- generateConstraints binding
      (bodyTyp, bodyConstraint) <- local (bindingTyp :) (generateConstraints body)
      pure (bodyTyp, bindingConstraint ++ bodyConstraint)
    else generateConstraints $ termSubstTop' binding body
generateConstraints (DeBruijnIfThenElse _pos cond thenBranch elseBranch) = do
  (condTyp, condConstraint) <- generateConstraints cond
  (thenBranchTyp, thenBranchConstraint) <- generateConstraints thenBranch
  (elseBranchTyp, elseBranchConstraint) <- generateConstraints elseBranch
  let newConstraints = [(condTyp, TBool), (thenBranchTyp, elseBranchTyp)]
  let c' = mconcat [condConstraint, thenBranchConstraint, elseBranchConstraint, newConstraints]
  pure (thenBranchTyp, c')
generateConstraints (DeBruijnFix _pos f) = do
  (fTyp, fConstraint) <- generateConstraints f
  inputTypeVar <- TVariable <$> generateNewTypVariableIdent
  outputTypeVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(fTyp, TArrow inputTypeVar outputTypeVar), (inputTypeVar, outputTypeVar)]
  let c' = newConstraint ++ fConstraint
  pure (inputTypeVar, c')
-- -- Tuple
generateConstraints (DeBruijnPair _pos first second) = do
  (firstTyp, firstConstraint) <- generateConstraints first
  (secondTyp, secondConstraint) <- generateConstraints second
  newTypeVariableID <- generateNewTypVariableIdent
  let newTypeVariable = TVariable newTypeVariableID
  let newConstraint = [(newTypeVariable, TPair firstTyp secondTyp)]
  let c' = mconcat [firstConstraint, secondConstraint, newConstraint]
  pure (newTypeVariable, c')
generateConstraints (DeBruijnFirst _pos pair) = do
  (pairTyp, pairConstraint) <- generateConstraints pair
  firstElemVar <- TVariable <$> generateNewTypVariableIdent
  secondElemVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(pairTyp, TPair firstElemVar secondElemVar)]
  let c' = newConstraint ++ pairConstraint
  pure $ (firstElemVar, c')
generateConstraints (DeBruijnSecond _pos pair) = do
  (pairTyp, pairConstraint) <- generateConstraints pair
  firstElemVar <- TVariable <$> generateNewTypVariableIdent
  secondElemVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(pairTyp, TPair firstElemVar secondElemVar)]
  let c' = newConstraint ++ pairConstraint
  pure $ (secondElemVar, c')
-- -- Boolean
generateConstraints (DeBruijnTrue _pos) = pure $ (TBool, [])
generateConstraints (DeBruijnFalse _pos) = pure $ (TBool, [])
-- -- Num
generateConstraints (DeBruijnNum num) = do
  (numTyp, numConstraint) <- reconstructNumType num
  newTypeVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(newTypeVar, TNat), (newTypeVar, numTyp), (numTyp, TNat)]
  let c' = newConstraint ++ numConstraint
  pure $ (newTypeVar, c')
generateConstraints (DeBruijnIsZero _pos num) = do
  (numTyp, numConstraint) <- generateConstraints num
  let newConstraint = [(numTyp, TNat)]
  let c' = newConstraint ++ numConstraint
  pure $ (TBool, c')
generateConstraints (DeBruijnIsSucc _pos num) = do
  (numTyp, numConstraint) <- generateConstraints num
  let newConstraint = [(numTyp, TNat)]
  let c' = newConstraint ++ numConstraint
  pure $ (TBool, c')
generateConstraints (DeBruijnIsPred _pos num) = do
  (numTyp, numConstraint) <- generateConstraints num
  let newConstraint = [(numTyp, TNat)]
  let c' = newConstraint ++ numConstraint
  pure $ (TBool, c')
-- -- List
generateConstraints (DeBruijnCons _pos x xs) = do
  (xTyp, xConstraint) <- generateConstraints x
  (xsTyp, xsConstraint) <- generateConstraints xs
  newTypeVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(newTypeVar, xsTyp), (TList xTyp, xsTyp)]
  let c' = newConstraint ++ xConstraint ++ xsConstraint
  pure $ (newTypeVar, c')
generateConstraints (DeBruijnNil _pos) = do
  newTypeVar <- TVariable <$> generateNewTypVariableIdent
  pure (newTypeVar, [])
generateConstraints (DeBruijnIsNil _pos xs) = do
  (xsTyp, xsConstraint) <- generateConstraints xs
  elemTypeVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(xsTyp, TList elemTypeVar)]
  let c' = newConstraint ++ xsConstraint
  pure (TBool, c')
generateConstraints (DeBruijnHead _pos xs) = do
  (xsTyp, xsConstraint) <- generateConstraints xs
  newTypeVar <- TVariable <$> generateNewTypVariableIdent
  elemTypeVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(newTypeVar, elemTypeVar), (xsTyp, TList elemTypeVar)]
  let c' = newConstraint ++ xsConstraint
  pure (newTypeVar, c')
generateConstraints (DeBruijnTail _pos xs) = do
  (xsTyp, xsConstraint) <- generateConstraints xs
  newTypeVar <- TVariable <$> generateNewTypVariableIdent
  elemTypeVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(newTypeVar, xsTyp), (xsTyp, TList elemTypeVar)]
  let c' = newConstraint ++ xsConstraint
  pure (newTypeVar, c')

reconstructNumType :: DeBruijnNum -> TypeReconstruction (Typ, [Constraint])
reconstructNumType (DeBruijnSucc _pos term) = do
  (termTyp, termConstraint) <- generateConstraints term
  newTypeVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(termTyp, newTypeVar), (termTyp, TNat)]
  let c' = newConstraint ++ termConstraint
  pure (newTypeVar, c')
reconstructNumType (DeBruijnZero _pos) = pure $ (TNat, [])
reconstructNumType (DeBruijnPred _pos term) = do
  (termTyp, termConstraint) <- generateConstraints term
  newTypeVar <- TVariable <$> generateNewTypVariableIdent
  let newConstraint = [(termTyp, newTypeVar), (termTyp, TNat)]
  let c' = newConstraint ++ termConstraint
  pure (newTypeVar, c')

occursin :: T.Text -> Typ -> Bool
occursin typeVariableIdent (TArrow tyT1 tyT2) = occursin typeVariableIdent tyT1 || occursin typeVariableIdent tyT2
occursin _typeVariableIdent TNat = False
occursin _typeVariableIdent TBool = False
occursin typeVariableIdent (TVariable s) = typeVariableIdent == s
occursin typeVariableIdent (TPair fstTyp sndTyp) = occursin typeVariableIdent fstTyp || occursin typeVariableIdent sndTyp
occursin typeVariableIdent (TList xsTyp) = occursin typeVariableIdent xsTyp

substituteType :: T.Text -> Typ -> Typ -> Typ
substituteType typeVariableIdent replacement (TArrow tyT1 tyT2) = TArrow (substituteType typeVariableIdent replacement tyT1) (substituteType typeVariableIdent replacement tyT2)
substituteType _typeVariableIdent _replacement TNat = TNat
substituteType _typeVariableIdent _replacement TBool = TBool
substituteType typeVariableIdent replacement (TVariable s) = if typeVariableIdent == s then replacement else (TVariable s)
substituteType typeVariableIdent replacement (TPair fstTyp sndTyp) = TPair (substituteType typeVariableIdent replacement fstTyp) (substituteType typeVariableIdent replacement sndTyp)
substituteType typeVariableIdent replacement (TList xsTyp) = TList $ substituteType typeVariableIdent replacement xsTyp

substituteConstraint :: T.Text -> Typ -> [Constraint] -> [Constraint]
substituteConstraint typeVariableIdent replacement constraints = map (\(tyS1, tyS2) -> (substituteType typeVariableIdent replacement tyS1, substituteType typeVariableIdent replacement tyS2)) constraints

unify :: [Constraint] -> Either TypeInferenceError [Constraint]
unify [] = pure []
unify ((TNat, TNat) : xs) = unify xs
unify ((TBool, TBool) : xs) = unify xs
unify ((s@(TVariable typeVariableIdent), t) : xs) =
  if s == t
    then unify xs
    else
      if occursin typeVariableIdent t
        then
          throwError $
            UnificationError $
              mconcat
                [ "circular constraints"
                ]
        else do
          let xs' = substituteConstraint typeVariableIdent t xs
          constraints <- unify xs'
          pure $ [(s, t)] ++ constraints
unify ((s, t@(TVariable typeVariableIdent)) : xs) =
  if s == t
    then unify xs
    else
      if occursin typeVariableIdent s
        then
          throwError $
            UnificationError $
              mconcat
                ["circular constraints"]
        else do
          let xs' = substituteConstraint typeVariableIdent s xs
          constraints <- unify xs'
          pure $ [(t, s)] ++ constraints
unify ((TArrow tyS1 tyS2, TArrow tyT1 tyT2) : xs) =
  unify ((tyS1, tyT1) : (tyS2, tyT2) : xs)
unify ((TPair fstTyp sndTyp, TPair fstTyp' sndTyp') : xs) =
  unify ((fstTyp, fstTyp') : (sndTyp, sndTyp') : xs)
unify (((TList xsTyp), (TList xsTyp')) : xs) =
  unify ((xsTyp, xsTyp') : xs)
unify xs =
  throwError $
    UnificationError $
      mconcat
        ["Unsolvable constraints : ", show xs]

tmmap :: (BNFC'Position -> Int -> Int -> UntypedDeBruijnExpr) -> Int -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
tmmap onvar c t = go c t
  where
    go cutoff = \case
      (DeBruijnIndex pos index) -> onvar pos cutoff index
      (DeBruijnAbs pos binder body) -> DeBruijnAbs pos binder $ go (cutoff + 1) body
      (DeBruijnApp pos f arg) -> DeBruijnApp pos (go cutoff f) (go cutoff arg)
      (DeBruijnLet pos (binder, binding) body) -> DeBruijnLet pos (binder, go cutoff binding) $ go (cutoff + 1) body
      (DeBruijnIfThenElse pos cond thenBranch elseBranch) -> DeBruijnIfThenElse pos (go cutoff cond) (go cutoff thenBranch) (go cutoff elseBranch)
      (DeBruijnFix pos f) -> DeBruijnFix pos $ go cutoff f
      (DeBruijnPair pos firstElem secondElem) -> DeBruijnPair pos (go cutoff firstElem) (go cutoff secondElem)
      (DeBruijnFirst pos pair) -> DeBruijnFirst pos (go cutoff pair)
      (DeBruijnSecond pos pair) -> DeBruijnSecond pos (go cutoff pair)
      (DeBruijnTrue pos) -> DeBruijnTrue pos
      (DeBruijnFalse pos) -> DeBruijnFalse pos
      (DeBruijnNum num) -> DeBruijnNum (goNum cutoff num)
      (DeBruijnIsZero pos num) -> DeBruijnIsZero pos $ go cutoff num
      (DeBruijnIsSucc pos num) -> DeBruijnIsSucc pos $ go cutoff num
      (DeBruijnIsPred pos num) -> DeBruijnIsPred pos $ go cutoff num
      -- List
      (DeBruijnCons pos x xs) -> DeBruijnCons pos (go cutoff x) (go cutoff xs)
      (DeBruijnNil pos) -> DeBruijnNil pos
      (DeBruijnIsNil pos xs) -> DeBruijnIsNil pos (go cutoff xs)
      (DeBruijnHead pos xs) -> DeBruijnHead pos (go cutoff xs)
      (DeBruijnTail pos xs) -> DeBruijnTail pos (go cutoff xs)
    goNum :: Int -> DeBruijnNum -> DeBruijnNum
    goNum cutoff (DeBruijnSucc pos num) = DeBruijnSucc pos (go cutoff num)
    goNum _cutoff (DeBruijnZero pos) = DeBruijnZero pos
    goNum cutoff (DeBruijnPred pos num) = DeBruijnPred pos (go cutoff num)

termShiftAbove :: Int -> Int -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
termShiftAbove d c t =
  tmmap
    (\pos c' x -> if x >= c' then (DeBruijnIndex pos (x + d)) else (DeBruijnIndex pos x))
    c
    t

termShift :: Int -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
termShift d t = termShiftAbove d 0 t

termSubst :: Int -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
termSubst j s t =
  tmmap
    (\pos j' x -> if x == j' then termShift j' s else (DeBruijnIndex pos x))
    j
    t

termSubstTop' :: UntypedDeBruijnExpr -> UntypedDeBruijnExpr -> UntypedDeBruijnExpr
termSubstTop' s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

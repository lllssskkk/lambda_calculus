module Language.UntypedLC.EvaluationResult where

import Language.UntypedLC.Debruijn

-- data DebruijnExpr where
--   DeBruijnIndex :: Syn.BNFC'Position -> Int -> DebruijnExpr
--   DeBruijnAbs :: Syn.BNFC'Position -> Syn.Ident -> DebruijnExpr -> DebruijnExpr
--   DeBruijnApp :: Syn.BNFC'Position -> DebruijnExpr -> DebruijnExpr -> DebruijnExpr
--   DeBruijnLet :: Syn.BNFC'Position -> (Syn.Ident, DebruijnExpr) -> DebruijnExpr -> DebruijnExpr

createDeBruijnIndex :: Int -> DebruijnExpr
createDeBruijnIndex index = DeBruijnIndex undefined index

createDeBruijnAbs :: DebruijnExpr -> DebruijnExpr
createDeBruijnAbs expr = DeBruijnAbs undefined undefined expr

createDeBruijnApp :: DebruijnExpr -> DebruijnExpr -> DebruijnExpr
createDeBruijnApp f arg = DeBruijnApp undefined f arg

createDeBruijnLet :: DebruijnExpr -> DebruijnExpr -> DebruijnExpr
createDeBruijnLet binder body = DeBruijnLet undefined (undefined, binder) body

addOperator :: DebruijnExpr
addOperator = createDeBruijnAbs (createDeBruijnAbs (createDeBruijnApp (createDeBruijnIndex 1) $ createDeBruijnApp (createDeBruijnIndex 1) (createDeBruijnIndex 0)))

factorialThree :: DebruijnExpr
factorialThree = createDebruijnInt 6

secondTuple :: DebruijnExpr
secondTuple = createDebruijnInt 0

ifStatement :: DebruijnExpr
ifStatement = createDebruijnInt 1

true :: DebruijnExpr
true = createDeBruijnAbs (createDeBruijnAbs (createDeBruijnIndex 1))

false :: DebruijnExpr
false = createDeBruijnAbs (createDeBruijnAbs (createDeBruijnIndex 0))

isOneZero :: DebruijnExpr
isOneZero = false

isZeroZero :: DebruijnExpr
isZeroZero = true

mulOperator :: DebruijnExpr
mulOperator = createDebruijnInt 4

oneLet :: DebruijnExpr
oneLet = createDebruijnInt 1

onePrime :: DebruijnExpr
onePrime = createDebruijnInt 1

predTwo :: DebruijnExpr
predTwo = createDebruijnInt 1

isPredOneZero :: DebruijnExpr
isPredOneZero = true

isPredZeroZero :: DebruijnExpr
isPredZeroZero = true

twoPlusThree :: DebruijnExpr
twoPlusThree = createDeBruijnIndex 5

twoTimesThree :: DebruijnExpr
twoTimesThree = createDeBruijnIndex 6

createDebruijnInt :: Word -> DebruijnExpr
createDebruijnInt n = createDeBruijnAbs (createDeBruijnAbs $ go n)
  where
    go :: Word -> DebruijnExpr
    go 0 = createDeBruijnIndex 0
    go n = createDeBruijnApp (createDeBruijnIndex 1) (go (n - 1))

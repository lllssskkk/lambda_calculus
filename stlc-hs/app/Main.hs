{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.STLC.BigStepCBV (Value, callByValueBigStep, getType)
-- import Language.STLC.BigStepNbeCBV (callByValueBigStepNbe)
import Language.STLC.Debruijn (DebruijnExpr, debruijnization)
-- import Language.STLC.EvaluationResult
-- import Language.STLC.SmallStepCBV (smallStepCBV)
import Language.STLC.Syntax qualified as LC
import Options.Applicative
import System.Exit
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- 1.  A first-class data type for the strategy
--------------------------------------------------------------------------------

data Strategy
  = -- | direct big-step evaluator
    BigStepCBV
  | -- | big-step via NBE
    BigStepNbeCBV
  | -- | standard small-step machine
    SmallStepCBV
  deriving stock (Show, Read, Eq, Enum, Bounded)

--------------------------------------------------------------------------------
-- 2.  Command-line parsing for Strategy
--------------------------------------------------------------------------------
-- With 'Read' in the deriving list we can reuse optparse-applicative's 'auto'
-- reader.  The user must pass *exactly* one of the constructor names:
--
--   --strategy BigStepCBV
--   --strategy BigStepNbeCBV
--   --strategy SmallStepCBV
--
-- If you want looser spelling (e.g. lowercase), replace 'auto' with a custom
-- 'eitherReader'.

strategyP :: Parser Strategy
strategyP =
  option
    auto
    ( long "strategy"
        <> metavar "STRATEGY"
        <> help "Evaluation strategy: BigStepCBV | BigStepNbeCBV | SmallStepCBV"
        <> value BigStepCBV -- default when flag is omitted
        <> showDefaultWith show
    )

opts :: ParserInfo Strategy
opts =
  info
    (strategyP <**> helper)
    ( fullDesc
        <> progDesc "Evaluate an untyped λ-term with the chosen strategy"
        <> header "untypedLC-hs – a λ-calculus evaluator"
    )

data Result = BigStepCBVResult Value | BigStepNbeCBVResult DebruijnExpr | SmallStepCBVResult DebruijnExpr deriving stock (Show)

main :: IO ()
main = do
  chosen <- execParser opts -- Strategy, not String
  putStrLn "Waiting for input on stdin..."
  input <- getContents

  ast <- either err pure $ LC.parse input
  T.putStrLn "---------De Bruijnize the Lambda Calculus---"
  debruijnExpr <- either (\x -> err $ show x) pure $ debruijnization ast
  T.putStrLn (T.pack (show debruijnExpr))
  T.putStrLn "---------Type Checking the De Bruijnization Expression------"
  _typ <- either (\x -> err $ show x) pure $ getType debruijnExpr
  let normalized = BigStepCBVResult $ callByValueBigStep debruijnExpr
  -- let normalized = case chosen of -- pattern-match on Strategy
  --       BigStepCBV -> BigStepCBVResult $ callByValueBigStep debruijnExpr
  --       BigStepNbeCBV -> BigStepNbeCBVResult $ callByValueBigStepNbe debruijnExpr
  --       SmallStepCBV -> SmallStepCBVResult $ smallStepCBV debruijnExpr
  ok
  T.putStrLn "---------After the Evaluation---------------"
  T.putStrLn (T.pack (show normalized))

-- Only works for `callByValueBigStepNbe`
--   T.putStrLn "---------Match Reresult---------------------"
--   print (value == isOneZero)

--------------------------------------------------------------------------------
-- 4.  Helpers
--------------------------------------------------------------------------------
ok :: IO ()
ok = hPutStrLn stderr "OK"

err :: String -> IO a
err msg = do
  hPutStrLn stderr "ERROR"
  hPutStrLn stderr msg
  exitFailure

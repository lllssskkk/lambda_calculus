module Language.UntypedLC.Syntax (module Syn, parse, stringify) where

import Language.UntypedLC.Syntax.Generated.AbsUntypedLC as Syn
import Language.UntypedLC.Syntax.Generated.LexUntypedLC (tokens)
import Language.UntypedLC.Syntax.Generated.ParUntypedLC (pProg)
import Language.UntypedLC.Syntax.Generated.PrintUntypedLC
  ( Print,
    printTree,
  )

parse :: String -> Either String Prog
parse = pProg . tokens

stringify :: (Print a) => a -> String
stringify = printTree

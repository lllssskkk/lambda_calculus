module Language.STLC.Syntax (module Syn, parse, stringify) where

import Language.STLC.Syntax.Generated.AbsSyntax as Syn
import Language.STLC.Syntax.Generated.LexSyntax
import Language.STLC.Syntax.Generated.ParSyntax
import Language.STLC.Syntax.Generated.PrintSyntax

parse :: String -> Either String Prog
parse = pProg . tokens

stringify :: (Print a) => a -> String
stringify = printTree

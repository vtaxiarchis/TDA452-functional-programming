module Expr where

import Parsing

import Data.Char
import Data.Maybe

import Test.QuickCheck

-- Part A
-- Data type that represents all expressions.
-- It can be number, variable, addition, multiplication, sin or cos.
data Expr = Num Double | Var |
            Add Expr Expr | Mul Expr Expr |
            Sin Expr | Cos Expr
  deriving(Eq)

-- Part B
-- Function to convert any expression to string.
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr Var = "x"
showExpr (Add n1 n2) = showExpr n1 ++ "+" ++ showExpr n2
showExpr (Mul n1 n2) = showExpr'' n1 ++ "*" ++ showExpr'' n2
showExpr (Sin angle) = "sin " ++ showExpr' angle
showExpr (Cos angle) = "cos " ++ showExpr' angle

-- Helper function that adds parenthesis as part of functions
showExpr' :: Expr -> String
showExpr' (Num n) = show n
showExpr' Var = "x"
showExpr' x@(Add _ _) = "(" ++ showExpr x ++ ")"
showExpr' x@(Mul _ _) = "(" ++ showExpr'' x ++ ")"
showExpr' x = showExpr x

-- Helper functions that adds parenthesis only for additions as part of multiply
showExpr'' :: Expr -> String
showExpr'' (Add n1 n2) = "(" ++ showExpr (Add n1 n2) ++")"
showExpr'' n = showExpr n

instance Show Expr where
  show = showExpr

-- Part C
-- Function to evaluate an expression given a value for x
eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval Var x = x
eval (Add n1 n2) x = (eval n1 x) + (eval n2 x)
eval (Mul n1 n2) x = (eval n1 x) * (eval n2 x)
eval (Sin n) x = sin (eval n x)
eval (Cos n) x = cos (eval n x)

-- Part D
-- Reads a string and converts it to an expression
readExpr :: String -> Maybe Expr
readExpr s = do
             r <- parse expr s
             return $ fst r

{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= number | variable | "(" expr ") | sin factor | cos factor"

We are following the same structure presented in lectures.
-}
expr, term, factor :: Parser Expr

expr = do t <- term
          ts <- zeroOrMore rest
          return (foldl Add t ts)
          where rest = do skipSpaces
                          char '+'
                          skipSpaces
                          next <- term
                          skipSpaces
                          return next

term = do t <- factor
          ts <- zeroOrMore rest
          return (foldl Mul t ts)
          where rest = do skipSpaces
                          char '*'
                          skipSpaces
                          next <- factor
                          skipSpaces
                          return next

factor = numberParser <|>
         variableParser <|>
         parenthesisParser <|>
         functionParser Sin "sin" <|>
         functionParser Cos "cos"

-- Parser for parenthesis expressions
parenthesisParser :: Parser Expr
parenthesisParser = do char '('
                       e <- expr
                       char ')'
                       return e

-- Parser to skip spaces in an expression
skipSpaces :: Parser String
skipSpaces = zeroOrMore (sat isSpace)

-- Parser for functions, captures function parameter
functionParser :: (Expr -> b) -> String -> Parser b
functionParser f s = do discardName
                        skipSpaces
                        e <- factor
                        return (f e)
                     where discardName = sequence_ (replicate (length s) item)

-- Parser for variables
variableParser :: Parser Expr
variableParser = do char 'x'
                    return Var

-- Parser for numbers
numberParser :: Parser Expr
numberParser = do n <- readsP :: Parser Double
                  return (Num n)

-- Part E
-- Property that checks whether generated string can be parsed back
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = case parsed of Just p -> p == e
                                     Nothing -> False
                      where parsed = readExpr (showExpr e)

-- Random generator that generates an expression based on a supplied size
arbExpr :: Int -> Gen Expr
arbExpr 0 = do e <- elements [Num n | n<-[1..10]]
               roll <- elements [1, 2]
               return (case roll of 1 -> e
                                    2 -> Var)

arbExpr n = do op <- elements [Add, Mul]
               func <- elements [Sin, Cos]
               n' <- elements [0 .. n-1]
               e1 <- arbExpr n'
               e2 <- arbExpr (n-1 - n')
               e3 <- arbExpr (n - 1)
               roll <- elements [1, 2]
               return (case roll of 1 -> func e3
                                    2 -> op e1 e2)

instance Arbitrary Expr where
  arbitrary = sized arbExpr

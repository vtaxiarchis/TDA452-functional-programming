module Expr where

import Parsing

import Data.Char
import Data.Maybe

import Test.QuickCheck

-- Part A
-- Data type that represents expressions.
-- An expression can be number, variable, addition, multiplication, sin or cos.
data Expr = Num Double | Var |
            Add Expr Expr | Mul Expr Expr |
            Sin Expr | Cos Expr
  deriving(Eq)

-- Part B
-- Function to convert any expression to string
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr Var = "x"
showExpr (Add n1 n2@(Add n3 n4)) = showExpr n1 ++ "+(" ++ showExpr n2 ++ ")"
showExpr (Add n1 n2) = showExpr n1 ++ "+" ++ showExpr n2
showExpr (Mul n1 n2@(Mul n3 n4)) = showExpr'' n1 ++ "*(" ++ showExpr'' n2 ++ ")"
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
functionParser f s = do skipName
                        skipSpaces
                        e <- factor
                        return (f e)
                     -- ensures name is skipped only if matching
                     where skipName = sequence_ (fmap (char) s)

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

-- Part F
-- Function to simplify an expression to an equivalent expression
simplify :: Expr -> Expr
simplify (Add n1 n2) = simplify' (Add (simplify n1) (simplify n2))
simplify (Mul n1 n2) = simplify' (Mul (simplify n1) (simplify n2))
simplify (Sin n) = simplify' (Sin (simplify n))
simplify (Cos n) = simplify' (Cos (simplify n))
simplify n = n

-- Helper function that simplifies expressions under following rules:
--  x + y: if both x and y are numbers, or one of them is zero
--  x * y: if both x and y are numbers, or one of them is zero or one
simplify' :: Expr -> Expr
simplify' (Add (Num n1) (Num n2)) = Num (n1 + n2)
simplify' orig@(Add (Num n) n2) = case n of 0 -> simplify n2
                                            _ -> orig
simplify' orig@(Add n1 (Num n)) = case n of 0 -> simplify n1
                                            _ -> orig

simplify' (Mul (Num n1) (Num n2)) = Num (n1 * n2)
simplify' orig@(Mul (Num n) n2) = case n of 0 -> Num 0
                                            1 -> simplify n2
                                            _ -> orig
simplify' orig@(Mul n1 (Num n)) = case n of 0 -> Num 0
                                            1 -> simplify n1
                                            _ -> orig
simplify' n = n

-- Function that determine if an expression can be simplified further
isSimple :: Expr -> Bool
isSimple (Add (Num n1) (Num n2)) = False
isSimple (Add (Num n) n2) = case n of 0 -> False
                                      _ -> isSimple n2
isSimple (Add n1 (Num n)) = case n of 0 -> False
                                      _ -> isSimple n1

isSimple (Mul (Num n1) (Num n2)) = False
isSimple (Mul (Num n) n2) = case n of 0 -> False
                                      1 -> False
                                      _ -> isSimple n2
isSimple (Mul n1 (Num n)) = case n of 0 -> False
                                      1 -> False
                                      _ -> isSimple n1

isSimple (Sin n) = isSimple n
isSimple (Cos n) = isSimple n
isSimple n = True

-- Property that checks simplified expression evaluates to the same value
--  and cannot be simplified further
prop_canSimplify :: Expr -> Bool
prop_canSimplify e = all (\x -> x == True) results
                     where simple = simplify e
                           results = [(eval e r == eval simple r) &&
                                      (isSimple simple) | r <- [0..10]]


-- Part G
-- Differentiates a given expression with respect to x
differentiate :: Expr -> Expr
differentiate e = differentiate' simple
                  where simple = simplify e

differentiate' :: Expr -> Expr
differentiate' (Num _) = Num 0
differentiate' Var     = Num 1

differentiate' (Add n1 n2) = simplify (Add n1' n2')
                             where n1' = differentiate' n1
                                   n2' = differentiate' n2

differentiate' (Mul n1 n2) = simplify (Add (Mul n1' n2) (Mul n1 n2'))
                             where n1' = differentiate' n1
                                   n2' = differentiate' n2

differentiate' (Sin n) = Cos n
differentiate' (Cos n) = (Mul (Num (-1)) (Sin n))

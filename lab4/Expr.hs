module Expr where
import Data.Char
import Data.Maybe

-- Data type that represents all expressions.
-- It can be number, variable, addition, multiplication, sin or cos.
data Expr = Num Double | Var | Add Expr Expr | Mul Expr Expr | Sin Expr | Cos Expr
  deriving(Eq)

-- Function to convert any expression to string.
showExpr :: Expr -> String
showExpr (Num num) = show num
showExpr Var = "x"
showExpr (Add num1 num2) = showExpr num1 ++ "+" ++ showExpr num2
showExpr (Mul num1 num2) = showExpr'' num1 ++ "*" ++ showExpr'' num2
showExpr (Sin angle) = "sin " ++ showExpr' angle
showExpr (Cos angle) = "cos " ++ showExpr' angle

showExpr' :: Expr -> String
showExpr' (Num num) = show num
showExpr' Var = "x"
showExpr' x@(Add num1 num2) = "(" ++ showExpr x ++ ")"
showExpr' x@(Mul num1 num2) = "(" ++ showExpr'' x ++ ")"
showExpr' x = showExpr x

showExpr'' :: Expr -> String
showExpr'' (Add num1 num2) = "(" ++ showExpr (Add num1 num2) ++")"
showExpr'' num = showExpr num

instance Show Expr where
  show = showExpr

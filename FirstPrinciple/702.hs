data Expr
  = Lit Integer
  | Add Expr
        Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

a1 = Add (Lit 9001) (Lit 1)

a2 = Add a1 (Lit 20001)

a3 = Add (Lit 1) a2

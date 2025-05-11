data Expr = Lit Int       |
            Add Expr Expr |
            Sub Expr Expr |
            Mul Expr Expr
            
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

main = do
    putStrLn $ show( eval (Mul (Lit 5) (Lit 2)))

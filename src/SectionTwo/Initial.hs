module SectionTwo.Initial where

data Exp = Lit Int
         | Neg Exp
         | Add Exp Exp
         | Mult Exp Exp

example :: Exp
example = Mult (Lit 2) (Add (Lit 8) (Neg (Add (Lit 1) (Lit 2))))

eval :: Exp -> Int
eval (Lit n) = n
eval (Neg e) = -1 * (eval e)
eval (Add e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2

view :: Exp -> String
view (Lit n) = show n
view (Neg e) = "(-" ++ view e ++ ")"
view (Add e1 e2) = "(" ++ view e1 ++ " + " ++ view e2 ++ ")"
view (Mult e1 e2) = "(" ++ view e1 ++ " * " ++ view e2 ++ ")"

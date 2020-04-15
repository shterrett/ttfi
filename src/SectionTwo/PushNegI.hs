module SectionTwo.PushNegI where

import SectionTwo.Initial

push_neg :: Exp -> Exp
push_neg e@Lit{} = e
push_neg e@(Neg (Lit{})) = e
push_neg (Neg (Neg e)) = e
push_neg (Neg (Add e1 e2)) = Add (push_neg (Neg e1)) (push_neg (Neg e2))
push_neg (Add e1 e2) = Add (push_neg e1) (push_neg e2)
 -- arbitrarily pushing negation down the first branch of multiplication
 -- because -1 * (x * y) == (-1 * x) * y == x * (-1 * y)
push_neg (Neg (Mult e1 e2)) = Mult (push_neg (Neg e1)) (push_neg e2)
push_neg (Mult e1 e2) = Mult (push_neg e1) (push_neg e2)

example_norm :: Exp
example_norm = push_neg example

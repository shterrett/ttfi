module SectionTwo.FlatI where

import SectionTwo.Initial
import SectionTwo.PushNegI (push_neg)

flatten :: Exp -> Exp
flatten e@Lit{} = e
-- assume that we have already run push_neg
flatten e@Neg{} = e
flatten (Add (Add e1 e2) e3) = flatten (Add e1 (Add e2 e3))
flatten (Add e1 e2) = Add e1 (flatten e2)
-- flatten multiplication in the same way. Don't try to normalize
-- distribution of multiplication over addition
flatten (Mult (Mult e1 e2) e3) = flatten (Mult e1 (Mult e2 e3))
flatten (Mult e1 e2) = Mult e1 (flatten e2)

norm :: Exp -> Exp
norm = flatten . push_neg

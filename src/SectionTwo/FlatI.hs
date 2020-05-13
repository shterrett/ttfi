{-# LANGUAGE DeriveFunctor #-}

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

data ExpF a =
    LitF Int
    | NegF a
    | AddF a a
    deriving (Functor, Show)

data Fix f = Fix { unFix :: f (Fix f) }

type Expp = Fix ExpF

data Ctx a =
    LCA a
    | NonLCA

alg :: ExpF (Ctx Expp -> Expp) -> (Ctx Expp -> Expp)
alg (LitF i) = \ctx -> Fix $ case ctx of
                               NonLCA -> LitF i
                               LCA rhs -> AddF (Fix $ LitF i) rhs
alg (NegF e) = \ctx -> Fix $ case ctx of
                         NonLCA -> NegF (e NonLCA)
                         LCA rhs -> AddF (Fix $ NegF $ e NonLCA) rhs
alg (AddF x y) = \ctx -> x (LCA (y ctx))

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata f fx = f $ fmap (cata f) (unFix fx)

flatten' :: Expp -> Expp
flatten' e = cata alg e NonLCA

flattenMe :: Expp
flattenMe = Fix $ AddF (Fix $ AddF (Fix $ LitF 5) (Fix $ LitF 6))
                       (Fix $ LitF 7)
flattenMe2 :: Expp
flattenMe2 = Fix $ AddF (Fix $ LitF 7)
                        (Fix $ AddF (Fix $ LitF 5) (Fix $ LitF 6))

printAlg :: ExpF String -> String
printAlg (LitF i) = show i
printAlg (NegF e) = "-" <> e
printAlg (AddF x y) = "(" <> x <> " + " <> y <> ")"

pprintF :: Expp -> String
pprintF = cata printAlg

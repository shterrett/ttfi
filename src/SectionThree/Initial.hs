{-# LANGUAGE GADTs #-}

module SectionThree.Initial where

import Prelude hiding (lookup)

data Exp env t where
    B :: Bool -> Exp env Bool
    V :: Var env t -> Exp env t
    L :: Exp (a, env) b -> Exp env (a -> b)
    A :: Exp env (a -> b) -> Exp env a -> Exp env b

data Var env t where
    VZ :: Var (t, env) t
    VS :: Var env t -> Var (a, env) t

eval :: env -> Exp env t -> t
eval env (V v) = lookup v env
eval _ (B b) = b
eval env (L e) = \x -> eval (x, env) e
eval env (A e1 e2) = (eval env e1) (eval env e2)

lookup :: Var env t -> env -> t
lookup VZ (x, _) = x
lookup (VS v) (_, env) = lookup v env

-- | \x -> x $ True
tf1 :: Exp env Bool
tf1 = A -- apply
      (L -- lambda abstraction
        (V VZ) -- Variable V0 (the closest variable)
      )        -- Which will be the variable bound by the lambda abstraction
      (B True) -- Boolean True

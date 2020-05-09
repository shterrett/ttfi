{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module SectionThree.Bijection where

import SectionThree.Hoas (Symantics(..), R(..))

data IR h t where
    INT :: Int -> IR h Int
    Add :: IR h Int -> IR h Int -> IR h Int
    Var :: h a -> IR h a
    Lam :: (IR h a -> IR h b) -> IR h (a -> b)
    App :: IR h (a -> b) -> IR h a -> IR h b

evalI :: IR R t -> t
evalI (INT n) = n
evalI (Add e1 e2) = evalI e1 + evalI e2
evalI (Var v) = unR v
evalI (Lam f) = \x -> evalI (f . Var . R $ x)
evalI (App f a) = evalI f $ evalI a

instance Symantics (IR h) where
    int = INT
    add = Add
    lam = Lam
    app = App

f2i :: IR h a -> IR h a
f2i = id

i2f :: Symantics repr => IR repr t -> repr t
i2f (INT x) = int x
i2f (Add e1 e2) = add (i2f e1) (i2f e2)
i2f (Var v) = v
i2f (Lam f) = lam (\x -> i2f (f (Var x)))
i2f (App e1 e2) = app (i2f e1) (i2f e2)

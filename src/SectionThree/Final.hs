{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module SectionThree.Final where

import Prelude hiding (lookup)

class Exp (repr :: * -> * -> *) where
    b :: Bool -> repr h Bool
    -- z :: repr (a, h) a
    -- s :: repr h a -> repr (any, h) a
    v :: Var h a -> repr h a
    lam :: repr (a, h) b -> repr h (a -> b)
    app :: repr h (a -> b) -> repr h a -> repr h b

newtype Eval env t = Eval { unEval :: env -> t }

instance Exp Eval where
    b bool = Eval $ \_ -> bool
--     z = Eval $ \(x, _) -> x
--     s (Eval f) = Eval $ \(_, x) -> f x
    v vr = Eval $ \h -> lookup vr h
    lam (Eval f) = Eval $ \e -> \x -> f (x, e)
    app (Eval f) (Eval g) = Eval $ \e -> (f e) (g e)

lookup :: Var env t -> env -> t
lookup VZ (x, _) = x
lookup (VS v) (_, env) = lookup v env

data Var env t where
    VZ :: Var (t, env) t
    VS :: Var env t -> Var (a, env) t

tf1 :: (Exp repr) => repr env Bool
tf1 = app -- apply
      (lam -- lambda abstraction
        $ v VZ -- zero-index variable
      ) -- (the lambda abstraction's bound variable)
      (b True) -- boolean literal True

-- -- \x -> \y -> \z -> s (app x y)
-- -- tf2 :: (Exp repr) => repr env ((a -> b) -> a -> x -> b)
-- tf2 :: (Exp repr) => repr env ((a -> b) -> x -> a -> b)
-- tf2 = lam
--       (lam
--         $ s (lam
--           $ (app (s z) z)
--         )
--       )

eval :: Eval () Bool -> Bool
eval e = unEval e ()

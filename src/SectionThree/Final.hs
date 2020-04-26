{-# LANGUAGE KindSignatures #-}

module SectionThree.Final where

class Exp (repr :: * -> * -> *) where
    b :: Bool -> repr h Bool
    z :: repr (a, h) a
    s :: repr h a -> repr (any, h) a
    lam :: repr (a, h) b -> repr h (a -> b)
    app :: repr h (a -> b) -> repr h a -> repr h b

newtype Eval env t = Eval { unEval :: env -> t }

instance Exp Eval where
    b bool = Eval $ \_ -> bool
    z = Eval $ \(x, _) -> x
    s (Eval f) = Eval $ \(_, x) -> f x
    lam (Eval f) = Eval $ \e -> \x -> f (x, e)
    app (Eval f) (Eval g) = Eval $ \e -> (f e) (g e)

tf1 :: (Exp repr) => repr env Bool
tf1 = app -- apply
      (lam -- lambda abstraction
        z -- zero-index variable
      ) -- (the lambda abstraction's bound variable)
      (b True) -- boolean literal True

eval :: Eval () Bool -> Bool
eval e = unEval e ()

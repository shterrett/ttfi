-- | In which we implement the simply-typed lambda calculus
module SectionThree.DeBruijn where

-- | h is the environment
-- it is modeled as nested tuples (ie an h-list)
-- a and b are the result types of computations
class Symantics repr where
    int :: Int -> repr h Int
    add :: repr h Int -> repr h Int -> repr h Int
    -- zero-index variable
    z :: repr (a, h) a
    -- "successor" indexed variable
    s :: repr h a -> repr (any, h) a
    lam :: repr (a, h) b -> repr h (a -> b)
    app :: repr h (a -> b) -> repr h a -> repr h b

td1 :: (Symantics repr) => repr h Int
td1 = add (int 1) (int 2)

td2 :: (Symantics repr) => repr (Int, h) (Int -> Int)
td2 = lam (add z (s z))

td3 :: (Symantics repr) => repr h ((Int -> Int) -> Int)
td3 = lam (add (app z (int 1)) (int 2))

-- | instance for the arithmetic evaluation
newtype R h a = R { unR :: h -> a }

instance Symantics R where
    int x = R $ const x
    add e1 e2 = R $ \h -> (unR e1 h) + (unR e2 h)
    z = R fst
    s v = R $ \(_, h) -> unR v h
    lam e = R $ \h -> \x -> unR e (x, h)
    app f a = R $ \h -> (unR f h) (unR a h)

eval :: R h a -> h -> a
eval r h = unR r h

-- | instance for pretty printing
newtype S h a = S { unS :: Int -> String }

instance Symantics S where
    int x = S $ const (show x)
    add e1 e2 = S $ \h -> "(" <> unS e1 h <> " + " <> unS e2 h <> ")"
    z = S $ ("x" <>) . show . (flip (-) 1)
    s v = S $ \h -> unS v h
    lam e =
      S $ \h ->
        let
          x = "x" <> show h
        in
          "(\\" <> x <> ". " <>  unS e  (succ h) <> ")"
    app f a = S $ \h -> (unS f h) <> " " <> (unS a h)

view :: S h a -> String
view r = unS r 0

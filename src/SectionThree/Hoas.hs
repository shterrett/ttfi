module SectionThree.Hoas where

class Symantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b

th1 :: (Symantics repr) => repr Int
th1 = add (int 1) (int 2)

th2 :: (Symantics repr) => repr (Int -> Int)
th2 = lam (\x -> add x x)

th3 :: (Symantics repr) => repr ((Int -> Int) -> Int)
th3 = lam (\f -> add (app f (int 1)) (int 2))

newtype R a = R { unR :: a }

instance Symantics R where
    int = R
    add e1 e2 = R $ unR e1 + unR e2
    lam f = R $ unR . f . R
    app f a = R $ (unR f) (unR a)

eval :: R a -> a
eval = unR

type VarCounter = Int
newtype S a = S { unS :: VarCounter -> String }

instance Symantics S where
    int x = S $ \_ -> show x
    add e1 e2 = S $ \c ->"(" <> (unS e1 c) <> " + " <> (unS e2 c) <> ")"
    lam f = S $ \c ->
      let
        x = "x" <> show c
      in
        "(" <>"\\" <> x <> ".\n" <> unS (f (S $ const x)) (c + 1) <> ")"
    app f a = S $ \c -> "(" <> (unS f c) <> " " <>  (unS a c) <> ")"

view :: S a -> String
view e = unS e 0

class MulSym repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolSym repr where
    bool :: Bool -> repr Bool
    leq :: repr Int -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

class FixSym repr where
    fix :: (repr a -> repr a) -> repr a

tpow ::
  ( Symantics repr
  , BoolSym repr
  , MulSym repr
  , FixSym repr
  )
  => repr (Int -> Int -> Int)
tpow = lam (\x -> fix (\self -> lam (\n ->
           if_ (leq n (int 0))
               (int 1)
               (mul x (app self (add n (int (-1))))))))

tpow7 ::
  ( Symantics repr
  , BoolSym repr
  , MulSym repr
  , FixSym repr
  )
  => repr (Int -> Int)
tpow7 = lam (\x -> (tpow `app` x) `app` int 7)

tpow27 ::
  ( Symantics repr
  , BoolSym repr
  , MulSym repr
  , FixSym repr
  )
  => repr Int
tpow27 = app tpow7 (int 2)

instance MulSym R where
    mul e1 e2 = R $ unR e1 * unR e2

instance MulSym S where
    mul e1 e2 = S $ \c -> "(" <> unS e1 c <> " * " <> unS e2 c <> ")"

instance BoolSym R where
    bool b = R b
    leq e1 e2 = R $ unR e1 <= unR e2
    if_ b t f = R $ if unR b
                      then unR t
                      else unR f

instance BoolSym S where
    bool b = S $ \_ -> show b
    leq e1 e2 = S $ \c -> "(" <> unS e1 c <> " <= " <> unS e2 c <> ")"
    if_ b t f = S $ \c -> "(if " <> unS b c <> "\nthen " <> unS t c <> "\n else " <> unS f c <> ")"

instance FixSym R where
    fix f = R $ fx (unR . f . R)
      where fx g = g (fx g)

instance FixSym S where
    fix f = S $ \c ->
      let
        self = "self" <> show c
      in
        "(fix " <> self <> ".\n"
          <> unS (f (S $ const self)) (c + 1) <> ")"

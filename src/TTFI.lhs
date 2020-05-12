\documentclass[pdf]{beamer}
%include polycode.fmt
%include lhs2TeX.sty
%include lhs2TeX.fmt
\mode<presentation>{}

\usepackage{xcolor}
\definecolor{light-gray}{gray}{0.95}
\newcommand{\code}[1]{\colorbox{light-gray}{\texttt{#1}}}

\title{Typed Tagless Final Interpreters}
\author{Stuart Terrett}

\begin{document}
\begin{frame}
  \titlepage
\end{frame}

\begin{frame}
\frametitle{preamble}

\begin{code}
{-# LANGUAGE GADTs #-}
module TTFI where
\end{code}

\end{frame}

\begin{frame}
\frametitle{typed}

The object language is \emph{typed}
\pause
\begin{spec}
add (int 2) (bool 3)
\end{spec}
\pause
will not compile

\end{frame}

\begin{frame}
\frametitle{tagless}
The object language terms are not wrapped in a sum type with type tags

\pause

\begin{code}
data Term =
  TInt Int
  | TBool Bool
\end{code}
\pause
The object language is "tight": if it compiles, it cannot get stuck
\begin{code}
addU :: Term -> Term -> Term
addU (TInt x) (TInt y) = TInt $ x + y
addU _ _ = error "make illegal states unrepresentable"
\end{code}

\end{frame}

\begin{frame}
\frametitle{final}

Terms of the object language are represented as expressions in the metalanguage

\pause
\begin{code}
twoPlusTwo :: (AddSym repr) => repr Int
twoPlusTwo = add (int 2) (int 2)
\end{code}

\pause

Not abstract syntax
\begin{code}
data AST =
  Lit Term
  | AddU AST AST
  | AndU AST AST

twoPlusTwo' :: AST
twoPlusTwo' = AddU (Lit (TInt 2)) (Lit (TInt 2))
\end{code}

\end{frame}

\begin{frame}
\frametitle{interpreter}

The syntax is a typeclass

\begin{code}
class AddSym repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
\end{code}
\end{frame}

\begin{frame}
\frametitle{typed tagless}

The tagless-typed-ness comes from
\begin{spec}
repr :: * -> *
\end{spec}
\pause
Which is why the above example won't typecheck
\begin{spec}
int 2 :: repr Int
bool 3 :: repr Bool
add :: repr Int -> repr Int -> repr Int
\end{spec}

\end{frame}

\begin{frame}
\frametitle{interpreter}

The semantics are an instance of the typeclass

\begin{code}
newtype R a = R { unR :: a }

instance AddSym R where
  int = R
  add (R x) (R y) = R $ x + y
\end{code}

\end{frame}

\begin{frame}
\frametitle{extensible}
Adding interpretations

\begin{code}
newtype S a = S { unS :: Int -> String }
instance AddSym S where
  int = S . const . show
  add (S x) (S y) = S $ \c ->
    "(" <> x c <> " + " <> y c <> ")"
\end{code}
\end{frame}

\begin{frame}
\frametitle{extensible}
Adding operations

\begin{code}
class MultSym repr where
  mul :: repr Int -> repr Int -> repr Int

instance MultSym R where
  mul (R x) (R y) = R $ x * y

instance MultSym S where
  mul (S x) (S y) = S $ \c ->
    "(" <> x c <> " * " <> y c <> ")"
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding booleans}
\begin{code}
class BoolSym repr where
  bool :: Bool -> repr Bool
  lte :: repr Int -> repr Int -> repr Bool
  when :: repr Bool -> repr a -> repr a -> repr a
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding booleans}
\begin{code}
instance BoolSym R where
  bool = R
  lte (R x) (R y) = R $ x <= y
  when (R b) (R t) (R f) = R $ if b then t else f
\end{code}
\pause
\begin{code}
instance BoolSym S where
  bool = S . const . show
  lte (S x) (S y) = S $ \c ->
    "(" <> x c <> " <= " <> y c <> ")"
  when (S b) (S t) (S f) = S $ \c ->
    "if " <> b c
    <> "\nthen " <> t c
    <> "\n else" <> f c
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding lambda abstraction}
\begin{code}
class LamSym repr where
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding lambda abstraction}
\begin{code}
instance LamSym R where
  lam f = R $ unR . f . R
  app (R f) (R a) = R $ f a
\end{code}
\pause
\begin{code}
instance LamSym S where
  lam f = S $ \c ->
    let
      x = "x" <> show c
    in
      "\\" <> x <>".\n"
      <> (unS . f . S $ const x) (c + 1)
  app (S f) (S a) = S $ \c ->
    "(" <> f c <> " " <> a c <> ")"
\end{code}
\end{frame}

\begin{frame}
\frametitle{higher-order abstract syntax}
Notice that the metalanguage (Haskell) maintains the variable bindings
\end{frame}

\begin{frame}
\frametitle{adding recursion!!}
\begin{code}
class FixSym repr where
  fix :: (repr a -> repr a) -> repr a
\end{code}
\end{frame}

\begin{frame}
\frametitle{adding recursion!!}
\begin{code}
instance FixSym R where
  fix f = R $ fx (unR . f . R)
    where fx g = g (fx g)
\end{code}
\pause
\begin{code}
instance FixSym S where
  fix f = S $ \c ->
    let
      self = "self" <> show c
    in
      "(fix " <> self <> ".\n"
      <> (unS . f . S $ const self) (c + 1) <> ")"
\end{code}
\end{frame}

\begin{frame}
\frametitle{simply typed lambda calculus with integer and boolean literals}
\begin{code}
class ( AddSym repr
      , MultSym repr
      , BoolSym repr
      , LamSym repr
      , FixSym repr
      ) => Symantics repr

instance Symantics R
instance Symantics S

eval :: R a -> a
eval = unR

pprint :: S a -> String
pprint e = unS e 0
\end{code}
\end{frame}

\begin{frame}
\frametitle{factorial}
\begin{code}
factorial :: (Symantics repr) => repr (Int -> Int)
factorial = fix (\self -> lam (\n ->
              when (lte n (int 0))
                   (int 1)
                   (mul n (self `app` (add n (int (-1)))))))
\end{code}
(hint: now's a good time to \code{stack ghci src/TTFI.hs})
\end{frame}

\begin{frame}
\frametitle{contrast: typed tagless initial encoding}
\begin{code}
data SymI h a where
  INT :: Int -> SymI h Int
  Add :: SymI h Int -> SymI h Int -> SymI h Int
  Mul :: SymI h Int -> SymI h Int -> SymI h Int
  BOOL :: Bool -> SymI h Bool
  Lte :: SymI h Int -> SymI h Int -> SymI h Bool
  When :: SymI h Bool -> SymI h a -> SymI h a -> SymI h a
  Var :: h a -> SymI h a
  Lam :: (SymI h a -> SymI h b) -> SymI h (a -> b)
  App :: SymI h (a -> b) -> SymI h a -> SymI h b
  Fix :: (SymI h a -> SymI h a) -> SymI h a
\end{code}
\code{h} is the evaluation context (eg \code{R} or \code{S} etc)
\end{frame}

\begin{frame}
Like the final encoding, this is well typed (if it compiles, it won't get stuck)
and there are no "type tags".
\newline
\pause
\newline
This requires \code{GADTs} so that the result of pattern-matching on
different branches of a sum type can have different types
\end{frame}

\begin{frame}
\frametitle{initial interpreter}
Here we evaluate in \code{R}
\begin{code}
evalI :: SymI R a -> a
evalI (INT i) = i
evalI (Add x y) = (evalI x) + (evalI y)
evalI (Mul x y) = (evalI x) * (evalI y)
evalI (BOOL b) = b
evalI (Lte x y) = (evalI x) <= (evalI y)
evalI (When b t f) = if (evalI b) then (evalI t) else (evalI f)
evalI (Var x) = unR x
evalI (Lam f) = evalI . f . Var . R
evalI (App f a) = (evalI f) (evalI a)
evalI (Fix f) = evalI (fx f)
  where fx g = g (fx g)
\end{code}
\end{frame}

\begin{frame}
\frametitle{expression problem}
Adding interpreters to \code{SymI} is straightforward

\begin{spec}
pprintI :: Sym S a -> String
pprintI = ...
\end{spec}

\pause
But adding new operations isn't possible
\pause
\newline
\newline
Because of the contravariant \code{SymI} in \code{Lam}, \code{SymI} is not
representable as the fixpoint of a functor, and so we can't use the
coproduct-of-functors representation.
\end{frame}

\begin{frame}
\frametitle{bijection}
We can show the final and initial embeddings are equivalent by establishing a
bijection
\end{frame}

\begin{frame}
\frametitle{final to initial}
\begin{code}
instance AddSym (SymI h) where
  int = INT
  add = Add
instance MultSym (SymI h) where
  mul = Mul
instance BoolSym (SymI h) where
  bool = BOOL
  lte = Lte
  when = When
instance LamSym (SymI h) where
  lam = Lam
  app = App
instance FixSym (SymI h) where
  fix = Fix

instance Symantics (SymI h)

fToI :: SymI h a -> SymI h a
fToI = id
\end{code}
\end{frame}

\begin{frame}
\frametitle{initial to final}
\begin{code}
iToF :: (Symantics repr) => SymI repr a -> repr a
iToF (INT x) = int x
iToF (Add x y) = add (iToF x) (iToF y)
iToF (Mul x y) = mul (iToF x) (iToF y)
iToF (BOOL b) = bool b
iToF (Lte x y) = lte (iToF x) (iToF y)
iToF (When b t f) = when (iToF b) (iToF t) (iToF f)
iToF (Var a) = a
iToF (Lam f) = lam (\x -> iToF (f (Var x)))
iToF (App f a) = app (iToF f) (iToF a)
iToF (Fix f) = fix (\self -> iToF (f (Var self)))
\end{code}
\end{frame}

\begin{frame}
\frametitle{end}
Check out Oleg's writeups at
\begin{itemize}
\item http://okmij.org/ftp/tagless-final/index.html
\item http://okmij.org/ftp/tagless-final/course/index.html
\end{itemize}
This presentation and some code snippets at
\newline
https://github.com/shterrett/ttfi
\end{frame}

\end{document}

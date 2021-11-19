module P9 where

data Expr
  = Lit Literal
  | Add Expr Expr
  | Div Expr Expr
  | Gth Expr Expr
  | If Expr Expr Expr
  deriving Show

data Literal = N Int | B Bool
  deriving (Eq, Show)

num :: Int -> Expr
num = Lit . N

bool :: Bool -> Expr
bool = Lit . B

e1, e2 :: Expr
e1 = (num 4 `Add` num 5) `Div` num 2
e2 = If (e1 `Gth` num 4)
        (num 3)
        (num 4 `Add` num 1)

e3_bad, e4_bad :: Expr
e3_bad = If (num 4) (bool True) (num 0)

e4_bad = If (bool False) (num 4) (num 3 `Div` num 0)

eval :: Expr -> Maybe Literal
eval (Lit n) = return n
eval (Add e1 e2) = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (N n1, N n2) -> return (N (n1 + n2))
    _ -> Nothing
eval (Div e1 e2) = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (N n1, N 0) -> Nothing
    (N n1, N n2) -> return (N (n1 `div` n2))
    _ -> Nothing
eval (Gth e1 e2) = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (N n1, N n2) -> return (B (n1 > n2))
    _ -> Nothing
eval (If e1 e2 e3) = do
  res1 <- eval e1
  case res1 of
    B True -> eval e2
    B False -> eval e3
    _ -> Nothing

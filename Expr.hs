{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
module Expr where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Map (Map)

default (Text, Double)

data Var = Var Text
    deriving (Eq, Ord, Read, Show)

ppVar :: Var -> String
ppVar (Var n) = Text.unpack n

class ToVar a where
  toVar :: a -> Var

instance ToVar Text where
  toVar a = Var a

data Unit
  = Meter
  | Kg
  | Farad
    deriving (Eq, Ord, Read, Show)

ppUnit :: Unit -> String
ppUnit Meter = "m"
ppUnit Kg    = "kg"

data Expr where
  Lambda :: Var -> Expr -> Expr
  App    :: Expr -> Expr -> Expr
  EVar   :: Var -> Expr
  V      :: Double -> Expr
  U      :: Unit -> Expr
  Add    :: Expr -> Expr -> Expr
  Sub    :: Expr -> Expr -> Expr
  Mult   :: Expr -> Expr -> Expr
  Div    :: Expr -> Expr -> Expr
  Power  :: Expr -> Expr -> Expr
  E      :: Expr
  Sin    :: Expr -> Expr
  Cos    :: Expr -> Expr
  Ln     :: Expr -> Expr
  Deriv  :: Expr -> Var -> Expr

class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr Double where
  toExpr d = V d

instance ToExpr Text where
  toExpr v = EVar (toVar v)

instance ToExpr Var where
  toExpr v = EVar v

instance ToExpr Expr where
  toExpr e = e

ppExpr :: Expr -> String
ppExpr (Lambda v e) = "\\" <> ppVar v <> " -> " <> ppExpr e
ppExpr (App f x) = "(" <> ppExpr f <> " " <> ppExpr x <> ")"
ppExpr (EVar v) = ppVar v
ppExpr (V d) = show d
ppExpr (U u) = ppUnit u
ppExpr (Add x y)  = "(" <> (ppExpr x) <> " + " <> (ppExpr y) <> ")"
ppExpr (Sub x y)  = "(" <> (ppExpr x) <> " - " <> (ppExpr y) <> ")"
ppExpr (Mult x y) = "(" <> (ppExpr x) <> " * " <> (ppExpr y) <> ")"
ppExpr (Div x y)  = "(" <> (ppExpr x) <> " / " <> (ppExpr y) <> ")"
ppExpr (Power x y)  = "(" <> (ppExpr x) <> " ^ " <> (ppExpr y) <> ")"
ppExpr E        =  "e"
ppExpr (Ln e)      = "ln(" <> ppExpr e <> ")"
ppExpr (Sin e)     = "sin(" <> ppExpr e <> ")"
ppExpr (Cos e)     = "cos(" <> ppExpr e <> ")"
ppExpr (Deriv x v) = "((d" <> (ppExpr x) <>")/d" <> ppVar v <>")"

printExpr :: Expr -> IO ()
printExpr e = putStrLn $ ppExpr e

-- FIXME: variable capture
subst :: Var -> Expr -> Expr -> Expr
subst v e e'@(EVar (Var v'))
  | v == v = e
  | otherwise = e'
subst v e e'@(V {}) = e'
subst v e e'@(U {}) = e'
subst v e (Add x y) = Add (subst v e x) (subst v e y)
subst v e (Sub x y) = Sub (subst v e x) (subst v e y)
subst v e (Mult x y) = Mult (subst v e x) (subst v e y)
subst v e (Div x y) = Div (subst v e x) (subst v e y)
subst v e (Power x y) = Power (subst v e x) (subst v e y)
subst v e E = E
subst v e (Ln x) = Ln (subst v e x)
subst v e (Sin x) = Sin (subst v e x)
subst v e (Cos x) = Cos (subst v e x)
subst v e (Deriv x v') = Deriv (subst v e x) v' -- FIXME: should be substitute for v'?
subst v e (Lambda x e') = Lambda x (subst v e e')
subst v e (App f x) = App (subst v e f) (subst v e x)

eval :: Map Var Double -> Expr -> Expr
eval env e =
  case e of
    (Lambda v e) -> (Lambda v (eval env e))
    (App f x) ->
      let f' = eval env f
          x' = eval env x
      in case f' of
        (Lambda v e) ->
          eval env (subst v x' e)
    (EVar v) ->
      case Map.lookup v env of
        Nothing -> (EVar v)
        (Just d) -> (V d)
    (V d) -> e
    (U u) -> e
    (Add x y) ->
      let x' = eval env x
          y' = eval env y
      in case (x', y') of
           (V d1, y)
             | d1 == 0   -> eval env y
           (x, V d2)
             | d2 == 0   -> eval env x
           (V d1, V d2) -> V (d1 + d2)
           _            -> Add x' y'
    (Sub x y) ->
      let x' = eval env x
          y' = eval env y
      in case (x', y') of
           (V d1, V d2) -> V (d1 - d2)
           _            -> Sub x' y'
    (Mult x y) ->
      let x' = eval env x
          y' = eval env y
      in case (x', y') of
           (V d1, y)
             | d1 == 1   -> eval env y
           (x, V d2)
             | d2 == 1   -> eval env x
           (V d1, V d2) -> V (d1 * d2)
           _            -> Mult x' y'

    (Div x y) ->
      let x' = eval env x
          y' = eval env y
      in case (x', y') of
           (V d1, V d2) -> V (d1 / d2)
           _            -> Div x' y'

    (Power x y) ->
      let x' = eval env x
          y' = eval env y
      in case (x', y') of
           (V d1, V d2) -> V (d1 ** d2)
--           (Var _, y)   -> Power x' y'
           _ -> distributePower x' y'
--           _            -> Power x' y'

    -- tricky. We like to keep E around, except when we realllly want a decimal value
    E -> E -- (V (exp 1))
      {-
      let v' = eval env v
      in case v' of
        (V d) -> V (exp d)
        _ -> E v'
-}
    (Ln v) ->
      let v' = eval env v
      in case v' of
        (V d) -> V (logBase (exp 1) d)
        v' -> Ln v'
--        _ -> E v'
    (Sin v) ->
      let v' = eval env v in
      case v' of
        (V d) -> V (sin d)
        _ -> Sin v'

    (Cos v) ->
      let v' = eval env v in
      case v' of
        (V d) -> V (cos d)
        _ -> Cos v'

    (Deriv x v) -> derive x v
--     e -> error $ "eval can't handle " ++ ppExpr e


-- add cases for raising to the 0th power
distributePower :: Expr -> Expr -> Expr
distributePower x@(EVar _) (V 1) = x
distributePower x@(EVar _) y = Power x y
distributePower E y = Power E y
distributePower (Power x y') y = eval Map.empty (Power x (Add y y'))
distributePower (Mult a b) y = Mult (distributePower a y) (distributePower b y)
distributePower (V d) (V y) = V (d ** y)
distributePower e (V 1) = e
distributePower e y = Power e y
-- distributePower x y = error $ "distributePower x = " ++ ppExpr x ++ " y = " ++ ppExpr y

-- In the example, x^r, the derivative is only defined when x /= 0, but how do we handle that?
-- https://en.wikipedia.org/wiki/Derivative
derive :: Expr -> Var -> Expr
derive (V _) v = V 0
derive (Lambda x e) v = (Lambda x (derive e v))
derive (EVar x) v
  -- same as x^1
  | x == v = V 1
  | otherwise = Deriv (EVar x) v
    -- e rule
derive e@(Power E y) v =
  case y of
    (EVar v') | v == v' -> e
derive (Power (EVar x) y) v
  | x == v = (Mult y (Power (EVar x) ({- eval Map.empty  -}(Sub y (V 1)))))
    -- constant rule
  | otherwise = V 0
    -- constant rule
derive E _ = V 0
 -- FIXME: only valid for x > 0, but not enforced
  -- what if v is not in e ?
derive (Ln e) v = Div (V 1) e
derive (Add x y) v = Add (derive x v) (derive y v)
    -- sum rule
derive (Sub x y) v = Sub (derive x v) (derive y v)
  -- product rule
derive (Mult f g) v = Add (Mult (derive f v) g) (Mult f (derive g v))
  -- quotient rule
derive (Div f g) v = Div (Sub (Mult (derive f v) g) (Mult f (derive g v))) (Power g (V 2))
 -- sine rule -- what if 'v' does not appear in 'e'
derive (Sin e) v = Mult (derive e v) (Cos e)
 -- cosine rule -- what if 'v' does not appear in 'e'
derive (Cos e) v = Sub (V 0) (Mult (derive e v) (Sin e))

-- instance (ToExpr a, ToExpr b) => MkAdd a b where
(.+.) ::  (ToExpr a, ToExpr b) => a -> b -> Expr
a .+. b = Add (toExpr a) (toExpr b)

(.*.) ::  (ToExpr a, ToExpr b) => a -> b -> Expr
a .*. b = Mult (toExpr a) (toExpr b)

(.-.) ::  (ToExpr a, ToExpr b) => a -> b -> Expr
a .-. b = Sub (toExpr a) (toExpr b)

(.^.) ::  (ToExpr a, ToExpr b) => a -> b -> Expr
a .^. b = Power (toExpr a) (toExpr b)

t :: Expr
t = Add (Add (("x"::Text) .^. (Div (V 1) (V 4))) (toExpr ("x"::Text))) (V 5)

productRule = ppExpr $ eval Map.empty (derive (Mult (EVar (Var "u")) (EVar (Var "v"))) (Var "x"))
quotientRule = ppExpr $ eval Map.empty (derive (Div (EVar (Var "u")) (EVar (Var "v"))) (Var "x"))

-- https://en.wikipedia.org/wiki/Quotient_rule
-- could stand to factor out and cancel some stuff
quotientRule2 =
  let x = Var "x"
  in ppExpr $ eval Map.empty (derive (Div (Power E (EVar x)) (Power (EVar x) (V 2))) x)

example =
    let x' = Var "x"
        x = EVar x'
        e = (Sub
             (Add (Power x (V 4)) (Sin (Power x (V 2))))
             (Add (Mult (Ln x) (Power E x)) (V 7))
            )
    in ppExpr $ eval Map.empty (derive e x')


-- https://www.dummies.com/education/science/science-electronics/analyze-a-series-rc-circuit-using-a-differential-equation/

capacitor_i c v =
  let t' = Var "t"
      t = EVar t'
  in c  .*. Deriv v t'

{-# language DataKinds #-}
{-# language TypeOperators #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map

default (Double)

{-
fB – Tuning frequency of the port in the vented chamber.
R – The radius of the port in centimeters.
Lv – Length of the port in centimeters.
fL – f3 of the low frequency roll-off.
fH – f3 of the high frequency roll-off.
Qbp – Q of the sealed rear chamber.
Vf – Volume of the front chamber (the acoustic filter chamber) in liters.
Vr – Volume of the rear chamber (the sealed chamber) in liters.
S – Passband ripple (How many ±db does the frequency response deviate from linear frequency response).
Vt – Total volume (Vf+Vr) in liters.
-}

data DriverParameter
  = Fs   -- Free Air Resonance
  | Qes  -- Electrical “Q”
  | Qms  -- Mechanical “Q”
  | Qts  -- Total Speaker “Q”
  | Vas  -- Equivalent Compliance
  | Xmax -- One-Way Linear Excursion
  | No   -- no - Reference Efficiency
  | Sd   -- Effective Piston Area
  | Re   -- DC Resistance
  deriving (Eq, Ord, Read, Show)

data Value
  = V Double Unit
    deriving (Eq, Ord, Read, Show)

data Unit
  = Hz
  | L
  | CubicFeet
  | CM
  | MM
  | IN
  | One -- unitless
  | Percent
  | SqIn
  | Ohm
  | Divide Unit Unit
  | Combine Unit Unit
    deriving (Eq, Ord, Read, Show)


data Formula
  = DP DriverParameter
  | Val Value
  | Mul Formula Formula
  | Add Formula Formula
  | Subtract Formula Formula
  | Power Formula Formula
  | Div Formula Formula
    deriving (Eq, Ord, Read, Show)

pretty :: Formula -> String
pretty (DP dp)         = show dp
pretty (Val (V v u))   = show v ++ show u
pretty (Mul a b)       = "(" ++ pretty a ++ ") * (" ++ pretty b ++ ")"
pretty (Div a b)       = "(" ++ pretty a ++ ") / (" ++ pretty b ++ ")"
pretty (Add a b)       = "(" ++ pretty a ++ ") + (" ++ pretty b ++ ")"
pretty (Subtract a b)  = "(" ++ pretty a ++ ") - (" ++ pretty b ++ ")"
pretty (Power a b)     = "(" ++ pretty a ++ ") ** (" ++ pretty b ++ ")"

class ToFormula a where
  toFormula :: a -> Formula

instance ToFormula DriverParameter where
  toFormula = DP

instance ToFormula Value where
  toFormula = Val

instance ToFormula Double where
  toFormula d = Val (V d One)

instance ToFormula Formula where
  toFormula = id

(./.) :: (ToFormula a, ToFormula b) => a -> b -> Formula
a ./. b = Div (toFormula a) (toFormula b)

(.*.) :: (ToFormula a, ToFormula b) => a -> b -> Formula
a .*. b = Mul (toFormula a) (toFormula b)

(.+.) :: (ToFormula a, ToFormula b) => a -> b -> Formula
a .+. b = Add (toFormula a) (toFormula b)

(.-.) :: (ToFormula a, ToFormula b) => a -> b -> Formula
a .-. b = Subtract (toFormula a) (toFormula b)

(.**.) :: (ToFormula a, ToFormula b) => a -> b -> Formula
a .**. b = Power (toFormula a) (toFormula b)

d :: Double -> Formula
d = toFormula


-- infixr (./.)
-- infixr (.*.)

data UnitError =
  IncompatibleUnits Formula Unit Unit
  deriving (Eq, Ord, Read, Show)

mUnit :: Unit -> Unit -> Either UnitError Unit
mUnit IN IN = Right SqIn
mUnit a One = Right a
mUnit One b = Right b
mUnit a b = Right (Combine a b)
-- mUnit a b = Left (IncompatibleUnits a b)

dUnit :: Unit -> Unit -> Either UnitError Unit
dUnit a b | a == b = Right One
dUnit a One = Right a
dUnit a b = Right $ Divide a b
-- dUnit a b = Left (IncompatibleUnits a b)

data EvalError
     = MissingDriverParameter Driver DriverParameter
     | UnitError UnitError
       deriving (Eq, Ord, Read, Show)

evaluate :: Driver -> Formula -> Either EvalError Value
evaluate driver formula =
  case formula of
    (Val v) -> Right v
    (DP p) ->
      case Map.lookup p driver of
        Nothing -> Left (MissingDriverParameter driver p)
        (Just v) -> Right v
    (Mul a b) ->
      case evaluate driver a of
        (Left e) -> Left e
        (Right (V a' ua)) ->
          case evaluate driver b of
            (Left e) -> Left e
            (Right (V b' ub)) ->
              case mUnit ua ub of
                (Left ue) -> Left (UnitError ue)
                (Right uc) ->
                  Right $ V (a' * b') uc
    (Div a b) ->
      case evaluate driver a of
        (Left e) -> Left e
        (Right (V a' ua)) ->
          case evaluate driver b of
            (Left e) -> Left e
            (Right (V b' ub)) ->
              case dUnit ua ub of
                (Left ue) -> Left (UnitError ue)
                (Right uc) ->
                  Right $ V (a' / b') uc
    (Power a b) ->
      case evaluate driver a of
        (Left e) -> Left e
        (Right (V a' ua)) ->
          case evaluate driver b of
            (Left e) -> Left e
            (Right (V b' One)) ->
                  Right $ V (a' ** b') ua

    (Add a b) ->
      case evaluate driver a of
        (Left e) -> Left e
        (Right (V a' ua)) ->
          case evaluate driver b of
            (Left e) -> Left e
            (Right (V b' ub)) ->
              if (ua == ub) || True
              then Right (V (a' + b') ua)
              else Left (UnitError (IncompatibleUnits formula ua ub))

    (Subtract a b) ->
      case evaluate driver a of
        (Left e) -> Left e
        (Right (V a' ua)) ->
          case evaluate driver b of
            (Left e) -> Left e
            (Right (V b' ub)) ->
              if (ua == ub) || True
              then Right (V (a' - b') ua)
              else Left (UnitError (IncompatibleUnits formula ua ub))

{-
data Volume
  = Liter
  | CubicFeet
    deriving (Eq, Ord, Read, Show)

data Length
  = IN
  | CM
  | MM
  deriving (Eq, Ord, Read, Show)

data Unit
  = Vol Volume
  | Len Length
    deriving (Eq, Ord, Read, Show)
-}

type Driver = Map DriverParameter Value

jlAudioM10W5 :: Driver
jlAudioM10W5 =
  Map.fromList [ (Fs , V 31.55 Hz)
               , (Qes, V 0.484 One)
               , (Qms, V 8.948 One)
               , (Qts, V 0.46 One)
               , (Vas, V 44.75 L)
               , (Xmax, V 13.2 MM)
               , (No, V 0.28 Percent)
               , (Sd, V 50.11 SqIn)
               , (Re, V 3.602 Ohm)
               ]

jlAudio10TW3_D4 :: Driver
jlAudio10TW3_D4 =
  Map.fromList [ (Fs, V 32.3 Hz)
               , (Qes, V 0.656 One)
               , (Qms, V 11.35 One)
               , (Qts, V 0.62 One)
               , (Vas, V 19.82 L)
               , (Xmax, V 15.2 MM)
               ]

wetSoundsRevo15XXXV4_B :: Driver
wetSoundsRevo15XXXV4_B =
  Map.fromList [ (Fs, V 35.0 Hz)
               , (Qes, V 0.4 One)
               , (Qms, V 6.00 One)
               , (Qts, V 0.38 One)
               , (Vas, V 24 L)
               , (Xmax, V 23 MM)
               ]

wetSoundsRevo12HP_S4 :: Driver
wetSoundsRevo12HP_S4 =
  Map.fromList [ (Fs, V 30.0 Hz)
               , (Qes, V 0.51 One)
               , (Qms, V 3.80 One)
               , (Qts, V 0.45 One)
               , (Vas, V 46.5 L)
               , (Xmax, V 11.9 MM) -- printed as 1.9mm in PDF -- probably a typo?
               ]

-- * sealed box

-- qtc a = ((a .+. (d 1)) .**. (d 0.5)) .*. Qts

fc qtc = (qtc .*. Fs) ./. Qts

fs qtc = ((fc qtc) .*. Qts) ./. qtc

f3 qtc = (((  (((d 1) ./. (qtc .**. (d 2))) .-. (d 2)) .+.
            (((((((d 1) ./. (qtc .**. (d 2))) .-. (d 2)) .**. (d 2)) .+. (d 4))) .**. (d 0.5)) ./. (d 2))) .**. (d 0.5)) .*. (fc qtc)

-- * 4th order bandpass

-- Qbp, passband ripple (S), sensitivity gain (Pa)
qbp :: Double -> Double -> Value
qbp s pa = V (1.0 / (2.0 * s * (10 ** (-pa / 40)))) One


-- where do these magic numbers come from? Bessel function?
b 0.7 = 0.7206
b 0.6 = 0.9560
b 0.5 = 1.2712

-- S, Pa
fLowFactor :: Double -> Double -> Value
fLowFactor s pa =
  let (V qBP One) = qbp s pa
      b' = b s
  in V (((0-b') + (((b' ** 2) + (4 * (qBP ** 2))) ** 0.5)) / 2) One

fHighFactor :: Double -> Double -> Value
fHighFactor s pa =
  let (V qBP One) = qbp s pa
      b' = b s
  in V ((((0-b') + (((b' ** 2) + (4 * (qBP ** 2))) ** 0.5)) / 2) + b') One

fL :: Double -> Double -> Formula
fL s pa = (Fs ./. Qts) .*. (Val $ fLowFactor s pa)

fH :: Double -> Double -> Formula
fH s pa = (Fs ./. Qts) .*. (Val $ fHighFactor s pa)

-- V_f - volume of front enclosure
vf :: Double -- ^ S
   -> Formula
vf s = (((V 2 One) .*. s .*. Qts) .**. (2 :: Double)) .*. Vas

-- V_r - volume of rear enclosure
vr :: Double -- ^ S
   -> Double -- ^ Pa
   -> Formula
vr s pa =
  let qBp = qbp s pa
  in Vas ./. (((qBp ./. Qts) .**. (2 :: Double)) .-. (1 :: Double))

-- f_b - tuning of the front chamber
fb s pa =
    let qBp = qbp s pa
    in qBp .*. (Fs ./. Qts)

-- L_v -- port length
lv :: Double -> Double -> Value -> Formula
lv s pa r =
  let vf' = vf s
      fb' = fb s pa
  in (((d 94250) .*. (r .**. (d 2)))  ./. ((fb' .**. d 2) .*. vf')) .-. ((d 1.595) .*. r)
--  in (((d 94250) .*. (r .**. (d 2))) ./. (fb' .*. vf')) .-. ((d 1.595) .*. r)

main = pure ()

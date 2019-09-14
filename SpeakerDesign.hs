{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

-- http://audiojudgement.com/4th-order-bandpass-design/
-- http://audiojudgement.com/sealed-enclosure-closed-box/
-- https://engineering.purdue.edu/ece103/LectureNotes/SRS_Loudspeaker_Parameters.pdf
-- https://www.linkwitzlab.com/thor-design.htm
-- http://www.faqs.org/faqs/car-audio/part3/
-- http://audiojudgement.com/thiele-small-parameters-explained/

{-
To graph the response of a speaker box, perhaps we convert it to an
equivalent electrical model, and then use circuit analysis technics to
calculate the response. Transfer Function ?
-}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

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

data ThieleSmall
  = B    -- ^ Magnetic flux density in gap
  | Bl   -- ^ product of magnetic field strength in voice coil gap and length of wire in magnetic field
  | Cas  -- ^ Accoustic equivalent of Cms
  | Cmes -- ^ The electrical capacitive equivalent of Mms
  | Cms  -- ^ The compliance of the suspension (the spider and the surround, to be exact). If the suspension is stiff, the driver is not compliant. So, the easy it is to move the speaker, the more compliant it is.
  | D    -- ^ effective diameter of the driver
  | Fs   -- ^ Free Air Resonance
  | Fc   -- ^ System Resosance (usually for sealed box systems), in Hz
  | Fb   -- ^ Enclosure Resonance (usually for reflex systems), in Hz
  | F3   -- ^ -3dB cutoff frequency
  | Ll    -- ^ Length of wire immersed in magnetic field (usually just written with a lowercase l)
  | Lces -- ^ The electrical inductive equivalent of Cms
  | Le   -- ^ voice coil inductance
  | Mas  -- ^ Acoustic equivalent of Mms
  | Mms  -- ^ Mass of diaphragm
  | N0   -- ^ no - Reference Efficiency
  | Pa   -- ^ Accoustical power
  | Pe   -- ^ Electrical power
  | Qec  -- ^ System's “Q” @ Fc, due to electrical losses
  | Qes  -- ^ Driver's “Q” @ Fs, due to electrical losses
  | Qms  -- ^ Driver's "Q" @ Fs, due to mechanical losses
  | Qmc  -- ^ System's "Q" @ Fc, due to mechanical losses
  | Qtc  -- ^ System's “Q” @ Fc, due to all losses
  | Qts  -- ^ Driver's “Q” @ Fs, due to all losses
  | Q1   -- ^ System's “Q” @ Fb due to leakage losses
  | Qa   -- ^ System's “Q” @ Fb due to absorption losses
  | Qp   -- ^ System's “Q” @ Fb due to port losses
  | Re   -- ^ Driver DC Resistance
  | Rg   -- ^ Amplifier DC Resistance
  | Ras  -- ^ Accoustice equivalent of Rms
  | Res  -- ^ The electrical resistive equivalent of Rms
  | Rms  -- ^ mechanical resistance of driver's suspension
  | Sd   -- ^ Effective Piston Area
  | Vas  -- ^ Equivalent Compliance
  | Vc   -- ^ Volume of the box
  | Vd   -- ^ Maximum linear volume of the displacement of the driver (Sd * Xmax)
  | Xmax -- ^ One-Way Linear Excursion -- one way or two way?
  deriving (Eq, Ord, Read, Show)

data Value
  = V Double Unit
    deriving (Eq, Ord, Read, Show)

data Unit
  = Hz
  | Kg
  | L
  | CubicFeet
  | M -- ^ meter
  | CM
  | MM
  | IN
  | One -- unitless
  | Percent
  | S -- ^ second
  | SqIn
  | Ohm
  | Divide Unit Unit
  | Multiply Unit Unit
    deriving (Eq, Ord, Read, Show)


data Formula
  = DP ThieleSmall
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

instance ToFormula ThieleSmall where
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
mUnit a b = Right (Multiply a b)
-- mUnit a b = Left (IncompatibleUnits a b)

dUnit :: Unit -> Unit -> Either UnitError Unit
dUnit a b | a == b = Right One
dUnit a One = Right a
dUnit a b = Right $ Divide a b
-- dUnit a b = Left (IncompatibleUnits a b)

data EvalError
     = MissingThieleSmall Driver ThieleSmall
     | UnitError UnitError
       deriving (Eq, Ord, Read, Show)

evaluate :: Driver -> Formula -> Either EvalError Value
evaluate driver formula =
  case formula of
    (Val v) -> Right v
    (DP p) ->
      case Map.lookup p (_driverParams driver) of
        Nothing -> Left (MissingThieleSmall driver p)
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

-- * Drivers

data Driver = Driver
  { _driverName   :: Text
  , _driverParams :: Map ThieleSmall Value
  }
  deriving (Eq, Ord, Read, Show)

-- | nullDriver
-- used for calculations which are not driver specific
nullDriver :: Driver
nullDriver = Driver
  { _driverName = "Null Driver"
  , _driverParams = Map.empty
  }

jlAudioM8W5 :: Driver
jlAudioM8W5 = Driver
  { _driverName = "JL Audio M8W5"
  , _driverParams =
       Map.fromList [ (Fs , V 40.53 Hz)
                    , (Qes, V 0.676 One)
                    , (Qms, V 13.11 One)
                    , (Qts, V 0.643 One)
                    , (Vas, V 24.07 L)
                    , (Xmax, V 14 MM) -- one way
                    , (N0, V 0.28 Percent)
                    , (Sd, V 35.15 SqIn)
                    , (Re, V 3.147 Ohm)
                    ]
  }

jlAudioM10W5 :: Driver
jlAudioM10W5 = Driver
  { _driverName = "JL Audio M10W5"
  , _driverParams =
       Map.fromList [ (Fs , V 31.55 Hz)
                    , (Qes, V 0.484 One)
                    , (Qms, V 8.948 One)
                    , (Qts, V 0.46 One)
                    , (Vas, V 44.75 L)
                    , (Xmax, V 13.2 MM)
                    , (N0, V 0.28 Percent)
                    , (Sd, V 50.11 SqIn)
                    , (Re, V 3.602 Ohm)
                    ]
  }

-- | used on this page http://audiojudgement.com/4th-order-bandpass-design/
jlAudio10TW3_D4 :: Driver
jlAudio10TW3_D4 = Driver
  { _driverName = "JL Audio 10TW3-D4"
  , _driverParams =
      Map.fromList [ (Fs, V 32.3 Hz)
                   , (Qes, V 0.656 One)
                   , (Qms, V 11.35 One)
                   , (Qts, V 0.62 One)
                   , (Vas, V 19.82 L)
                   , (Xmax, V 15.2 MM)
                   ]
  }

pylePLMRW10 :: Driver
pylePLMRW10 = Driver
 { _driverName = "Pyle PLMR W10"
 , _driverParams =
     Map.fromList [ (Re, V 3.6 Ohm)
                  , (Fs, V 32 Hz)
                  , (Qms, V 5.269 One)
                  , (Qes, V 0.504 One)
                  , (Qts, V 0.46 One)
                  , (Xmax, V 4 MM)
                  , (Vas, V 3.012 CubicFeet)
                  ]
 }

wetSoundsRevo15XXXV4_B :: Driver
wetSoundsRevo15XXXV4_B = Driver
  { _driverName = "WetSounds Revo 15 XXX V4-B"
  , _driverParams =
      Map.fromList [ (Fs, V 35.0 Hz)
                   , (Qes, V 0.4 One)
                   , (Qms, V 6.00 One)
                   , (Qts, V 0.38 One)
                   , (Vas, V 24 L)
                   , (Xmax, V 23 MM)
                   ]
  }

wetSoundsRevo12HP_S4 :: Driver
wetSoundsRevo12HP_S4 = Driver
  { _driverName = "WetSounds Revo 12 HP-S4"
  , _driverParams =
      Map.fromList [ (Fs, V 30.0 Hz)
                   , (Qes, V 0.51 One)
                   , (Qms, V 3.80 One)
                   , (Qts, V 0.45 One)
                   , (Vas, V 46.5 L)
                   , (Xmax, V 11.9 MM) -- printed as 1.9mm in PDF -- probably a typo?
                   ]
  }

-- allDrivers

allDrivers :: [Driver]
allDrivers =
  [ jlAudioM8W5, jlAudioM10W5, jlAudio10TW3_D4, pylePLMRW10, wetSoundsRevo15XXXV4_B, wetSoundsRevo12HP_S4 ]

evalAll :: Formula -> [(Text, Either EvalError Value)]
evalAll f = map (\d -> (_driverName d, evaluate d f)) allDrivers

-- * General forumlas

-- | Propagation velocity of sound at STP, approx. 342 m/s
c :: Value
c = V 342 (Divide M S)

-- | Density of air at STP 1.18 kg/m^3 (rho)
p :: Value
p = V 1.8 (Divide Kg (Multiply (Multiply M M) M))

ebp :: Formula
ebp = Fs ./. Qes

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

-- | f3 of the low frequency roll-off
--
-- passband ripple (S), sensitivity gain (Pa)
--
--    Best transients for S = 0.7, and 0 db ripple.
--    S = 0.6 , somewhat degraded transients , ±0.35 db ripple.
--    S = 0.5 , worse transients than S = 0.6 , ± 1.25 db ripple.
--
--  Pa might range from +/- 8dB
fL :: Double -- ^ S
   -> Double -- ^ Pa
   -> Formula
fL s pa = (Fs ./. Qts) .*. (Val $ fLowFactor s pa)

fH :: Double -> Double -> Formula
fH s pa = (Fs ./. Qts) .*. (Val $ fHighFactor s pa)

-- | V_f - volume of front enclosure
vf :: Double -- ^ S
   -> Formula
vf s = (((V 2 One) .*. s .*. Qts) .**. (2 :: Double)) .*. Vas

-- | V_r - volume of rear enclosure
vr :: Double -- ^ S
   -> Double -- ^ Pa
   -> Formula
vr s pa =
  let qBp = qbp s pa
  in Vas ./. (((qBp ./. Qts) .**. (2 :: Double)) .-. (1 :: Double))

-- | f_b - tuning of the front chamber
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

{- Calculating Orders:

There is disagreement as to whether in IB / Free-air speaker is 1st order or 2nd order. Some argue that the speaker itself is second order so you can get any less than that.

Some suggest that a sealed box does not add an order because the sealed box is in parallel with the compliance of the speaker itself.

-}

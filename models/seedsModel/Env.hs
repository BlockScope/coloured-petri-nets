{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SeedsModel.Env where

import Chromar
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Map.Strict as Map


-- TODO: Need to generate this data.
-- SEE: doi:10.1093/jxb/ery394
-- SEE: A multi-model framework for the Arabidopsis life cycle
-- For the growth simulations we used weather data from the ECMWF ERA-Interim
-- dataset over the years 2010–2011 (Dee et al., 2011). A program provided by
-- Mathew Williams and Luke Smallman (School of GeoSciences, University of
-- Edinburgh) that uses methods from Williams et al. (2001) was used to
-- generate hourly inputs given daily averages from the dataset for temperature
-- and radiation. For the soil moisture input used in the photosynthesis rate
-- calculation, we used a daily average of soil moisture values from the
-- dataset and assumed that to be constant throughout the day (swvl parameters
-- in the ERA dataset). The soil moisture parameter here is a number in
-- arbitrary units from 0 to 1 that represents the ‘wetness’ of the soil while
-- the soil moisture used above measures water potential and is given in MPa.
dataFile :: String
dataFile = "data/weatherValencia10yrs.csv"

temp', photo', day', moist :: Fluent Double
temp' = unsafePerformIO (readTable dataFile 4)
photo' = unsafePerformIO (readTable dataFile 2)
day' = unsafePerformIO (readTable dataFile 3)
moist = unsafePerformIO (readTable dataFile 5)

fi, fu :: Double
fi = 0.598
--fi = 0.737
fu = 0

psmax, psmin, psu, psl, psSc, tbar, tbg, tbd, kt, to :: Double
psmax = -5
psmin = -1
psu = -50
psl = -350
psSc = 1.0
tbar = 3.0
tbg = 3.0
tbd = 3.0
kt = 0.12
to = 22

day :: Fluent Bool
day = day' <>*> constant 0.0

idev, idev', idev'' :: Fluent Double
idev = (*) <$> constant 0.374 <*> (photo' <-*> constant 10.0)
idev' = (/) <$> idev <*> constant 4.0
idev'' = constant 0.626 <+*> idev'

tempBase, temp, thermal, pperiod, ptu :: Fluent Double
tempBase = constant 3.0
temp = max <$> (temp' <-*> tempBase) <*> pure 0.0
thermal = when day temp `orElse` constant 0.0

pperiod =
  when (photo' <<*> constant 10.0) (constant 0.626) `orElse`
  (when (photo' <<*> constant 14.0) idev'' `orElse` constant 1.0)

ptu = (*) <$> thermal <*> pperiod

tmin, tmax, wcsat :: Double
tmin = -3.5
tmax = 6.0
wcsat = 960.0

favTemp :: Double -> Bool
favTemp temp = temp >= tmin && temp <= tmax

wcAcc :: Double -> Double -> Double
wcAcc wc t = wc + exp k * ((t-tmin)**o) * ((tmax-t)**ksi)
  where
    k   = -5.1748
    o   = 2.2256
    ksi = 0.99590

wcUpd :: Double -> Double -> Double
wcUpd t wc =
  if favTemp ctemp
    then wc'
    else wc
  where
    ctemp = at temp t
    wc' = min (wcAcc wc ctemp) wcsat

fp :: Double -> Double
fp wc =
  if wc < wcsat
    then fp1
    else fp2
  where
    wcRat = wc / wcsat
    fp1 = 1 - fi + (fi - fu) * wcRat
    fp2 = 1 - fu

arUpd, psB :: Double -> Double -> Double
arUpd moist temp
  | moist <= psmax && moist >= psu = temp - tbar
  | moist < psu && moist > psl = ((psl - moist) / (psl - psu)) * (temp - tbar)
  | moist <= psmax || moist <= psl = 0.0
  | otherwise = 0.0


psB ar psi =
  if psb' > psmin
     then psb'
     else psmin
  where
    arlab = arUpd (-200) 20
    dsat = 40
    psb' = psi - psSc * (ar / (arlab * dsat * 24) )


htuSub, htuOpt :: Double -> Double -> Double -> Double -> Double
htuSub ar psi moist temp = (moist - psB ar psi) * (temp - tbg)

htuOpt ar psi moist temp = (moist - mpsB) * (to - tbg)
  where
    mpsB = psB ar psi + kt * (temp - to)

--- t : time
--- a : afterripening
--- psi : initial dorm
htu :: Double -> Double -> Double -> Double
htu t a psi
  | moistt > psb && tempt > tbg && tempt < to = htuSub ar psi moistt tempt
  | mpsB < moistt && tempt > to = htuOpt a psi moistt tempt
  | otherwise = 0.0
  where
    tempt = at temp t
    moistt = at moist t
    ar = a + arUpd moistt tempt
    psb = psB ar psi
    mpsB = psB ar psi + kt * (tempt-to)


disp :: Fluent Double
disp = when (ntemp <>*> constant 0.0) ntemp `orElse` (constant 0.0) where
  ntemp = temp <-*> constant tbd

parseLine :: Int -> T.Text -> (Double, Double)
parseLine n ln = (read $ T.unpack time, read $ T.unpack temp) where
  elems = T.splitOn (T.pack ",") ln
  time = T.dropEnd 1 (T.drop 1 $ elems !! 0)
  temp = elems !! n


readTable :: FilePath -> Int -> IO (Fluent Double)
readTable fn n = do
  contents <- TI.readFile fn
  let vals = map (parseLine n) (T.lines contents)
  return $ flookupM (Map.fromList vals)

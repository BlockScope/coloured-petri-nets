module Chromar.Observables where

import Numeric (showFFloat)
import qualified System.Random as R (getStdGen)
import Chromar.Multiset (Multiset, toList)
import Chromar.Core (Model(..), Time, State(..), simulate, getT)

type Obs = Double

type ObsF a = Multiset a -> Obs

type TObs = (Time, [Obs])

data Observable a = Observable
    { name :: String
    , gen :: ObsF a
    }

names :: [Observable a] -> [String]
names = map name

gens :: [Observable a] -> [ObsF a]
gens = map gen

select :: (a -> Bool) -> Multiset a -> Multiset a
select f = filter (\(el, _) -> f el)

aggregate :: (a -> Obs -> Obs) -> Obs -> Multiset a -> Obs
aggregate f i s = foldr f i (toList s)

sumM :: (a -> Obs) -> Multiset a -> Obs
sumM f m = sum (map (\(el, n) -> f el * fromIntegral n) m)

countM :: Multiset a -> Obs
countM s = sum [ fromIntegral n | (_el, n) <- s ]

selectAttr :: (Eq b) => (a -> b) -> b -> Multiset a -> Multiset a
selectAttr f v = filter (\(el, _) -> f el == v)

applyObs :: [State a] -> [ObsF a] -> [TObs]
applyObs ss fs = [ (t, map ($ s) fs) | (State s t _) <- ss ]

formatFloatN :: RealFloat a => Int -> a -> String
formatFloatN numOfDecimals floatNum =
    showFFloat (Just numOfDecimals) floatNum ""

printObs :: [State a] -> [Observable a] -> IO ()
printObs ss fs = do
    putStrLn header
    showObs obs
    where
        obs = applyObs ss (gens fs)
        obsNames = names fs
        header = unwords ("time" : obsNames)

show' :: TObs -> IO ()
show' tobs = putStrLn $ showTObs tobs

showObs :: [TObs] -> IO ()
showObs = mapM_ show'

showTObs :: TObs -> String
showTObs (t, obss) = show t ++ " " ++ obssS
  where
    obssS = unwords (map show obss)

writeObs :: FilePath -> [Observable a] -> [State a] -> IO ()
writeObs fn fs ss =
    writeFile fn (unlines obsS)
    where
        obs = applyObs ss (gens fs)
        obsNames = names fs
        header = unwords ("time" : obsNames)
        obsS = header : map showTObs obs

run :: Eq a => Model a -> Int -> [Observable a] -> IO ()
run Model{rules = rs, initState = s} n obss = do
    rgen <- R.getStdGen
    let traj = take n (simulate rgen rs s)
    printObs traj obss

runW :: Eq a => Model a -> Int -> FilePath -> [Observable a] -> IO ()
runW Model{rules = rs, initState = s} n fn obss = do
    rgen <- R.getStdGen
    let traj = take n (simulate rgen rs s)
    writeObs fn obss traj

runT :: Eq a => Model a -> Time -> [Observable a] -> IO ()
runT Model{rules = rs, initState = s} t obss = do
    rgen <- R.getStdGen
    let traj = takeWhile (\s' -> getT s' < t) (simulate rgen rs s)
    printObs traj obss

runTW :: Eq a => Model a -> Time -> FilePath -> [Observable a] -> IO ()
runTW Model{rules = rs, initState = s} t fn obss = do
    rgen <- R.getStdGen
    let traj = takeWhile (\s' -> getT s' < t) (simulate rgen rs s)
    writeObs fn obss traj

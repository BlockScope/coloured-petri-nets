{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Chromar

--- Agents
data Agent = C
    { cid :: Int
    , nextTo :: Int
    , x :: Double
    } deriving (Eq, Show)

idx :: Agent -> Int
idx (C{cid=i, nextTo=_, x=_}) = i

isC :: Agent -> Bool
isC = const True

f, c :: Double -> Double
f = fromIntegral . floor
c = fromIntegral . ceiling

cv :: Multiset Agent -> Obs
cv m = sd $ map (\(el, _) -> x el) m

sd :: [Double] -> Obs
sd m =
    sqrt ((sum [(obs - mean)^^2 | obs <- m]) / n)
    where
        n = fromIntegral $ length m
        mean = (sum m) / n

g :: Double -> Double
g s' = gmax * (s' / (ks + s'))
  where
    gmax = 0.25
    ks = 2.5

s :: Fluent Double
s = repeatEvery 10.0 (between 0.0 5.0 (constant smax) (constant 0.0))
  where
    smax = 10.0

nc, conc1, total, var :: Observable Agent
nc =
    Observable
    { name = "ncells"
    , gen = aggregate ((+) . const 1.0) 0.0 . select isC
    }

conc1 =
    Observable
    { name = "conc1"
    , gen = aggregate ((+) . x) 0.0 . select (\at' -> idx at' == 1)
    }

total =
    Observable
    { name = "total"
    , gen = aggregate ((+) . x) 0.0 . select isC
    }

var =
    Observable
    { name = "var"
    , gen = cv . select isC
    }

$(return [])

--- Rules
df, df' :: [(Agent, Int)] -> p -> [Rxn Agent]

df = [rule|
       C{nextTo=p, x=x}, C{cid=p, x=x}  -->
       C{nextTo=p, x=x-1}, C{cid=p, x=x+1}  @x [x>0]
     |]

df' = [rule|
       C{cid=p, x=x}, C{nextTo=p, x=x}   -->
       C{cid=p, x=x-1}, C{nextTo=p, x=x+1} @x [x>0]
     |]

growth :: [(Agent, Int)] -> Time -> [Rxn Agent]
growth =
    [rule|
           C{nextTo=p, x=x} -->
           C{nextTo=round nc+1, x=f (x/2.0)}, C{cid=round nc+1, nextTo=p, x=c (x/2.0)} @(g s)
         |]

--- Initial state
s0 :: Multiset Agent
s0 = ms [ C{cid=1, nextTo=2, x=50.0},
          C{cid=2, nextTo=3, x=0.0},
          C{cid=3, nextTo=4, x=0.0},
          C{cid=4, nextTo=5, x=0.0},
          C{cid=5, nextTo=6, x=0.0},
          C{cid=6, nextTo=7, x=0.0},
          C{cid=7, nextTo=8, x=0.0},
          C{cid=8, nextTo=9, x=0.0},
          C{cid=9, nextTo=10, x=0.0},
          C{cid=10, nextTo=0, x=0.0} ]

model :: Model Agent
model = Model{ rules = [df, df', growth], initState = s0 }

main :: IO ()
main = runTW model 20.0 "out/out.txt" [nc, var, conc1]

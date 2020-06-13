{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

import Chromar

-- Agent declarations
data Agent = A { x :: Int } deriving (Eq, Show)

$(return [])

-- Rules
r1, r2 :: [(Agent, Int)] -> p -> [Rxn Agent]
r1 = [rule| A{x=x}, A{x=x} --> A{x=x+1}, A{x=x-1} @1.0 [x > 0] |]
r2 = [rule| A{x=x} --> A{x=x}, A{x=0} @1.0 [True] |]

--- Initial state
s :: Multiset Agent
s = ms [A{x=5}, A{x=5}]

model :: Model Agent
model = Model{ rules = [r1, r2], initState = s }

main :: IO ()
main = let nsteps = 100 in run model nsteps []

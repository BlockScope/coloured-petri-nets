module Chromar.Quotes where

import Prelude hiding (exp)
import Language.Haskell.TH (Q, Name, Exp(..), reify)
import Chromar.Info (isFluent, isObservable, mkFApp, mkObsApp)

tName :: Maybe Name -> Exp -> Q Exp
tName (Just nm) exp = do
    info <- reify nm
    if isFluent info
        then return $ mkFApp nm
        else return exp
tName Nothing exp = return exp

tNameObs :: Maybe Name -> Exp -> Q Exp
tNameObs (Just nm) e = do
    info <- reify nm
    if isObservable info
        then return $ mkObsApp nm
        else return e
tNameObs Nothing e = return e

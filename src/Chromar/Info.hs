module Chromar.Info (isFluent, isObservable, mkFApp, mkObsApp) where

import Data.List (isSuffixOf)
import Language.Haskell.TH (Name, Type(..), Info(..), Exp(..), mkName)

isFluent :: Info -> Bool
isFluent (VarI _m t _) =
    case t of
        (AppT (ConT tnm) _) -> "Fluent" `isSuffixOf` show tnm
        _ -> False
isFluent _ = False

isObservable :: Info -> Bool
isObservable (VarI _ t _) =
    case t of
        (AppT (ConT tnm) _) -> "Observable" `isSuffixOf` show tnm
        _ -> False
isObservable _ = False

mkFApp :: Name -> Exp
mkFApp nm =
    ParensE (AppE (AppE (VarE $ mkName "at") (VarE nm)) (VarE $ mkName "t"))

mkObsApp :: Name -> Exp
mkObsApp nm = ParensE (AppE obsFExp stateExp)
  where
    obsFExp = AppE (VarE $ mkName "gen") (VarE nm)
    stateExp = VarE $ mkName "s"


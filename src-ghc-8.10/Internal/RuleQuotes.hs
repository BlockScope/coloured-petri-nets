module Internal.RuleQuotes (tExp, tuplify, tuplify2) where

import Prelude hiding (exp)
import Data.Maybe (catMaybes)
import Language.Haskell.TH
    ( Q, Name, Stmt(..)
    , Exp(..), FieldExp
    , lookupValueName
    )
import Chromar.Quotes

--- there's probably a better way of doing this
tExp :: Exp -> Q Exp
tExp var@(VarE nm) = do
    mnm <- lookupValueName (show nm)
    e <- tName mnm var
    tNameObs mnm e
tExp (AppE e1 e2) = do
    te1 <- tExp e1
    te2 <- tExp e2
    return $ AppE te1 te2
tExp (TupE exps) = do
    texps <- mapM tExp $ catMaybes exps
    return . TupE $ Just <$> texps
tExp (ListE exps) = do
    texps <- mapM tExp exps
    return $ ListE texps
tExp (UInfixE e1 e2 e3) = do
    te1 <- tExp e1
    te2 <- tExp e2
    te3 <- tExp e3
    return $ UInfixE te1 te2 te3
tExp (ParensE e) = do
    te <- tExp e
    return $ ParensE te
tExp (LamE pats e) = do
    te <- tExp e
    return $ LamE pats te
tExp (CompE stmts) = do
    tstmts <- mapM tStmt stmts
    return $ CompE tstmts
tExp (InfixE me1 e me2) = do
    tme1 <- tMExp me1
    te <- tExp e
    tme2 <- tMExp me2
    return $ InfixE tme1 te tme2
tExp (LitE lit) = return $ LitE lit
tExp (ConE nm) = return $ ConE nm
tExp (RecConE nm fexps) = do
    tfexps <- mapM tFExp fexps
    return $ RecConE nm tfexps
tExp _ = undefined

tMExp :: Maybe Exp -> Q (Maybe Exp)
tMExp (Just e) = do
    te <- tExp e
    return (Just te)
tMExp Nothing = return Nothing

tFExp :: FieldExp -> Q FieldExp
tFExp (nm, exp) = do
    te <- tExp exp
    return (nm, te)

tStmt :: Stmt -> Q Stmt
tStmt (BindS p e) = do
    te <- tExp e
    return $ BindS p te
tStmt (NoBindS e) = do
    te <- tExp e
    return $ NoBindS te
tStmt LetS{} = error "Unexpected let statement"
tStmt ParS{} = error "Unexpected par statement"
tStmt RecS{} = error "Unexpected rec statement"

tuplify :: Name -> Exp -> Exp -> Exp
tuplify s lhs r = TupE [Just lhs, Just $ VarE s, Just r]

tuplify2 :: Exp -> Exp -> Exp
tuplify2 m ar = TupE [Just m, Just ar]

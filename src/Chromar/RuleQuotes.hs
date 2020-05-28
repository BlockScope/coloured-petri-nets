module Chromar.RuleQuotes where

import Prelude hiding (exp)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH
    ( Q, Name, Stmt(..), Dec(..), Body(..)
    , Pat(..), FieldPat
    , Exp(..), FieldExp
    , newName, mkName
    )
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (showName)
import Text.ParserCombinators.Parsec (parse)
import Chromar.MRuleParser (SRule(..), parseRule)
import Chromar.MAttrs (fillAttrs)
import Internal.RuleQuotes

type FieldProd = (FieldPat, [Exp], Set Name)

rule :: QuasiQuoter
rule =
    QuasiQuoter
    { quoteExp = ruleQuoter
    , quotePat = undefined
    , quoteDec = undefined
    , quoteType = undefined
    }

--- pure action
tFieldPat :: Set Name -> Name -> FieldExp -> FieldProd
tFieldPat names freshNm (nm, VarE pnm) =
    if Set.member pnm names
        then ( (nm, VarP freshNm)
             , [UInfixE (VarE freshNm) (VarE $ mkName "==") (VarE pnm)]
             , Set.empty)
        else ((nm, VarP pnm), [], Set.fromList [pnm])
tFieldPat _name freshNm (nm, exp) =
    ( (nm, VarP freshNm)
    , [UInfixE (VarE freshNm) (VarE $ mkName "==") exp]
    , Set.empty)

--- monadic action
qtFieldPat :: Set Name -> FieldExp -> Q FieldProd
qtFieldPat names fexp@(nm, _exp) = do
    fn <- newName (showName nm)
    return $ tFieldPat names fn fexp

mkGuardExp :: [[Exp]] -> Exp
mkGuardExp expss = AppE andFunc (ListE exps)
  where
    andFunc = VarE (mkName "and")
    exps = concat expss

mkAgentExps :: Q [FieldProd] -> Q ([FieldPat], Exp, Set Name)
mkAgentExps qfps = do
    fps <- qfps
    let (fpats, exprss, sets) = unzip3 fps
    let guardExp = mkGuardExp exprss
    let sn = Set.unions sets
    return (fpats, guardExp, sn)

mkPatStmt :: Name -> [FieldPat] -> Stmt
mkPatStmt nm fpats = BindS pat (VarE $ mkName "s")
  where
    pat = TupP [RecP nm fpats, WildP]

mkGuardStmt :: Exp -> Stmt
mkGuardStmt = NoBindS

mkAgentStmts :: Name -> Q ([FieldPat], Exp, Set Name) -> Q ([Stmt], Set Name)
mkAgentStmts nm qexps = do
    (fpats, gExp, sn) <- qexps
    let patStmt = mkPatStmt nm fpats
    let guardStmt = mkGuardStmt gExp
    return ([patStmt, guardStmt], sn)

tAgentPat :: Set Name -> Exp -> Q ([Stmt], Set Name)
tAgentPat sn (RecConE nm fexps) = mkAgentStmts nm qexps
  where
    qfps = mapM (qtFieldPat sn) fexps
    qexps = mkAgentExps qfps
tAgentPat _ _ = error "expected records"

mkLhsStmts :: Set Name -> [Stmt] -> [Exp] -> Q [Stmt]
mkLhsStmts _sn allStmts [] = return allStmts
mkLhsStmts sn allStmts (exp:exps) = do
    (stmts, sn') <- tAgentPat sn exp
    mkLhsStmts (Set.union sn sn') (allStmts ++ stmts) exps

mkLhs :: [Exp] -> Q [Stmt]
mkLhs = mkLhsStmts Set.empty []

tBody :: Body -> Q Body
tBody (NormalB exp) = do
  te <- tExp exp
  return (NormalB te)
tBody _ = error "expected NormalB constr"

tDec :: Dec -> Q Dec
tDec (ValD p bd xs) = do
  tbd <- tBody bd
  return (ValD p tbd xs)
tDec _ = error "expected ValD constr"

mkActExp :: Name -> Exp -> Exp -> Exp
mkActExp s lhs r =
    AppE (VarE $ mkName "fullRate") args
    where
        args = tuplify s lhs r

mkReturnStmt :: Exp -> Stmt
mkReturnStmt = NoBindS

mkRxnExp :: Name -> SRule -> Exp
mkRxnExp s r =
    RecConE (mkName "Rxn") fields
    where
        lhsSym = mkName "lhs"
        rhsSym = mkName "rhs"
        rateSym = mkName "rate"
        actSym = mkName "act"
        mrexps =
            AppE
                (VarE $ mkName "nrepl")
                (tuplify2 (ListE $ mults r) (ListE $ rexps r))
        lexps' = AppE (VarE $ mkName "ms") (ListE $ lexps r)
        rexps' = AppE (VarE $ mkName "ms") (ParensE mrexps)
        rateExp = srate r
        actExp = mkActExp s lexps' (srate r)
        fields =
            [ (lhsSym, lexps')
            , (rhsSym, rexps')
            , (rateSym, rateExp)
            , (actSym, actExp)
            ]

mkCompStmts :: Name -> SRule -> Q [Stmt]
mkCompStmts s r = do
    let rxnExp = mkRxnExp s r
    let retStmt = mkReturnStmt rxnExp
    let guardStmt = NoBindS (cond r)
    let letStmt = LetS (decs r)
    patStmts <- mkLhs (lexps r)
    return $ patStmts ++ [letStmt, guardStmt, retStmt]

ruleQuoter' :: SRule -> Q Exp
ruleQuoter' r = do
    state <- newName "s"
    time <- newName "t"
    stmts <- mkCompStmts state r
    return $ LamE [VarP state, VarP time] (CompE stmts)

fluentTransform :: SRule -> Q SRule
fluentTransform
    SRule
        { lexps = les
        , rexps = res
        , mults = m
        , srate = r
        , cond = c
        , decs = ds
        } = do
    re <- tExp r
    ce <- tExp c
    tres <- mapM tExp res
    tds <- mapM tDec ds
    return
        SRule
            { lexps = les
            , rexps = tres
            , mults = m
            , srate = re
            , cond = ce
            , decs = tds
            }

ruleQuoter :: String -> Q Exp
ruleQuoter s =
    case parse parseRule "" s of
        Left err -> error (show err)
        Right r -> do
            sr <- fluentTransform r
            sr' <- fillAttrs sr
            ruleQuoter' sr'

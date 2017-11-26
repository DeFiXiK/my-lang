module Syntax
    ( Identifier
    , Type (..)
    , NumberLiteral (..)
    , BoolLiteral (..)
    , Multiplier (..)
    , MultOperation (..)
    , Multiplication (..)
    , SumOperation (..)
    , Summation (..)
    , LogOperation (..)
    , Expression (..)
    , Statement (..)
    , Block (..)
    , Program (..)
    , parse
    , ParserError (..)
    ) where

import           Control.Monad (when)
import           Parcomb       (Parser (..), ParserError (..), consume, expect,
                                expectAny, lookahead, lookahead2, many1sep,
                                parserError, skip, tryParse)
import qualified Parcomb
import           Tokens        (Token (..), TokenType (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

type Identifier = String

identifier :: Parser Identifier
identifier = do
    tt <- lookahead
    case tt of
        TokIdent s -> do { consume; return s }
        _          -> parserError "Ожидался идентификатор"

data Type
    = TypeInt
    | TypeReal
    | TypeBool
    deriving (Show)

typeFromToken :: TokenType -> Type
typeFromToken TokInt = TypeInt
typeFromToken TokFloat = TypeReal
typeFromToken TokBool = TypeBool
typeFromToken tt = error $ "Токен " ++ show tt ++ " не соответствует ни одному типу"


declaration :: Parser ([Identifier], Type)
declaration = do
    idents <- many1sep (expect TokComma) identifier
    expect TokColon
    typeTok <- expectAny [TokInt, TokFloat, TokBool]
    return (idents, typeFromToken typeTok)

data NumberLiteral
    = NumInteger Int Int
    | NumReal Double
    deriving (Show)

numberLiteral :: Parser NumberLiteral
numberLiteral = do
    tt <- lookahead
    case tt of
        TokInteger int base -> do
            skip
            return $ NumInteger int base
        TokReal dbl -> do
            skip
            return $ NumReal dbl
        _ -> parserError $ "Ожидалось число, получено " ++ show tt

data BoolLiteral = BoolTrue | BoolFalse deriving (Show)

boolLiteral :: Parser BoolLiteral
boolLiteral = do
    tt <- lookahead
    case tt of
        TokTrue -> do
            skip
            return BoolTrue
        TokFalse -> do
            skip
            return BoolFalse
        _ -> parserError $ "Ожидалось логическое значение, получено " ++ show tt

data Multiplier
    = MultIdent Identifier
    | MultNumber NumberLiteral
    | MultBool BoolLiteral
    | MultNot Multiplier
    | MultGrouped Expression
    deriving (Show)

multiplier :: Parser Multiplier
multiplier = do
    tt <- lookahead
    case tt of
        TokIdent s -> do
            skip
            return $ MultIdent s
        TokInteger _ _ ->
            fmap MultNumber numberLiteral
        TokReal _ ->
            fmap MultNumber numberLiteral
        TokTrue ->
            fmap MultBool boolLiteral
        TokFalse ->
            fmap MultBool boolLiteral
        TokNot -> do
            skip
            fmap MultNot multiplier
        TokSelOpen -> do
            skip
            expr <- expression
            expect TokSelClose
            return $ MultGrouped expr
        _ -> parserError $ "Ожидалось выражение, получено " ++ show tt

data MultOperation
    = MultOpMult -- *
    | MultOpDiv  -- /
    | MultOpAnd  -- and
    deriving (Show)

multOperation :: Parser MultOperation
multOperation = do
    tt <- expectAny [TokMult, TokDiv, TokAnd]
    case tt of
        TokMult -> return MultOpMult
        TokDiv  -> return MultOpDiv
        TokAnd  -> return MultOpAnd

data Multiplication
    = Multiplication Multiplier [(MultOperation, Multiplier)]
    deriving (Show)

multiplication :: Parser Multiplication
multiplication = do
    m <- multiplier
    ms <- multiplication'
    return $ Multiplication m ms

multiplication' :: Parser [(MultOperation, Multiplier)]
multiplication' = do
    res <- tryParse multOperation
    case res of
        Left _ -> return []
        Right op -> do
            mult <- multiplier
            pairs <- multiplication'
            return $ (op, mult):pairs

data SumOperation
    = SumPlus
    | SumMinus
    | SumOr
    deriving (Show)

sumOperation :: Parser SumOperation
sumOperation = do
    tt <- expectAny [TokPlus, TokMinus, TokOr]
    case tt of
        TokPlus  -> return SumPlus
        TokMinus -> return SumMinus
        TokOr    -> return SumOr

data Summation
    = Summation Multiplication [(SumOperation, Multiplication)]
    deriving (Show)

summation :: Parser Summation
summation = do
    m <- multiplication
    ms <- summation'
    return $ Summation m ms

summation' :: Parser [(SumOperation, Multiplication)]
summation' = do
    res <- tryParse sumOperation
    case res of
        Left _ -> return []
        Right op -> do
            mult <- multiplication
            pairs <- summation'
            return $ (op, mult):pairs

data LogOperation
    = LogNeq
    | LogEq
    | LogLt
    | LogLte
    | LogGt
    | LogGte
    deriving (Show)

logOperation :: Parser LogOperation
logOperation = do
    tt <- expectAny [TokNotEqual, TokEqual, TokLess, TokLessOrEqual, TokGreater, TokGreaterOrEqual]
    case tt of
        TokNotEqual       -> return LogNeq
        TokEqual          -> return LogEq
        TokLess           -> return LogLt
        TokLessOrEqual    -> return LogLte
        TokGreater        -> return LogGt
        TokGreaterOrEqual -> return LogGte

data Expression
    = Expression Summation [(LogOperation, Summation)]
    deriving (Show)

expression :: Parser Expression
expression = do
    sm <- summation
    sms <- expression'
    return $ Expression sm sms

expression' :: Parser [(LogOperation, Summation)]
expression' = do
    res <- tryParse logOperation
    case res of
        Left _ -> return []
        Right op -> do
            sm <- summation
            pairs <- expression'
            return $ (op, sm):pairs

data Statement
    = StmtCompound [Statement]
    | StmtAssignment Identifier Expression
    | StmtCondition Expression Statement (Maybe Statement)
    | StmtForLoop Identifier Expression Expression (Maybe Expression) Statement
    | StmtWhileLoop Expression Statement
    | StmtRead [Identifier]
    | StmtWrite [Expression]
    deriving (Show)

compound' :: Parser [Statement]
compound' = do
    stmt <- statement
    expect TokSemicolon
    tt <- lookahead
    case tt of
        TokCompoundStatementEnd -> do
            skip
            return [stmt]
        _ -> do
            stmts <- compound'
            return (stmt:stmts)

compound :: Parser [Statement]
compound = do
    expect TokCompoundStatementBegin
    compound'

assignment :: Parser (Identifier, Expression)
assignment = do
    ident <- identifier
    expect TokAssignment
    expr <- expression
    return (ident, expr)

condition :: Parser (Expression, Statement, Maybe Statement)
condition = do
    expect TokIf
    expect TokSelOpen
    expr <- expression
    expect TokSelClose
    thenBranch <- statement
    tt <- lookahead
    case tt of
        TokElse -> do
            skip
            elseBranch <- statement
            return (expr, thenBranch, Just elseBranch)
        _ -> return (expr, thenBranch, Nothing)

forLoop :: Parser (Identifier, Expression, Expression, Maybe Expression, Statement)
forLoop = do
    expect TokParametrizedCycleFor
    (ident, initExpr) <- assignment
    expect TokParametrizedCycleTo
    targetExpr <- expression
    tt <- lookahead
    maybeStepExp <- case tt of
        TokParametrizedCycleStep -> do
            skip
            stepExp <- expression
            return (Just stepExp)
        _ -> return Nothing
    body <- statement
    expect TokParametrizedCycleNext
    return (ident, initExpr, targetExpr, maybeStepExp, body)

whileLoop :: Parser (Expression, Statement)
whileLoop = do
    expect TokCondLoop
    expect TokSelOpen
    expr <- expression
    expect TokSelClose
    body <- statement
    return (expr, body)

readStmt :: Parser [Identifier]
readStmt = do
    expect TokInput
    idents <- many1sep (expect TokComma) identifier
    return idents

writeStmt' :: Parser [Expression]
writeStmt' = do
    tt <- lookahead
    case tt of
        TokComma -> do
            expr <- expression
            exprs <- writeStmt'
            return (expr:exprs)
        _ ->
            return []

writeStmt :: Parser [Expression]
writeStmt = do
    expect TokOutput
    expr <- expression
    exprs <- writeStmt'
    return (expr:exprs)

statement :: Parser Statement
statement = do
    tt <- lookahead
    case tt of
        TokCompoundStatementBegin -> do
            stmts <- compound
            return $ StmtCompound stmts
        TokIdent _ -> do
            (ident, expr) <- assignment
            return $ StmtAssignment ident expr
        TokIf -> do
            (expr, thenBranch, maybeElseBranch) <- condition
            return $ StmtCondition expr thenBranch maybeElseBranch
        TokParametrizedCycleFor -> do
            (ident, initExpr, targetExpr, maybeStepExp, body) <- forLoop
            return $ StmtForLoop ident initExpr targetExpr maybeStepExp body
        TokCondLoop -> do
            (expr, body) <- whileLoop
            return $ StmtWhileLoop expr body
        TokInput -> do
            idents <- readStmt
            return $ StmtRead idents
        TokOutput -> do
            exprs <- writeStmt
            return $ StmtWrite exprs
        _ -> parserError $ "Ожидался оператор, получено " ++ show tt


data Block
    = BlockDecl [Identifier] Type
    | BlockStmt Statement
    deriving (Show)

block :: Parser Block
block = do
    (tt1, tt2) <- lookahead2
    case (tt1, tt2) of
        (TokIdent _, TokComma) -> do
            (idents, type_) <- declaration
            return $ BlockDecl idents type_
        (TokIdent _, TokColon) -> do
            (idents, type_) <- declaration
            return $ BlockDecl idents type_
        (TokIdent _, TokAssignment) -> do
            stmt <- statement
            return $ BlockStmt stmt
        (TokIdent _, _) -> do
            parserError $ "Ожидался один из операторов: [" ++ show TokComma ++ ", " ++ show TokColon ++ ", " ++ show TokAssignment ++ "], получено " ++ show tt2
        _ -> do
            stmt <- statement
            return $ BlockStmt stmt

newtype Program = Program [Block]
    deriving (Show)

blocks :: Parser [Block]
blocks = do
    tt <- lookahead
    if tt == TokEndProgramm
        then return []
        else do
            b <- block
            expect TokSemicolon
            bs <- blocks
            return $ b:bs

program :: Parser Program
program = do
    expect TokStartProgramm
    bs <- blocks
    expect TokEndProgramm
    return $ Program bs

parse :: [Token] -> Either ParserError Program
parse = Parcomb.parse program

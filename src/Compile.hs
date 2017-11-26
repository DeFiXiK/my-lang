{-# LANGUAGE DeriveFunctor #-}
module Compile
    ( Error (..)
    , compile
    ) where

import           Control.Monad (foldM, forM_, mapM, when)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import qualified Exec          as Exec
import qualified Syntax        as Syn
import           Table         (Table)
import qualified Table         as Tbl

data Error = Error
    {
        errorMsg    :: String
    }

data Type = TypeInt | TypeReal | TypeBool
    deriving (Show, Eq)

typeOf :: Exec.Value -> Type
typeOf (Exec.ValInt _)  = TypeInt
typeOf (Exec.ValReal _) = TypeReal
typeOf (Exec.ValBool _) = TypeBool

defaultOf :: Type -> Exec.Value
defaultOf TypeInt  = Exec.ValInt 0
defaultOf TypeReal = Exec.ValReal 0.0
defaultOf TypeBool = Exec.ValBool False

fromSynType :: Syn.Type -> Type
fromSynType Syn.TypeInt  = TypeInt
fromSynType Syn.TypeReal = TypeReal
fromSynType Syn.TypeBool = TypeBool

data State = State
    { stInstr     :: Table Exec.Instruction
    , stTmpVarCnt :: Integer
    , stTypeMap   :: Map String Type
    }

newState :: State
newState = State
    { stInstr = Tbl.empty
    , stTmpVarCnt = 0
    , stTypeMap = M.empty
    }

newtype Compiler a = Compiler (State -> Either Error (a, State))
    deriving (Functor)

runCompiler :: Compiler a -> State -> Either Error (a, State)
runCompiler (Compiler f) = f

instance Applicative Compiler where
    pure x = Compiler (\s -> Right (x, s))
    cf <*> cx = Compiler (\s1 -> do
        (f, s2) <- runCompiler cf s1
        (x, s3) <- runCompiler cx s2
        return (f x, s3))

instance Monad Compiler where
    return = pure
    (Compiler cf) >>= f = Compiler (\st -> do
        (val, st2) <- cf st
        let Compiler cf2 = f val
        cf2 st2)

cerror :: String -> Compiler ()
cerror msg = Compiler (\_ -> Left $ Error msg)

ins :: Exec.Instruction -> Compiler Int
ins i = Compiler (\(State is cnt tm) ->
    let (ind, is2) = Tbl.append is i in
    Right (ind, State is2 cnt tm))

setins :: Int -> Exec.Instruction -> Compiler ()
setins ind ins = Compiler (\(State is cnt tm) ->
    Right ((), State (Tbl.set is ind ins) cnt tm))

tmpcnt :: Compiler Integer
tmpcnt = Compiler (\st@(State _ cnt _) -> Right (cnt, st))

inctmpcnt :: Compiler ()
inctmpcnt = Compiler (\(State is tc tm) -> Right ((), State is (tc + 1) tm))

isdefined :: String -> Compiler Bool
isdefined varname = Compiler (\st@(State _ _ tm) ->
    case M.lookup varname tm of
        Just _  -> Right (True, st)
        Nothing -> Right (False, st))

gettype :: String -> Compiler Type
gettype varname  = Compiler (\st@(State _ _ tm) ->
    case M.lookup varname tm of
        Just t  -> Right (t, st)
        Nothing -> Left $ Error ("Undefined variable '" ++ varname ++ "'"))

settype :: String -> Type -> Compiler ()
settype varname tp = Compiler (\(State is tc tm) ->
    Right ((), State is tc (M.insert varname tp tm)))

tmpvar :: Exec.Value -> Compiler String
tmpvar val = do
    tmpcnt <- tmpcnt
    let varname = "$t" ++ show (tmpcnt + 1)
    ins $ Exec.IDecl varname val
    inctmpcnt
    settype varname (typeOf val)
    return varname

compileMultiplier :: Syn.Multiplier -> Compiler (String, Type)
compileMultiplier  (Syn.MultIdent ident) = do
    tp <- gettype ident
    return (ident, tp)
compileMultiplier (Syn.MultNumber num) = do
    case num of
        Syn.NumInteger int _ -> do
            varname <- tmpvar (Exec.ValInt (fromIntegral int))
            return (varname, TypeInt)
        Syn.NumReal dbl -> do
            varname <- tmpvar  (Exec.ValReal dbl)
            return (varname, TypeReal)
compileMultiplier (Syn.MultBool bool) = do
    let b = case bool of
            Syn.BoolTrue  -> True
            Syn.BoolFalse -> False
    varname <- tmpvar (Exec.ValBool b)
    return (varname, TypeBool)
compileMultiplier (Syn.MultNot mulr) = do
    (varname, tp) <- compileMultiplier mulr
    when (tp /= TypeBool) $
        cerror $ "Can't apply 'not' operator to a value of type " ++ show tp
    outname <- tmpvar (defaultOf TypeBool)
    ins (Exec.INot varname outname)
    return (outname, TypeBool)
compileMultiplier (Syn.MultGrouped expr) =
    compileExpression expr

compileMultiplication :: Syn.Multiplication -> Compiler (String, Type)
compileMultiplication (Syn.Multiplication mulr oppairs) = do
    (var1, typ) <- compileMultiplier mulr

    if null oppairs
    then
        return (var1, typ)
    else do
        outvar <- tmpvar $ defaultOf typ
        ins (Exec.IMov var1 outvar)
        forM_ oppairs (\(op, mul) -> do
            (varx, typx) <- compileMultiplier mul
            case (op, typ, typx) of
                (Syn.MultOpMult, TypeInt, TypeInt) -> do
                    ins $ Exec.IMulI outvar varx outvar
                    return ()
                (Syn.MultOpMult, TypeReal, TypeReal) -> do
                    ins $ Exec.IMulF outvar varx outvar
                    return ()
                (Syn.MultOpMult, _, _) ->
                    cerror $ "Cannot multiply values of type " ++ show typ ++ " and " ++ show typx

                (Syn.MultOpDiv, TypeInt, TypeInt) -> do
                    ins $ Exec.IDivI outvar varx outvar
                    return ()
                (Syn.MultOpDiv, TypeReal, TypeReal) -> do
                    ins $ Exec.IDivF outvar varx outvar
                    return ()
                (Syn.MultOpDiv, _, _) ->
                    cerror $ "Cannot divide values of type " ++ show typ ++ " and " ++ show typx

                (Syn.MultOpAnd, TypeBool, TypeBool) -> do
                    ins (Exec.IAnd outvar varx outvar)
                    return ()
                (Syn.MultOpAnd, _, _) ->
                    cerror $ "Cannot apply AND operator to values of type " ++ show typ ++ " and " ++ show typx)

        return (outvar, typ)

compileSummation :: Syn.Summation -> Compiler (String, Type)
compileSummation (Syn.Summation muln oppairs) = do
    (var1, typ) <- compileMultiplication muln

    if null oppairs
    then
        return (var1, typ)
    else do
        outvar <- tmpvar $ defaultOf typ
        ins (Exec.IMov var1 outvar)
        forM_ oppairs (\(op, mul) -> do
            (varx, typx) <- compileMultiplication mul
            case (op, typ, typx) of
                (Syn.SumPlus, TypeInt, TypeInt) -> do
                    ins $ Exec.IAddI outvar varx outvar
                    return ()
                (Syn.SumPlus, TypeReal, TypeReal) -> do
                    ins $ Exec.IAddF outvar varx outvar
                    return ()
                (Syn.SumPlus, _, _) ->
                    cerror $ "Cannot add values of type " ++ show typ ++ " and " ++ show typx

                (Syn.SumMinus, TypeInt, TypeInt) -> do
                    ins $ Exec.ISubI outvar varx outvar
                    return ()
                (Syn.SumMinus, TypeReal, TypeReal) -> do
                    ins $ Exec.ISubF outvar varx outvar
                    return ()
                (Syn.SumMinus, _, _) ->
                    cerror $ "Cannot subtract values of type " ++ show typ ++ " and " ++ show typx

                (Syn.SumOr, TypeBool, TypeBool) -> do
                    ins (Exec.IOr outvar varx outvar)
                    return ()
                (Syn.SumOr, _, _) ->
                    cerror $ "Cannot apply OR operator to values of type " ++ show typ ++ " and " ++ show typx)

        return (outvar, typ)

compileLogOp :: (String, Type) -> Syn.LogOperation -> (String, Type) -> Compiler String
compileLogOp (var1, typ1) op (var2, typ2) = do
    outvar <- tmpvar $ defaultOf TypeBool
    case (op, typ1, typ2) of
        (Syn.LogNeq, TypeInt, TypeInt) -> do
            ins $ Exec.INeqI var1 var2 outvar
            return ()
        (Syn.LogNeq, TypeReal, TypeReal) -> do
            ins $ Exec.INeqF var1 var2 outvar
            return ()
        (Syn.LogNeq, TypeBool, TypeBool) -> do
            ins $ Exec.INeqB var1 var2 outvar
            return ()

        (Syn.LogEq, TypeInt, TypeInt) -> do
            ins $ Exec.IEqI var1 var2 outvar
            return ()
        (Syn.LogEq, TypeReal, TypeReal) -> do
            ins $ Exec.IEqF var1 var2 outvar
            return ()
        (Syn.LogEq, TypeBool, TypeBool) -> do
            ins $ Exec.IEqB var1 var2 outvar
            return ()

        (Syn.LogLt, TypeInt, TypeInt) -> do
            ins $ Exec.ILtI var1 var2 outvar
            return ()
        (Syn.LogLt, TypeReal, TypeReal) -> do
            ins $ Exec.ILtF var1 var2 outvar
            return ()
        (Syn.LogLt, TypeBool, TypeBool) -> do
            ins $ Exec.ILtB var1 var2 outvar
            return ()

        (Syn.LogLte, TypeInt, TypeInt) -> do
            ins $ Exec.ILteI var1 var2 outvar
            return ()
        (Syn.LogLte, TypeReal, TypeReal) -> do
            ins $ Exec.ILteF var1 var2 outvar
            return ()
        (Syn.LogLte, TypeBool, TypeBool) -> do
            ins $ Exec.ILteB var1 var2 outvar
            return ()

        (Syn.LogGt, TypeInt, TypeInt) -> do
            ins $ Exec.IGtI var1 var2 outvar
            return ()
        (Syn.LogGt, TypeReal, TypeReal) -> do
            ins $ Exec.IGtF var1 var2 outvar
            return ()
        (Syn.LogGt, TypeBool, TypeBool) -> do
            ins $ Exec.IGtB var1 var2 outvar
            return ()

        (Syn.LogGte, TypeInt, TypeInt) -> do
            ins $ Exec.IGteI var1 var2 outvar
            return ()
        (Syn.LogGte, TypeReal, TypeReal) -> do
            ins $ Exec.IGteF var1 var2 outvar
            return ()
        (Syn.LogGte, TypeBool, TypeBool) -> do
            ins $ Exec.IGteB var1 var2 outvar
            return ()

        (_, _, _) ->
            cerror $ "Cannot compare values of type " ++ show typ1 ++ " and " ++ show typ2

    return outvar

compileExpression :: Syn.Expression -> Compiler (String, Type)
compileExpression (Syn.Expression sumn oppairs) = do
    (var1, typ1) <- compileSummation sumn
    case oppairs of
        [] ->
            return (var1, typ1)
        [(nopx, sumnx)] -> do
            (var2, typ2) <- compileSummation sumnx
            outvar <- compileLogOp (var1, typ1) nopx (var2, typ2)
            return (outvar, TypeBool)
        _ -> do
            (logPairs, _, _) <- foldM (\(pairs, lastvar, lasttyp) (nopx, sumnx) -> do
                (varx, typx) <- compileSummation sumnx
                let newPairs = ((lastvar, lasttyp), nopx, (varx, typx)):pairs
                return (newPairs, varx, typx)) ([], var1, typ1) oppairs

            pairVars <- mapM (\(vt1, op, vt2) -> compileLogOp vt1 op vt2) logPairs

            outvar <- tmpvar $ Exec.ValBool True
            forM_ pairVars (\var -> ins $ Exec.IAnd outvar var outvar)

            return (outvar, TypeBool)

compileStatement :: Syn.Statement -> Compiler Int
compileStatement (Syn.StmtCompound stmts) = do
    case stmts of
        [] ->
            ins  Exec.INop
        s:ss -> do
            ind <- compileStatement s
            forM_ ss compileStatement
            return ind

compileStatement (Syn.StmtAssignment ident expr)= do
    vtyp <- gettype ident
    ind <- ins Exec.INop
    (evar, etyp) <- compileExpression expr
    when (etyp /= vtyp) $
        cerror $ "Cannot assign value of typ " ++ show etyp ++ " to a variable of type " ++ show vtyp
    ins $ Exec.IMov evar ident
    return ind

compileStatement (Syn.StmtCondition cond thenB mElseB) = do
    firstInd <- ins  Exec.INop
    (cvar, ctype) <- compileExpression cond
    when (ctype /= TypeBool) $
        cerror "IF operator's condition must be of type Bool"

    notCvar <- tmpvar  (defaultOf TypeBool)
    ins $ Exec.INot cvar notCvar

    jmpToElseInd <- ins  $ Exec.IJmpC notCvar 0

    compileStatement thenB
    jmpToEndInd <- ins $ Exec.IJmp 0

    case mElseB of
        Nothing -> do
            endInd <- ins Exec.INop
            setins jmpToElseInd $ Exec.IJmpC notCvar endInd
            setins jmpToEndInd Exec.INop
            return firstInd
        Just elseB -> do
            elseInd <- compileStatement elseB
            endInd <- ins  Exec.INop
            setins jmpToElseInd $ Exec.IJmpC notCvar elseInd
            setins jmpToEndInd $ Exec.IJmp endInd
            return firstInd

compileStatement (Syn.StmtForLoop ident initv finv maybeStepv body) = do
    typ <- gettype ident
    when (typ /= TypeInt) $
        cerror "FOR loop's parameter must be of integer type"

    firstInd <- ins Exec.INop

    (initVar, initTyp) <- compileExpression initv
    when (initTyp /= TypeInt) $
        cerror "FOR loop's initial value must be of integer type"

    (finVar, finTyp) <- compileExpression finv
    when (finTyp /= TypeInt) $
        cerror "FOR loop's final value must be of integer type"

    -- Initial assignment
    ins $ Exec.IMov initVar ident

    -- Increment step
    incStep <- case maybeStepv of
        Nothing -> tmpvar $ Exec.ValInt 1
        Just stepv -> do
            (stepVar, stepTyp) <- compileExpression stepv
            when (stepTyp /= TypeInt) $
                cerror "FOR loop's step value must be of integer type"
            return stepVar

    -- Condition check
    condVar <- tmpvar $ defaultOf TypeBool
    bodyStartInd <- ins $ Exec.IGteI ident finVar condVar
    jmpEndInd <- ins $ Exec.IJmpC condVar 0

    compileStatement body
    ins $ Exec.IAddI ident incStep ident
    ins $ Exec.IJmp bodyStartInd

    endInd <- ins Exec.INop
    setins jmpEndInd $ Exec.IJmpC condVar endInd

    return firstInd

compileStatement (Syn.StmtWhileLoop cond body) = do
    startInd <- ins Exec.INop
    (condVar, condTyp) <- compileExpression cond
    when (condTyp /= TypeBool) $
        cerror "WHILE loops's condition must of type Bool"

    notCondVar <- tmpvar $ defaultOf TypeBool
    ins $ Exec.INot condVar notCondVar
    jmpInd <- ins $ Exec.IJmpC notCondVar 0

    compileStatement body
    ins $ Exec.IJmp startInd
    endInd <- ins Exec.INop

    setins jmpInd $ Exec.IJmpC notCondVar endInd

    return startInd

compileStatement (Syn.StmtRead idents)= do
    startInd <- ins Exec.INop
    forM_ idents (\ident -> do
        typ <- gettype ident
        case typ of
            TypeInt  -> ins $ Exec.IReadI ident
            TypeReal -> ins $ Exec.IReadF ident
            TypeBool -> ins $ Exec.IReadB ident
        return ())
    return startInd

compileStatement (Syn.StmtWrite exprs) = do
    startInd <- ins Exec.INop
    forM_ exprs (\expr -> do
        (var, typ) <- compileExpression expr
        case typ of
            TypeInt  -> ins $ Exec.IWriteI var
            TypeReal -> ins $ Exec.IWriteF var
            TypeBool -> ins $ Exec.IWriteB var
        return ())
    return startInd

compileBlock :: Syn.Block -> Compiler ()
compileBlock (Syn.BlockDecl idents styp) = do
    let typ = fromSynType styp
    forM_ idents (\ident -> do
        isdef <- isdefined ident
        when isdef $
            cerror $ "Variable " ++ show ident ++ " is already defined"
        _ <- ins $ Exec.IDecl ident (defaultOf typ)
        settype ident typ
        return ())
compileBlock (Syn.BlockStmt stmt) = do
    _ <- compileStatement stmt
    return ()

compileProgram :: Syn.Program -> Compiler ()
compileProgram (Syn.Program blocks) =
    forM_ blocks compileBlock

compile :: Syn.Program -> Either Error (Table Exec.Instruction)
compile prg =
    case runCompiler (compileProgram prg) newState of
        Left err      -> Left err
        Right (_, st) -> Right $ stInstr st

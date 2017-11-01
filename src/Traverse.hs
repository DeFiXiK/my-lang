{-# LANGUAGE FlexibleInstances #-}

module Traverse
    ( ASTTraversible (..)
    ) where

import           Control.Monad (forM_)
import qualified Syntax        as Syn

class ASTTraversible n where
    asttraverse :: (Monad m) => (String -> a -> m a) -> n -> a -> m ()

instance ASTTraversible Syn.Identifier where
    asttraverse f node acc = do
        _ <- f node acc
        return ()

instance ASTTraversible Syn.Type where
    asttraverse f Syn.TypeInt acc = do
        _ <- f "Целое число" acc
        return ()
    asttraverse f Syn.TypeReal acc = do
        _ <- f "Дробное число" acc
        return ()
    asttraverse f Syn.TypeBool acc = do
        _ <- f "Логический тип" acc
        return ()

instance ASTTraversible Syn.NumberLiteral where
    asttraverse f (Syn.NumInteger value base) acc = do
        _ <- f ("Целое число " ++ show value ++ " с основанием " ++ show base) acc
        return ()
    asttraverse f (Syn.NumReal value) acc = do
        _ <- f ("Дробное число " ++ show value) acc
        return ()

instance ASTTraversible Syn.BoolLiteral where
    asttraverse f Syn.BoolTrue acc = do
        _ <- f "Истина" acc
        return ()
    asttraverse f Syn.BoolFalse acc = do
        _ <- f "Ложь" acc
        return ()

instance ASTTraversible Syn.Multiplier where
    asttraverse f node acc =
        case node of
            Syn.MultIdent ident   -> asttraverse f ident acc
            Syn.MultNumber numlit -> asttraverse f numlit acc
            Syn.MultBool boollit  -> asttraverse f boollit acc
            Syn.MultNot mulr  -> do
                ownAcc <- f "Не" acc
                asttraverse f mulr ownAcc
            Syn.MultGrouped expr -> asttraverse f expr acc

instance ASTTraversible Syn.MultOperation where
    asttraverse f Syn.MultOpMult acc = do
        _ <- f "Умножение" acc
        return ()
    asttraverse f Syn.MultOpDiv acc = do
        _ <- f "Деление" acc
        return ()
    asttraverse f Syn.MultOpAnd acc = do
        _ <- f "И" acc
        return ()

instance ASTTraversible Syn.Multiplication where
    asttraverse f (Syn.Multiplication mulr pairs) acc = do
        ownAcc <- f "Слагаемое" acc
        asttraverse f mulr ownAcc
        forM_ pairs (\(op, othMulr) -> do
            asttraverse f op ownAcc
            asttraverse f othMulr ownAcc)

instance ASTTraversible Syn.SumOperation where
    asttraverse f Syn.SumPlus acc = do
        _ <- f "Плюс" acc
        return ()
    asttraverse f Syn.SumMinus acc = do
        _ <- f "Минус" acc
        return ()
    asttraverse f Syn.SumOr acc = do
        _ <- f "Или" acc
        return ()

instance ASTTraversible Syn.Summation where
    asttraverse f (Syn.Summation muln pairs) acc = do
        ownAcc <- f "Операнд" acc
        asttraverse f muln ownAcc
        forM_ pairs (\(op, othMuln) -> do
            asttraverse f op ownAcc
            asttraverse f othMuln ownAcc)

instance ASTTraversible Syn.LogOperation where
    asttraverse f Syn.LogNeq acc = do
        _ <- f "Не равно" acc
        return ()
    asttraverse f Syn.LogEq acc = do
        _ <- f "Равно" acc
        return ()
    asttraverse f Syn.LogLt acc = do
        _ <- f "Меньше" acc
        return ()
    asttraverse f Syn.LogLte acc = do
        _ <- f "Меньше или равно" acc
        return ()
    asttraverse f Syn.LogGt acc = do
        _ <- f "Больше" acc
        return ()
    asttraverse f Syn.LogGte acc = do
        _ <- f "Больше или равно" acc
        return ()


instance ASTTraversible Syn.Expression where
    asttraverse f (Syn.Expression sumn pairs) acc = do
        ownAcc <- f "Выражение" acc
        asttraverse f sumn ownAcc
        forM_ pairs (\(op, othSumn) -> do
            asttraverse f op ownAcc
            asttraverse f othSumn ownAcc)

instance ASTTraversible Syn.Statement where
    asttraverse f (Syn.StmtCompound stmts) acc = do
        ownAcc <- f "Составной оператор" acc
        forM_ stmts (\stmt -> asttraverse f stmt ownAcc)
    asttraverse f (Syn.StmtAssignment ident expr) acc = do
        ownAcc <- f "Присваивание" acc
        asttraverse f ident ownAcc
        asttraverse f expr ownAcc
    asttraverse f (Syn.StmtCondition cond thenBranch mElseBranch) acc = do
        ownAcc <- f "Условный оператор" acc
        asttraverse f cond ownAcc
        asttraverse f thenBranch ownAcc
        case mElseBranch of
            Just elseBranch -> asttraverse f elseBranch ownAcc
            Nothing         -> return ()
    asttraverse f (Syn.StmtForLoop ident from to maybeStep body) acc = do
        ownAcc <- f "Параметрический цикл" acc
        asttraverse f ident ownAcc
        asttraverse f from ownAcc
        asttraverse f to ownAcc
        case maybeStep of
            Just step ->
                asttraverse f step ownAcc
            Nothing ->
                return ()
        asttraverse f body ownAcc
    asttraverse f (Syn.StmtWhileLoop cond body) acc = do
        ownAcc <- f "Цикл с условием" acc
        asttraverse f cond ownAcc
        asttraverse f body ownAcc
    asttraverse f (Syn.StmtRead idents) acc = do
        ownAcc <- f "Оператор ввода" acc
        forM_ idents (\ident -> asttraverse f ident ownAcc)
    asttraverse f (Syn.StmtWrite exprs) acc = do
        ownAcc <- f "Оператор вывода" acc
        forM_ exprs (\expr -> asttraverse f expr ownAcc)

instance ASTTraversible Syn.Block where
    asttraverse f (Syn.BlockDecl idents typ) acc = do
        ownAcc <- f "Обьявление переменных" acc
        forM_ idents (\ident -> asttraverse f ident ownAcc)
        asttraverse f typ ownAcc
    asttraverse f (Syn.BlockStmt stmt) acc =
        asttraverse f stmt acc

instance ASTTraversible Syn.Program where
    asttraverse f (Syn.Program blocks) acc = do
        ownAcc <- f "Программа" acc
        forM_ blocks (\block -> asttraverse f block ownAcc)
